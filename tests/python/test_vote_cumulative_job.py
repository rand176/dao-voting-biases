import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.preparation.duplicate_ballots as proposal_duplicate_ballot_flags
from dao_bias.jobs import cumulative as vote_cumulative_job


def build_context(proposals, votes, duplicate_proposal_ids=None, issue_proposal_details=None):
    df_proposals = pd.DataFrame(proposals).set_index("id")
    df_votes = pd.DataFrame(votes)
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": df_votes.groupby("proposal"),
        "duplicate_proposal_ids": set(duplicate_proposal_ids or []),
        "issue_proposal_details": dict(issue_proposal_details or {}),
    }


class ProposalDuplicateBallotFlagTests(unittest.TestCase):
    def test_build_duplicate_ballot_outputs_flags_duplicate_proposals_and_wallet_pairs(self):
        df_votes = pd.DataFrame(
            [
                {"proposal": "prop-dup", "voter": "0xaaa", "created": 200, "id": "vote-2"},
                {"proposal": "prop-dup", "voter": "0xaaa", "created": 100, "id": "vote-1"},
                {"proposal": "prop-dup", "voter": "0xbbb", "created": 150, "id": "vote-3"},
                {"proposal": "prop-keep", "voter": "0xccc", "created": 300, "id": "vote-4"},
            ]
        )

        flags, pairs = proposal_duplicate_ballot_flags.build_duplicate_ballot_outputs(
            df_votes,
            proposal_ids=["prop-dup", "prop-keep"],
        )

        dup_flag = flags.loc[flags["id"] == "prop-dup"].iloc[0]
        keep_flag = flags.loc[flags["id"] == "prop-keep"].iloc[0]
        pair = pairs.iloc[0]

        self.assertEqual(pair["proposal"], "prop-dup")
        self.assertEqual(pair["voter"], "0xaaa")
        self.assertEqual(pair["n_ballots"], 2)
        self.assertEqual(pair["first_created"], 100)
        self.assertEqual(pair["last_created"], 200)
        self.assertEqual(pair["first_vote_id"], "vote-1")
        self.assertEqual(pair["last_vote_id"], "vote-2")

        self.assertEqual(dup_flag["flag_duplicate_ballots"], 1)
        self.assertEqual(dup_flag["duplicate_pairs"], 1)
        self.assertEqual(dup_flag["duplicate_ballot_rows"], 2)
        self.assertEqual(dup_flag["duplicate_wallets"], 1)
        self.assertEqual(dup_flag["flag_category"], "duplicate_ballots")
        self.assertEqual(keep_flag["flag_duplicate_ballots"], 0)
        self.assertEqual(keep_flag["duplicate_pairs"], 0)
        self.assertEqual(keep_flag["duplicate_ballot_rows"], 0)
        self.assertEqual(keep_flag["duplicate_wallets"], 0)


class VoteCumulativeJobTests(unittest.TestCase):
    def test_process_proposal_tracks_approval_actual_vp_and_vote_counts(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-approval",
                    "type": "approval",
                    "choices": ["A", "B", "C"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-approval",
                    "id": "vote-1",
                    "voter": "0xaaa",
                    "created": 100,
                    "choice": json.dumps([1, 3]),
                    "vp": 10.0,
                },
                {
                    "proposal": "prop-approval",
                    "id": "vote-2",
                    "voter": "0xbbb",
                    "created": 200,
                    "choice": json.dumps([3]),
                    "vp": 4.0,
                },
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_cumulative_job.process_proposal("prop-approval", context, tmpdir)

            cumulative = pd.read_csv(
                vote_cumulative_job.proposal_output_path(tmpdir, "prop-approval", "cumulative_actuals"),
                header=None,
                names=vote_cumulative_job.OUTPUTS["cumulative_actuals"]["columns"],
            ).sort_values(["vote_pos", "choice_idx"])

        self.assertEqual(cumulative["choice_idx"].tolist(), [1, 3, 3])
        self.assertEqual(cumulative["vp_delta"].tolist(), [10.0, 10.0, 4.0])
        self.assertEqual(cumulative["cum_vp"].tolist(), [10.0, 10.0, 14.0])
        self.assertEqual(cumulative["vote_delta"].tolist(), [1.0, 1.0, 1.0])
        self.assertEqual(cumulative["cum_votes"].tolist(), [1.0, 1.0, 2.0])

    def test_process_proposal_allocates_weighted_vp_proportionally_and_counts_supporters(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-weighted",
                    "type": "weighted",
                    "choices": ["A", "B", "C", "D"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-weighted",
                    "id": "vote-1",
                    "voter": "0xaaa",
                    "created": 100,
                    "choice": json.dumps({"2": 5, "4": 5, "1": 1}),
                    "vp": 7.0,
                },
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_cumulative_job.process_proposal("prop-weighted", context, tmpdir)

            cumulative = pd.read_csv(
                vote_cumulative_job.proposal_output_path(tmpdir, "prop-weighted", "cumulative_actuals"),
                header=None,
                names=vote_cumulative_job.OUTPUTS["cumulative_actuals"]["columns"],
            ).sort_values("choice_idx")

        self.assertEqual(cumulative["choice_idx"].tolist(), [1, 2, 4])
        self.assertEqual(cumulative["vp_delta"].round(6).tolist(), [0.636364, 3.181818, 3.181818])
        self.assertEqual(cumulative["cum_vp"].round(6).tolist(), [0.636364, 3.181818, 3.181818])
        self.assertEqual(cumulative["vote_delta"].tolist(), [1.0, 1.0, 1.0])
        self.assertEqual(cumulative["cum_votes"].tolist(), [1.0, 1.0, 1.0])

    def test_process_proposal_records_skip_for_duplicate_ballot_proposal(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-dup",
                    "type": "single-choice",
                    "choices": ["A", "B"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-dup",
                    "id": "vote-1",
                    "voter": "0xaaa",
                    "created": 100,
                    "choice": "1",
                    "vp": 3.0,
                }
            ],
            duplicate_proposal_ids={"prop-dup"},
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_cumulative_job.process_proposal("prop-dup", context, tmpdir)

            skipped = pd.read_csv(
                vote_cumulative_job.proposal_output_path(tmpdir, "prop-dup", "skipped_proposals"),
                header=None,
                names=vote_cumulative_job.OUTPUTS["skipped_proposals"]["columns"],
            )
            cumulative_path = vote_cumulative_job.proposal_output_path(tmpdir, "prop-dup", "cumulative_actuals")

        self.assertEqual(skipped["reason"].tolist(), ["duplicate_ballots"])
        self.assertFalse(os.path.exists(cumulative_path))

    def test_process_proposal_records_skip_for_invalid_vote_issue_proposal(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-invalid",
                    "type": "single-choice",
                    "choices": ["A", "B"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-invalid",
                    "id": "vote-1",
                    "voter": "0xaaa",
                    "created": 100,
                    "choice": "1",
                    "vp": 3.0,
                }
            ],
            issue_proposal_details={
                "prop-invalid": {
                    "issue_categories": "invalid_votes",
                    "flag_reason": "Contains malformed vote rows in canonical vote-expansion errors",
                }
            },
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_cumulative_job.process_proposal("prop-invalid", context, tmpdir)

            skipped = pd.read_csv(
                vote_cumulative_job.proposal_output_path(tmpdir, "prop-invalid", "skipped_proposals"),
                header=None,
                names=vote_cumulative_job.OUTPUTS["skipped_proposals"]["columns"],
            )
            cumulative_path = vote_cumulative_job.proposal_output_path(tmpdir, "prop-invalid", "cumulative_actuals")

        self.assertEqual(skipped["reason"].tolist(), ["invalid_votes"])
        self.assertIn("malformed vote rows", skipped["detail"].tolist()[0])
        self.assertFalse(os.path.exists(cumulative_path))


if __name__ == "__main__":
    unittest.main()
