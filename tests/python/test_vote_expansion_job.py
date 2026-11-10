import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]

from dao_bias.jobs import votes as vote_expansion_job


def build_context(proposals, votes):
    df_proposals = pd.DataFrame(proposals).set_index("id")
    df_votes = pd.DataFrame(votes)
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": df_votes.groupby("proposal"),
    }


def write_attempt_chunk(worker_dir, proposal_id, mode_name, df, attempt_id, err=False):
    df.to_csv(
        vote_expansion_job.proposal_output_path(
            str(worker_dir),
            proposal_id,
            mode_name,
            err=err,
            attempt_id=attempt_id,
        ),
        index=False,
        header=False,
    )


def write_attempt_marker(worker_dir, proposal_id, attempt_id, completed_at):
    marker_path = vote_expansion_job.latest_attempt_marker_path(str(worker_dir), proposal_id)
    Path(marker_path).write_text(
        json.dumps(
            {
                "proposal": proposal_id,
                "attempt_id": attempt_id,
                "completed_at": completed_at,
            }
        ),
        encoding="utf-8",
    )


class VoteExpansionJobTests(unittest.TestCase):
    def test_process_proposal_splits_multi_approval_ballots_in_favorite_and_support(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-1",
                    "type": "approval",
                    "choices": ["A", "B", "C", "D"],
                }
            ],
            votes=[
                {"proposal": "prop-1", "choice": json.dumps([2, 4])},
                {"proposal": "prop-1", "choice": json.dumps([4])},
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_expansion_job.process_proposal("prop-1", context, tmpdir)

            favorite = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-1", "favorite_choice_counts"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["favorite_choice_counts"]["columns"],
            ).sort_values("choice_idx")
            support = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-1", "support_fraction"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["support_fraction"]["columns"],
            ).sort_values("choice_idx")

            favorite_err_path = vote_expansion_job.proposal_output_path(
                tmpdir, "prop-1", "favorite_choice_counts", err=True
            )

        self.assertEqual(favorite["choice_idx"].tolist(), [2, 4])
        self.assertEqual(favorite["n_votes"].tolist(), [0.5, 1.5])
        self.assertTrue((favorite["tot_votes"] == 2).all())
        self.assertEqual(support["choice_idx"].tolist(), [2, 4])
        self.assertEqual(support["support_fraction_sum"].tolist(), [0.5, 1.5])
        self.assertTrue((support["tot_votes"] == 2).all())
        self.assertFalse(os.path.exists(favorite_err_path))

    def test_process_proposal_deduplicates_repeated_approval_choices_before_allocating_mass(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-2",
                    "type": "approval",
                    "choices": ["A", "B", "C", "D"],
                }
            ],
            votes=[
                {"proposal": "prop-2", "choice": json.dumps([2, 2, 4])},
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_expansion_job.process_proposal("prop-2", context, tmpdir)

            favorite = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-2", "favorite_choice_counts"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["favorite_choice_counts"]["columns"],
            ).sort_values("choice_idx")
            support = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-2", "support_fraction"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["support_fraction"]["columns"],
            ).sort_values("choice_idx")

        self.assertEqual(favorite["choice_idx"].tolist(), [2, 4])
        self.assertEqual(favorite["n_votes"].tolist(), [0.5, 0.5])
        self.assertEqual(support["choice_idx"].tolist(), [2, 4])
        self.assertEqual(support["support_fraction_sum"].tolist(), [0.5, 0.5])

    def test_process_proposal_overwrites_existing_proposal_chunk_instead_of_appending_duplicate_rows(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-3",
                    "type": "single-choice",
                    "choices": ["A", "B"],
                }
            ],
            votes=[
                {"proposal": "prop-3", "choice": 1},
                {"proposal": "prop-3", "choice": 1},
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_expansion_job.process_proposal("prop-3", context, tmpdir)
            vote_expansion_job.process_proposal("prop-3", context, tmpdir)

            favorite = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-3", "favorite_choice_counts"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["favorite_choice_counts"]["columns"],
            )

        self.assertEqual(len(favorite), 1)
        self.assertEqual(favorite["proposal"].tolist(), ["prop-3"])
        self.assertEqual(favorite["n_votes"].tolist(), [2.0])

    def test_process_proposal_clears_stale_error_chunk_before_retry(self):
        bad_context = build_context(
            proposals=[
                {
                    "id": "prop-4",
                    "type": "single-choice",
                    "choices": ["A", "B"],
                }
            ],
            votes=[
                {"proposal": "prop-4", "choice": "bad"},
            ],
        )
        good_context = build_context(
            proposals=[
                {
                    "id": "prop-4",
                    "type": "single-choice",
                    "choices": ["A", "B"],
                }
            ],
            votes=[
                {"proposal": "prop-4", "choice": 1},
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            vote_expansion_job.process_proposal("prop-4", bad_context, tmpdir)
            err_path = vote_expansion_job.proposal_output_path(
                tmpdir,
                "prop-4",
                "favorite_choice_counts",
                err=True,
            )
            self.assertTrue(os.path.exists(err_path))

            vote_expansion_job.process_proposal("prop-4", good_context, tmpdir)

            favorite = pd.read_csv(
                vote_expansion_job.proposal_output_path(tmpdir, "prop-4", "favorite_choice_counts"),
                header=None,
                names=vote_expansion_job.VOTE_MODES["favorite_choice_counts"]["columns"],
            )
            latest_err_exists_after_retry = os.path.exists(
                vote_expansion_job.proposal_output_path(
                    tmpdir,
                    "prop-4",
                    "favorite_choice_counts",
                    err=True,
                )
            )
            old_err_exists_after_retry = os.path.exists(err_path)

        self.assertEqual(favorite["proposal"].tolist(), ["prop-4"])
        self.assertTrue(old_err_exists_after_retry)
        self.assertFalse(latest_err_exists_after_retry)

    def test_reduce_vote_expansion_outputs_filters_finished_proposals(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir = tmp_root / "worker_00"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-1",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-2",
                    "status": "pending",
                    "worker_id": "",
                    "started_at": "",
                    "finished_at": "",
                    "error_count": "0",
                },
            ]

            favorite_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "favorite_choice_counts",
                    },
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "favorite_choice_counts",
                    },
                    {
                        "proposal": "prop-2",
                        "choice_idx": 3,
                        "n_votes": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            support_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "support_fraction_sum": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "support_fraction",
                    },
                    {
                        "proposal": "prop-2",
                        "choice_idx": 3,
                        "support_fraction_sum": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "support_fraction",
                    },
                ]
            )

            write_attempt_chunk(
                worker_dir,
                "prop-1",
                "favorite_choice_counts",
                favorite_rows.loc[favorite_rows["proposal"] == "prop-1"].head(1),
                attempt_id="attempt-1",
            )
            write_attempt_chunk(
                worker_dir,
                "prop-1",
                "support_fraction",
                support_rows,
                attempt_id="attempt-1",
            )
            write_attempt_chunk(
                worker_dir,
                "prop-2",
                "favorite_choice_counts",
                favorite_rows.loc[favorite_rows["proposal"] == "prop-2"],
                attempt_id="attempt-2",
            )
            write_attempt_chunk(
                worker_dir,
                "prop-2",
                "support_fraction",
                support_rows.loc[support_rows["proposal"] == "prop-2"],
                attempt_id="attempt-2",
            )
            write_attempt_marker(worker_dir, "prop-1", "attempt-1", "2026-04-16T10:01:00+00:00")
            write_attempt_marker(worker_dir, "prop-2", "attempt-2", "2026-04-16T10:02:00+00:00")

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            favorite_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_FILE)
            support_out = pd.read_csv(output_dir / vote_expansion_job.SUPPORT_FILE)

        self.assertEqual(favorite_out["proposal"].tolist(), ["prop-1"])
        self.assertEqual(favorite_out["n_votes"].tolist(), [2.0])
        self.assertEqual(support_out["proposal"].tolist(), ["prop-1"])
        self.assertEqual(summary["favorite_choice_counts"]["rows"], 1)
        self.assertEqual(summary["support_fraction"]["rows"], 1)

    def test_reduce_vote_expansion_outputs_recovers_completed_worker_rows_when_manifest_is_still_running(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir = tmp_root / "worker_00"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-1",
                    "status": "running",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "",
                    "error_count": "0",
                },
            ]

            favorite_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            support_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "support_fraction_sum": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "support_fraction",
                    },
                ]
            )

            write_attempt_chunk(worker_dir, "prop-1", "favorite_choice_counts", favorite_rows, attempt_id="attempt-1")
            write_attempt_chunk(worker_dir, "prop-1", "support_fraction", support_rows, attempt_id="attempt-1")
            write_attempt_marker(worker_dir, "prop-1", "attempt-1", "2026-04-16T10:01:00+00:00")
            (worker_dir / "worker_progress.json").write_text(
                json.dumps(
                    {
                        "completed": ["prop-1"],
                        "failed": [],
                        "updated_at": "2026-04-16T10:01:00+00:00",
                        "last_error_count": 0,
                    }
                ),
                encoding="utf-8",
            )

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            favorite_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_FILE)
            support_out = pd.read_csv(output_dir / vote_expansion_job.SUPPORT_FILE)

        self.assertEqual(favorite_out["proposal"].tolist(), ["prop-1"])
        self.assertEqual(support_out["proposal"].tolist(), ["prop-1"])
        self.assertEqual(summary["finished_proposals"], 1)

    def test_reduce_vote_expansion_outputs_excludes_duplicate_and_invalid_vote_proposals(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir = tmp_root / "worker_00"
            proposal_flags_dir = Path(tmpdir) / "proposal_flags"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir.mkdir(parents=True, exist_ok=True)
            proposal_flags_dir.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-good",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-invalid",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-dup",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
            ]

            favorite_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-good",
                        "choice_idx": 1,
                        "n_votes": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "favorite_choice_counts",
                    },
                    {
                        "proposal": "prop-invalid",
                        "choice_idx": 2,
                        "n_votes": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "favorite_choice_counts",
                    },
                    {
                        "proposal": "prop-dup",
                        "choice_idx": 3,
                        "n_votes": 4.0,
                        "tot_votes": 4,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            support_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-good",
                        "choice_idx": 1,
                        "support_fraction_sum": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "support_fraction",
                    },
                    {
                        "proposal": "prop-invalid",
                        "choice_idx": 2,
                        "support_fraction_sum": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "support_fraction",
                    },
                    {
                        "proposal": "prop-dup",
                        "choice_idx": 3,
                        "support_fraction_sum": 4.0,
                        "tot_votes": 4,
                        "vote_mode": "support_fraction",
                    },
                ]
            )
            invalid_err_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-invalid",
                        "vote_mode": "favorite_choice_counts",
                        "type": "single-choice",
                        "reason": "invalid choice_idx",
                        "detail": "",
                        "choice": "'bad'",
                    },
                ]
            )

            for proposal_id in ["prop-good", "prop-invalid", "prop-dup"]:
                attempt_id = f"{proposal_id}-attempt"
                write_attempt_chunk(
                    worker_dir,
                    proposal_id,
                    "favorite_choice_counts",
                    favorite_rows.loc[favorite_rows["proposal"] == proposal_id],
                    attempt_id=attempt_id,
                )
                write_attempt_chunk(
                    worker_dir,
                    proposal_id,
                    "support_fraction",
                    support_rows.loc[support_rows["proposal"] == proposal_id],
                    attempt_id=attempt_id,
                )
                if proposal_id == "prop-invalid":
                    write_attempt_chunk(
                        worker_dir,
                        proposal_id,
                        "favorite_choice_counts",
                        invalid_err_rows,
                        attempt_id=attempt_id,
                        err=True,
                    )
                write_attempt_marker(
                    worker_dir,
                    proposal_id,
                    attempt_id,
                    "2026-04-16T10:01:00+00:00",
                )

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            duplicate_flags = pd.DataFrame(
                [
                    {
                        "id": "prop-good",
                        "flag_duplicate_ballots": 0,
                        "duplicate_pairs": 0,
                        "duplicate_ballot_rows": 0,
                        "duplicate_wallets": 0,
                        "flag_category": "not_flagged",
                        "flag_reason": "clean",
                    },
                    {
                        "id": "prop-invalid",
                        "flag_duplicate_ballots": 0,
                        "duplicate_pairs": 0,
                        "duplicate_ballot_rows": 0,
                        "duplicate_wallets": 0,
                        "flag_category": "not_flagged",
                        "flag_reason": "clean",
                    },
                    {
                        "id": "prop-dup",
                        "flag_duplicate_ballots": 1,
                        "duplicate_pairs": 2,
                        "duplicate_ballot_rows": 4,
                        "duplicate_wallets": 2,
                        "flag_category": "duplicate_ballots",
                        "flag_reason": "duplicate",
                    },
                ]
            )
            duplicate_flags_path = proposal_flags_dir / "dao_bias.preparation.duplicate_ballots.csv.gz"
            duplicate_flags.to_csv(duplicate_flags_path, index=False, compression="gzip")

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")), \
                 mock.patch.object(vote_expansion_job, "PROPOSAL_DUPLICATE_BALLOT_FLAGS_PATH", str(duplicate_flags_path)), \
                 mock.patch.object(vote_expansion_job, "PROPOSAL_VOTE_ISSUE_FLAGS_PATH", str(proposal_flags_dir / "dao_bias.preparation.vote_issues.csv.gz")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            favorite_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_FILE)
            support_out = pd.read_csv(output_dir / vote_expansion_job.SUPPORT_FILE)
            issue_flags = pd.read_csv(proposal_flags_dir / "dao_bias.preparation.vote_issues.csv.gz")

        self.assertEqual(favorite_out["proposal"].tolist(), ["prop-good"])
        self.assertEqual(support_out["proposal"].tolist(), ["prop-good"])
        self.assertEqual(summary["favorite_choice_counts"]["rows"], 1)
        self.assertEqual(summary["support_fraction"]["rows"], 1)
        self.assertEqual(summary["skipped_proposals"]["duplicate_ballots"], 1)
        self.assertEqual(summary["skipped_proposals"]["invalid_votes"], 1)
        self.assertEqual(
            set(issue_flags.loc[issue_flags["flag_any_vote_issues"] == 1, "id"].tolist()),
            {"prop-dup", "prop-invalid"},
        )

    def test_reduce_vote_expansion_outputs_preserves_identical_repeated_error_rows(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir = tmp_root / "worker_00"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-1",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
            ]

            err_rows = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "vote_mode": "favorite_choice_counts",
                        "type": "approval",
                        "reason": "invalid approval choice",
                        "detail": "",
                        "choice": "'x'",
                    },
                    {
                        "proposal": "prop-1",
                        "vote_mode": "favorite_choice_counts",
                        "type": "approval",
                        "reason": "invalid approval choice",
                        "detail": "",
                        "choice": "'x'",
                    },
                ]
            )
            write_attempt_chunk(
                worker_dir,
                "prop-1",
                "favorite_choice_counts",
                err_rows,
                attempt_id="attempt-1",
                err=True,
            )
            write_attempt_marker(worker_dir, "prop-1", "attempt-1", "2026-04-16T10:01:00+00:00")

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            err_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_ERR_FILE)

        self.assertEqual(len(err_out), 2)
        self.assertEqual(summary["favorite_choice_counts"]["error_rows"], 2)

    def test_reduce_vote_expansion_outputs_uses_one_worker_attempt_for_data_and_errors(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir_0 = tmp_root / "worker_00"
            worker_dir_1 = tmp_root / "worker_01"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir_0.mkdir(parents=True, exist_ok=True)
            worker_dir_1.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-1",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
            ]

            old_data = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            old_err = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "vote_mode": "favorite_choice_counts",
                        "type": "single-choice",
                        "reason": "invalid choice_idx",
                        "detail": "",
                        "choice": "'bad'",
                    },
                ]
            )
            new_data = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 2.0,
                        "tot_votes": 2,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            write_attempt_chunk(worker_dir_0, "prop-1", "favorite_choice_counts", old_data, attempt_id="attempt-old")
            write_attempt_chunk(
                worker_dir_0,
                "prop-1",
                "favorite_choice_counts",
                old_err,
                attempt_id="attempt-old",
                err=True,
            )
            write_attempt_marker(worker_dir_0, "prop-1", "attempt-old", "2026-04-16T10:01:00+00:00")
            write_attempt_chunk(worker_dir_1, "prop-1", "favorite_choice_counts", new_data, attempt_id="attempt-new")
            write_attempt_marker(worker_dir_1, "prop-1", "attempt-new", "2026-04-16T10:02:00+00:00")

            (worker_dir_1 / "worker_progress.json").write_text(
                json.dumps(
                    {
                        "completed": ["prop-1"],
                        "failed": [],
                        "updated_at": "2026-04-16T10:02:00+00:00",
                        "last_error_count": 0,
                    }
                ),
                encoding="utf-8",
            )

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            favorite_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_FILE)
            err_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_ERR_FILE)

        self.assertEqual(favorite_out["n_votes"].tolist(), [2.0])
        self.assertEqual(len(err_out), 0)
        self.assertEqual(summary["favorite_choice_counts"]["error_rows"], 0)

    def test_reduce_vote_expansion_outputs_ignores_newer_uncommitted_partial_attempt(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            worker_dir = tmp_root / "worker_00"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            worker_dir.mkdir(parents=True, exist_ok=True)

            manifest_rows = [
                {
                    "job_name": vote_expansion_job.JOB_NAME,
                    "work_unit_id": "prop-1",
                    "status": "finished",
                    "worker_id": "0",
                    "started_at": "2026-04-16T10:00:00+00:00",
                    "finished_at": "2026-04-16T10:01:00+00:00",
                    "error_count": "0",
                },
            ]

            complete_data = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 1.0,
                        "tot_votes": 1,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )
            partial_newer_data = pd.DataFrame(
                [
                    {
                        "proposal": "prop-1",
                        "choice_idx": 1,
                        "n_votes": 3.0,
                        "tot_votes": 3,
                        "vote_mode": "favorite_choice_counts",
                    },
                ]
            )

            write_attempt_chunk(worker_dir, "prop-1", "favorite_choice_counts", complete_data, attempt_id="attempt-old")
            write_attempt_marker(worker_dir, "prop-1", "attempt-old", "2026-04-16T10:01:00+00:00")
            write_attempt_chunk(
                worker_dir,
                "prop-1",
                "favorite_choice_counts",
                partial_newer_data,
                attempt_id="attempt-newer-uncommitted",
            )

            run_meta_path = manifest_dir / f"{vote_expansion_job.JOB_NAME}_run.json"
            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": vote_expansion_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            import dao_bias.jobs.batch as batch_runner

            batch_runner.write_manifest_rows(
                str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                manifest_rows,
            )

            with mock.patch.object(vote_expansion_job, "RUN_META_PATH", str(run_meta_path)), \
                 mock.patch.object(vote_expansion_job, "RUN_SUMMARY_PATH", str(output_dir / "run_summary.json")):
                summary = vote_expansion_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_dir / f"{vote_expansion_job.JOB_NAME}_manifest.csv"),
                )

            favorite_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_FILE)
            err_out = pd.read_csv(output_dir / vote_expansion_job.FAVORITE_ERR_FILE)

        self.assertEqual(favorite_out["n_votes"].tolist(), [1.0])
        self.assertEqual(len(err_out), 0)
        self.assertEqual(summary["favorite_choice_counts"]["error_rows"], 0)


if __name__ == "__main__":
    unittest.main()
