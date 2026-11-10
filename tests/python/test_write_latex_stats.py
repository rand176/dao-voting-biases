import csv
import gzip
import json
import re
import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.reporting.counts as write_latex_stats


def write_csv(path, rows, fieldnames):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def write_gzip_csv(path, rows, fieldnames):
    path.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(path, "wt", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def write_json(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload), encoding="utf-8")


class WriteLatexStatsTests(unittest.TestCase):
    def build_fixture(self, tmpdir):
        root = Path(tmpdir)
        analysis_root = root / "analysis"
        paper_root = root / "paper"

        write_csv(
            analysis_root / "metadata/analysis_data_releases.csv",
            [
                {
                    "release_name": "current",
                    "relative_path": "data/2024/derived/active/current.csv",
                    "is_frozen": "FALSE",
                    "notes": "test release",
                },
                {
                    "release_name": "candidate",
                    "relative_path": "data/2024/derived/active/candidate.csv",
                    "is_frozen": "FALSE",
                    "notes": "normalized stage-5 candidate",
                }
            ],
            ["release_name", "relative_path", "is_frozen", "notes"],
        )
        write_gzip_csv(
            analysis_root / "data/2024/raw/proposals_0.csv.gz",
            [
                {"id": "prop-1", "space": "space-a", "choices": '["Yes", "No"]', "flagged": "0"},
                {"id": "prop-2", "space": "space-b", "choices": '["A"]', "flagged": "1"},
                {"id": "prop-3", "space": "space-c", "choices": '["A"]', "flagged": "1"},
            ],
            ["id", "space", "choices", "flagged"],
        )
        write_gzip_csv(
            analysis_root / "data/2024/raw/votes/votes_0.csv.gz",
            [
                {"proposal": "prop-1", "choice": "1"},
                {"proposal": "prop-1", "choice": "2"},
                {"proposal": "prop-2", "choice": "[1]"},
            ],
            ["proposal", "choice"],
        )
        write_gzip_csv(
            analysis_root / "data/2024/raw/votes/votes_1.csv.gz",
            [{"proposal": "prop-2", "choice": "[1]"}],
            ["proposal", "choice"],
        )
        write_csv(
            analysis_root / "data/2024/derived/active/current.csv",
            [
                {
                    "proposal": "prop-1",
                    "space": "space-a",
                    "type": "single-choice",
                    "scores_total": "10",
                    "n_votes": "6",
                    "flagged": "0",
                    "stance": "APPROVE",
                },
                {
                    "proposal": "prop-1",
                    "space": "space-a",
                    "type": "single-choice",
                    "scores_total": "10",
                    "n_votes": "4",
                    "flagged": "0",
                    "stance": "REJECT",
                },
                {
                    "proposal": "prop-2",
                    "space": "space-b",
                    "type": "approval",
                    "scores_total": "5",
                    "n_votes": "2",
                    "flagged": "1",
                    "stance": "OTHER",
                },
            ],
            ["proposal", "space", "type", "scores_total", "n_votes", "flagged", "stance"],
        )
        write_csv(
            analysis_root / "data/2024/derived/active/candidate.csv",
            [
                {
                    "proposal": "prop-a",
                    "choice_idx": "1",
                    "prop_chid": "prop-a1",
                    "space": "space-a",
                    "type": "single-choice",
                    "scores_total": "10",
                    "n_votes": "6",
                    "flagged": "0",
                    "stance": "APPROVE",
                    "stance_oai": "APPROVE",
                    "suggested_stance_oai": "APPROVE",
                    "effect_oai": "E1",
                    "stance_llama": "APPROVE",
                    "suggested_stance_llama": "APPROVE",
                    "effect_llama": "E1",
                    "stance_chatgpt": "APPROVE",
                    "suggested_stance_chatgpt": "APPROVE",
                    "effect_chatgpt": "E1",
                    "stance_llama33": "APPROVE",
                    "suggested_stance_llama33": "APPROVE",
                    "effect_llama33": "E1",
                    "stance_hl": "APPROVE",
                    "suggested_stance_hl": "APPROVE",
                    "n_evaluations": "2",
                    "n_non_low_effort_evaluations": "2",
                    "n_low_effort_evaluations": "0",
                    "low_effort_only": "FALSE",
                },
                {
                    "proposal": "prop-a",
                    "choice_idx": "2",
                    "prop_chid": "prop-a2",
                    "space": "space-a",
                    "type": "single-choice",
                    "scores_total": "10",
                    "n_votes": "4",
                    "flagged": "0",
                    "stance": "REJECT",
                    "stance_oai": "REJECT",
                    "suggested_stance_oai": "REJECT",
                    "effect_oai": "E1",
                    "stance_llama": "",
                    "suggested_stance_llama": "",
                    "effect_llama": "",
                    "stance_chatgpt": "",
                    "suggested_stance_chatgpt": "",
                    "effect_chatgpt": "",
                    "stance_llama33": "REJECT",
                    "suggested_stance_llama33": "REJECT",
                    "effect_llama33": "E1",
                    "stance_hl": "REJECT",
                    "suggested_stance_hl": "REJECT",
                    "n_evaluations": "1",
                    "n_non_low_effort_evaluations": "0",
                    "n_low_effort_evaluations": "1",
                    "low_effort_only": "TRUE",
                },
                {
                    "proposal": "prop-b",
                    "choice_idx": "1",
                    "prop_chid": "prop-b1",
                    "space": "space-b",
                    "type": "approval",
                    "scores_total": "5",
                    "n_votes": "2",
                    "flagged": "1",
                    "stance": "ABSTAIN",
                    "stance_oai": "",
                    "suggested_stance_oai": "",
                    "effect_oai": "",
                    "stance_llama": "ABSTAIN",
                    "suggested_stance_llama": "ABSTAIN",
                    "effect_llama": "E2",
                    "stance_chatgpt": "ABSTAIN",
                    "suggested_stance_chatgpt": "ABSTAIN",
                    "effect_chatgpt": "E2",
                    "stance_llama33": "",
                    "suggested_stance_llama33": "",
                    "effect_llama33": "",
                    "stance_hl": "",
                    "suggested_stance_hl": "",
                    "n_evaluations": "1",
                    "n_non_low_effort_evaluations": "1",
                    "n_low_effort_evaluations": "0",
                    "low_effort_only": "FALSE",
                },
                {
                    "proposal": "prop-c",
                    "choice_idx": "1",
                    "prop_chid": "prop-c1",
                    "space": "space-c",
                    "type": "weighted",
                    "scores_total": "7",
                    "n_votes": "3",
                    "flagged": "0",
                    "stance": "APPROVE",
                    "stance_oai": "APPROVE",
                    "suggested_stance_oai": "APPROVE",
                    "effect_oai": "E3",
                    "stance_llama": "",
                    "suggested_stance_llama": "",
                    "effect_llama": "",
                    "stance_chatgpt": "REJECT",
                    "suggested_stance_chatgpt": "REJECT",
                    "effect_chatgpt": "E3",
                    "stance_llama33": "APPROVE",
                    "suggested_stance_llama33": "APPROVE",
                    "effect_llama33": "E3",
                    "stance_hl": "APPROVE",
                    "suggested_stance_hl": "APPROVE",
                    "n_evaluations": "3",
                    "n_non_low_effort_evaluations": "2",
                    "n_low_effort_evaluations": "1",
                    "low_effort_only": "FALSE",
                },
                {
                    "proposal": "prop-c",
                    "choice_idx": "2",
                    "prop_chid": "prop-c2",
                    "space": "space-c",
                    "type": "weighted",
                    "scores_total": "7",
                    "n_votes": "1",
                    "flagged": "0",
                    "stance": "REJECT",
                    "stance_oai": "",
                    "suggested_stance_oai": "",
                    "effect_oai": "",
                    "stance_llama": "",
                    "suggested_stance_llama": "",
                    "effect_llama": "",
                    "stance_chatgpt": "REJECT",
                    "suggested_stance_chatgpt": "REJECT",
                    "effect_chatgpt": "E4",
                    "stance_llama33": "",
                    "suggested_stance_llama33": "",
                    "effect_llama33": "",
                    "stance_hl": "",
                    "suggested_stance_hl": "",
                    "n_evaluations": "1",
                    "n_non_low_effort_evaluations": "0",
                    "n_low_effort_evaluations": "1",
                    "low_effort_only": "TRUE",
                },
            ],
            [
                "proposal",
                "choice_idx",
                "prop_chid",
                "space",
                "type",
                "scores_total",
                "n_votes",
                "flagged",
                "stance",
                "stance_oai",
                "suggested_stance_oai",
                "effect_oai",
                "stance_llama",
                "suggested_stance_llama",
                "effect_llama",
                "stance_chatgpt",
                "suggested_stance_chatgpt",
                "effect_chatgpt",
                "stance_llama33",
                "suggested_stance_llama33",
                "effect_llama33",
                "stance_hl",
                "suggested_stance_hl",
                "n_evaluations",
                "n_non_low_effort_evaluations",
                "n_low_effort_evaluations",
                "low_effort_only",
            ],
        )
        write_csv(
            analysis_root / "data/2024/humanlabels/dao_rnd_merged.csv",
            [
                {"proposal": "prop-a", "body_lang": "en", "title_lang": "en", "choices_len": "2"},
                {"proposal": "prop-a", "body_lang": "en", "title_lang": "en", "choices_len": "2"},
                {"proposal": "prop-b", "body_lang": "zh-cn", "title_lang": "zh-cn", "choices_len": "1"},
            ],
            ["proposal", "body_lang", "title_lang", "choices_len"],
        )
        write_csv(
            analysis_root / "data/2024/clean_human_label_outputs/human_label_evaluator_labels.csv",
            [
                {"proposal": "prop-a", "evaluator_id": "alice"},
                {"proposal": "prop-a", "evaluator_id": "bob"},
                {"proposal": "prop-a", "evaluator_id": "carol"},
                {"proposal": "prop-b", "evaluator_id": "alice"},
                {"proposal": "prop-b", "evaluator_id": "bob"},
            ],
            ["proposal", "evaluator_id"],
        )
        room_dir = analysis_root / "data/2024/humanlabels/rooms/room000001"
        room_dir.mkdir(parents=True, exist_ok=True)
        room_events = [
            {"stepId": "loadnews", "player": "alice", "news": "prop-a", "time": 300000},
            {"stepId": "feedback", "player": "alice", "time": 120000},
            {"stepId": "demo_initial", "player": "alice", "time": 180000},
            {"stepId": "loadnews", "player": "bob", "news": "prop-b", "time": 120000},
            {"stepId": "feedback", "player": "bob", "time": 60000},
            {"stepId": "demo_initial", "player": "bob", "time": 60000},
        ]
        room_dir.joinpath("news.ndjson").write_text(
            "\n".join(json.dumps(event) for event in room_events) + "\n",
            encoding="utf-8",
        )
        write_gzip_csv(
            analysis_root / "data/2024/clean_proposal_flags/proposal_vote_issue_flags.csv.gz",
            [
                {
                    "id": "prop-1",
                    "flag_duplicate_ballots": "0",
                    "flag_invalid_votes": "0",
                    "flag_any_vote_issues": "0",
                },
                {
                    "id": "prop-2",
                    "flag_duplicate_ballots": "1",
                    "flag_invalid_votes": "1",
                    "flag_any_vote_issues": "1",
                },
            ],
            ["id", "flag_duplicate_ballots", "flag_invalid_votes", "flag_any_vote_issues"],
        )
        write_csv(
            analysis_root / "data/2024/derived/debug/stage3_vote_merge_audit.csv",
            [
                {"phase": "load_proposals", "metric": "final_proposals", "value": "2", "detail": ""},
                {"phase": "apply_stage_exclusions", "metric": "proposals_removed", "value": "1", "detail": ""},
                {"phase": "remove_invalid_choice_idx", "metric": "rows_removed", "value": "3", "detail": ""},
                {"phase": "remove_invalid_choice_idx", "metric": "invalid_vote_mass", "value": "4.5", "detail": ""},
                {"phase": "final_output", "metric": "rows", "value": "3", "detail": ""},
                {"phase": "final_output", "metric": "proposals", "value": "2", "detail": ""},
            ],
            ["phase", "metric", "value", "detail"],
        )
        write_json(
            analysis_root / "data/2024/clean_vote_outputs/run_summary.json",
            {
                "finished_proposals": 2,
                "favorite_choice_counts": {"rows": 3, "error_rows": 1},
                "support_fraction": {"rows": 3, "error_rows": 1},
            },
        )
        write_json(
            analysis_root / "data/2024/clean_author_vote_outputs/run_summary.json",
            {
                "finished_proposals": 2,
                "favorite_choice": {"rows": 2, "error_rows": 0},
                "support_fraction": {"rows": 2, "error_rows": 0},
            },
        )
        write_csv(
            analysis_root / "metadata/exceptions/stance_choice_overrides.csv",
            [],
            ["stage", "proposal", "stance_sequence", "source_line", "note"],
        )
        write_csv(
            analysis_root / "metadata/exceptions/stance_one_sided_exceptions.csv",
            [],
            ["stage", "proposal", "allowed_state", "source_line", "note"],
        )
        return analysis_root, paper_root

    def test_writes_valid_latex_macro_commands(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, paper_root = self.build_fixture(tmpdir)

            write_latex_stats.generate_latex_counts(
                release="current",
                paper_root=paper_root,
                analysis_root=analysis_root,
            )

            macro_text = (paper_root / "sections/macros/analysis_counts.tex").read_text(encoding="utf-8")
            command_lines = [
                line for line in macro_text.splitlines() if line and not line.startswith("%")
            ]
            self.assertTrue(command_lines)
            for line in command_lines:
                self.assertRegex(line, r"^\\newcommand\{\\DaoBias[A-Za-z]+\}\{.+\}$")

    def test_generated_counts_match_current_release_and_report_flagged_proposals(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, paper_root = self.build_fixture(tmpdir)

            rows = write_latex_stats.generate_latex_counts(
                release="current",
                paper_root=paper_root,
                analysis_root=analysis_root,
            )
            values = {row.macro: str(row.value) for row in rows}

            self.assertEqual(values["DaoBiasRawProposalN"], "3")
            self.assertEqual(values["DaoBiasRawSpaceN"], "3")
            self.assertEqual(values["DaoBiasRawChoiceN"], "4")
            self.assertEqual(values["DaoBiasRawSnapshotFlaggedProposalN"], "2")
            self.assertEqual(values["DaoBiasRawVoteN"], "4")
            self.assertEqual(values["DaoBiasFinalProposalN"], "1")
            self.assertEqual(values["DaoBiasFinalSpaceN"], "1")
            self.assertEqual(values["DaoBiasFinalChoiceN"], "2")
            self.assertEqual(values["DaoBiasFinalVoteMassN"], "10")
            self.assertEqual(values["DaoBiasDatasetSpaceRemovedN"], "2")
            self.assertEqual(values["DaoBiasDatasetSpaceReductionPct"], "66.67")
            self.assertEqual(values["DaoBiasDatasetProposalRemovedN"], "2")
            self.assertEqual(values["DaoBiasDatasetProposalReductionPct"], "66.67")
            self.assertEqual(values["DaoBiasDatasetChoiceRemovedN"], "2")
            self.assertEqual(values["DaoBiasDatasetChoiceReductionPct"], "50")
            self.assertEqual(values["DaoBiasDatasetVoteRemovedN"], "-6")
            self.assertEqual(values["DaoBiasDatasetVoteReductionPct"], "-150")
            self.assertEqual(values["DaoBiasFinalFlaggedProposalN"], "0")
            self.assertEqual(values["DaoBiasExcludedSnapshotFlaggedProposalN"], "1")
            self.assertEqual(values["DaoBiasExcludedSnapshotFlaggedProposalPct"], "50")
            self.assertEqual(values["DaoBiasExcludedSingleOptionProposalN"], "0")
            self.assertEqual(values["DaoBiasExcludedSingleOptionChoiceN"], "0")
            self.assertEqual(values["DaoBiasSinglePreferenceProposalPct"], "100")
            self.assertEqual(values["DaoBiasFinalVoteIssueProposalN"], "0")

            provenance = json.loads(
                (analysis_root / "metadata/analysis_data_provenance/latex_counts_current.json").read_text(
                    encoding="utf-8"
                )
            )
            self.assertEqual(provenance["counts"]["DaoBiasFinalFlaggedProposalN"]["value"], 0)
            self.assertEqual(provenance["counts"]["DaoBiasExcludedSnapshotFlaggedProposalPct"]["value"], "50")
            provenance_csv = (
                analysis_root / "metadata/analysis_data_provenance/latex_counts_current.csv"
            ).read_bytes()
            self.assertNotIn(b"\r\n", provenance_csv)

    def test_release_stats_exclude_single_option_proposals_and_count_all_choice_rows(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            release_path = Path(tmpdir) / "release.csv"
            write_csv(
                release_path,
                [
                    {
                        "proposal": "prop-two",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "10",
                        "n_votes": "6",
                        "flagged": "0",
                        "stance": "APPROVE",
                    },
                    {
                        "proposal": "prop-two",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "10",
                        "n_votes": "4",
                        "flagged": "0",
                        "stance": "REJECT",
                    },
                    {
                        "proposal": "prop-three",
                        "space": "space-b",
                        "type": "weighted",
                        "scores_total": "7",
                        "n_votes": "3",
                        "flagged": "0",
                        "stance": "APPROVE",
                    },
                    {
                        "proposal": "prop-three",
                        "space": "space-b",
                        "type": "weighted",
                        "scores_total": "7",
                        "n_votes": "2",
                        "flagged": "0",
                        "stance": "REJECT",
                    },
                    {
                        "proposal": "prop-three",
                        "space": "space-b",
                        "type": "weighted",
                        "scores_total": "7",
                        "n_votes": "1",
                        "flagged": "0",
                        "stance": "ABSTAIN",
                    },
                    {
                        "proposal": "prop-one",
                        "space": "space-c",
                        "type": "single-choice",
                        "scores_total": "5",
                        "n_votes": "5",
                        "flagged": "0",
                        "stance": "OTHER",
                    },
                ],
                ["proposal", "space", "type", "scores_total", "n_votes", "flagged", "stance"],
            )

            stats = write_latex_stats.read_release_stats(release_path, {})

            self.assertEqual(stats["final_proposals"], 2)
            self.assertEqual(stats["final_choices"], 5)
            self.assertEqual(stats["excluded_single_option_proposals"], 1)
            self.assertEqual(stats["excluded_single_option_choices"], 1)
            self.assertEqual(stats["single_preference_proposals"], 1)
            self.assertEqual(stats["single_preference_choices"], 2)
            self.assertEqual(stats["multi_preference_proposals"], 1)
            self.assertEqual(stats["multi_preference_choices"], 3)

    def test_current_release_writes_llm_and_suggested_stance_macros(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, paper_root = self.build_fixture(tmpdir)
            release_path = analysis_root / "data/2024/derived/active/current.csv"
            fieldnames = [
                "proposal",
                "space",
                "type",
                "scores_total",
                "n_votes",
                "flagged",
                "stance",
                "choice_won",
                "choice_won_no_author_vp",
                "stance_oai",
                "suggested_stance_oai",
                "effect_oai",
                "stance_llama",
                "suggested_stance_llama",
                "effect_llama",
                "stance_chatgpt",
                "suggested_stance_chatgpt",
                "effect_chatgpt",
                "stance_llama33",
                "suggested_stance_llama33",
                "effect_llama33",
                "source_file_llama33",
                "llama33_reviewed",
                "llama33_not_reviewed",
                "llama33_label_conflict",
                "llama33_basic_proposal_exclusion",
                "stance_hl",
                "suggested_stance_hl",
                "n_evaluations",
                "n_non_low_effort_evaluations",
                "n_low_effort_evaluations",
                "low_effort_only",
            ]
            write_csv(
                release_path,
                [
                    {
                        "proposal": "prop-a",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "10",
                        "n_votes": "6",
                        "flagged": "0",
                        "stance": "APPROVE",
                        "choice_won": "1",
                        "choice_won_no_author_vp": "1",
                        "stance_oai": "APPROVE",
                        "suggested_stance_oai": "APPROVE",
                        "effect_oai": "E1",
                        "stance_llama": "APPROVE",
                        "suggested_stance_llama": "APPROVE",
                        "effect_llama": "E1",
                        "stance_chatgpt": "APPROVE",
                        "suggested_stance_chatgpt": "",
                        "effect_chatgpt": "E1",
                        "stance_llama33": "APPROVE",
                        "suggested_stance_llama33": "",
                        "effect_llama33": "E1",
                        "source_file_llama33": "fixture.csv",
                        "llama33_reviewed": "TRUE",
                        "llama33_not_reviewed": "FALSE",
                        "llama33_label_conflict": "FALSE",
                        "llama33_basic_proposal_exclusion": "FALSE",
                        "stance_hl": "APPROVE",
                        "suggested_stance_hl": "APPROVE",
                        "n_evaluations": "1",
                        "n_non_low_effort_evaluations": "1",
                        "n_low_effort_evaluations": "0",
                        "low_effort_only": "FALSE",
                    },
                    {
                        "proposal": "prop-a",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "10",
                        "n_votes": "4",
                        "flagged": "0",
                        "stance": "REJECT",
                        "choice_won": "0",
                        "choice_won_no_author_vp": "0",
                        "stance_oai": "REJECT",
                        "suggested_stance_oai": "",
                        "effect_oai": "E1",
                        "stance_llama": "REJECT",
                        "suggested_stance_llama": "",
                        "effect_llama": "E1",
                        "stance_chatgpt": "REJECT",
                        "suggested_stance_chatgpt": "",
                        "effect_chatgpt": "E1",
                        "stance_llama33": "REJECT",
                        "suggested_stance_llama33": "",
                        "effect_llama33": "E1",
                        "source_file_llama33": "fixture.csv",
                        "llama33_reviewed": "TRUE",
                        "llama33_not_reviewed": "FALSE",
                        "llama33_label_conflict": "FALSE",
                        "llama33_basic_proposal_exclusion": "FALSE",
                        "stance_hl": "REJECT",
                        "suggested_stance_hl": "",
                        "n_evaluations": "1",
                        "n_non_low_effort_evaluations": "1",
                        "n_low_effort_evaluations": "0",
                        "low_effort_only": "FALSE",
                    },
                    {
                        "proposal": "prop-b",
                        "space": "space-b",
                        "type": "approval",
                        "scores_total": "5",
                        "n_votes": "2",
                        "flagged": "1",
                        "stance": "OTHER",
                        "choice_won": "1",
                        "choice_won_no_author_vp": "1",
                        "stance_oai": "OTHER",
                        "suggested_stance_oai": "APPROVE",
                        "effect_oai": "E2",
                        "stance_llama": "OTHER",
                        "suggested_stance_llama": "",
                        "effect_llama": "E2",
                        "stance_chatgpt": "OTHER",
                        "suggested_stance_chatgpt": "APPROVE",
                        "effect_chatgpt": "E2",
                        "stance_llama33": "OTHER",
                        "suggested_stance_llama33": "APPROVE",
                        "effect_llama33": "E2",
                        "source_file_llama33": "fixture.csv",
                        "llama33_reviewed": "TRUE",
                        "llama33_not_reviewed": "FALSE",
                        "llama33_label_conflict": "FALSE",
                        "llama33_basic_proposal_exclusion": "FALSE",
                        "stance_hl": "OTHER",
                        "suggested_stance_hl": "APPROVE",
                        "n_evaluations": "1",
                        "n_non_low_effort_evaluations": "1",
                        "n_low_effort_evaluations": "0",
                        "low_effort_only": "FALSE",
                    },
                    {
                        "proposal": "prop-d",
                        "space": "space-d",
                        "type": "single-choice",
                        "scores_total": "8",
                        "n_votes": "5",
                        "flagged": "0",
                        "stance": "APPROVE",
                        "choice_won": "1",
                        "choice_won_no_author_vp": "0",
                        "stance_oai": "APPROVE",
                        "suggested_stance_oai": "REJECT",
                        "effect_oai": "E3",
                        "stance_llama": "APPROVE",
                        "suggested_stance_llama": "REJECT",
                        "effect_llama": "E3",
                        "stance_chatgpt": "APPROVE",
                        "suggested_stance_chatgpt": "APPROVE",
                        "effect_chatgpt": "E3",
                        "stance_llama33": "APPROVE",
                        "suggested_stance_llama33": "APPROVE",
                        "effect_llama33": "E3",
                        "source_file_llama33": "fixture.csv",
                        "llama33_reviewed": "TRUE",
                        "llama33_not_reviewed": "FALSE",
                        "llama33_label_conflict": "FALSE",
                        "llama33_basic_proposal_exclusion": "FALSE",
                        "stance_hl": "APPROVE",
                        "suggested_stance_hl": "APPROVE",
                        "n_evaluations": "1",
                        "n_non_low_effort_evaluations": "1",
                        "n_low_effort_evaluations": "0",
                        "low_effort_only": "FALSE",
                    },
                    {
                        "proposal": "prop-d",
                        "space": "space-d",
                        "type": "single-choice",
                        "scores_total": "8",
                        "n_votes": "3",
                        "flagged": "0",
                        "stance": "REJECT",
                        "choice_won": "0",
                        "choice_won_no_author_vp": "1",
                        "stance_oai": "REJECT",
                        "suggested_stance_oai": "REJECT",
                        "effect_oai": "E3",
                        "stance_llama": "REJECT",
                        "suggested_stance_llama": "REJECT",
                        "effect_llama": "E3",
                        "stance_chatgpt": "REJECT",
                        "suggested_stance_chatgpt": "APPROVE",
                        "effect_chatgpt": "E3",
                        "stance_llama33": "REJECT",
                        "suggested_stance_llama33": "APPROVE",
                        "effect_llama33": "E3",
                        "source_file_llama33": "fixture.csv",
                        "llama33_reviewed": "TRUE",
                        "llama33_not_reviewed": "FALSE",
                        "llama33_label_conflict": "FALSE",
                        "llama33_basic_proposal_exclusion": "FALSE",
                        "stance_hl": "REJECT",
                        "suggested_stance_hl": "APPROVE",
                        "n_evaluations": "1",
                        "n_non_low_effort_evaluations": "1",
                        "n_low_effort_evaluations": "0",
                        "low_effort_only": "FALSE",
                    },
                ],
                fieldnames,
            )

            rows = write_latex_stats.generate_latex_counts(
                release="current",
                paper_root=paper_root,
                analysis_root=analysis_root,
            )
            values = {row.macro: str(row.value) for row in rows}
            descriptions = {row.macro: row.description for row in rows}

            self.assertEqual(values["DaoBiasLlmKappaLlama"], "1")
            self.assertEqual(values["DaoBiasLlmKappaLlamaThreePointThree"], "1")
            self.assertEqual(values["DaoBiasLlmKappaOai"], "1")
            self.assertEqual(values["DaoBiasLlmKappaChatgpt"], "1")
            self.assertEqual(values["DaoBiasLlmKappaLlamaAgreement"], "almost perfect")
            self.assertEqual(values["DaoBiasLlmKappaLlamaThreePointThreeAgreement"], "almost perfect")
            self.assertEqual(values["DaoBiasLlmKappaHighestSource"], "Llama 3.1")
            self.assertEqual(values["DaoBiasHumanEvaluatedProposalN"], "2")
            self.assertEqual(values["DaoBiasHumanEnglishProposalN"], "1")
            self.assertEqual(values["DaoBiasHumanChineseProposalN"], "1")
            self.assertEqual(values["DaoBiasHumanProposalReviewMinN"], "2")
            self.assertEqual(values["DaoBiasHumanEvaluatorN"], "2")
            self.assertEqual(values["DaoBiasHumanLowEffortEvaluatorN"], "1")
            self.assertEqual(values["DaoBiasHumanAverageTaskMinutes"], "7")
            self.assertEqual(values["DaoBiasHumanKappa"], "1")
            self.assertEqual(values["DaoBiasHumanKappaAgreement"], "almost perfect")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctOai"], "0")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctLlama"], "0")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctLlamaThreePointThree"], "50")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctChatgpt"], "50")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctMin"], "0")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctMax"], "50")
            self.assertEqual(values["DaoBiasSuggestedNoSuggestionPctMaxSource"], "Llama 3.3")
            self.assertEqual(values["DaoBiasSuggestedLlamaThreePointThreeReviewedProposalN"], "2")
            self.assertEqual(values["DaoBiasSuggestedLlamaThreePointThreeNoSuggestionProposalN"], "1")
            self.assertEqual(values["DaoBiasSuggestedLlamaThreePointThreePositiveProposalN"], "1")
            self.assertEqual(values["DaoBiasSuggestedApprovePositivePctMin"], "50")
            self.assertEqual(values["DaoBiasSuggestedApprovePrecisionMin"], "1")
            self.assertEqual(values["DaoBiasSuggestedApprovePrecisionMax"], "1")
            self.assertEqual(values["DaoBiasSuggestedApproveFOneMax"], "0.67")
            self.assertEqual(
                descriptions["DaoBiasSuggestedApproveFOneMax"],
                "Maximum Approve F1 across automated suggested-stance sources",
            )

    def test_stance_imbalance_documentation_uses_one_sided_exceptions(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            release_path = root / "current.csv"
            write_csv(
                release_path,
                [
                    {
                        "proposal": "prop-approve",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "1",
                        "n_votes": "1",
                        "flagged": "0",
                        "stance": "APPROVE",
                    },
                    {
                        "proposal": "prop-approve",
                        "space": "space-a",
                        "type": "single-choice",
                        "scores_total": "1",
                        "n_votes": "0",
                        "flagged": "0",
                        "stance": "APPROVE",
                    },
                    {
                        "proposal": "prop-reject",
                        "space": "space-b",
                        "type": "single-choice",
                        "scores_total": "1",
                        "n_votes": "1",
                        "flagged": "0",
                        "stance": "REJECT",
                    },
                    {
                        "proposal": "prop-reject",
                        "space": "space-b",
                        "type": "single-choice",
                        "scores_total": "1",
                        "n_votes": "0",
                        "flagged": "0",
                        "stance": "REJECT",
                    },
                    {
                        "proposal": "prop-both",
                        "space": "space-c",
                        "type": "single-choice",
                        "scores_total": "2",
                        "n_votes": "2",
                        "flagged": "0",
                        "stance": "APPROVE",
                    },
                    {
                        "proposal": "prop-both",
                        "space": "space-c",
                        "type": "single-choice",
                        "scores_total": "2",
                        "n_votes": "2",
                        "flagged": "0",
                        "stance": "REJECT",
                    },
                ],
                ["proposal", "space", "type", "scores_total", "n_votes", "flagged", "stance"],
            )

            documented = {
                "prop-approve": {"approve_only"},
                "prop-reject": {"approve_only"},
            }
            stats = write_latex_stats.read_release_stats(release_path, documented)

            self.assertEqual(stats["stance_imbalance_proposals"], 2)
            self.assertEqual(stats["stance_imbalance_documented_proposals"], 1)
            self.assertEqual(stats["stance_imbalance_undocumented_proposals"], 1)

    def test_enriched_candidate_release_writes_stage5_coverage_and_kappas(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, paper_root = self.build_fixture(tmpdir)

            rows = write_latex_stats.generate_latex_counts(
                release="candidate",
                paper_root=paper_root,
                analysis_root=analysis_root,
            )
            values = {row.macro: str(row.value) for row in rows}
            descriptions = {row.macro: row.description for row in rows}
            candidate_path = analysis_root / "data/2024/derived/active/candidate.csv"

            self.assertEqual(values["DaoBiasCandidateProposalN"], "2")
            self.assertEqual(values["DaoBiasCandidateChoiceN"], "4")
            self.assertEqual(values["DaoBiasCandidateRetainedPct"], "100")
            self.assertEqual(values["DaoBiasCandidateHumanChoiceN"], "3")
            self.assertEqual(values["DaoBiasCandidateHumanProposalN"], "2")
            self.assertEqual(values["DaoBiasCandidateHumanEvaluatorN"], "7")
            self.assertEqual(values["DaoBiasCandidateHumanLowEffortEvaluatorN"], "3")
            self.assertEqual(values["DaoBiasCandidateHumanNonLowEffortEvaluatorN"], "4")
            self.assertEqual(values["DaoBiasCandidateHumanLowEffortOnlyChoiceN"], "2")
            self.assertEqual(values["DaoBiasCandidateKappaOai"], "1")
            self.assertEqual(values["DaoBiasCandidateKappaLlama"], "1")
            self.assertEqual(values["DaoBiasCandidateKappaLlamaThreePointThree"], "1")
            self.assertEqual(
                descriptions["DaoBiasCandidateKappaLlamaThreePointThree"],
                "Stance vs Llama 3.3 Cohen kappa in candidate release",
            )
            self.assertEqual(values["DaoBiasCandidateKappaHuman"], "1")
            self.assertIn("DaoBiasCandidateKappaChatgpt", values)
            self.assertIn("DaoBiasCandidateHumanCompleteProposalN", values)
            self.assertIn("DaoBiasCandidateLlamaLabeledChoiceN", values)
            self.assertIn("DaoBiasCandidateLlamaThreePointThreeLabeledChoiceN", values)
            self.assertEqual(
                descriptions["DaoBiasCandidateLlamaThreePointThreeLabeledChoiceN"],
                "Llama 3.3 labeled choice rows in candidate release",
            )

            macro_text = (paper_root / "sections/macros/analysis_counts.tex").read_text(encoding="utf-8")
            command_lines = [
                line for line in macro_text.splitlines() if line and not line.startswith("%")
            ]
            self.assertTrue(command_lines)
            for line in command_lines:
                self.assertRegex(line, r"^\\newcommand\{\\DaoBias[A-Za-z]+\}\{.+\}$")

            with (
                analysis_root / "metadata/analysis_data_provenance/latex_counts_candidate.csv"
            ).open(newline="", encoding="utf-8") as handle:
                csv_rows = list(csv.DictReader(handle))
            self.assertTrue(any(row["macro"] == "DaoBiasCandidateProposalN" for row in csv_rows))
            self.assertTrue(
                all(
                    row["source"] == str(candidate_path)
                    for row in csv_rows
                    if row["macro"].startswith("DaoBiasCandidate")
                )
            )

            provenance = json.loads(
                (analysis_root / "metadata/analysis_data_provenance/latex_counts_candidate.json").read_text(
                    encoding="utf-8"
                )
            )
            self.assertEqual(provenance["release"], "candidate")
            self.assertEqual(provenance["release_path"], str(candidate_path))
            self.assertEqual(
                provenance["counts"]["DaoBiasCandidateProposalN"]["source"],
                str(candidate_path),
            )
            self.assertEqual(provenance["counts"]["DaoBiasCandidateKappaChatgpt"]["value"], "0.4")

    def test_candidate_counts_can_write_provenance_without_macro_output(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, paper_root = self.build_fixture(tmpdir)

            rows = write_latex_stats.generate_latex_counts(
                release="candidate",
                analysis_root=analysis_root,
                write_macro_output=False,
            )

            self.assertTrue(rows)
            self.assertFalse((paper_root / "sections/macros/analysis_counts.tex").exists())
            self.assertTrue(
                (analysis_root / "metadata/analysis_data_provenance/latex_counts_candidate.csv").exists()
            )
            provenance = json.loads(
                (analysis_root / "metadata/analysis_data_provenance/latex_counts_candidate.json").read_text(
                    encoding="utf-8"
                )
            )
            self.assertIsNone(provenance["paper_root"])

    def test_candidate_counts_treat_missing_llama33_columns_as_zero_coverage(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root, _ = self.build_fixture(tmpdir)
            candidate_path = analysis_root / "data/2024/derived/active/candidate.csv"

            with candidate_path.open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))

            fieldnames = [
                field
                for field in rows[0].keys()
                if field not in {"stance_llama33", "suggested_stance_llama33", "effect_llama33"}
            ]
            trimmed_rows = [{field: row[field] for field in fieldnames} for row in rows]
            write_csv(candidate_path, trimmed_rows, fieldnames)

            stats = write_latex_stats.generate_latex_counts(
                release="candidate",
                analysis_root=analysis_root,
                write_macro_output=False,
            )
            values = {row.macro: str(row.value) for row in stats}

            self.assertEqual(values["DaoBiasCandidateLlamaThreePointThreeLabeledChoiceN"], "0")
            self.assertEqual(values["DaoBiasCandidateLlamaThreePointThreeLabeledProposalN"], "0")
            self.assertEqual(values["DaoBiasCandidateKappaLlamaThreePointThree"], "0")

    def test_missing_release_path_fails_loudly(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            analysis_root = root / "analysis"
            paper_root = root / "paper"
            write_csv(
                analysis_root / "metadata/analysis_data_releases.csv",
                [
                    {
                        "release_name": "current",
                        "relative_path": "data/2024/derived/active/missing.csv",
                        "is_frozen": "FALSE",
                        "notes": "missing",
                    }
                ],
                ["release_name", "relative_path", "is_frozen", "notes"],
            )

            with self.assertRaises(FileNotFoundError):
                write_latex_stats.generate_latex_counts(
                    release="current",
                    paper_root=paper_root,
                    analysis_root=analysis_root,
                )


if __name__ == "__main__":
    unittest.main()
