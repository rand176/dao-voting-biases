import json
import sys
import tempfile
import unittest
from pathlib import Path

import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]

from dao_bias.jobs import JOB_REGISTRY
from dao_bias.jobs import human_labels as human_labels_job


class HumanLabelsJobTests(unittest.TestCase):
    def test_extract_label_rows_parses_nested_choice_labels_and_malformed_rows(self):
        raw = pd.DataFrame(
            [
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 12_000,
                    "choice_1": {"value": "Approve"},
                    "choice_2": {"value": "Reject"},
                    "proposal_issues": {"value": "Yes"},
                    "dao_suggest": {"value": "Needs work"},
                },
                {
                    "news": "prop-2",
                    "player": "bob",
                    "time": 8_000,
                    "choice_1": "Approve",
                    "proposal_issues": {"value": "Yes"},
                    "dao_suggest": {"value": "N/A"},
                },
                {
                    "news": None,
                    "player": "carol",
                    "time": 8_000,
                    "choice_1": {"value": "Other"},
                },
            ]
        )

        normalized, errors = human_labels_job.extract_label_rows(raw, source_file="room/news.ndjson")

        rows = normalized[["proposal", "choice_idx", "evaluator_id", "label"]].to_dict("records")
        self.assertEqual(
            rows[:2],
            [
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Approve",
                },
                {
                    "proposal": "prop-1",
                    "choice_idx": 2,
                    "evaluator_id": "alice",
                    "label": "Reject",
                },
            ],
        )
        self.assertEqual(len(rows), 3)
        self.assertEqual(rows[2]["proposal"], "prop-2")
        self.assertEqual(rows[2]["choice_idx"], 2)
        self.assertEqual(rows[2]["evaluator_id"], "bob")
        self.assertTrue(pd.isna(rows[2]["label"]))
        self.assertEqual(set(errors["reason"]), {"malformed_choice", "missing_proposal"})

    def test_extract_label_rows_materializes_missing_choice_labels(self):
        raw = pd.DataFrame(
            [
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500_000,
                    "choice_1": {"value": "Reject"},
                    "choice_2": {"value": None},
                    "choice_3": "Approve",
                },
            ]
        )

        normalized, errors = human_labels_job.extract_label_rows(raw, source_file="room/news.ndjson")

        rows = normalized[["proposal", "choice_idx", "evaluator_id", "label"]].to_dict("records")
        self.assertEqual(rows[0], {"proposal": "prop-1", "choice_idx": 1, "evaluator_id": "alice", "label": "Reject"})
        self.assertEqual(rows[1]["proposal"], "prop-1")
        self.assertEqual(rows[1]["choice_idx"], 2)
        self.assertTrue(pd.isna(rows[1]["label"]))
        self.assertEqual(errors["reason"].tolist(), ["malformed_choice"])

    def test_extract_label_rows_materializes_raw_missing_choice_cells(self):
        raw = pd.DataFrame(
            [
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500,
                    "choice_1": None,
                    "choice_2": "",
                    "choice_3": pd.NA,
                    "choice_4": {"value": "Reject"},
                },
            ]
        )

        normalized, errors = human_labels_job.extract_label_rows(raw, source_file="room/news.ndjson")

        rows = normalized.set_index("choice_idx")
        self.assertEqual(rows.index.tolist(), [1, 2, 3, 4])
        self.assertTrue(pd.isna(rows.loc[1, "label"]))
        self.assertTrue(pd.isna(rows.loc[2, "label"]))
        self.assertTrue(pd.isna(rows.loc[3, "label"]))
        self.assertEqual(rows.loc[4, "label"], "Reject")
        self.assertTrue(errors.empty)

    def test_extract_label_rows_materializes_pandas_created_missing_choice_cells(self):
        raw = pd.DataFrame(
            [
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500,
                    "choice_1": {"value": "Approve"},
                    "choice_2": {"value": "Reject"},
                },
                {
                    "news": "prop-2",
                    "player": "bob",
                    "time": 600,
                    "choice_1": {"value": "Approve"},
                },
            ]
        )

        normalized, errors = human_labels_job.extract_label_rows(raw, source_file="room/news.ndjson")

        rows = normalized.set_index(["proposal", "choice_idx"])
        self.assertEqual(rows.loc[("prop-1", 2), "label"], "Reject")
        self.assertTrue(pd.isna(rows.loc[("prop-2", 2), "label"]))
        self.assertTrue(errors.empty)

    def test_extract_label_rows_preserves_seconds_columns_and_converts_ms_like_time_columns(self):
        raw = pd.DataFrame(
            [
                {"news": "prop-1", "player": "alice", "time": 500, "choice_1": {"value": "Approve"}},
                {"news": "prop-2", "player": "bob", "eval_seconds": 1500, "choice_1": {"value": "Reject"}},
            ]
        )

        normalized, _ = human_labels_job.extract_label_rows(raw, source_file="room/news.ndjson")

        seconds = normalized.set_index("proposal")["eval_seconds"].to_dict()
        self.assertEqual(seconds["prop-1"], 0.5)
        self.assertEqual(seconds["prop-2"], 1500)

    def test_build_outputs_deduplicates_repeated_evaluator_labels_deterministically(self):
        raw = pd.DataFrame(
            [
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Reject",
                    "eval_seconds": 40,
                    "source_file": "b.ndjson",
                    "source_row": 1,
                },
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Approve",
                    "eval_seconds": 60,
                    "source_file": "a.ndjson",
                    "source_row": 1,
                },
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Approve",
                    "eval_seconds": 70,
                    "source_file": "a.ndjson",
                    "source_row": 2,
                },
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "bob",
                    "label": "Reject",
                    "eval_seconds": 500,
                    "source_file": "a.ndjson",
                    "source_row": 3,
                },
            ]
        )

        evaluator, choice = human_labels_job.build_canonical_outputs(raw)

        self.assertEqual(len(evaluator), 2)
        alice = evaluator[evaluator["evaluator_id"] == "alice"].iloc[0]
        self.assertEqual(alice["label"], "Approve")
        self.assertEqual(alice["raw_label_count"], 3)
        self.assertEqual(alice["raw_distinct_label_count"], 2)
        self.assertTrue(bool(alice["has_repeated_label"]))
        self.assertTrue(bool(alice["has_label_conflict"]))

        choice_row = choice.iloc[0]
        self.assertEqual(choice_row["n_evaluations"], 2)
        self.assertEqual(choice_row["n_non_low_effort_evaluations"], 1)
        self.assertEqual(choice_row["n_low_effort_evaluations"], 1)
        self.assertEqual(choice_row["stance_hl"], "Reject")
        self.assertEqual(choice_row["label_conflict"], False)

    def test_issue_starting_with_no_overrides_label_before_choice_counts(self):
        raw = pd.DataFrame(
            [
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Approve",
                    "issue": "No clear DAO vote",
                    "eval_seconds": 500,
                    "source_file": "a.ndjson",
                    "source_row": 1,
                },
            ]
        )

        evaluator, choice = human_labels_job.build_canonical_outputs(raw)

        self.assertEqual(evaluator.iloc[0]["label"], "Other")
        choice_row = choice.iloc[0]
        self.assertEqual(choice_row["stance_hl"], "Other")
        self.assertEqual(choice_row["n_effective_approve"], 0)
        self.assertEqual(choice_row["n_effective_other"], 1)

    def test_low_effort_flags_use_evaluator_total_and_short_long_proposal_rule(self):
        raw = pd.DataFrame(
            [
                {
                    "proposal": "prop-1",
                    "choice_idx": 1,
                    "evaluator_id": "alice",
                    "label": "Approve",
                    "eval_seconds": 200,
                    "source_file": "a.ndjson",
                    "source_row": 1,
                },
                {
                    "proposal": "prop-1",
                    "choice_idx": 2,
                    "evaluator_id": "alice",
                    "label": "Reject",
                    "eval_seconds": 70,
                    "source_file": "a.ndjson",
                    "source_row": 2,
                },
                {
                    "proposal": "prop-2",
                    "choice_idx": 1,
                    "evaluator_id": "bob",
                    "label": "Approve",
                    "eval_seconds": 10,
                    "body_mod_len_n": 0.5,
                    "source_file": "a.ndjson",
                    "source_row": 3,
                },
            ]
        )

        evaluator, _ = human_labels_job.build_canonical_outputs(raw)

        flags = evaluator.set_index(["proposal", "choice_idx", "evaluator_id"])["low_effort"].to_dict()
        self.assertEqual(flags[("prop-1", 1, "alice")], True)
        self.assertEqual(flags[("prop-1", 2, "alice")], True)
        self.assertEqual(flags[("prop-2", 1, "bob")], True)

    def test_choice_inference_uses_matching_raw_label(self):
        raw = pd.DataFrame(
            [
                {"proposal": "prop-1", "choice_idx": 1, "evaluator_id": "alice", "label": "Reject", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 2, "evaluator_id": "bob", "label": "Approve", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 2, "evaluator_id": "carol", "label": "Reject", "eval_seconds": 500},
            ]
        )

        _, choice = human_labels_job.build_canonical_outputs(raw)

        stances = choice.set_index("choice_idx")["stance_hl"].to_dict()
        self.assertEqual(stances[1], "Reject")
        self.assertEqual(stances[2], "Approve")

    def test_choice_inference_requires_matching_raw_label(self):
        raw = pd.DataFrame(
            [
                {"proposal": "prop-1", "choice_idx": 1, "evaluator_id": "alice", "label": "Reject", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 2, "evaluator_id": "bob", "label": pd.NA, "eval_seconds": 500},
            ]
        )

        _, choice = human_labels_job.build_canonical_outputs(raw)

        stances = choice.set_index("choice_idx")["stance_hl"].to_dict()
        self.assertEqual(stances[1], "Reject")
        self.assertTrue(pd.isna(stances[2]))

    def test_choice_inference_skips_multiple_missing_final_labels(self):
        raw = pd.DataFrame(
            [
                {"proposal": "prop-1", "choice_idx": 1, "evaluator_id": "alice", "label": "Reject", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 2, "evaluator_id": "bob", "label": "Approve", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 2, "evaluator_id": "carol", "label": "Reject", "eval_seconds": 500},
                {"proposal": "prop-1", "choice_idx": 3, "evaluator_id": "dana", "label": pd.NA, "eval_seconds": 500},
            ]
        )

        _, choice = human_labels_job.build_canonical_outputs(raw)

        stances = choice.set_index("choice_idx")["stance_hl"].to_dict()
        self.assertEqual(stances[1], "Reject")
        self.assertTrue(pd.isna(stances[2]))
        self.assertTrue(pd.isna(stances[3]))

    def test_room_export_low_effort_uses_full_stream_evaluator_time(self):
        raw = pd.DataFrame(
            [
                {"stepId": "instructions", "player": "alice", "time": 350_000},
                {
                    "stepId": "news",
                    "news": "prop-1",
                    "player": "alice",
                    "time": 10_000,
                    "choice_1": {"value": "Approve"},
                },
            ]
        )

        normalized, _ = human_labels_job.extract_label_rows(raw, source_file="rooms/room1/news.ndjson")
        evaluator, _ = human_labels_job.build_canonical_outputs(normalized)

        row = evaluator.iloc[0]
        self.assertEqual(row["evaluator_total_seconds"], 360)
        self.assertFalse(bool(row["low_effort_evaluator"]))
        self.assertFalse(bool(row["low_effort"]))

    def test_body_mod_len_is_normalized_for_response_low_effort(self):
        raw = pd.DataFrame(
            [
                {"stepId": "instructions", "player": "bob", "time": 400_000},
                {
                    "stepId": "news",
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500_000,
                    "choice_1": {"value": "Approve"},
                    "body_mod_len": 500,
                    "body_lang": "en",
                },
                {
                    "stepId": "news",
                    "news": "prop-2",
                    "player": "bob",
                    "time": 10_000,
                    "choice_1": {"value": "Approve"},
                    "body_mod_len": 1000,
                    "body_lang": "en",
                },
            ]
        )

        normalized, _ = human_labels_job.extract_label_rows(raw, source_file="rooms/room1/news.ndjson")
        evaluator, _ = human_labels_job.build_canonical_outputs(normalized)

        raw_lengths = normalized.set_index("proposal")["body_mod_len_n"].to_dict()
        flags = evaluator.set_index("proposal")["low_effort_response"].to_dict()
        self.assertEqual(raw_lengths["prop-1"], 0.5)
        self.assertEqual(raw_lengths["prop-2"], 1.0)
        self.assertFalse(bool(flags["prop-1"]))
        self.assertTrue(bool(flags["prop-2"]))

    def test_job_writes_unique_choice_and_evaluator_outputs_without_touching_input(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            input_dir = root / "humanlabels"
            room_dir = input_dir / "rooms" / "room1"
            output_dir = root / "clean_human_label_outputs"
            room_dir.mkdir(parents=True)
            news_path = room_dir / "news.ndjson"
            raw_rows = [
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500_000,
                    "choice_1": {"value": "Approve"},
                    "proposal_issues": {"value": "Yes"},
                    "dao_suggest": {"value": "N/A"},
                },
                {
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500_000,
                    "choice_1": {"value": "Reject"},
                    "proposal_issues": {"value": "Yes"},
                    "dao_suggest": {"value": "N/A"},
                },
            ]
            original = "\n".join(json.dumps(row) for row in raw_rows) + "\n"
            news_path.write_text(original, encoding="utf-8")

            summary = human_labels_job.run_human_label_cleaning_job(
                input_dir=str(input_dir),
                clean_output_dir=str(output_dir),
            )

            evaluator = pd.read_csv(output_dir / human_labels_job.EVALUATOR_LABELS_FILE)
            choice = pd.read_csv(output_dir / human_labels_job.CHOICE_LABELS_FILE)

            self.assertEqual(news_path.read_text(encoding="utf-8"), original)
            self.assertEqual(summary["evaluator_rows"], 1)
            self.assertEqual(summary["choice_rows"], 1)
            self.assertFalse(evaluator.duplicated(["proposal", "choice_idx", "evaluator_id"]).any())
            self.assertFalse(choice.duplicated(["proposal", "choice_idx"]).any())

    def test_job_discovers_only_label_sources_and_skips_non_label_room_events(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            input_dir = root / "humanlabels"
            room_dir = input_dir / "rooms" / "room1"
            output_dir = root / "clean_human_label_outputs"
            room_dir.mkdir(parents=True)
            pd.DataFrame(
                [
                    {
                        "proposal": "helper-prop",
                        "choice_idx": 1,
                        "eval_1": "Approve",
                    }
                ]
            ).to_csv(input_dir / "dao_rnd_merged.csv", index=False)
            pd.DataFrame([{"id": "missing-prop"}]).to_csv(input_dir / "en_missing.csv", index=False)
            pd.DataFrame([{"id": "missing-prop-2"}]).to_csv(input_dir / "en_missing2.csv", index=False)
            news_rows = [
                {
                    "stepId": "instructions_daos",
                    "stageId": "instructions",
                    "player": "alice",
                    "forms": {"check_dao_voting": {"value": "Members vote on proposals using tokens"}},
                },
                {
                    "stepId": "news",
                    "stageId": "news",
                    "news": "prop-1",
                    "player": "alice",
                    "time": 500_000,
                    "choice_1": {"value": "Approve"},
                    "proposal_issues": {"value": "Yes"},
                    "dao_suggest": {"value": "N/A"},
                },
            ]
            (room_dir / "news.ndjson").write_text(
                "\n".join(json.dumps(row) for row in news_rows) + "\n",
                encoding="utf-8",
            )

            self.assertEqual(
                human_labels_job.discover_human_label_files(input_dir),
                ["rooms/room1/news.ndjson"],
            )

            summary = human_labels_job.run_human_label_cleaning_job(
                input_dir=str(input_dir),
                clean_output_dir=str(output_dir),
            )
            errors = pd.read_csv(output_dir / human_labels_job.PARSE_ERRORS_FILE)

            self.assertEqual(summary["raw_label_rows"], 1)
            self.assertEqual(summary["parse_error_rows"], 0)
            self.assertTrue(errors.empty)

    def test_reduce_reads_only_finished_manifest_chunks(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            tmp_root = root / "tmp" / "run-1"
            output_dir = root / "clean_human_label_outputs"
            manifest_path = root / "manifest.csv"
            run_meta_path = root / "run.json"

            chunks = [
                ("finished.ndjson", 0, "finished", "alice", "Approve"),
                ("failed.ndjson", 1, "failed", "bob", "Reject"),
                ("stale.ndjson", 2, "", "carol", "Reject"),
                ("non-numeric.ndjson", 3, "finished", "dana", "Reject"),
            ]
            for work_unit_id, worker_id, _status, evaluator_id, label in chunks:
                worker_dir = tmp_root / f"worker_{worker_id:02d}"
                labels = pd.DataFrame(
                    [
                        {
                            "proposal": "prop-1",
                            "choice_idx": 1,
                            "prop_chid": "prop-1_1",
                            "evaluator_id": evaluator_id,
                            "label": label,
                            "eval_seconds": 500,
                            "issue": "Yes",
                            "suggest": pd.NA,
                            "raw_low_effort": False,
                            "body_mod_len_n": pd.NA,
                            "source_file": work_unit_id,
                            "source_row": 1,
                            "source_event_id": f"{work_unit_id}:1:prop-1:{evaluator_id}",
                        }
                    ],
                    columns=human_labels_job.LABEL_COLUMNS,
                )
                human_labels_job.write_chunk(
                    human_labels_job._chunk_path(str(worker_dir), work_unit_id, "labels"),
                    labels,
                    human_labels_job.LABEL_COLUMNS,
                )
                human_labels_job.write_chunk(
                    human_labels_job._chunk_path(str(worker_dir), work_unit_id, "errors"),
                    human_labels_job.empty_error_df(),
                    human_labels_job.ERROR_COLUMNS,
                )

            manifest = pd.DataFrame(
                [
                    {
                        "job_name": human_labels_job.JOB_NAME,
                        "work_unit_id": "finished.ndjson",
                        "status": "finished",
                        "worker_id": "0",
                        "started_at": "",
                        "finished_at": "",
                        "error_count": "0",
                    },
                    {
                        "job_name": human_labels_job.JOB_NAME,
                        "work_unit_id": "failed.ndjson",
                        "status": "failed",
                        "worker_id": "1",
                        "started_at": "",
                        "finished_at": "",
                        "error_count": "1",
                    },
                    {
                        "job_name": human_labels_job.JOB_NAME,
                        "work_unit_id": "non-numeric.ndjson",
                        "status": "finished",
                        "worker_id": "worker_03",
                        "started_at": "",
                        "finished_at": "",
                        "error_count": "0",
                    },
                ]
            )
            manifest.to_csv(manifest_path, index=False)

            original_run_meta_path = human_labels_job.RUN_META_PATH
            try:
                human_labels_job.RUN_META_PATH = str(run_meta_path)
                human_labels_job.write_json_output(
                    str(run_meta_path),
                    {"job_name": human_labels_job.JOB_NAME, "run_id": "run-1", "tmp_root": str(tmp_root)},
                )

                summary = human_labels_job.reduce_vote_expansion_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_path),
                )
            finally:
                human_labels_job.RUN_META_PATH = original_run_meta_path

            choice = pd.read_csv(output_dir / human_labels_job.CHOICE_LABELS_FILE)
            self.assertEqual(summary["raw_label_rows"], 1)
            self.assertEqual(summary["evaluator_rows"], 1)
            self.assertEqual(choice.iloc[0]["stance_hl"], "Approve")

    def test_job_is_registered_and_parser_accepts_it(self):
        import dao_bias.jobs.runner as job_runner

        self.assertIs(JOB_REGISTRY["human_labels"], human_labels_job)
        parser = job_runner.build_parser()
        args = parser.parse_args(["build-manifest", "human_labels"])
        self.assertEqual(args.job_name, "human_labels")
