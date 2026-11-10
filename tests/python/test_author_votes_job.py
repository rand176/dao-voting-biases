import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.jobs.batch as batch_runner
from dao_bias.jobs import author_votes as author_votes_job


def build_context(proposals, votes):
    df_proposals = pd.DataFrame(proposals).set_index("id")
    df_votes = pd.DataFrame(votes)
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": df_votes.groupby("proposal"),
    }


def build_compact_context(proposals, author_vote_rows, proposal_ids_with_votes=None):
    df_proposals = pd.DataFrame(proposals).set_index("id")
    author_vote_columns = ["proposal", "author_vp", "author_pos", "author_rank", "choice_raw"]
    if author_vote_rows:
        author_vote_lookup = pd.DataFrame(author_vote_rows, columns=author_vote_columns).set_index("proposal")
    else:
        author_vote_lookup = pd.DataFrame(columns=author_vote_columns).set_index("proposal")
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": set(proposal_ids_with_votes or df_proposals.index.tolist()),
        "author_vote_lookup": author_vote_lookup,
    }


def build_manifest_rows(status_rows, job_name=None):
    job_name = job_name or author_votes_job.JOB_NAME
    rows = []
    for row in status_rows:
        rows.append(
            {
                "job_name": job_name,
                "work_unit_id": row["work_unit_id"],
                "status": row["status"],
                "worker_id": str(row.get("worker_id", "")),
                "started_at": row.get("started_at", ""),
                "finished_at": row.get("finished_at", ""),
                "error_count": str(row.get("error_count", 0)),
            }
        )
    return rows


def write_worker_mode_output(worker_dir, proposal_id, mode_name, rows):
    path = author_votes_job.proposal_output_path(worker_dir, proposal_id, mode_name)
    pd.DataFrame(rows, columns=author_votes_job.VOTE_MODES[mode_name]["columns"]).to_csv(
        path,
        index=False,
        header=False,
    )


class AuthorVotesJobTests(unittest.TestCase):
    def test_list_work_units_accepts_compact_vote_id_sets(self):
        df_proposals = pd.DataFrame(
            [
                {"id": "prop-a", "author": "0xa"},
                {"id": "prop-b", "author": "0xb"},
                {"id": "prop-c", "author": "0xc"},
            ]
        ).set_index("id")

        work_units = author_votes_job.list_work_units(df_proposals, {"prop-c", "prop-a"})

        self.assertEqual(work_units, ["prop-a", "prop-c"])

    def test_process_proposal_accepts_compact_author_vote_lookup_context(self):
        context = build_compact_context(
            proposals=[
                {
                    "id": "prop-compact",
                    "author": "0xauthor",
                    "type": "approval",
                    "choices": ["A", "B", "C"],
                }
            ],
            author_vote_rows=[
                {
                    "proposal": "prop-compact",
                    "author_vp": 4.0,
                    "author_pos": 2,
                    "author_rank": 2 / 3,
                    "choice_raw": json.dumps([1, 3]),
                }
            ],
            proposal_ids_with_votes={"prop-compact"},
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            author_votes_job.process_proposal("prop-compact", context, tmpdir)

            favorite = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-compact", "favorite_choice"),
                header=None,
                names=author_votes_job.VOTE_MODES["favorite_choice"]["columns"],
            ).sort_values("author_choice")
            support = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-compact", "support_fraction"),
                header=None,
                names=author_votes_job.VOTE_MODES["support_fraction"]["columns"],
            ).sort_values("author_choice")

        self.assertEqual(favorite["author_choice"].tolist(), [1, 3])
        self.assertEqual(favorite["author_choice_weight"].tolist(), [0.5, 0.5])
        self.assertEqual(support["author_support_fraction"].tolist(), [0.5, 0.5])
        self.assertTrue((favorite["author_pos"] == 2).all())
        self.assertTrue((support["author_rank"].round(6) == round(2 / 3, 6)).all())

    def test_process_proposal_splits_multi_approval_author_ballot_in_both_modes(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-1",
                    "author": "0xauthor",
                    "type": "approval",
                    "choices": ["A", "B", "C", "D"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-1",
                    "voter": "0xauthor",
                    "created": 100,
                    "choice": json.dumps([2, 4]),
                    "vp": 12.5,
                }
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            author_votes_job.process_proposal("prop-1", context, tmpdir)

            favorite = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-1", "favorite_choice"),
                header=None,
                names=author_votes_job.VOTE_MODES["favorite_choice"]["columns"],
            ).sort_values("author_choice")
            support = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-1", "support_fraction"),
                header=None,
                names=author_votes_job.VOTE_MODES["support_fraction"]["columns"],
            ).sort_values("author_choice")

        self.assertEqual(favorite["author_choice"].tolist(), [2, 4])
        self.assertEqual(support["author_choice"].tolist(), [2, 4])
        self.assertEqual(favorite["author_choice_weight"].tolist(), [0.5, 0.5])
        self.assertEqual(support["author_support_fraction"].tolist(), [0.5, 0.5])
        self.assertTrue((favorite["author_pos"] == 1).all())
        self.assertTrue((support["author_rank"] == 1.0).all())

    def test_process_proposal_splits_weighted_top_ties_for_favorite_and_normalizes_support(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-2",
                    "author": "0xauthor",
                    "type": "weighted",
                    "choices": ["A", "B", "C", "D"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-2",
                    "voter": "0xother",
                    "created": 100,
                    "choice": json.dumps({"2": 1}),
                    "vp": 1.0,
                },
                {
                    "proposal": "prop-2",
                    "voter": "0xauthor",
                    "created": 200,
                    "choice": json.dumps({"2": 5, "4": 5, "1": 1}),
                    "vp": 7.0,
                },
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            author_votes_job.process_proposal("prop-2", context, tmpdir)

            favorite = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-2", "favorite_choice"),
                header=None,
                names=author_votes_job.VOTE_MODES["favorite_choice"]["columns"],
            ).sort_values("author_choice")
            support = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-2", "support_fraction"),
                header=None,
                names=author_votes_job.VOTE_MODES["support_fraction"]["columns"],
            ).sort_values("author_choice")

        self.assertEqual(favorite["author_choice"].tolist(), [2, 4])
        self.assertEqual(favorite["author_choice_weight"].tolist(), [0.5, 0.5])
        self.assertEqual(support["author_choice"].tolist(), [1, 2, 4])
        self.assertEqual(support["author_support_fraction"].round(6).tolist(), [0.090909, 0.454545, 0.454545])
        self.assertTrue((favorite["author_pos"] == 2).all())
        self.assertTrue((support["author_vp"] == 7.0).all())

    def test_process_proposal_preserves_na_row_and_logs_invalid_author_ballot(self):
        context = build_context(
            proposals=[
                {
                    "id": "prop-3",
                    "author": "0xauthor",
                    "type": "approval",
                    "choices": ["A", "B", "C"],
                }
            ],
            votes=[
                {
                    "proposal": "prop-3",
                    "voter": "0xauthor",
                    "created": 100,
                    "choice": "shutter",
                    "vp": 5.0,
                }
            ],
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            author_votes_job.process_proposal("prop-3", context, tmpdir)

            favorite = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-3", "favorite_choice"),
                header=None,
                names=author_votes_job.VOTE_MODES["favorite_choice"]["columns"],
            )
            support = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-3", "support_fraction"),
                header=None,
                names=author_votes_job.VOTE_MODES["support_fraction"]["columns"],
            )
            favorite_err = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-3", "favorite_choice", err=True),
                header=None,
                names=author_votes_job.ERROR_COLUMNS,
            )
            support_err = pd.read_csv(
                author_votes_job.proposal_output_path(tmpdir, "prop-3", "support_fraction", err=True),
                header=None,
                names=author_votes_job.ERROR_COLUMNS,
            )

        self.assertTrue(pd.isna(favorite["author_choice"].iloc[0]))
        self.assertTrue(pd.isna(support["author_choice"].iloc[0]))
        self.assertTrue(pd.isna(favorite["author_choice_weight"].iloc[0]))
        self.assertTrue(pd.isna(support["author_support_fraction"].iloc[0]))
        self.assertEqual(favorite_err["reason"].tolist(), ["invalid json"])
        self.assertEqual(support_err["reason"].tolist(), ["invalid json"])

    def test_reduce_author_vote_outputs_refuses_incomplete_manifest_and_preserves_root_outputs(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "clean_author_vote_outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-1"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            tmp_root.mkdir(parents=True, exist_ok=True)

            manifest_path = manifest_dir / f"{author_votes_job.JOB_NAME}_manifest.csv"
            run_meta_path = manifest_dir / f"{author_votes_job.JOB_NAME}_run.json"
            run_summary_path = output_dir / "run_summary.json"
            reduce_status_path = manifest_dir / f"{author_votes_job.JOB_NAME}_reduce_status.json"

            batch_runner.write_manifest_rows(
                str(manifest_path),
                build_manifest_rows(
                    [
                        {
                            "work_unit_id": "prop-1",
                            "status": "finished",
                            "worker_id": 0,
                            "started_at": "2026-04-16T10:00:00+00:00",
                            "finished_at": "2026-04-16T10:00:01+00:00",
                        },
                        {
                            "work_unit_id": "prop-2",
                            "status": "pending",
                        },
                    ]
                ),
            )

            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": author_votes_job.JOB_NAME,
                        "run_id": "run-1",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            sentinel_summary = {"run_id": "older-run", "is_complete": True}
            run_summary_path.write_text(json.dumps(sentinel_summary), encoding="utf-8")
            favorite_root = output_dir / author_votes_job.FAVORITE_FILE
            support_root = output_dir / author_votes_job.SUPPORT_FILE
            favorite_root.write_text("sentinel favorite\n", encoding="utf-8")
            support_root.write_text("sentinel support\n", encoding="utf-8")

            worker_dir = tmp_root / "worker_00"
            write_worker_mode_output(
                str(worker_dir),
                "prop-1",
                "favorite_choice",
                [["prop-1", "0xauthor", 2, 1.0, 5.0, 1, 1.0, "favorite_choice"]],
            )
            write_worker_mode_output(
                str(worker_dir),
                "prop-1",
                "support_fraction",
                [["prop-1", "0xauthor", 2, 1.0, 5.0, 1, 1.0, "support_fraction"]],
            )

            with mock.patch.object(author_votes_job, "RUN_META_PATH", str(run_meta_path)), \
                mock.patch.object(author_votes_job, "RUN_SUMMARY_PATH", str(run_summary_path)), \
                mock.patch.object(author_votes_job, "REDUCE_STATUS_PATH", str(reduce_status_path)):
                with self.assertRaises(RuntimeError):
                    author_votes_job.reduce_author_vote_outputs(
                        clean_output_dir=str(output_dir),
                        manifest_path=str(manifest_path),
                    )

            self.assertEqual(favorite_root.read_text(encoding="utf-8"), "sentinel favorite\n")
            self.assertEqual(support_root.read_text(encoding="utf-8"), "sentinel support\n")
            self.assertEqual(json.loads(run_summary_path.read_text(encoding="utf-8")), sentinel_summary)

            reduce_status = json.loads(reduce_status_path.read_text(encoding="utf-8"))
            self.assertFalse(reduce_status["is_complete"])
            self.assertEqual(reduce_status["manifest_total"], 2)
            self.assertEqual(reduce_status["manifest_finished"], 1)
            self.assertEqual(reduce_status["manifest_pending"], 1)
            self.assertEqual(reduce_status["run_id"], "run-1")

    def test_reduce_author_vote_outputs_writes_complete_contract_metadata(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "clean_author_vote_outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-2"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            tmp_root.mkdir(parents=True, exist_ok=True)

            manifest_path = manifest_dir / f"{author_votes_job.JOB_NAME}_manifest.csv"
            run_meta_path = manifest_dir / f"{author_votes_job.JOB_NAME}_run.json"
            run_summary_path = output_dir / "run_summary.json"
            reduce_status_path = manifest_dir / f"{author_votes_job.JOB_NAME}_reduce_status.json"

            batch_runner.write_manifest_rows(
                str(manifest_path),
                build_manifest_rows(
                    [
                        {
                            "work_unit_id": "prop-1",
                            "status": "finished",
                            "worker_id": 0,
                            "started_at": "2026-04-16T10:00:00+00:00",
                            "finished_at": "2026-04-16T10:00:01+00:00",
                        }
                    ]
                ),
            )

            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": author_votes_job.JOB_NAME,
                        "run_id": "run-2",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-16T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            worker_dir = tmp_root / "worker_00"
            write_worker_mode_output(
                str(worker_dir),
                "prop-1",
                "favorite_choice",
                [["prop-1", "0xauthor", 2, 1.0, 5.0, 1, 1.0, "favorite_choice"]],
            )
            write_worker_mode_output(
                str(worker_dir),
                "prop-1",
                "support_fraction",
                [["prop-1", "0xauthor", 2, 1.0, 5.0, 1, 1.0, "support_fraction"]],
            )

            with mock.patch.object(author_votes_job, "RUN_META_PATH", str(run_meta_path)), \
                mock.patch.object(author_votes_job, "RUN_SUMMARY_PATH", str(run_summary_path)), \
                mock.patch.object(author_votes_job, "REDUCE_STATUS_PATH", str(reduce_status_path)):
                summary = author_votes_job.reduce_author_vote_outputs(
                    clean_output_dir=str(output_dir),
                    manifest_path=str(manifest_path),
                )

            favorite = pd.read_csv(output_dir / author_votes_job.FAVORITE_FILE)
            support = pd.read_csv(output_dir / author_votes_job.SUPPORT_FILE)
            run_summary = json.loads(run_summary_path.read_text(encoding="utf-8"))

            self.assertEqual(len(favorite), 1)
            self.assertEqual(len(support), 1)
            self.assertTrue(run_summary["is_complete"])
            self.assertEqual(run_summary["manifest_total"], 1)
            self.assertEqual(run_summary["manifest_finished"], 1)
            self.assertEqual(run_summary["manifest_pending"], 0)
            self.assertEqual(run_summary["run_id"], "run-2")
            self.assertEqual(run_summary["tmp_root"], str(tmp_root))
            self.assertEqual(summary["manifest_finished"], 1)
            self.assertEqual(summary["favorite_choice"]["rows"], 1)
            self.assertEqual(summary["support_fraction"]["rows"], 1)

    def test_reduce_author_vote_outputs_refuses_missing_manifest_or_run_meta(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "clean_author_vote_outputs"
            manifest_dir = output_dir / "manifests"
            manifest_dir.mkdir(parents=True, exist_ok=True)

            run_summary_path = output_dir / "run_summary.json"
            reduce_status_path = manifest_dir / f"{author_votes_job.JOB_NAME}_reduce_status.json"
            favorite_root = output_dir / author_votes_job.FAVORITE_FILE
            favorite_root.parent.mkdir(parents=True, exist_ok=True)
            favorite_root.write_text("sentinel favorite\n", encoding="utf-8")

            with mock.patch.object(author_votes_job, "RUN_META_PATH", str(manifest_dir / "missing_run.json")), \
                mock.patch.object(author_votes_job, "RUN_SUMMARY_PATH", str(run_summary_path)), \
                mock.patch.object(author_votes_job, "REDUCE_STATUS_PATH", str(reduce_status_path)):
                with self.assertRaises(RuntimeError) as exc_info:
                    author_votes_job.reduce_author_vote_outputs(
                        clean_output_dir=str(output_dir),
                        manifest_path=str(manifest_dir / "missing_manifest.csv"),
                    )

            self.assertIn("missing manifest", str(exc_info.exception))
            self.assertIn("missing run_id", str(exc_info.exception))
            self.assertEqual(favorite_root.read_text(encoding="utf-8"), "sentinel favorite\n")
            self.assertFalse(run_summary_path.exists())

            reduce_status = json.loads(reduce_status_path.read_text(encoding="utf-8"))
            self.assertFalse(reduce_status["is_complete"])
            self.assertEqual(reduce_status["manifest_total"], 0)
            self.assertEqual(reduce_status["run_id"], "")

    def test_reduce_author_vote_outputs_refuses_finished_proposal_with_empty_worker_chunks(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir) / "clean_author_vote_outputs"
            manifest_dir = output_dir / "manifests"
            tmp_root = output_dir / "tmp" / "run-empty"
            manifest_dir.mkdir(parents=True, exist_ok=True)
            tmp_root.mkdir(parents=True, exist_ok=True)

            manifest_path = manifest_dir / f"{author_votes_job.JOB_NAME}_manifest.csv"
            run_meta_path = manifest_dir / f"{author_votes_job.JOB_NAME}_run.json"
            run_summary_path = output_dir / "run_summary.json"
            reduce_status_path = manifest_dir / f"{author_votes_job.JOB_NAME}_reduce_status.json"

            batch_runner.write_manifest_rows(
                str(manifest_path),
                build_manifest_rows(
                    [
                        {
                            "work_unit_id": "prop-empty",
                            "status": "finished",
                            "worker_id": 0,
                            "started_at": "2026-04-20T10:00:00+00:00",
                            "finished_at": "2026-04-20T10:00:01+00:00",
                        }
                    ]
                ),
            )

            run_meta_path.write_text(
                json.dumps(
                    {
                        "job_name": author_votes_job.JOB_NAME,
                        "run_id": "run-empty",
                        "tmp_root": str(tmp_root),
                        "generated_at": "2026-04-20T10:00:00+00:00",
                    }
                ),
                encoding="utf-8",
            )

            favorite_root = output_dir / author_votes_job.FAVORITE_FILE
            favorite_root.parent.mkdir(parents=True, exist_ok=True)
            favorite_root.write_text("sentinel favorite\n", encoding="utf-8")

            proposal_dir = tmp_root / "worker_00" / "proposals" / "prop-empty"
            proposal_dir.mkdir(parents=True, exist_ok=True)
            (proposal_dir / "favorite_choice.csv").touch()
            (proposal_dir / "support_fraction.csv").touch()

            with mock.patch.object(author_votes_job, "RUN_META_PATH", str(run_meta_path)), \
                mock.patch.object(author_votes_job, "RUN_SUMMARY_PATH", str(run_summary_path)), \
                mock.patch.object(author_votes_job, "REDUCE_STATUS_PATH", str(reduce_status_path)):
                with self.assertRaises(RuntimeError) as exc_info:
                    author_votes_job.reduce_author_vote_outputs(
                        clean_output_dir=str(output_dir),
                        manifest_path=str(manifest_path),
                    )

            self.assertIn("empty data chunk", str(exc_info.exception))
            self.assertEqual(favorite_root.read_text(encoding="utf-8"), "sentinel favorite\n")

            reduce_status = json.loads(reduce_status_path.read_text(encoding="utf-8"))
            self.assertFalse(reduce_status["is_complete"])
            self.assertEqual(reduce_status["manifest_finished"], 1)

    def test_commit_staged_outputs_rolls_back_partial_replace_on_failure(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir_path = Path(tmpdir)
            favorite_path = tmpdir_path / "prop_author_favorite_choice.csv"
            support_path = tmpdir_path / "prop_author_support_fraction.csv"
            favorite_path.write_text("old favorite\n", encoding="utf-8")
            support_path.write_text("old support\n", encoding="utf-8")

            staged_favorite = tmpdir_path / "favorite.tmp"
            staged_support = tmpdir_path / "support.tmp"
            staged_favorite.write_text("new favorite\n", encoding="utf-8")
            staged_support.write_text("new support\n", encoding="utf-8")

            original_replace = os.replace
            failure_state = {"raised": False}

            def flaky_replace(src, dst):
                original_replace(src, dst)
                if (
                    src == str(staged_support)
                    and dst == str(support_path)
                    and not failure_state["raised"]
                ):
                    failure_state["raised"] = True
                    raise OSError("simulated commit failure")

            with mock.patch("dao_bias.jobs.author_votes.os.replace", side_effect=flaky_replace):
                with self.assertRaises(OSError):
                    author_votes_job.commit_staged_outputs(
                        [
                            (str(staged_favorite), str(favorite_path)),
                            (str(staged_support), str(support_path)),
                        ]
                    )

            self.assertEqual(favorite_path.read_text(encoding="utf-8"), "old favorite\n")
            self.assertEqual(support_path.read_text(encoding="utf-8"), "old support\n")
            self.assertFalse(staged_favorite.exists())
            self.assertFalse(staged_support.exists())


if __name__ == "__main__":
    unittest.main()
