import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.jobs.batch as batch_runner


class BatchRunnerTests(unittest.TestCase):
    def test_summarize_manifest_rows_counts_all_statuses(self):
        rows = [
            {"status": "pending"},
            {"status": "pending"},
            {"status": "running"},
            {"status": "finished"},
            {"status": "failed"},
        ]

        summary = batch_runner.summarize_manifest_rows(rows)

        self.assertEqual(
            summary,
            {
                "pending": 2,
                "running": 1,
                "finished": 1,
                "failed": 1,
                "total": 5,
            },
        )

    def test_summarize_manifest_matches_written_snapshot(self):
        rows = [
            {
                "job_name": "job",
                "work_unit_id": "a",
                "status": "finished",
                "worker_id": "0",
                "started_at": "2026-04-16T10:00:00+00:00",
                "finished_at": "2026-04-16T10:00:01+00:00",
                "error_count": "0",
            },
            {
                "job_name": "job",
                "work_unit_id": "b",
                "status": "running",
                "worker_id": "1",
                "started_at": "2026-04-16T10:00:02+00:00",
                "finished_at": "",
                "error_count": "0",
            },
            {
                "job_name": "job",
                "work_unit_id": "c",
                "status": "pending",
                "worker_id": "",
                "started_at": "",
                "finished_at": "",
                "error_count": "0",
            },
        ]

        with tempfile.TemporaryDirectory() as tmpdir:
            manifest_path = Path(tmpdir) / "manifest.csv"
            batch_runner.write_manifest_rows(str(manifest_path), rows)

            loaded_rows = batch_runner.load_manifest(str(manifest_path))
            summary = batch_runner.summarize_manifest(str(manifest_path))

        self.assertEqual(loaded_rows, rows)
        self.assertEqual(
            summary,
            {
                "pending": 1,
                "running": 1,
                "finished": 1,
                "failed": 0,
                "total": 3,
            },
        )

    def test_buffered_manifest_updater_batches_finished_updates(self):
        rows = [
            {
                "job_name": "job",
                "work_unit_id": "a",
                "status": "pending",
                "worker_id": "",
                "started_at": "",
                "finished_at": "",
                "error_count": "0",
            },
            {
                "job_name": "job",
                "work_unit_id": "b",
                "status": "pending",
                "worker_id": "",
                "started_at": "",
                "finished_at": "",
                "error_count": "0",
            },
        ]

        with tempfile.TemporaryDirectory() as tmpdir:
            manifest_path = Path(tmpdir) / "manifest.csv"
            batch_runner.write_manifest_rows(str(manifest_path), rows)

            updater = batch_runner.BufferedManifestUpdater(
                str(manifest_path),
                flush_every=2,
            )
            updater.mark_partition_running(["a", "b"], worker_id=7, started_at="2026-04-16T10:00:00+00:00")

            running_rows = batch_runner.load_manifest(str(manifest_path))
            self.assertEqual([row["status"] for row in running_rows], ["running", "running"])
            self.assertEqual([row["worker_id"] for row in running_rows], ["7", "7"])

            updater.mark_finished("a", worker_id=7, finished_at="2026-04-16T10:01:00+00:00")
            still_running_rows = batch_runner.load_manifest(str(manifest_path))
            self.assertEqual([row["status"] for row in still_running_rows], ["running", "running"])

            updater.mark_finished("b", worker_id=7, finished_at="2026-04-16T10:02:00+00:00")
            finished_rows = batch_runner.load_manifest(str(manifest_path))
            self.assertEqual([row["status"] for row in finished_rows], ["finished", "finished"])
            self.assertEqual(
                [row["finished_at"] for row in finished_rows],
                ["2026-04-16T10:01:00+00:00", "2026-04-16T10:02:00+00:00"],
            )


if __name__ == "__main__":
    unittest.main()
