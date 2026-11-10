import io
import sys
import unittest
from types import SimpleNamespace
from unittest import mock
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.jobs.runner as job_runner


class JobRunnerTests(unittest.TestCase):
    def test_command_status_uses_cli_job_name_for_worker_liveness_hint(self):
        args = SimpleNamespace(job_name="author_votes")
        fake_job = SimpleNamespace(
            MANIFEST_PATH="/tmp/fake_manifest.csv",
            JOB_NAME="author_votes_v1",
            current_tmp_root=lambda: "/tmp/fake_tmp_root",
        )

        with mock.patch.object(job_runner, "get_job", return_value=fake_job), \
            mock.patch.object(job_runner, "load_manifest_snapshot", return_value=([{"status": "running"}], {
                "total": 1,
                "pending": 0,
                "running": 1,
                "finished": 0,
                "failed": 0,
            })), \
            mock.patch.object(job_runner, "worker_states", return_value=[] ) as worker_states_mock, \
            mock.patch("sys.stdout", new_callable=io.StringIO):
            job_runner.command_status(args)

        worker_states_mock.assert_called_once_with("/tmp/fake_tmp_root", expected_hint="author_votes")

    def test_command_resume_uses_cli_job_name_for_live_worker_hint(self):
        args = SimpleNamespace(
            job_name="author_votes",
            no_build=True,
            reclaim_running=True,
            limit=None,
            workers=8,
        )
        fake_job = SimpleNamespace(
            MANIFEST_PATH="/tmp/fake_manifest.csv",
            JOB_NAME="author_votes_v1",
            current_tmp_root=lambda: "/tmp/fake_tmp_root",
            sample_work_units=lambda limit, selected: selected,
            reduce_vote_expansion_outputs=lambda: {"ok": True},
        )

        with mock.patch.object(job_runner, "get_job", return_value=fake_job), \
            mock.patch.object(job_runner, "load_context_and_work_units", return_value=({}, ["prop-1"])), \
            mock.patch.object(job_runner, "has_running_work", return_value=True), \
            mock.patch.object(job_runner, "any_live_workers", return_value=False) as any_live_workers_mock, \
            mock.patch.object(job_runner, "reclaim_running_work", return_value=[]), \
            mock.patch.object(job_runner, "pending_work_units", return_value=[]), \
            mock.patch.object(job_runner, "failed_work_units", return_value=[]), \
            mock.patch.object(job_runner, "_run_with_work_units", return_value=[]), \
            mock.patch.object(job_runner, "summarize_manifest", return_value={
                "total": 1,
                "pending": 0,
                "running": 0,
                "finished": 1,
                "failed": 0,
            }), \
            mock.patch("sys.stdout", new_callable=io.StringIO):
            job_runner.command_resume(args)

        any_live_workers_mock.assert_called_once_with("/tmp/fake_tmp_root", expected_hint="author_votes")


if __name__ == "__main__":
    unittest.main()
