import argparse
import os
import sys

from dao_bias.jobs.batch import any_live_workers, failed_work_units, has_running_work, load_manifest_snapshot, partition_work, pending_work_units, reclaim_running_work, run_partitions, summarize_manifest, worker_states
from dao_bias.jobs import JOB_REGISTRY, get_job
from dao_bias.jobs import batch as batch_runner


def ensure_log_dir():
    from dao_bias.data.paths import OUTPUT_ROOT
    (OUTPUT_ROOT / "logs").mkdir(parents=True,exist_ok=True)



def print_summary(summary):
    print("Manifest summary:")
    for key in ["total", "pending", "running", "finished", "failed"]:
        if key in summary:
            print(f"  {key}: {summary[key]}")


def assert_manifest_not_running(job_module):
    if os.path.exists(job_module.MANIFEST_PATH) and has_running_work(job_module.MANIFEST_PATH):
        raise RuntimeError(
            f"Cannot rebuild manifest for {job_module.JOB_NAME} while work units are marked running. "
            "Finish or clean up the current run before building a new manifest."
        )


def load_context_and_work_units(job_module):
    context = job_module.init_global_context()
    work_unit_ids = job_module.list_work_units(context["df_proposals"], context["votes_by_prop"])
    return context, work_unit_ids


def command_build_manifest(args):
    job_module = get_job(args.job_name)
    assert_manifest_not_running(job_module)
    work_unit_ids = job_module.list_manifest_work_units()
    if args.limit is not None:
        work_unit_ids = job_module.sample_work_units(args.limit, work_unit_ids)
    job_module.build_work_manifest(work_unit_ids, batch_runner)
    print(f"Manifest written to {job_module.MANIFEST_PATH}")
    print(f"Work units: {len(work_unit_ids)}")


def _run_with_work_units(job_module, work_unit_ids, num_workers):
    if not work_unit_ids:
        print("No work units to process.")
        return []

    partitions = partition_work(work_unit_ids, num_workers)
    tmp_root = job_module.current_tmp_root()
    if not tmp_root:
        raise RuntimeError("Missing run metadata. Rebuild the manifest before running this job.")
    exit_codes = run_partitions(
        worker_fn=job_module.worker_process_vote_expansion,
        partitions=partitions,
        tmp_root=tmp_root,
        manifest_path=job_module.MANIFEST_PATH,
        num_workers=num_workers,
        job_name=job_module.JOB_NAME,
    )
    return exit_codes


def command_run(args):
    job_module = get_job(args.job_name)
    _, work_unit_ids = load_context_and_work_units(job_module)

    if (not args.no_build) and ((not os.path.exists(job_module.MANIFEST_PATH)) or (not job_module.current_tmp_root())):
        job_module.build_work_manifest(work_unit_ids, batch_runner)

    if args.limit is not None:
        work_unit_ids = set(job_module.sample_work_units(args.limit, work_unit_ids))
    else:
        work_unit_ids = set(work_unit_ids)

    if args.resume_only:
        selected = set(pending_work_units(job_module.MANIFEST_PATH)).intersection(work_unit_ids)
    elif args.failed_only:
        selected = set(failed_work_units(job_module.MANIFEST_PATH)).intersection(work_unit_ids)
    else:
        selected = set(pending_work_units(job_module.MANIFEST_PATH)).intersection(work_unit_ids)

    exit_codes = _run_with_work_units(job_module, sorted(selected), args.workers)
    print(f"Worker exit codes: {exit_codes}")
    print_summary(summarize_manifest(job_module.MANIFEST_PATH))


def command_reduce(args):
    job_module = get_job(args.job_name)
    summary = job_module.reduce_vote_expansion_outputs()
    print("Reduce complete.")
    print(summary)


def command_status(args):
    job_module = get_job(args.job_name)
    manifest_rows, summary = load_manifest_snapshot(job_module.MANIFEST_PATH)
    print_summary(summary)
    tmp_root = job_module.current_tmp_root()
    print(f"tmp_root: {tmp_root or '<missing>'}")
    if not tmp_root:
        print("No current run metadata found.")
        return
    manifest_running_units = sum(1 for row in manifest_rows if row.get("status") == "running")
    print(f"manifest_running_units: {manifest_running_units}")
    worker_state_rows = worker_states(tmp_root, expected_hint=args.job_name)
    live_rows = [row for row in worker_state_rows if row.get('is_alive')]
    print(f"tracked_workers: {len(worker_state_rows)}")
    print(f"live_workers: {len(live_rows)}")
    for row in worker_state_rows:
        print(
            f"  {row.get('worker_name','?')}: pid={row.get('pid','')} status={row.get('status','')} "
            f"alive={row.get('is_alive', False)} exit_code={row.get('exit_code','')}"
        )


def command_resume(args):
    job_module = get_job(args.job_name)
    _, work_unit_ids = load_context_and_work_units(job_module)
    if (not args.no_build) and ((not os.path.exists(job_module.MANIFEST_PATH)) or (not job_module.current_tmp_root())):
        job_module.build_work_manifest(work_unit_ids, batch_runner)

    reclaimed = []
    if args.reclaim_running and has_running_work(job_module.MANIFEST_PATH):
        tmp_root = job_module.current_tmp_root()
        if tmp_root and any_live_workers(tmp_root, expected_hint=args.job_name):
            raise RuntimeError(
                "Cannot reclaim running work units because tracked workers are still alive. "
                "Use `PYTHONPATH=src/python python -B -m dao_bias.jobs.runner status <job_name>` to inspect worker liveness first."
            )
        reclaimed = reclaim_running_work(job_module.MANIFEST_PATH, new_status="failed")
        print(f"Reclaimed {len(reclaimed)} interrupted running work units as failed.")

    pending = set(pending_work_units(job_module.MANIFEST_PATH))
    failed = set(failed_work_units(job_module.MANIFEST_PATH))
    selected = sorted(pending.union(failed))
    if args.limit is not None:
        selected = job_module.sample_work_units(args.limit, selected)

    exit_codes = _run_with_work_units(job_module, selected, args.workers)
    print(f"Worker exit codes: {exit_codes}")
    print_summary(summarize_manifest(job_module.MANIFEST_PATH))
    summary = job_module.reduce_vote_expansion_outputs()
    print("Reduce complete.")
    print(summary)


def command_sample(args):
    job_module = get_job(args.job_name)
    assert_manifest_not_running(job_module)
    _, work_unit_ids = load_context_and_work_units(job_module)
    sample_ids = job_module.sample_work_units(args.limit, work_unit_ids)
    job_module.build_work_manifest(sample_ids, batch_runner)
    exit_codes = _run_with_work_units(job_module, sample_ids, args.workers)
    print(f"Worker exit codes: {exit_codes}")
    print_summary(summarize_manifest(job_module.MANIFEST_PATH))
    summary = job_module.reduce_vote_expansion_outputs()
    print("Reduce complete.")
    print(summary)


def build_parser():
    parser = argparse.ArgumentParser(description="Generic parallel batch job runner")
    subparsers = parser.add_subparsers(dest="command", required=True)

    manifest_parser = subparsers.add_parser("build-manifest", help="Build the work manifest")
    manifest_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")
    manifest_parser.add_argument("--limit", type=int, default=None, help="Limit manifest to the first N work units")

    run_parser = subparsers.add_parser("run", help="Run pending work units")
    run_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")
    run_parser.add_argument("--workers", type=int, default=None, help="Number of workers")
    run_parser.add_argument("--limit", type=int, default=None, help="Only run the first N selected work units")
    run_parser.add_argument("--resume-only", action="store_true", help="Run only pending work units from the manifest")
    run_parser.add_argument("--failed-only", action="store_true", help="Run only failed work units from the manifest")
    run_parser.add_argument("--no-build", action="store_true", help="Do not create a missing manifest automatically")

    reduce_parser = subparsers.add_parser("reduce", help="Merge worker outputs into canonical files")
    reduce_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")

    status_parser = subparsers.add_parser("status", help="Show manifest and worker liveness status")
    status_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")

    resume_parser = subparsers.add_parser("resume", help="Run pending and failed work, then reduce")
    resume_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")
    resume_parser.add_argument("--workers", type=int, default=None, help="Number of workers")
    resume_parser.add_argument("--limit", type=int, default=None, help="Only run the first N pending/failed units")
    resume_parser.add_argument("--no-build", action="store_true", help="Assume the manifest already exists")
    resume_parser.add_argument(
        "--no-reclaim-running",
        dest="reclaim_running",
        action="store_false",
        help="Do not reclaim stale running work units before resuming",
    )
    resume_parser.set_defaults(reclaim_running=True)

    sample_parser = subparsers.add_parser("sample", help="Run a small sample end-to-end")
    sample_parser.add_argument("job_name", choices=sorted(JOB_REGISTRY), help="Registered job name")
    sample_parser.add_argument("--limit", type=int, required=True, help="Number of work units in the sample")
    sample_parser.add_argument("--workers", type=int, default=None, help="Number of workers")

    return parser


def normalize_worker_defaults(args):
    if hasattr(args, "job_name"):
        job_module = get_job(args.job_name)
        if hasattr(args, "workers") and args.workers is None:
            args.workers = job_module.DEFAULT_WORKERS
    return args


def main(argv=None):
    ensure_log_dir()
    parser = build_parser()
    args = parser.parse_args(argv)
    args = normalize_worker_defaults(args)

    if args.command == "build-manifest":
        command_build_manifest(args)
    elif args.command == "run":
        command_run(args)
    elif args.command == "reduce":
        command_reduce(args)
    elif args.command == "status":
        command_status(args)
    elif args.command == "resume":
        command_resume(args)
    elif args.command == "sample":
        command_sample(args)
    else:
        parser.print_help()
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
