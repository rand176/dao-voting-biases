import csv
import json
import os
import signal
import tempfile
import traceback
from datetime import datetime, timezone
from multiprocessing import Process

import fcntl


# Per-process shutdown flag. Set by SIGHUP/SIGTERM handlers inside worker
# processes so the main work loop can exit cleanly after the current unit.
_worker_shutdown_requested = False


def _install_worker_signal_handlers(worker_id, log_path):
    """Register graceful shutdown handlers for SIGHUP and SIGTERM in a worker process.

    On disconnect (SIGHUP) or termination (SIGTERM) Python's default action is
    immediate process death, leaving the manifest with orphaned 'running' rows.
    Installing a Python-level handler prevents that: the signal sets a flag and
    the work loop exits cleanly after the current unit finishes.
    """
    sig_names = {signal.SIGHUP: "SIGHUP", signal.SIGTERM: "SIGTERM"}

    def _handler(signum, frame):
        global _worker_shutdown_requested
        _worker_shutdown_requested = True
        sig_name = sig_names.get(signum, f"SIG{signum}")
        try:
            append_worker_log(
                log_path,
                f"[{utc_now_iso()}] worker {worker_id} received {sig_name}, "
                "stopping after current unit",
            )
        except Exception:
            pass

    signal.signal(signal.SIGHUP, _handler)
    signal.signal(signal.SIGTERM, _handler)


MANIFEST_FIELDS = [
    "job_name",
    "work_unit_id",
    "status",
    "worker_id",
    "started_at",
    "finished_at",
    "error_count",
]

DEFAULT_MANIFEST_FLUSH_EVERY = 25


def utc_now_iso():
    return datetime.now(timezone.utc).isoformat()


def ensure_parent_dir(path):
    os.makedirs(os.path.dirname(path), exist_ok=True)


def ensure_dir(path):
    os.makedirs(path, exist_ok=True)


def worker_state_path(worker_dir):
    return os.path.join(worker_dir, "worker_state.json")


def with_locked_file(lock_path, fn):
    ensure_parent_dir(lock_path)
    with open(lock_path, "a+", encoding="utf-8") as handle:
        fcntl.flock(handle.fileno(), fcntl.LOCK_EX)
        try:
            return fn()
        finally:
            fcntl.flock(handle.fileno(), fcntl.LOCK_UN)


def load_manifest(manifest_path):
    if not os.path.exists(manifest_path):
        return []

    with open(manifest_path, "r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return list(reader)


def _atomic_write_csv_rows(path, fieldnames, rows):
    ensure_parent_dir(path)
    fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".csv", dir=os.path.dirname(path))
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def write_manifest_rows(manifest_path, rows):
    _atomic_write_csv_rows(manifest_path, MANIFEST_FIELDS, rows)


def build_manifest(work_unit_ids, manifest_path, job_name):
    rows = []
    for work_unit_id in work_unit_ids:
        rows.append(
            {
                "job_name": job_name,
                "work_unit_id": str(work_unit_id),
                "status": "pending",
                "worker_id": "",
                "started_at": "",
                "finished_at": "",
                "error_count": "0",
            }
        )

    write_manifest_rows(manifest_path, rows)
    return rows


def partition_work(work_unit_ids, num_workers):
    partitions = [[] for _ in range(num_workers)]
    for idx, work_unit_id in enumerate(sorted(work_unit_ids)):
        partitions[idx % num_workers].append(work_unit_id)
    return partitions


def update_manifest_row(manifest_path, work_unit_id, **updates):
    return bulk_update_manifest_rows(manifest_path, {work_unit_id: updates})


def bulk_update_manifest_rows(manifest_path, updates_by_work_unit):
    if not updates_by_work_unit:
        return

    lock_path = manifest_path + ".lock"
    normalized_updates = {
        str(work_unit_id): {key: str(value) for key, value in updates.items()}
        for work_unit_id, updates in updates_by_work_unit.items()
    }

    def _update():
        rows = load_manifest(manifest_path)
        rows_by_work_unit = {row["work_unit_id"]: row for row in rows}
        for work_unit_id, updates in normalized_updates.items():
            row = rows_by_work_unit.get(work_unit_id)
            if row is None:
                continue
            for key, value in updates.items():
                row[key] = value
        write_manifest_rows(manifest_path, rows)

    with_locked_file(lock_path, _update)


class BufferedManifestUpdater:
    def __init__(self, manifest_path, flush_every=DEFAULT_MANIFEST_FLUSH_EVERY):
        self.manifest_path = manifest_path
        self.flush_every = max(1, int(flush_every))
        self._pending_updates = {}

    def mark_partition_running(self, work_unit_ids, worker_id, started_at):
        updates = {
            str(work_unit_id): {
                "status": "running",
                "worker_id": str(worker_id),
                "started_at": str(started_at),
                "finished_at": "",
            }
            for work_unit_id in work_unit_ids
        }
        bulk_update_manifest_rows(self.manifest_path, updates)

    def _queue(self, work_unit_id, **updates):
        work_unit_id = str(work_unit_id)
        pending = self._pending_updates.setdefault(work_unit_id, {})
        for key, value in updates.items():
            pending[key] = str(value)

    def mark_finished(self, work_unit_id, worker_id, finished_at):
        self._queue(
            work_unit_id,
            status="finished",
            worker_id=worker_id,
            finished_at=finished_at,
        )
        self.flush_if_needed()

    def mark_failed(self, work_unit_id, worker_id, finished_at, error_count):
        self._queue(
            work_unit_id,
            status="failed",
            worker_id=worker_id,
            finished_at=finished_at,
            error_count=error_count,
        )
        self.flush(force=True)

    def flush_if_needed(self):
        if len(self._pending_updates) >= self.flush_every:
            self.flush(force=True)

    def flush(self, force=False):
        if not self._pending_updates:
            return
        if not force and len(self._pending_updates) < self.flush_every:
            return
        updates = self._pending_updates
        self._pending_updates = {}
        bulk_update_manifest_rows(self.manifest_path, updates)


def rows_by_status(manifest_path, statuses):
    rows = load_manifest(manifest_path)
    return [row for row in rows if row["status"] in statuses]


def pending_work_units(manifest_path):
    return [row["work_unit_id"] for row in rows_by_status(manifest_path, {"pending"})]


def failed_work_units(manifest_path):
    return [row["work_unit_id"] for row in rows_by_status(manifest_path, {"failed"})]


def running_work_units(manifest_path):
    return [row["work_unit_id"] for row in rows_by_status(manifest_path, {"running"})]


def finished_work_units(manifest_path):
    return [row["work_unit_id"] for row in rows_by_status(manifest_path, {"finished"})]


def summarize_manifest_rows(rows):
    summary = {
        "pending": 0,
        "running": 0,
        "finished": 0,
        "failed": 0,
        "total": len(rows),
    }
    for row in rows:
        status = row.get("status")
        summary[status] = summary.get(status, 0) + 1
    return summary


def load_manifest_snapshot(manifest_path):
    rows = load_manifest(manifest_path)
    return rows, summarize_manifest_rows(rows)


def summarize_manifest(manifest_path):
    _, summary = load_manifest_snapshot(manifest_path)
    return summary


def has_running_work(manifest_path):
    return any(row["status"] == "running" for row in load_manifest(manifest_path))


def reclaim_running_work(manifest_path, new_status="failed"):
    lock_path = manifest_path + ".lock"

    def _reclaim():
        rows = load_manifest(manifest_path)
        reclaimed = []
        for row in rows:
            if row["status"] == "running":
                row["status"] = new_status
                row["finished_at"] = utc_now_iso()
                row["worker_id"] = ""
                reclaimed.append(row["work_unit_id"])
        write_manifest_rows(manifest_path, rows)
        return reclaimed

    return with_locked_file(lock_path, _reclaim)


def append_worker_log(log_path, message):
    ensure_parent_dir(log_path)
    with open(log_path, "a", encoding="utf-8") as handle:
        handle.write(message.rstrip() + "\n")


def _safe_load_json(path):
    if not os.path.exists(path):
        return {}
    try:
        with open(path, "r", encoding="utf-8") as handle:
            raw = handle.read().strip()
    except OSError:
        return {}
    if not raw:
        return {}
    try:
        return json.loads(raw)
    except json.JSONDecodeError:
        return {}


def _atomic_write_json(path, payload):
    ensure_parent_dir(path)
    fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".json", dir=os.path.dirname(path))
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            json.dump(payload, handle, indent=2, sort_keys=True)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def write_worker_state(worker_dir, **state):
    ensure_dir(worker_dir)
    path = worker_state_path(worker_dir)
    lock_path = path + ".lock"

    def _write():
        existing = _safe_load_json(path)
        existing.update(state)
        _atomic_write_json(path, existing)

    with_locked_file(lock_path, _write)


def load_worker_state(worker_dir):
    path = worker_state_path(worker_dir)
    return _safe_load_json(path)


def _pid_exists(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _pid_cmdline(pid):
    proc_cmdline = f"/proc/{pid}/cmdline"
    if not os.path.exists(proc_cmdline):
        return ""
    try:
        with open(proc_cmdline, "rb") as handle:
            raw = handle.read().replace(b"\x00", b" ").decode("utf-8", errors="ignore")
        return raw.strip()
    except OSError:
        return ""


def is_pid_running(pid, expected_hint=None):
    if not pid:
        return False
    try:
        pid = int(pid)
    except (TypeError, ValueError):
        return False
    if not _pid_exists(pid):
        return False
    if expected_hint:
        cmdline = _pid_cmdline(pid)
        if cmdline and expected_hint not in cmdline:
            return False
    return True


def worker_states(tmp_root, expected_hint=None):
    if not tmp_root or not os.path.exists(tmp_root):
        return []
    states = []
    for entry in sorted(os.listdir(tmp_root)):
        worker_dir = os.path.join(tmp_root, entry)
        if not os.path.isdir(worker_dir) or not entry.startswith("worker_"):
            continue
        state = load_worker_state(worker_dir)
        state["worker_dir"] = worker_dir
        state["worker_name"] = entry
        pid = state.get("pid")
        state["is_alive"] = is_pid_running(pid, expected_hint=expected_hint)
        states.append(state)
    return states


def any_live_workers(tmp_root, expected_hint=None):
    return any(state.get("is_alive") for state in worker_states(tmp_root, expected_hint=expected_hint))


def update_worker_progress(worker_progress_path, work_unit_id, status, error_count=0):
    ensure_parent_dir(worker_progress_path)
    progress = _safe_load_json(worker_progress_path)
    if not progress:
        progress = {"completed": [], "failed": [], "updated_at": ""}

    if status == "finished":
        if str(work_unit_id) not in progress["completed"]:
            progress["completed"].append(str(work_unit_id))
        progress["failed"] = [item for item in progress["failed"] if item != str(work_unit_id)]
    elif status == "failed":
        if str(work_unit_id) not in progress["failed"]:
            progress["failed"].append(str(work_unit_id))

    progress["updated_at"] = utc_now_iso()
    progress["last_error_count"] = int(error_count)

    _atomic_write_json(worker_progress_path, progress)


def _worker_entry(worker_id, work_unit_ids, worker_fn, worker_dir, manifest_path, job_name):
    global _worker_shutdown_requested
    _worker_shutdown_requested = False  # Reset for this process (fork may inherit parent state)

    log_path = os.path.join(worker_dir, "worker.log")
    progress_path = os.path.join(worker_dir, "worker_progress.json")
    manifest_updater = BufferedManifestUpdater(manifest_path)
    ensure_dir(worker_dir)

    # Install graceful shutdown handlers before any work begins so that SIGHUP
    # (terminal disconnect) and SIGTERM don't kill the process silently.
    _install_worker_signal_handlers(worker_id, log_path)

    try:
        write_worker_state(
            worker_dir,
            worker_id=worker_id,
            pid=os.getpid(),
            status="running",
            started_at=utc_now_iso(),
            finished_at="",
            exit_code="",
            job_name=job_name,
            manifest_path=manifest_path,
            assigned_units=len(work_unit_ids),
        )
        append_worker_log(log_path, f"[{utc_now_iso()}] worker {worker_id} starting with {len(work_unit_ids)} units")
        manifest_updater.mark_partition_running(work_unit_ids, worker_id=worker_id, started_at=utc_now_iso())

        for work_unit_id in work_unit_ids:
            if _worker_shutdown_requested:
                append_worker_log(
                    log_path,
                    f"[{utc_now_iso()}] worker {worker_id} stopping due to shutdown signal",
                )
                break

            try:
                worker_fn(worker_id=worker_id, work_unit_id=work_unit_id, worker_dir=worker_dir, manifest_path=manifest_path)
                manifest_updater.mark_finished(
                    work_unit_id,
                    worker_id=worker_id,
                    finished_at=utc_now_iso(),
                )
                update_worker_progress(progress_path, work_unit_id, "finished")
                append_worker_log(log_path, f"[{utc_now_iso()}] finished {work_unit_id}")
            except BaseException as exc:
                rows = load_manifest(manifest_path)
                current_error_count = 1
                for row in rows:
                    if row["work_unit_id"] == str(work_unit_id):
                        current_error_count = int(row["error_count"] or "0") + 1
                        break
                manifest_updater.mark_failed(
                    work_unit_id,
                    worker_id=worker_id,
                    finished_at=utc_now_iso(),
                    error_count=current_error_count,
                )
                update_worker_progress(progress_path, work_unit_id, "failed", error_count=current_error_count)
                append_worker_log(log_path, f"[{utc_now_iso()}] failed {work_unit_id}: {repr(exc)}")
                append_worker_log(log_path, traceback.format_exc())
                if not isinstance(exc, Exception):
                    # KeyboardInterrupt / SystemExit — mark state and re-raise so
                    # the outer handler can record the crash and propagate.
                    raise

        manifest_updater.flush(force=True)

        if _worker_shutdown_requested:
            append_worker_log(log_path, f"[{utc_now_iso()}] worker {worker_id} interrupted")
            write_worker_state(
                worker_dir,
                status="interrupted",
                finished_at=utc_now_iso(),
                exit_code=128,
            )
        else:
            append_worker_log(log_path, f"[{utc_now_iso()}] worker {worker_id} finished")
            write_worker_state(
                worker_dir,
                status="finished",
                finished_at=utc_now_iso(),
                exit_code=0,
            )
    except BaseException as exc:
        append_worker_log(log_path, f"[{utc_now_iso()}] worker {worker_id} crashed: {repr(exc)}")
        append_worker_log(log_path, traceback.format_exc())
        write_worker_state(
            worker_dir,
            status="crashed",
            finished_at=utc_now_iso(),
            exit_code=1,
        )
        raise


def run_partitions(worker_fn, partitions, tmp_root, manifest_path, num_workers, job_name):
    import gc
    gc.freeze()  # Prevent COW page copies for already-loaded objects (e.g. vote dataset)
    ensure_dir(tmp_root)
    processes = []
    for worker_id in range(num_workers):
        worker_dir = os.path.join(tmp_root, f"worker_{worker_id:02d}")
        process = Process(
            target=_worker_entry,
            args=(worker_id, partitions[worker_id], worker_fn, worker_dir, manifest_path, job_name),
        )
        process.start()
        write_worker_state(
            worker_dir,
            worker_id=worker_id,
            pid=process.pid,
            status="running",
            started_at=utc_now_iso(),
            finished_at="",
            exit_code="",
            job_name=job_name,
            manifest_path=manifest_path,
            assigned_units=len(partitions[worker_id]),
        )
        processes.append(process)

    exit_codes = []
    for worker_id, process in enumerate(processes):
        process.join()
        exit_codes.append(process.exitcode)
        worker_dir = os.path.join(tmp_root, f"worker_{worker_id:02d}")
        write_worker_state(
            worker_dir,
            status="finished" if process.exitcode == 0 else "exited",
            finished_at=utc_now_iso(),
            exit_code=process.exitcode,
        )
    return exit_codes
