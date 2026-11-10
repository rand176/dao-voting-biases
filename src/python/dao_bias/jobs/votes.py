import json
import hashlib
import os
import tempfile
import uuid
from datetime import datetime, timezone

import pandas as pd

from dao_bias.data.paths import CLEAN_VOTE_OUTPUTS_DIR, raw_data_path
from dao_bias.preparation.vote_issues import CANONICAL_ISSUE_FLAGS_FILE, DEFAULT_DUPLICATE_FLAGS_PATH, build_and_write_proposal_vote_issue_flags


DATA_DIR = str(raw_data_path())
OUTPUT_DIR = str(CLEAN_VOTE_OUTPUTS_DIR)
MANIFEST_DIR = os.path.join(OUTPUT_DIR, "manifests")
TMP_DIR = os.path.join(OUTPUT_DIR, "tmp")

JOB_NAME = "vote_expansion_v3"
DEFAULT_WORKERS = 8
MANIFEST_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_manifest.csv")
RUN_META_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_run.json")
RUN_SUMMARY_PATH = os.path.join(OUTPUT_DIR, "run_summary.json")
PROPOSAL_DUPLICATE_BALLOT_FLAGS_PATH = str(DEFAULT_DUPLICATE_FLAGS_PATH)
PROPOSAL_VOTE_ISSUE_FLAGS_PATH = str(CANONICAL_ISSUE_FLAGS_FILE)

FAVORITE_FILE = "prop_choices_favorite_counts.csv"
FAVORITE_ERR_FILE = "prop_choices_favorite_counts_ERR.csv"
SUPPORT_FILE = "prop_choices_support_fraction.csv"
SUPPORT_ERR_FILE = "prop_choices_support_fraction_ERR.csv"

VOTE_MODES = {
    "favorite_choice_counts": {
        "file": FAVORITE_FILE,
        "err_file": FAVORITE_ERR_FILE,
        "value_col": "n_votes",
        "worker_file": "favorite_counts.csv",
        "worker_err_file": "favorite_errors.csv",
        "columns": ["proposal", "choice_idx", "n_votes", "tot_votes", "vote_mode"],
    },
    "support_fraction": {
        "file": SUPPORT_FILE,
        "err_file": SUPPORT_ERR_FILE,
        "value_col": "support_fraction_sum",
        "worker_file": "support_fraction.csv",
        "worker_err_file": "support_errors.csv",
        "columns": ["proposal", "choice_idx", "support_fraction_sum", "tot_votes", "vote_mode"],
    },
}

ERROR_COLUMNS = ["proposal", "vote_mode", "type", "reason", "detail", "choice"]

GLOBAL_CONTEXT = {}
ACTIVE_PROPOSAL_ATTEMPTS = {}


def utc_now_iso():
    return datetime.now(timezone.utc).isoformat()


def new_run_id():
    return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ")


def new_attempt_id():
    return uuid.uuid4().hex


def ensure_dir(path):
    os.makedirs(path, exist_ok=True)


def ensure_output_dirs():
    ensure_dir(OUTPUT_DIR)
    ensure_dir(MANIFEST_DIR)
    ensure_dir(TMP_DIR)


def write_run_meta(run_id, tmp_root):
    ensure_output_dirs()
    with open(RUN_META_PATH, "w", encoding="utf-8") as handle:
        json.dump(
            {
                "job_name": JOB_NAME,
                "run_id": run_id,
                "tmp_root": tmp_root,
                "generated_at": utc_now_iso(),
            },
            handle,
            indent=2,
            sort_keys=True,
        )


def load_run_meta():
    if not os.path.exists(RUN_META_PATH):
        return {}
    with open(RUN_META_PATH, "r", encoding="utf-8") as handle:
        return json.load(handle)


def current_tmp_root():
    meta = load_run_meta()
    return meta.get("tmp_root", "")


def is_int_like(value):
    return isinstance(value, int) or (isinstance(value, str) and value.isdigit())


def try_load_choices(choices):
    try:
        return json.loads(choices)
    except Exception:
        return []


def parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    try:
        return json.loads(choice_raw)
    except Exception as exc:
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid json", repr(exc), worker_dir)
        return None


def _output_subdir_name(mode_name, err=False):
    mode = VOTE_MODES[mode_name]
    filename = mode["worker_err_file"] if err else mode["worker_file"]
    return os.path.splitext(filename)[0]


def _proposal_file_token(proposal_id):
    return hashlib.sha1(str(proposal_id).strip().lower().encode("utf-8")).hexdigest()


def _active_attempt_key(worker_dir, proposal_id):
    return (str(worker_dir), str(proposal_id).strip().lower())


def latest_attempt_marker_path(worker_dir, proposal_id, ensure=True):
    token = _proposal_file_token(proposal_id)
    output_dir = os.path.join(worker_dir, "proposal_attempt_markers", token[:2])
    if ensure:
        ensure_dir(output_dir)
    return os.path.join(output_dir, f"{token}.json")


def load_latest_attempt_marker(worker_dir, proposal_id):
    path = latest_attempt_marker_path(worker_dir, proposal_id, ensure=False)
    if not os.path.exists(path):
        return {}
    try:
        with open(path, "r", encoding="utf-8") as handle:
            return json.load(handle)
    except (OSError, json.JSONDecodeError):
        return {}


def current_attempt_id(worker_dir, proposal_id):
    active_attempt_id = ACTIVE_PROPOSAL_ATTEMPTS.get(_active_attempt_key(worker_dir, proposal_id))
    if active_attempt_id:
        return active_attempt_id
    marker = load_latest_attempt_marker(worker_dir, proposal_id)
    return marker.get("attempt_id", "")


def proposal_output_path(worker_dir, proposal_id, mode_name, err=False, attempt_id=None, ensure=True):
    if attempt_id is None:
        attempt_id = current_attempt_id(worker_dir, proposal_id)
    token = _proposal_file_token(proposal_id)
    output_dir = os.path.join(worker_dir, _output_subdir_name(mode_name, err=err), token[:2], token)
    if ensure:
        ensure_dir(output_dir)
    return os.path.join(output_dir, f"{attempt_id or 'uncommitted'}.csv")


def _atomic_write_rows(path, df):
    ensure_dir(os.path.dirname(path))
    fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".csv", dir=os.path.dirname(path))
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="") as handle:
            df.to_csv(handle, index=False, header=False)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def _atomic_write_json(path, payload):
    ensure_dir(os.path.dirname(path))
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


def write_proposal_rows(worker_dir, proposal_id, mode_name, df, err=False, append=False):
    file_path = proposal_output_path(worker_dir, proposal_id, mode_name, err=err)
    columns = ERROR_COLUMNS if err else VOTE_MODES[mode_name]["columns"]
    output_df = df.loc[:, columns].copy()

    if append and os.path.exists(file_path):
        existing = load_csv_if_exists(file_path, columns)
        if not existing.empty:
            output_df = pd.concat([existing, output_df], ignore_index=True)

    _atomic_write_rows(file_path, output_df)


def commit_proposal_attempt(worker_dir, proposal_id, attempt_id):
    _atomic_write_json(
        latest_attempt_marker_path(worker_dir, proposal_id),
        {
            "proposal": str(proposal_id).strip().lower(),
            "attempt_id": attempt_id,
            "completed_at": utc_now_iso(),
        },
    )


def log_error(mode_name, prop_id, vote_type, choice_raw, reason, detail, worker_dir):
    df = pd.DataFrame(
        {
            "proposal": [prop_id],
            "vote_mode": [mode_name],
            "type": [vote_type],
            "reason": [reason],
            "detail": [detail],
            "choice": [repr(choice_raw)],
        }
    )
    write_proposal_rows(worker_dir, prop_id, mode_name, df, err=True, append=True)


def normalize_single_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    if is_int_like(choice_raw):
        return [{"choice_idx": int(choice_raw), "weight": 1.0}]
    log_error(mode_name, prop_id, vote_type, choice_raw, "invalid choice_idx", "", worker_dir)
    return []


def normalize_ranked_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if not isinstance(parsed, list) or len(parsed) == 0:
        log_error(mode_name, prop_id, vote_type, choice_raw, "empty ranked-choice ballot", "", worker_dir)
        return []

    top_choice = parsed[0]
    if not is_int_like(top_choice):
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid ranked choice", "", worker_dir)
        return []

    return [{"choice_idx": int(top_choice), "weight": 1.0}]


def normalize_approval(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if parsed is None:
        return []

    if isinstance(parsed, list) and len(parsed) > 0 and isinstance(parsed[0], list):
        parsed = parsed[0]

    if not isinstance(parsed, list):
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid approval ballot", "", worker_dir)
        return []

    clean_choices = []
    for choice_idx in parsed:
        if not is_int_like(choice_idx):
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid approval choice", repr(choice_idx), worker_dir)
            return []
        clean_choices.append(int(choice_idx))

    if not clean_choices:
        log_error(mode_name, prop_id, vote_type, choice_raw, "empty approval ballot", "", worker_dir)
        return []

    deduped_choices = list(dict.fromkeys(clean_choices))
    return [{"choice_idx": choice_idx, "weight": 1.0} for choice_idx in deduped_choices]


def normalize_weighted(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if not isinstance(parsed, dict):
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted ballot", "", worker_dir)
        return []

    clean_entries = []
    for raw_key, raw_value in parsed.items():
        if not is_int_like(raw_key):
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted choice", repr(raw_key), worker_dir)
            return []

        try:
            value = float(raw_value)
        except Exception as exc:
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted value", repr(exc), worker_dir)
            return []

        if value > 0:
            clean_entries.append((int(raw_key), value))

    if not clean_entries:
        log_error(mode_name, prop_id, vote_type, choice_raw, "no positive weighted entries", "", worker_dir)
        return []

    return clean_entries


def favorite_from_normalized(normalized_choices, vote_type, prop_id, choice_raw, worker_dir):
    if vote_type == "approval":
        split_weight = 1.0 / len(normalized_choices)
        return [{"choice_idx": item["choice_idx"], "weight": split_weight} for item in normalized_choices]

    if vote_type in ("single-choice", "basic", "ranked-choice"):
        return normalized_choices

    if vote_type in ("weighted", "quadratic"):
        max_weight = max(weight for _, weight in normalized_choices)
        top_choices = [choice_idx for choice_idx, weight in normalized_choices if weight == max_weight]
        split_weight = 1.0 / len(top_choices)
        return [{"choice_idx": choice_idx, "weight": split_weight} for choice_idx in top_choices]

    log_error("favorite_choice_counts", prop_id, vote_type, choice_raw, "unsupported vote type", "", worker_dir)
    return []


def support_from_normalized(normalized_choices, vote_type, prop_id, choice_raw, worker_dir):
    if vote_type in ("single-choice", "basic", "ranked-choice"):
        return normalized_choices

    if vote_type == "approval":
        split_weight = 1.0 / len(normalized_choices)
        return [{"choice_idx": item["choice_idx"], "weight": split_weight} for item in normalized_choices]

    if vote_type in ("weighted", "quadratic"):
        total_weight = sum(weight for _, weight in normalized_choices)
        if total_weight <= 0:
            log_error("support_fraction", prop_id, vote_type, choice_raw, "no positive weighted support", "", worker_dir)
            return []
        return [{"choice_idx": choice_idx, "weight": weight / total_weight} for choice_idx, weight in normalized_choices]

    log_error("support_fraction", prop_id, vote_type, choice_raw, "unsupported vote type", "", worker_dir)
    return []


def parse_vote_for_mode(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    if vote_type in ("single-choice", "basic"):
        normalized = normalize_single_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    elif vote_type == "ranked-choice":
        normalized = normalize_ranked_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    elif vote_type == "approval":
        normalized = normalize_approval(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    elif vote_type in ("weighted", "quadratic"):
        normalized = normalize_weighted(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    else:
        log_error(mode_name, prop_id, vote_type, choice_raw, "unsupported vote type", "", worker_dir)
        return []

    if not normalized:
        return []

    if mode_name == "favorite_choice_counts":
        return favorite_from_normalized(normalized, vote_type, prop_id, choice_raw, worker_dir)
    return support_from_normalized(normalized, vote_type, prop_id, choice_raw, worker_dir)


def aggregate_mode(prop_id, vote_type, votes, mode_name, worker_dir):
    value_col = VOTE_MODES[mode_name]["value_col"]
    ballot_records = []
    valid_ballots = 0

    for choice_raw in votes["choice"].tolist():
        parsed_choices = parse_vote_for_mode(choice_raw, vote_type, prop_id, mode_name, worker_dir)
        if not parsed_choices:
            continue

        valid_ballots += 1
        ballot_records.extend(parsed_choices)

    if not ballot_records:
        return pd.DataFrame(columns=VOTE_MODES[mode_name]["columns"])

    df = pd.DataFrame(ballot_records)
    df = df.groupby("choice_idx", as_index=False)["weight"].sum()
    df.rename(columns={"weight": value_col}, inplace=True)
    df["proposal"] = prop_id
    df["tot_votes"] = valid_ballots
    df["vote_mode"] = mode_name
    return df[VOTE_MODES[mode_name]["columns"]]


def load_vote_expansion_inputs(data_dir=DATA_DIR):
    print(f"[{utc_now_iso()}] Loading vote expansion context (this takes ~10 minutes)...", flush=True)

    print(f"[{utc_now_iso()}]   Reading vote files from {raw_data_path('votes')} ...", flush=True)
    votes_dir = str(raw_data_path("votes"))
    vote_frames = []
    for fname in sorted(os.listdir(votes_dir)):
        if not fname.endswith((".csv", ".csv.gz")):
            continue
        chunk = pd.read_csv(
            os.path.join(votes_dir, fname),
            usecols=["proposal", "choice"],
            dtype={"proposal": "category"},
            low_memory=False,
        )
        chunk["proposal"] = chunk["proposal"].str.lower()
        vote_frames.append(chunk)
    df_votes = pd.concat(vote_frames, ignore_index=True)
    df_votes["proposal"] = df_votes["proposal"].astype("category")
    print(f"[{utc_now_iso()}]   Loaded {len(df_votes):,} vote rows.", flush=True)

    print(f"[{utc_now_iso()}]   Reading proposals ...", flush=True)
    df_proposals = pd.read_csv(raw_data_path("proposals_0.csv.gz"), low_memory=False)
    df_proposals = df_proposals.loc[~((df_proposals.id.isna()) | (df_proposals.created.isna()))]
    df_proposals["choices"] = df_proposals["choices"].apply(try_load_choices)
    df_proposals["author"] = df_proposals["author"].str.lower()
    df_proposals["id"] = df_proposals["id"].str.lower()
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[df_proposals["id"].isin(df_votes["proposal"])]

    print(f"[{utc_now_iso()}]   Building vote groups ({len(df_proposals):,} proposals) ...", flush=True)
    df_votes_clean = df_votes.loc[df_votes["proposal"].isin(df_proposals["id"])]
    votes_by_prop = df_votes_clean.groupby("proposal")
    df_proposals = df_proposals.copy()
    df_proposals.set_index("id", inplace=True)

    print(
        f"[{utc_now_iso()}]   Context ready: {len(df_proposals):,} proposals, "
        f"{len(votes_by_prop):,} groups.",
        flush=True,
    )
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": votes_by_prop,
    }


def list_manifest_work_units(data_dir=DATA_DIR):
    proposal_cols = ["id", "created", "scores_state", "votes"]
    df_proposals = pd.read_csv(
        str(raw_data_path("proposals_0.csv.gz")),
        usecols=proposal_cols,
        low_memory=False,
    )
    df_proposals = df_proposals.loc[~((df_proposals["id"].isna()) | (df_proposals["created"].isna()))]
    df_proposals["id"] = df_proposals["id"].str.lower()
    df_proposals["votes"] = pd.to_numeric(df_proposals["votes"], errors="coerce").fillna(0)
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[df_proposals["votes"] > 0]
    return sorted(df_proposals["id"].drop_duplicates().tolist())


def init_global_context(context=None):
    global GLOBAL_CONTEXT
    if context is None:
        context = load_vote_expansion_inputs()
    GLOBAL_CONTEXT = context
    return GLOBAL_CONTEXT


def list_work_units(df_proposals, votes_by_prop):
    return sorted([proposal_id for proposal_id in df_proposals.index.tolist() if proposal_id in votes_by_prop.groups])


def process_proposal(proposal_id, context, worker_dir):
    prop = context["df_proposals"].loc[[proposal_id]].iloc[0]
    vote_type = prop["type"]
    votes = context["votes_by_prop"].get_group(proposal_id)
    attempt_id = new_attempt_id()
    ACTIVE_PROPOSAL_ATTEMPTS[_active_attempt_key(worker_dir, proposal_id)] = attempt_id
    try:
        favorite_df = aggregate_mode(proposal_id, vote_type, votes, "favorite_choice_counts", worker_dir)
        if not favorite_df.empty:
            write_proposal_rows(worker_dir, proposal_id, "favorite_choice_counts", favorite_df, append=False)

        support_df = aggregate_mode(proposal_id, vote_type, votes, "support_fraction", worker_dir)
        if not support_df.empty:
            write_proposal_rows(worker_dir, proposal_id, "support_fraction", support_df, append=False)

        commit_proposal_attempt(worker_dir, proposal_id, attempt_id)
    finally:
        ACTIVE_PROPOSAL_ATTEMPTS.pop(_active_attempt_key(worker_dir, proposal_id), None)


def worker_process_vote_expansion(worker_id, work_unit_id, worker_dir, manifest_path):
    if not GLOBAL_CONTEXT:
        init_global_context()
    process_proposal(work_unit_id, GLOBAL_CONTEXT, worker_dir)


def build_work_manifest(work_unit_ids, batch_runner_module):
    ensure_output_dirs()
    run_id = new_run_id()
    tmp_root = os.path.join(TMP_DIR, run_id)
    ensure_dir(tmp_root)
    write_run_meta(run_id, tmp_root)
    return batch_runner_module.build_manifest(work_unit_ids, MANIFEST_PATH, JOB_NAME)


def worker_dir_for_run(worker_id, tmp_root):
    return os.path.join(tmp_root, f"worker_{worker_id:02d}")


def worker_dirs_for_run(tmp_root):
    if not tmp_root or not os.path.exists(tmp_root):
        return []
    return [
        os.path.join(tmp_root, entry)
        for entry in sorted(os.listdir(tmp_root))
        if entry.startswith("worker_") and os.path.isdir(os.path.join(tmp_root, entry))
    ]


def completed_worker_progress_ids(worker_dir):
    progress_path = os.path.join(worker_dir, "worker_progress.json")
    if not os.path.exists(progress_path):
        return set()
    try:
        with open(progress_path, "r", encoding="utf-8") as handle:
            payload = json.load(handle)
    except (OSError, json.JSONDecodeError):
        return set()

    completed = payload.get("completed", [])
    return {
        str(work_unit_id).strip().lower()
        for work_unit_id in completed
        if str(work_unit_id).strip()
    }


def candidate_worker_dirs_by_proposal(tmp_root, finished_rows, durable_finished_ids):
    candidates = {proposal_id: set() for proposal_id in durable_finished_ids}
    worker_dirs = worker_dirs_for_run(tmp_root)

    for row in finished_rows:
        proposal_id = str(row["work_unit_id"]).lower()
        worker_id = row.get("worker_id", "")
        if proposal_id in candidates and str(worker_id).isdigit():
            candidates[proposal_id].add(worker_dir_for_run(int(worker_id), tmp_root))

    for worker_dir in worker_dirs:
        for proposal_id in completed_worker_progress_ids(worker_dir):
            if proposal_id in candidates:
                candidates[proposal_id].add(worker_dir)

    return candidates


def latest_attempt_artifacts(worker_dirs, proposal_id):
    candidates = []
    for worker_dir in worker_dirs:
        marker = load_latest_attempt_marker(worker_dir, proposal_id)
        attempt_id = marker.get("attempt_id", "")
        completed_at = marker.get("completed_at", "")
        if attempt_id and completed_at:
            candidates.append((completed_at, worker_dir, attempt_id))
    if not candidates:
        return {}
    candidates.sort()
    _, worker_dir, attempt_id = candidates[-1]
    return {
        (mode_name, err): proposal_output_path(
            worker_dir,
            proposal_id,
            mode_name,
            err=err,
            attempt_id=attempt_id,
            ensure=False,
        )
        for mode_name in VOTE_MODES
        for err in (False, True)
    }


def finished_manifest_rows(manifest_path=MANIFEST_PATH):
    import dao_bias.jobs.batch as batch_runner

    if not os.path.exists(manifest_path):
        return [], set()
    rows = batch_runner.load_manifest(manifest_path)
    finished_rows = [
        row for row in rows
        if row["status"] == "finished" and row["worker_id"].isdigit()
    ]
    finished_ids = {str(row["work_unit_id"]).lower() for row in finished_rows}
    return finished_rows, finished_ids


def load_csv_if_exists(path, columns):
    if not os.path.exists(path):
        return pd.DataFrame(columns=columns)
    return pd.read_csv(path, header=None, names=columns, low_memory=False)


def _write_final_output(df, path, columns):
    output_df = pd.DataFrame(columns=columns) if df.empty else df.loc[:, columns]
    output_df.to_csv(path, index=False)


def reduce_vote_expansion_outputs(clean_output_dir=OUTPUT_DIR, manifest_path=MANIFEST_PATH):
    ensure_output_dirs()
    run_meta = load_run_meta()
    tmp_root = run_meta.get("tmp_root", "")
    finished_rows, finished_ids = finished_manifest_rows(manifest_path)
    worker_dirs = worker_dirs_for_run(tmp_root)
    progress_finished_ids = set()
    for worker_dir in worker_dirs:
        progress_finished_ids.update(completed_worker_progress_ids(worker_dir))
    durable_finished_ids = finished_ids.union(progress_finished_ids)
    proposal_candidate_worker_dirs = candidate_worker_dirs_by_proposal(tmp_root, finished_rows, durable_finished_ids)
    summary = {
        "job_name": JOB_NAME,
        "generated_at": utc_now_iso(),
        "run_id": run_meta.get("run_id", ""),
        "finished_proposals": len(durable_finished_ids),
        "manifest_finished_proposals": len(finished_ids),
        "progress_recovered_proposals": len(durable_finished_ids.difference(finished_ids)),
        "worker_dirs_found": len(worker_dirs),
    }

    selected_paths_by_proposal = {}
    favorite_invalid_err_frames = []
    favorite_mode = "favorite_choice_counts"

    for proposal_id in sorted(durable_finished_ids):
        worker_candidates = proposal_candidate_worker_dirs.get(proposal_id, set())
        selected_paths = latest_attempt_artifacts(worker_candidates, proposal_id)
        selected_paths_by_proposal[proposal_id] = selected_paths

        favorite_err_path = selected_paths.get((favorite_mode, True), "")
        favorite_err_df = load_csv_if_exists(favorite_err_path, ERROR_COLUMNS)
        if not favorite_err_df.empty:
            favorite_err_df["proposal"] = favorite_err_df["proposal"].astype(str).str.lower()
            favorite_err_df = favorite_err_df[favorite_err_df["proposal"] == proposal_id]
            favorite_invalid_err_frames.append(favorite_err_df)

    favorite_invalid_err = (
        pd.concat(favorite_invalid_err_frames, ignore_index=True)
        if favorite_invalid_err_frames else
        pd.DataFrame(columns=ERROR_COLUMNS)
    )
    proposal_issue_flags = build_and_write_proposal_vote_issue_flags(
        proposal_ids=sorted(durable_finished_ids),
        invalid_vote_errors=favorite_invalid_err,
        duplicate_flags_path=PROPOSAL_DUPLICATE_BALLOT_FLAGS_PATH,
        output_path=PROPOSAL_VOTE_ISSUE_FLAGS_PATH,
    )
    run_issue_flags = proposal_issue_flags.loc[
        proposal_issue_flags["id"].isin(durable_finished_ids)
    ].copy()
    issue_proposal_ids = set(run_issue_flags.loc[run_issue_flags["flag_any_vote_issues"] == 1, "id"].tolist())
    summary["skipped_proposals"] = {
        "rows": int(len(issue_proposal_ids)),
        "duplicate_ballots": int(run_issue_flags["flag_duplicate_ballots"].sum()),
        "invalid_votes": int(run_issue_flags["flag_invalid_votes"].sum()),
    }

    for mode_name, mode in VOTE_MODES.items():
        frames = []
        err_frames = []

        for proposal_id in sorted(durable_finished_ids):
            selected_paths = selected_paths_by_proposal.get(proposal_id, {})
            worker_data_path = selected_paths.get((mode_name, False), "")
            worker_err_path = selected_paths.get((mode_name, True), "")

            if proposal_id not in issue_proposal_ids:
                df = load_csv_if_exists(worker_data_path, mode["columns"])
                if not df.empty:
                    df["proposal"] = df["proposal"].astype(str).str.lower()
                    df = df[df["proposal"] == proposal_id]
                    frames.append(df)

            err_df = load_csv_if_exists(worker_err_path, ERROR_COLUMNS)
            if not err_df.empty:
                err_df["proposal"] = err_df["proposal"].astype(str).str.lower()
                err_df = err_df[err_df["proposal"] == proposal_id]
                err_frames.append(err_df)

        merged = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame(columns=mode["columns"])
        merged_err = pd.concat(err_frames, ignore_index=True) if err_frames else pd.DataFrame(columns=ERROR_COLUMNS)

        if not merged.empty:
            merged = (
                merged
                .sort_values(["proposal", "choice_idx"])
                .reset_index(drop=True)
            )
        if not merged_err.empty:
            merged_err = (
                merged_err
                .sort_values(["proposal", "vote_mode", "reason"])
                .reset_index(drop=True)
            )

        _write_final_output(merged, os.path.join(clean_output_dir, mode["file"]), mode["columns"])
        _write_final_output(merged_err, os.path.join(clean_output_dir, mode["err_file"]), ERROR_COLUMNS)

        summary[mode_name] = {
            "rows": int(len(merged)),
            "error_rows": int(len(merged_err)),
        }

    with open(RUN_SUMMARY_PATH, "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)

    return summary


def sample_work_units(limit, work_unit_ids):
    return sorted(work_unit_ids)[:limit]


def safe_store_error(note):
    err_path = os.path.join(OUTPUT_DIR, f"{JOB_NAME}.err")
    with open(err_path, "a", encoding="utf-8") as handle:
        handle.write(f"{utc_now_iso()}\t{note}\n")
