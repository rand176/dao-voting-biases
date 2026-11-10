import json
import numbers
import os
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from dao_bias.data.paths import CLEAN_PROPOSAL_FLAGS_DIR, CLEAN_VOTE_CUMULATIVE_OUTPUTS_DIR, raw_data_path
from dao_bias.preparation.vote_issues import CANONICAL_ISSUE_FLAGS_FILE, load_proposal_issue_details


DATA_DIR = str(raw_data_path())
OUTPUT_DIR = str(CLEAN_VOTE_CUMULATIVE_OUTPUTS_DIR)
MANIFEST_DIR = os.path.join(OUTPUT_DIR, "manifests")
TMP_DIR = os.path.join(OUTPUT_DIR, "tmp")

JOB_NAME = "vote_cumulative_v1"
DEFAULT_WORKERS = 8
MANIFEST_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_manifest.csv")
RUN_META_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_run.json")
RUN_SUMMARY_PATH = os.path.join(OUTPUT_DIR, "run_summary.json")
DUPLICATE_BALLOT_FLAGS_PATH = str(CLEAN_PROPOSAL_FLAGS_DIR / "proposal_duplicate_ballot_flags.csv.gz")
PROPOSAL_VOTE_ISSUE_FLAGS_PATH = str(CANONICAL_ISSUE_FLAGS_FILE)

OUTPUTS = {
    "cumulative_actuals": {
        "file": "prop_choice_cumulative_actuals.csv.gz",
        "worker_file": "cumulative_actuals.csv",
        "columns": [
            "proposal",
            "choice_idx",
            "vote_pos",
            "vote_created",
            "vote_id",
            "voter",
            "vote_type",
            "ballot_vp",
            "vp_delta",
            "cum_vp",
            "vote_delta",
            "cum_votes",
        ],
    },
    "errors": {
        "file": "prop_choice_cumulative_actuals_ERR.csv.gz",
        "worker_file": "cumulative_actuals_errors.csv",
        "columns": [
            "proposal",
            "vote_pos",
            "vote_id",
            "voter",
            "vote_created",
            "vote_type",
            "reason",
            "detail",
            "choice",
        ],
    },
    "skipped_proposals": {
        "file": "prop_choice_cumulative_actuals_SKIPPED.csv.gz",
        "worker_file": "skipped_proposals.csv",
        "columns": ["proposal", "reason", "detail"],
    },
}

GLOBAL_CONTEXT = {}


def utc_now_iso():
    return datetime.now(timezone.utc).isoformat()


def new_run_id():
    return datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ")


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


def try_load_choices(choices):
    try:
        return json.loads(choices)
    except Exception:
        return []


def coerce_choice_idx(value):
    if pd.isna(value) or isinstance(value, bool):
        return None

    if isinstance(value, numbers.Integral):
        return int(value)

    if isinstance(value, numbers.Real):
        numeric = float(value)
        return int(numeric) if numeric.is_integer() else None

    if isinstance(value, str):
        value = value.strip()
        if not value:
            return None
        if value.isdigit():
            return int(value)
        try:
            numeric = float(value)
        except ValueError:
            return None
        return int(numeric) if numeric.is_integer() else None

    return None


def valid_choice_idx(choice_idx, num_choices):
    if choice_idx is None or choice_idx < 1:
        return False
    if num_choices is None:
        return True
    return choice_idx <= num_choices


def parse_json_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row):
    try:
        return json.loads(choice_raw)
    except Exception as exc:
        log_error(prop_id, vote_type, vote_row, choice_raw, "invalid json", repr(exc), worker_dir)
        return None


def normalize_single_basic(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices):
    choice_idx = coerce_choice_idx(choice_raw)
    if valid_choice_idx(choice_idx, num_choices):
        return [{"choice_idx": choice_idx, "weight": 1.0}]

    parsed = parse_json_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row)
    if parsed is None:
        return []

    if isinstance(parsed, list) and len(parsed) == 1:
        choice_idx = coerce_choice_idx(parsed[0])
        if valid_choice_idx(choice_idx, num_choices):
            return [{"choice_idx": choice_idx, "weight": 1.0}]
    elif not isinstance(parsed, list):
        choice_idx = coerce_choice_idx(parsed)
        if valid_choice_idx(choice_idx, num_choices):
            return [{"choice_idx": choice_idx, "weight": 1.0}]

    log_error(prop_id, vote_type, vote_row, choice_raw, "invalid single-choice ballot", "", worker_dir)
    return []


def normalize_ranked_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices):
    parsed = parse_json_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row)
    if not isinstance(parsed, list) or len(parsed) == 0:
        log_error(prop_id, vote_type, vote_row, choice_raw, "empty ranked-choice ballot", "", worker_dir)
        return []

    choice_idx = coerce_choice_idx(parsed[0])
    if valid_choice_idx(choice_idx, num_choices):
        return [{"choice_idx": choice_idx, "weight": 1.0}]

    log_error(prop_id, vote_type, vote_row, choice_raw, "invalid ranked choice", repr(parsed[0]), worker_dir)
    return []


def normalize_approval(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices):
    parsed = parse_json_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row)
    if parsed is None:
        return []

    if isinstance(parsed, list) and len(parsed) > 0 and isinstance(parsed[0], list):
        parsed = parsed[0]

    if not isinstance(parsed, list):
        log_error(prop_id, vote_type, vote_row, choice_raw, "invalid approval ballot", "", worker_dir)
        return []

    clean_choices = []
    for raw_choice in parsed:
        choice_idx = coerce_choice_idx(raw_choice)
        if not valid_choice_idx(choice_idx, num_choices):
            log_error(prop_id, vote_type, vote_row, choice_raw, "invalid approval choice", repr(raw_choice), worker_dir)
            return []
        clean_choices.append(choice_idx)

    if not clean_choices:
        log_error(prop_id, vote_type, vote_row, choice_raw, "empty approval ballot", "", worker_dir)
        return []

    return [{"choice_idx": choice_idx, "weight": 1.0} for choice_idx in dict.fromkeys(clean_choices)]


def normalize_weighted(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices):
    parsed = parse_json_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row)
    if not isinstance(parsed, dict):
        log_error(prop_id, vote_type, vote_row, choice_raw, "invalid weighted ballot", "", worker_dir)
        return []

    clean_entries = []
    for raw_choice, raw_value in parsed.items():
        choice_idx = coerce_choice_idx(raw_choice)
        if not valid_choice_idx(choice_idx, num_choices):
            log_error(prop_id, vote_type, vote_row, choice_raw, "invalid weighted choice", repr(raw_choice), worker_dir)
            return []
        try:
            value = float(raw_value)
        except Exception as exc:
            log_error(prop_id, vote_type, vote_row, choice_raw, "invalid weighted value", repr(exc), worker_dir)
            return []
        if value > 0:
            clean_entries.append({"choice_idx": choice_idx, "weight": value})

    if not clean_entries:
        log_error(prop_id, vote_type, vote_row, choice_raw, "no positive weighted entries", "", worker_dir)
        return []

    deduped = {}
    for item in clean_entries:
        deduped[item["choice_idx"]] = deduped.get(item["choice_idx"], 0.0) + item["weight"]
    return [{"choice_idx": choice_idx, "weight": weight} for choice_idx, weight in deduped.items()]


def normalize_ballot(choice_raw, vote_type, prop_id, worker_dir, vote_row, num_choices):
    if vote_type in ("single-choice", "basic"):
        return normalize_single_basic(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices)
    if vote_type == "ranked-choice":
        return normalize_ranked_choice(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices)
    if vote_type == "approval":
        return normalize_approval(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices)
    if vote_type in ("weighted", "quadratic"):
        return normalize_weighted(choice_raw, prop_id, vote_type, worker_dir, vote_row, num_choices)

    log_error(prop_id, vote_type, vote_row, choice_raw, "unsupported vote type", "", worker_dir)
    return []


def build_actual_tally_rows(normalized_choices, vote_type, ballot_vp):
    if vote_type in ("single-choice", "basic", "ranked-choice", "approval"):
        return [
            {
                "choice_idx": item["choice_idx"],
                "vp_delta": ballot_vp,
                "vote_delta": 1.0,
            }
            for item in normalized_choices
        ]

    if vote_type in ("weighted", "quadratic"):
        total_weight = sum(item["weight"] for item in normalized_choices)
        if total_weight <= 0:
            return []
        return [
            {
                "choice_idx": item["choice_idx"],
                "vp_delta": ballot_vp * item["weight"] / total_weight,
                "vote_delta": 1.0,
            }
            for item in normalized_choices
        ]

    return []


def proposal_output_dir(worker_dir, proposal_id):
    return os.path.join(worker_dir, "proposals", str(proposal_id).lower())


def proposal_output_path(worker_dir, proposal_id, output_name):
    output = OUTPUTS[output_name]
    output_dir = proposal_output_dir(worker_dir, proposal_id)
    ensure_dir(output_dir)
    return os.path.join(output_dir, output["worker_file"])


def clear_proposal_outputs(worker_dir, proposal_id):
    for output_name in OUTPUTS:
        path = proposal_output_path(worker_dir, proposal_id, output_name)
        if os.path.exists(path):
            os.remove(path)


def write_proposal_rows(worker_dir, proposal_id, output_name, df, append=False):
    file_path = proposal_output_path(worker_dir, proposal_id, output_name)
    df.to_csv(file_path, mode="a" if append else "w", index=False, header=False)


def log_error(prop_id, vote_type, vote_row, choice_raw, reason, detail, worker_dir):
    df = pd.DataFrame(
        {
            "proposal": [prop_id],
            "vote_pos": [vote_row.get("vote_pos", pd.NA)],
            "vote_id": [vote_row.get("id", "")],
            "voter": [vote_row.get("voter", "")],
            "vote_created": [vote_row.get("created", pd.NA)],
            "vote_type": [vote_type],
            "reason": [reason],
            "detail": [detail],
            "choice": [repr(choice_raw)],
        }
    )
    write_proposal_rows(worker_dir, prop_id, "errors", df, append=True)


def record_skipped_proposal(worker_dir, proposal_id, reason, detail=""):
    df = pd.DataFrame(
        {
            "proposal": [proposal_id],
            "reason": [reason],
            "detail": [detail],
        }
    )
    write_proposal_rows(worker_dir, proposal_id, "skipped_proposals", df, append=False)


def build_cumulative_output(prop_id, prop, votes, worker_dir):
    vote_type = prop["type"]
    num_choices = len(prop["choices"]) if isinstance(prop["choices"], list) else None
    votes = votes.copy()
    votes["proposal"] = votes["proposal"].astype(str).str.lower()
    votes["id"] = votes["id"].fillna("").astype(str)
    votes["voter"] = votes["voter"].fillna("").astype(str).str.lower()
    votes["created"] = pd.to_numeric(votes["created"], errors="coerce")
    votes["vp"] = pd.to_numeric(votes["vp"], errors="coerce")
    votes = votes.sort_values(["created", "id"], kind="mergesort").reset_index(drop=True)

    cum_vp = {}
    cum_votes = {}
    rows = []

    for idx, vote in votes.iterrows():
        vote_pos = idx + 1
        vote_row = {
            "vote_pos": vote_pos,
            "id": vote["id"],
            "voter": vote["voter"],
            "created": vote["created"],
        }
        ballot_vp = vote["vp"]
        if pd.isna(ballot_vp):
            log_error(prop_id, vote_type, vote_row, vote["choice"], "invalid vp", "", worker_dir)
            continue

        normalized_choices = normalize_ballot(vote["choice"], vote_type, prop_id, worker_dir, vote_row, num_choices)
        if not normalized_choices:
            continue

        actual_tallies = build_actual_tally_rows(normalized_choices, vote_type, float(ballot_vp))
        if not actual_tallies:
            log_error(prop_id, vote_type, vote_row, vote["choice"], "unsupported tally semantics", "", worker_dir)
            continue

        for item in sorted(actual_tallies, key=lambda row: row["choice_idx"]):
            choice_idx = item["choice_idx"]
            cum_vp[choice_idx] = cum_vp.get(choice_idx, 0.0) + float(item["vp_delta"])
            cum_votes[choice_idx] = cum_votes.get(choice_idx, 0.0) + float(item["vote_delta"])
            rows.append(
                {
                    "proposal": prop_id,
                    "choice_idx": choice_idx,
                    "vote_pos": vote_pos,
                    "vote_created": vote["created"],
                    "vote_id": vote["id"],
                    "voter": vote["voter"],
                    "vote_type": vote_type,
                    "ballot_vp": float(ballot_vp),
                    "vp_delta": float(item["vp_delta"]),
                    "cum_vp": cum_vp[choice_idx],
                    "vote_delta": float(item["vote_delta"]),
                    "cum_votes": cum_votes[choice_idx],
                }
            )

    return pd.DataFrame(rows, columns=OUTPUTS["cumulative_actuals"]["columns"])


def load_duplicate_ballot_ids(path=DUPLICATE_BALLOT_FLAGS_PATH):
    if not os.path.exists(path):
        return set()

    df = pd.read_csv(path, usecols=["id", "flag_duplicate_ballots"], low_memory=False)
    df["id"] = df["id"].fillna("").astype(str).str.strip().str.lower()
    return set(df.loc[df["flag_duplicate_ballots"] == 1, "id"].tolist())


def load_vote_cumulative_inputs(data_dir=DATA_DIR):
    print(f"[{utc_now_iso()}] Loading vote cumulative context (this takes a while)...", flush=True)

    print(f"[{utc_now_iso()}]   Reading proposals ...", flush=True)
    df_proposals = pd.read_csv(raw_data_path("proposals_0.csv.gz"), low_memory=False)
    df_proposals = df_proposals.loc[~((df_proposals.id.isna()) | (df_proposals.created.isna()))]
    df_proposals["choices"] = df_proposals["choices"].apply(try_load_choices)
    df_proposals["id"] = df_proposals["id"].str.lower()
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[pd.to_numeric(df_proposals["votes"], errors="coerce").fillna(0) > 0].copy()
    proposal_ids = set(df_proposals["id"].tolist())

    print(f"[{utc_now_iso()}]   Reading vote files from {raw_data_path('votes')} ...", flush=True)
    vote_frames = []
    votes_dir = str(raw_data_path("votes"))
    for fname in sorted(os.listdir(votes_dir)):
        if not fname.endswith((".csv", ".csv.gz")):
            continue
        chunk = pd.read_csv(
            os.path.join(votes_dir, fname),
            usecols=["proposal", "id", "voter", "created", "choice", "vp"],
            dtype={"proposal": "category"},
            low_memory=False,
        )
        chunk["proposal"] = chunk["proposal"].astype(str).str.lower()
        chunk = chunk.loc[chunk["proposal"].isin(proposal_ids)]
        vote_frames.append(chunk)

    df_votes = pd.concat(vote_frames, ignore_index=True) if vote_frames else pd.DataFrame(
        columns=["proposal", "id", "voter", "created", "choice", "vp"]
    )
    df_votes["proposal"] = df_votes["proposal"].astype("category")

    issue_proposal_details = load_proposal_issue_details(PROPOSAL_VOTE_ISSUE_FLAGS_PATH)
    duplicate_proposal_ids = load_duplicate_ballot_ids()
    df_proposals.set_index("id", inplace=True)
    votes_by_prop = df_votes.groupby("proposal")

    print(
        f"[{utc_now_iso()}]   Context ready: {len(df_proposals):,} proposals, "
        f"{len(votes_by_prop):,} vote groups, "
        f"{len(issue_proposal_details):,} proposal issues flagged.",
        flush=True,
    )
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": votes_by_prop,
        "duplicate_proposal_ids": duplicate_proposal_ids,
        "issue_proposal_details": issue_proposal_details,
    }


def list_manifest_work_units(data_dir=DATA_DIR):
    proposal_cols = ["id", "created", "scores_state", "votes"]
    df_proposals = pd.read_csv(str(raw_data_path("proposals_0.csv.gz")), usecols=proposal_cols, low_memory=False)
    df_proposals = df_proposals.loc[~((df_proposals["id"].isna()) | (df_proposals["created"].isna()))]
    df_proposals["id"] = df_proposals["id"].str.lower()
    df_proposals["votes"] = pd.to_numeric(df_proposals["votes"], errors="coerce").fillna(0)
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[df_proposals["votes"] > 0]
    return sorted(df_proposals["id"].drop_duplicates().tolist())


def init_global_context(context=None):
    global GLOBAL_CONTEXT
    if context is None:
        context = load_vote_cumulative_inputs()
    GLOBAL_CONTEXT = context
    return GLOBAL_CONTEXT


def list_work_units(df_proposals, votes_by_prop):
    return sorted([proposal_id for proposal_id in df_proposals.index.tolist() if proposal_id in votes_by_prop.groups])


def process_proposal(proposal_id, context, worker_dir):
    clear_proposal_outputs(worker_dir, proposal_id)

    issue_details = context.get("issue_proposal_details", {})
    if proposal_id in issue_details:
        record_skipped_proposal(
            worker_dir,
            proposal_id,
            issue_details[proposal_id].get("issue_categories", "vote_issues"),
            issue_details[proposal_id].get("flag_reason", ""),
        )
        return

    if proposal_id in context.get("duplicate_proposal_ids", set()):
        record_skipped_proposal(
            worker_dir,
            proposal_id,
            "duplicate_ballots",
            "Excluded because the proposal has repeated proposal+voter ballot histories",
        )
        return

    prop = context["df_proposals"].loc[[proposal_id]].iloc[0]
    votes = context["votes_by_prop"].get_group(proposal_id)
    cumulative_df = build_cumulative_output(proposal_id, prop, votes, worker_dir)
    if not cumulative_df.empty:
        write_proposal_rows(worker_dir, proposal_id, "cumulative_actuals", cumulative_df)


def worker_process_vote_cumulative(worker_id, work_unit_id, worker_dir, manifest_path):
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


def finished_manifest_rows(manifest_path=MANIFEST_PATH):
    import dao_bias.jobs.batch as batch_runner

    if not os.path.exists(manifest_path):
        return [], set()
    rows = batch_runner.load_manifest(manifest_path)
    finished_rows = [row for row in rows if row["status"] == "finished" and row["worker_id"].isdigit()]
    finished_ids = {str(row["work_unit_id"]).lower() for row in finished_rows}
    return finished_rows, finished_ids


def load_csv_if_exists(path, columns):
    if not os.path.exists(path):
        return pd.DataFrame(columns=columns)
    return pd.read_csv(path, header=None, names=columns, low_memory=False)


def _write_final_output(df, path, columns):
    output_df = pd.DataFrame(columns=columns) if df.empty else df.loc[:, columns]
    compression = "gzip" if str(path).endswith(".gz") else None
    output_df.to_csv(path, index=False, compression=compression)


def reduce_vote_cumulative_outputs(clean_output_dir=OUTPUT_DIR, manifest_path=MANIFEST_PATH):
    ensure_output_dirs()
    run_meta = load_run_meta()
    tmp_root = run_meta.get("tmp_root", "")
    finished_rows, finished_ids = finished_manifest_rows(manifest_path)
    summary = {
        "job_name": JOB_NAME,
        "generated_at": utc_now_iso(),
        "run_id": run_meta.get("run_id", ""),
        "finished_proposals": len(finished_ids),
        "worker_dirs_found": len({row["worker_id"] for row in finished_rows}),
    }

    for output_name, output in OUTPUTS.items():
        frames = []
        for row in finished_rows:
            proposal_id = str(row["work_unit_id"]).lower()
            worker_dir = worker_dir_for_run(int(row["worker_id"]), tmp_root)
            worker_path = proposal_output_path(worker_dir, proposal_id, output_name)
            df = load_csv_if_exists(worker_path, output["columns"])
            if not df.empty:
                df["proposal"] = df["proposal"].astype(str).str.lower()
                df = df[df["proposal"] == proposal_id]
                frames.append(df)

        merged = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame(columns=output["columns"])
        if not merged.empty:
            sort_cols = [col for col in ["proposal", "vote_pos", "choice_idx", "reason"] if col in merged.columns]
            if sort_cols:
                merged = merged.sort_values(sort_cols).reset_index(drop=True)

        _write_final_output(merged, os.path.join(clean_output_dir, output["file"]), output["columns"])
        summary[output_name] = {"rows": int(len(merged))}

    skipped_rows = summary.get("skipped_proposals", {}).get("rows", 0)
    summary["skipped_duplicate_proposals"] = int(skipped_rows)

    with open(RUN_SUMMARY_PATH, "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)

    return summary


def sample_work_units(limit, work_unit_ids):
    return sorted(work_unit_ids)[:limit]


def safe_store_error(note):
    err_path = os.path.join(OUTPUT_DIR, f"{JOB_NAME}.err")
    with open(err_path, "a", encoding="utf-8") as handle:
        handle.write(f"{utc_now_iso()}\t{note}\n")


worker_process_vote_expansion = worker_process_vote_cumulative
reduce_vote_expansion_outputs = reduce_vote_cumulative_outputs
