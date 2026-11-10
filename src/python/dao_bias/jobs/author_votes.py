import json
import numbers
import os
import tempfile
import gc
from datetime import datetime, timezone

import pandas as pd

from dao_bias.data.paths import CLEAN_AUTHOR_VOTE_OUTPUTS_DIR, raw_data_path


DATA_DIR = str(raw_data_path())
OUTPUT_DIR = str(CLEAN_AUTHOR_VOTE_OUTPUTS_DIR)
MANIFEST_DIR = os.path.join(OUTPUT_DIR, "manifests")
TMP_DIR = os.path.join(OUTPUT_DIR, "tmp")

JOB_NAME = "author_votes_v1"
DEFAULT_WORKERS = 8
MANIFEST_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_manifest.csv")
RUN_META_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_run.json")
RUN_SUMMARY_PATH = os.path.join(OUTPUT_DIR, "run_summary.json")
REDUCE_STATUS_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_reduce_status.json")

FAVORITE_FILE = "prop_author_favorite_choice.csv"
FAVORITE_ERR_FILE = "prop_author_favorite_choice_ERR.csv"
SUPPORT_FILE = "prop_author_support_fraction.csv"
SUPPORT_ERR_FILE = "prop_author_support_fraction_ERR.csv"

VOTE_MODES = {
    "favorite_choice": {
        "file": FAVORITE_FILE,
        "err_file": FAVORITE_ERR_FILE,
        "value_col": "author_choice_weight",
        "worker_file": "favorite_choice.csv",
        "worker_err_file": "favorite_choice_errors.csv",
        "columns": [
            "proposal",
            "author",
            "author_choice",
            "author_choice_weight",
            "author_vp",
            "author_pos",
            "author_rank",
            "vote_mode",
        ],
    },
    "support_fraction": {
        "file": SUPPORT_FILE,
        "err_file": SUPPORT_ERR_FILE,
        "value_col": "author_support_fraction",
        "worker_file": "support_fraction.csv",
        "worker_err_file": "support_fraction_errors.csv",
        "columns": [
            "proposal",
            "author",
            "author_choice",
            "author_support_fraction",
            "author_vp",
            "author_pos",
            "author_rank",
            "vote_mode",
        ],
    },
}

ERROR_COLUMNS = ["proposal", "vote_mode", "type", "reason", "detail", "choice"]

GLOBAL_CONTEXT = {}
AUTHOR_VOTE_LOOKUP_COLUMNS = ["author_vp", "author_pos", "author_rank", "choice_raw"]


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


def write_json_output(path, payload):
    ensure_dir(os.path.dirname(path))
    tmp_fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".json", dir=os.path.dirname(path))
    try:
        with os.fdopen(tmp_fd, "w", encoding="utf-8") as handle:
            json.dump(payload, handle, indent=2, sort_keys=True)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def stage_json_output(path, payload):
    ensure_dir(os.path.dirname(path))
    tmp_fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".json", dir=os.path.dirname(path))
    try:
        with os.fdopen(tmp_fd, "w", encoding="utf-8") as handle:
            json.dump(payload, handle, indent=2, sort_keys=True)
            handle.flush()
            os.fsync(handle.fileno())
        return tmp_path
    except Exception:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)
        raise


def commit_staged_outputs(staged_outputs):
    commit_log = []
    try:
        for staged_path, final_path in staged_outputs:
            ensure_dir(os.path.dirname(final_path))
            backup_fd, backup_path = tempfile.mkstemp(
                prefix=f".backup_{os.path.basename(final_path)}.",
                dir=os.path.dirname(final_path),
            )
            os.close(backup_fd)
            state = {
                "staged_path": staged_path,
                "final_path": final_path,
                "backup_path": backup_path,
                "had_existing": os.path.exists(final_path),
                "replaced": False,
            }
            commit_log.append(state)
            if state["had_existing"]:
                os.replace(final_path, backup_path)
            os.replace(staged_path, final_path)
            state["replaced"] = True

        for state in commit_log:
            if os.path.exists(state["backup_path"]):
                os.unlink(state["backup_path"])
    except Exception:
        for state in reversed(commit_log):
            final_path = state["final_path"]
            backup_path = state["backup_path"]
            staged_path = state["staged_path"]
            had_existing = state["had_existing"]
            replaced = state["replaced"]

            try:
                if replaced:
                    if os.path.exists(final_path):
                        os.unlink(final_path)
                    if had_existing and os.path.exists(backup_path):
                        os.replace(backup_path, final_path)
                else:
                    if had_existing and os.path.exists(backup_path):
                        os.replace(backup_path, final_path)
                if os.path.exists(staged_path):
                    os.unlink(staged_path)
                if os.path.exists(backup_path):
                    os.unlink(backup_path)
            except Exception:
                pass
        raise


def write_run_meta(run_id, tmp_root):
    ensure_output_dirs()
    write_json_output(
        RUN_META_PATH,
        {
            "job_name": JOB_NAME,
            "run_id": run_id,
            "tmp_root": tmp_root,
            "generated_at": utc_now_iso(),
        },
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


def proposal_output_dir(worker_dir, proposal_id):
    return os.path.join(worker_dir, "proposals", str(proposal_id).lower())


def proposal_output_path(worker_dir, proposal_id, mode_name, err=False):
    mode = VOTE_MODES[mode_name]
    output_dir = proposal_output_dir(worker_dir, proposal_id)
    ensure_dir(output_dir)
    filename = mode["worker_err_file"] if err else mode["worker_file"]
    return os.path.join(output_dir, filename)


def clear_proposal_outputs(worker_dir, proposal_id):
    for mode_name in VOTE_MODES:
        for err in (False, True):
            path = proposal_output_path(worker_dir, proposal_id, mode_name, err=err)
            if os.path.exists(path):
                os.remove(path)


def write_proposal_rows(worker_dir, proposal_id, mode_name, df, err=False, append=False):
    file_path = proposal_output_path(worker_dir, proposal_id, mode_name, err=err)
    df.to_csv(
        file_path,
        mode="a" if append else "w",
        index=False,
        header=False,
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


def parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir):
    try:
        return json.loads(choice_raw)
    except Exception as exc:
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid json", repr(exc), worker_dir)
        return None


def normalize_single_basic(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    choice_idx = coerce_choice_idx(choice_raw)
    if valid_choice_idx(choice_idx, num_choices):
        return [{"choice_idx": choice_idx, "weight": 1.0}]

    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
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

    log_error(mode_name, prop_id, vote_type, choice_raw, "invalid single-choice ballot", "", worker_dir)
    return []


def normalize_ranked_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if not isinstance(parsed, list) or len(parsed) == 0:
        log_error(mode_name, prop_id, vote_type, choice_raw, "empty ranked-choice ballot", "", worker_dir)
        return []

    choice_idx = coerce_choice_idx(parsed[0])
    if valid_choice_idx(choice_idx, num_choices):
        return [{"choice_idx": choice_idx, "weight": 1.0}]

    log_error(mode_name, prop_id, vote_type, choice_raw, "invalid ranked choice", repr(parsed[0]), worker_dir)
    return []


def normalize_approval(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if parsed is None:
        return []

    if isinstance(parsed, list) and len(parsed) > 0 and isinstance(parsed[0], list):
        parsed = parsed[0]

    if not isinstance(parsed, list):
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid approval ballot", "", worker_dir)
        return []

    clean_choices = []
    for raw_choice in parsed:
        choice_idx = coerce_choice_idx(raw_choice)
        if not valid_choice_idx(choice_idx, num_choices):
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid approval choice", repr(raw_choice), worker_dir)
            return []
        clean_choices.append(choice_idx)

    if not clean_choices:
        log_error(mode_name, prop_id, vote_type, choice_raw, "empty approval ballot", "", worker_dir)
        return []

    return [{"choice_idx": choice_idx, "weight": 1.0} for choice_idx in dict.fromkeys(clean_choices)]


def normalize_weighted(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    parsed = parse_json_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir)
    if not isinstance(parsed, dict):
        log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted ballot", "", worker_dir)
        return []

    clean_entries = []
    for raw_choice, raw_value in parsed.items():
        choice_idx = coerce_choice_idx(raw_choice)
        if not valid_choice_idx(choice_idx, num_choices):
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted choice", repr(raw_choice), worker_dir)
            return []

        try:
            value = float(raw_value)
        except Exception as exc:
            log_error(mode_name, prop_id, vote_type, choice_raw, "invalid weighted value", repr(exc), worker_dir)
            return []

        if value > 0:
            clean_entries.append({"choice_idx": choice_idx, "weight": value})

    if not clean_entries:
        log_error(mode_name, prop_id, vote_type, choice_raw, "no positive weighted entries", "", worker_dir)
        return []

    deduped = {}
    for item in clean_entries:
        deduped[item["choice_idx"]] = deduped.get(item["choice_idx"], 0.0) + item["weight"]
    return [{"choice_idx": choice_idx, "weight": weight} for choice_idx, weight in deduped.items()]


def normalize_author_ballot(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    if vote_type in ("single-choice", "basic"):
        return normalize_single_basic(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices)
    if vote_type == "ranked-choice":
        return normalize_ranked_choice(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices)
    if vote_type == "approval":
        return normalize_approval(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices)
    if vote_type in ("weighted", "quadratic"):
        return normalize_weighted(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices)

    log_error(mode_name, prop_id, vote_type, choice_raw, "unsupported vote type", "", worker_dir)
    return []


def favorite_from_normalized(normalized_choices, vote_type):
    if vote_type in ("single-choice", "basic", "ranked-choice"):
        return [{"choice_idx": normalized_choices[0]["choice_idx"], "weight": 1.0}]

    if vote_type == "approval":
        split_weight = 1.0 / len(normalized_choices)
        return [{"choice_idx": item["choice_idx"], "weight": split_weight} for item in normalized_choices]

    if vote_type in ("weighted", "quadratic"):
        max_weight = max(item["weight"] for item in normalized_choices)
        top_choices = [item["choice_idx"] for item in normalized_choices if item["weight"] == max_weight]
        split_weight = 1.0 / len(top_choices)
        return [{"choice_idx": choice_idx, "weight": split_weight} for choice_idx in top_choices]

    return []


def support_from_normalized(normalized_choices, vote_type):
    if vote_type in ("single-choice", "basic", "ranked-choice"):
        return [{"choice_idx": normalized_choices[0]["choice_idx"], "weight": 1.0}]

    if vote_type == "approval":
        split_weight = 1.0 / len(normalized_choices)
        return [{"choice_idx": item["choice_idx"], "weight": split_weight} for item in normalized_choices]

    if vote_type in ("weighted", "quadratic"):
        total_weight = sum(item["weight"] for item in normalized_choices)
        if total_weight <= 0:
            return []
        return [
            {"choice_idx": item["choice_idx"], "weight": item["weight"] / total_weight}
            for item in normalized_choices
        ]

    return []


def parse_author_vote_for_mode(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices):
    normalized = normalize_author_ballot(choice_raw, vote_type, prop_id, mode_name, worker_dir, num_choices)
    if not normalized:
        return []
    if mode_name == "favorite_choice":
        return favorite_from_normalized(normalized, vote_type)
    return support_from_normalized(normalized, vote_type)


def build_na_row(prop_id, author, mode_name, state):
    value_col = VOTE_MODES[mode_name]["value_col"]
    row = {
        "proposal": prop_id,
        "author": author,
        "author_choice": pd.NA,
        value_col: pd.NA,
        "author_vp": state["author_vp"],
        "author_pos": state["author_pos"],
        "author_rank": state["author_rank"],
        "vote_mode": mode_name,
    }
    return pd.DataFrame([row], columns=VOTE_MODES[mode_name]["columns"])


def build_mode_rows(prop_id, author, mode_name, state, parsed_choices):
    value_col = VOTE_MODES[mode_name]["value_col"]
    rows = []
    for item in parsed_choices:
        rows.append(
            {
                "proposal": prop_id,
                "author": author,
                "author_choice": item["choice_idx"],
                value_col: item["weight"],
                "author_vp": state["author_vp"],
                "author_pos": state["author_pos"],
                "author_rank": state["author_rank"],
                "vote_mode": mode_name,
            }
        )
    return pd.DataFrame(rows, columns=VOTE_MODES[mode_name]["columns"])


def empty_author_vote_lookup():
    return pd.DataFrame(columns=AUTHOR_VOTE_LOOKUP_COLUMNS, index=pd.Index([], name="proposal"))


def build_author_vote_lookup(df_proposals, df_votes):
    # The author-vote job only needs each proposal's first author ballot state.
    # Precomputing that compact lookup keeps workers from holding every vote
    # group in memory for the full run.
    if df_votes.empty:
        return empty_author_vote_lookup()

    vote_positions = df_votes.sort_values(["proposal", "created"], kind="mergesort").reset_index(drop=True)
    vote_positions["author"] = vote_positions["proposal"].map(df_proposals["author"])
    vote_positions = vote_positions.loc[vote_positions["author"].notna()].copy()
    if vote_positions.empty:
        return empty_author_vote_lookup()

    vote_positions["author_pos"] = vote_positions.groupby("proposal").cumcount() + 1
    vote_counts = vote_positions.groupby("proposal").size().rename("proposal_vote_count")

    author_votes = vote_positions.loc[
        vote_positions["voter"] == vote_positions["author"],
        ["proposal", "choice", "vp", "author_pos"],
    ].copy()
    if author_votes.empty:
        return empty_author_vote_lookup()

    author_votes = author_votes.drop_duplicates(subset=["proposal"], keep="first")
    author_votes = author_votes.merge(vote_counts, on="proposal", how="left", copy=False)
    author_votes["author_rank"] = author_votes["author_pos"] / author_votes["proposal_vote_count"]
    author_votes.rename(columns={"choice": "choice_raw", "vp": "author_vp"}, inplace=True)
    author_votes = author_votes.set_index("proposal")
    return author_votes.loc[:, AUTHOR_VOTE_LOOKUP_COLUMNS]


def load_author_vote_inputs(data_dir=DATA_DIR):
    print(f"[{utc_now_iso()}] Loading author vote context...", flush=True)

    proposal_cols = ["id", "author", "type", "choices", "created", "scores_state", "votes"]
    df_proposals = pd.read_csv(raw_data_path("proposals_0.csv.gz"), usecols=proposal_cols, low_memory=False)
    df_proposals = df_proposals.loc[~((df_proposals["id"].isna()) | (df_proposals["created"].isna()))]
    df_proposals["id"] = df_proposals["id"].astype(str).str.lower()
    df_proposals["author"] = df_proposals["author"].astype(str).str.lower()
    df_proposals["choices"] = df_proposals["choices"].apply(try_load_choices)
    df_proposals["votes"] = pd.to_numeric(df_proposals["votes"], errors="coerce").fillna(0)
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[df_proposals["votes"] > 0].copy()
    final_ids = set(df_proposals["id"].tolist())

    print(f"[{utc_now_iso()}]   Reading vote files from {raw_data_path('votes')} ...", flush=True)
    votes_dir = str(raw_data_path("votes"))
    vote_frames = []
    for fname in sorted(os.listdir(votes_dir)):
        if not fname.endswith((".csv", ".csv.gz")):
            continue
        chunk = pd.read_csv(
            os.path.join(votes_dir, fname),
            usecols=["proposal", "voter", "created", "choice", "vp"],
            low_memory=False,
        )
        chunk["proposal"] = chunk["proposal"].astype(str).str.lower()
        chunk = chunk.loc[chunk["proposal"].isin(final_ids)]
        if chunk.empty:
            continue
        chunk["voter"] = chunk["voter"].astype(str).str.lower()
        vote_frames.append(chunk)

    if vote_frames:
        df_votes = pd.concat(vote_frames, ignore_index=True)
    else:
        df_votes = pd.DataFrame(columns=["proposal", "voter", "created", "choice", "vp"])

    proposal_ids_with_votes = set(df_votes["proposal"].dropna().astype(str).str.lower().unique().tolist())
    df_proposals = df_proposals.loc[df_proposals["id"].isin(proposal_ids_with_votes)].copy()
    df_proposals.set_index("id", inplace=True)
    author_vote_lookup = build_author_vote_lookup(df_proposals, df_votes)
    del df_votes
    del vote_frames
    gc.collect()

    print(
        f"[{utc_now_iso()}]   Context ready: {len(df_proposals):,} proposals, "
        f"{len(proposal_ids_with_votes):,} proposals with votes, "
        f"{len(author_vote_lookup):,} proposals with author votes.",
        flush=True,
    )
    return {
        "df_proposals": df_proposals,
        "votes_by_prop": proposal_ids_with_votes,  # Generic runner only needs proposal membership here.
        "author_vote_lookup": author_vote_lookup,
    }


def list_manifest_work_units(data_dir=DATA_DIR):
    proposal_cols = ["id", "created", "scores_state", "votes"]
    df_proposals = pd.read_csv(
        str(raw_data_path("proposals_0.csv.gz")),
        usecols=proposal_cols,
        low_memory=False,
    )
    df_proposals = df_proposals.loc[~((df_proposals["id"].isna()) | (df_proposals["created"].isna()))]
    df_proposals["id"] = df_proposals["id"].astype(str).str.lower()
    df_proposals["votes"] = pd.to_numeric(df_proposals["votes"], errors="coerce").fillna(0)
    df_proposals = df_proposals.loc[df_proposals["scores_state"] == "final"]
    df_proposals = df_proposals.loc[df_proposals["votes"] > 0]
    return sorted(df_proposals["id"].drop_duplicates().tolist())


def init_global_context(context=None):
    global GLOBAL_CONTEXT
    if context is None:
        context = load_author_vote_inputs()
    GLOBAL_CONTEXT = context
    return GLOBAL_CONTEXT


def list_work_units(df_proposals, votes_by_prop):
    if hasattr(votes_by_prop, "groups"):
        vote_ids = set(votes_by_prop.groups)
    else:
        vote_ids = set(votes_by_prop)
    return sorted([proposal_id for proposal_id in df_proposals.index.tolist() if proposal_id in vote_ids])


def no_author_vote_state():
    return {
        "author_voted": False,
        "author_vp": pd.NA,
        "author_pos": pd.NA,
        "author_rank": pd.NA,
        "choice_raw": None,
    }


def author_vote_state_from_lookup(prop_id, author_vote_lookup):
    if author_vote_lookup is None or prop_id not in author_vote_lookup.index:
        return no_author_vote_state()

    author_vote = author_vote_lookup.loc[prop_id]
    if isinstance(author_vote, pd.DataFrame):
        author_vote = author_vote.iloc[0]

    return {
        "author_voted": True,
        "author_vp": author_vote["author_vp"],
        "author_pos": author_vote["author_pos"],
        "author_rank": author_vote["author_rank"],
        "choice_raw": author_vote["choice_raw"],
    }


def author_vote_state(prop_id, prop, votes=None, author_vote_lookup=None):
    if author_vote_lookup is not None:
        return author_vote_state_from_lookup(prop_id, author_vote_lookup)

    if votes is None:
        return no_author_vote_state()

    votes = votes.sort_values(by="created", ascending=True).reset_index(drop=True)
    author_vote = votes.loc[votes["voter"] == prop["author"]]

    if len(author_vote) == 0:
        return no_author_vote_state()

    author_pos = author_vote.index[0] + 1
    author_vote = author_vote.iloc[0]
    return {
        "author_voted": True,
        "author_vp": author_vote["vp"],
        "author_pos": author_pos,
        "author_rank": author_pos / len(votes),
        "choice_raw": author_vote["choice"],
    }


def aggregate_mode(prop_id, prop, votes, mode_name, worker_dir, author_vote_lookup=None):
    state = author_vote_state(
        prop_id,
        prop,
        votes=votes,
        author_vote_lookup=author_vote_lookup,
    )
    if not state["author_voted"]:
        return build_na_row(prop_id, prop["author"], mode_name, state)

    num_choices = len(prop["choices"]) if isinstance(prop["choices"], list) else None
    parsed_choices = parse_author_vote_for_mode(
        state["choice_raw"],
        prop["type"],
        prop_id,
        mode_name,
        worker_dir,
        num_choices,
    )
    if not parsed_choices:
        return build_na_row(prop_id, prop["author"], mode_name, state)
    return build_mode_rows(prop_id, prop["author"], mode_name, state, parsed_choices)


def process_proposal(proposal_id, context, worker_dir):
    prop = context["df_proposals"].loc[[proposal_id]].iloc[0]
    author_vote_lookup = context.get("author_vote_lookup")
    votes = None
    if author_vote_lookup is None:
        votes = context["votes_by_prop"].get_group(proposal_id)
    clear_proposal_outputs(worker_dir, proposal_id)

    for mode_name in VOTE_MODES:
        df = aggregate_mode(
            proposal_id,
            prop,
            votes,
            mode_name,
            worker_dir,
            author_vote_lookup=author_vote_lookup,
        )
        write_proposal_rows(worker_dir, proposal_id, mode_name, df)


def worker_process_author_votes(worker_id, work_unit_id, worker_dir, manifest_path):
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


def finished_manifest_rows(manifest_path=None, manifest_rows=None):
    import dao_bias.jobs.batch as batch_runner

    manifest_path = manifest_path or MANIFEST_PATH
    if not os.path.exists(manifest_path):
        return [], set()
    rows = manifest_rows if manifest_rows is not None else batch_runner.load_manifest(manifest_path)
    finished_rows = [row for row in rows if row["status"] == "finished" and str(row["worker_id"]).isdigit()]
    finished_ids = {str(row["work_unit_id"]).lower() for row in finished_rows}
    return finished_rows, finished_ids


def load_csv_if_exists(path, columns):
    if not os.path.exists(path):
        return pd.DataFrame(columns=columns)
    return pd.read_csv(path, header=None, names=columns, low_memory=False)


def validate_finished_worker_chunks(finished_rows, tmp_root, max_errors=25):
    errors = []
    for row in finished_rows:
        proposal_id = str(row["work_unit_id"]).lower()
        worker_dir = worker_dir_for_run(int(row["worker_id"]), tmp_root)
        for mode_name in VOTE_MODES:
            data_path = proposal_output_path(worker_dir, proposal_id, mode_name, err=False)
            if not os.path.exists(data_path):
                errors.append(f"missing data chunk for proposal={proposal_id} mode={mode_name}")
            elif os.path.getsize(data_path) == 0:
                errors.append(f"empty data chunk for proposal={proposal_id} mode={mode_name}")
            if len(errors) >= max_errors:
                return errors
    return errors


def _write_final_output(df, path, columns):
    output_df = pd.DataFrame(columns=columns) if df.empty else df.loc[:, columns]
    ensure_dir(os.path.dirname(path))
    tmp_fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".csv", dir=os.path.dirname(path))
    try:
        with os.fdopen(tmp_fd, "w", encoding="utf-8", newline="") as handle:
            output_df.to_csv(handle, index=False)
            handle.flush()
            os.fsync(handle.fileno())
        return tmp_path
    except Exception:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)
        raise


def manifest_reduce_status(run_meta, manifest_summary):
    return {
        "job_name": JOB_NAME,
        "generated_at": utc_now_iso(),
        "run_id": run_meta.get("run_id", ""),
        "tmp_root": run_meta.get("tmp_root", ""),
        "manifest_total": int(manifest_summary.get("total", 0)),
        "manifest_finished": int(manifest_summary.get("finished", 0)),
        "manifest_pending": int(manifest_summary.get("pending", 0)),
        "manifest_running": int(manifest_summary.get("running", 0)),
        "manifest_failed": int(manifest_summary.get("failed", 0)),
        "is_complete": (
            manifest_summary.get("finished", 0) == manifest_summary.get("total", 0)
            and manifest_summary.get("pending", 0) == 0
            and manifest_summary.get("running", 0) == 0
            and manifest_summary.get("failed", 0) == 0
        ),
    }


def reduce_author_vote_outputs(clean_output_dir=None, manifest_path=None):
    import dao_bias.jobs.batch as batch_runner

    clean_output_dir = clean_output_dir or OUTPUT_DIR
    manifest_path = manifest_path or MANIFEST_PATH
    ensure_output_dirs()
    run_meta = load_run_meta()
    tmp_root = run_meta.get("tmp_root", "")
    manifest_exists = os.path.exists(manifest_path)
    manifest_rows, manifest_summary = batch_runner.load_manifest_snapshot(manifest_path)
    reduce_status = manifest_reduce_status(run_meta, manifest_summary)
    contract_errors = []

    if not manifest_exists:
        contract_errors.append("missing manifest")
    if not run_meta.get("run_id"):
        contract_errors.append("missing run_id in run metadata")
    if not tmp_root:
        contract_errors.append("missing tmp_root in run metadata")
    elif not os.path.isdir(tmp_root):
        contract_errors.append(f"tmp_root does not exist: {tmp_root}")

    if contract_errors:
        reduce_status["is_complete"] = False
    write_json_output(REDUCE_STATUS_PATH, reduce_status)

    if not reduce_status["is_complete"]:
        raise RuntimeError(
            "Cannot write canonical author-vote outputs from an incomplete or invalid run state. "
            f"finished={reduce_status['manifest_finished']} total={reduce_status['manifest_total']} "
            f"pending={reduce_status['manifest_pending']} running={reduce_status['manifest_running']} "
            f"failed={reduce_status['manifest_failed']} "
            + (f"errors={'; '.join(contract_errors)}" if contract_errors else "")
        )

    finished_rows, finished_ids = finished_manifest_rows(manifest_path, manifest_rows=manifest_rows)
    finished_chunk_errors = validate_finished_worker_chunks(finished_rows, tmp_root)
    if finished_chunk_errors:
        reduce_status["is_complete"] = False
        reduce_status["contract_errors"] = finished_chunk_errors
        write_json_output(REDUCE_STATUS_PATH, reduce_status)
        raise RuntimeError(
            "Cannot write canonical author-vote outputs because finished work units have missing or empty "
            "worker chunk files. "
            + "; ".join(finished_chunk_errors)
        )

    summary = dict(reduce_status)
    summary.update(
        {
            "finished_proposals": len(finished_ids),
            "worker_dirs_found": len({row["worker_id"] for row in finished_rows}),
        }
    )
    staged_outputs = []

    for mode_name, mode in VOTE_MODES.items():
        frames = []
        err_frames = []

        for row in finished_rows:
            proposal_id = str(row["work_unit_id"]).lower()
            worker_dir = worker_dir_for_run(int(row["worker_id"]), tmp_root)
            worker_data_path = proposal_output_path(worker_dir, proposal_id, mode_name, err=False)
            worker_err_path = proposal_output_path(worker_dir, proposal_id, mode_name, err=True)

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
            merged = merged.sort_values(["proposal"], kind="mergesort").reset_index(drop=True)
        if not merged_err.empty:
            merged_err = merged_err.sort_values(["proposal", "vote_mode", "reason"]).reset_index(drop=True)

        staged_outputs.append(
            (_write_final_output(merged, os.path.join(clean_output_dir, mode["file"]), mode["columns"]),
             os.path.join(clean_output_dir, mode["file"]))
        )
        staged_outputs.append(
            (_write_final_output(merged_err, os.path.join(clean_output_dir, mode["err_file"]), ERROR_COLUMNS),
             os.path.join(clean_output_dir, mode["err_file"]))
        )

        summary[mode_name] = {
            "rows": int(len(merged)),
            "error_rows": int(len(merged_err)),
        }

    staged_outputs.append((stage_json_output(RUN_SUMMARY_PATH, summary), RUN_SUMMARY_PATH))
    commit_staged_outputs(staged_outputs)

    return summary


def sample_work_units(limit, work_unit_ids):
    return sorted(work_unit_ids)[:limit]


def safe_store_error(note):
    err_path = os.path.join(OUTPUT_DIR, f"{JOB_NAME}.err")
    with open(err_path, "a", encoding="utf-8") as handle:
        handle.write(f"{utc_now_iso()}\t{note}\n")


# Backward-compatible hook names expected by the current generic runner.
worker_process_vote_expansion = worker_process_author_votes
reduce_vote_expansion_outputs = reduce_author_vote_outputs
