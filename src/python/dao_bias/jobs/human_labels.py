from __future__ import annotations
# Human-label batch job.


import hashlib
import json
import os
import re
import tempfile
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

from dao_bias.data.paths import CLEAN_HUMAN_LABEL_OUTPUTS_DIR, HUMANLABELS_DIR


DATA_DIR = str(HUMANLABELS_DIR)
OUTPUT_DIR = str(CLEAN_HUMAN_LABEL_OUTPUTS_DIR)
MANIFEST_DIR = os.path.join(OUTPUT_DIR, "manifests")
TMP_DIR = os.path.join(OUTPUT_DIR, "tmp")

JOB_NAME = "human_labels_v1"
DEFAULT_WORKERS = 4
MANIFEST_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_manifest.csv")
RUN_META_PATH = os.path.join(MANIFEST_DIR, f"{JOB_NAME}_run.json")
RUN_SUMMARY_PATH = os.path.join(OUTPUT_DIR, "run_summary.json")

EVALUATOR_LABELS_FILE = "human_label_evaluator_labels.csv"
CHOICE_LABELS_FILE = "human_label_choice_labels.csv"
PARSE_ERRORS_FILE = "human_label_parse_errors.csv"

LABEL_CHUNK_DIR = "label_rows"
ERROR_CHUNK_DIR = "parse_errors"

CHOICE_COL_RE = re.compile(r"^choice_(\d+)$")
LABEL_ORDER = ["Approve", "Reject", "Abstain", "Other"]
LABEL_ALIASES = {
    "approve": "Approve",
    "approved": "Approve",
    "yes": "Approve",
    "support": "Approve",
    "for": "Approve",
    "accept": "Approve",
    "reject": "Reject",
    "rejected": "Reject",
    "no": "Reject",
    "against": "Reject",
    "oppose": "Reject",
    "abstain": "Abstain",
    "abstention": "Abstain",
    "neutral": "Abstain",
    "other": "Other",
    "unclear": "Other",
    "spam": "Other",
    "joke": "Other",
    "test": "Other",
}
MISSING_STRINGS = {"", "na", "n/a", "nan", "none", "null"}

PROPOSAL_COLUMNS = ["proposal", "news", "news_id", "proposal_id", "snapshot_id"]
EVALUATOR_COLUMNS = ["evaluator_id", "evaluator", "player", "annotator", "coder", "worker_id", "worker"]
TIME_COLUMNS = ["eval_seconds", "time_seconds", "seconds", "duration_seconds", "time", "duration", "eval_t", "time_ms", "duration_ms"]
ISSUE_COLUMNS = ["eval_issue", "proposal_issues", "issue"]
SUGGEST_COLUMNS = ["eval_suggest", "dao_suggest", "suggest", "suggested_stance_hl"]
BODY_LENGTH_COLUMNS = ["body_mod_len_n", "body_length_norm", "body_len_norm"]
RAW_BODY_LENGTH_COLUMNS = ["body_mod_len"]
BODY_LANG_COLUMNS = ["body_lang"]
SINGLE_LABEL_COLUMNS = ["label", "eval", "evaluation", "stance", "stance_hl"]
SINGLE_CHOICE_COLUMNS = ["choice_idx", "choice_id", "choice_number"]
EVENT_STEP_COLUMNS = ["stepId", "stageId"]
EXPLICIT_LABEL_EXPORT_RE = re.compile(r"(^|[_-])human[_-]?labels?([_.-]|$)", re.IGNORECASE)

LABEL_COLUMNS = [
    "proposal",
    "choice_idx",
    "prop_chid",
    "evaluator_id",
    "label",
    "eval_seconds",
    "evaluator_total_seconds",
    "issue",
    "suggest",
    "raw_low_effort",
    "body_mod_len_n",
    "source_file",
    "source_row",
    "source_event_id",
]
EVALUATOR_COLUMNS_OUT = [
    "proposal",
    "choice_idx",
    "prop_chid",
    "evaluator_id",
    "label",
    "eval_seconds",
    "evaluator_total_seconds",
    "low_effort",
    "low_effort_evaluator",
    "low_effort_response",
    "raw_low_effort",
    "issue_hl",
    "suggested_stance_hl",
    "raw_label_count",
    "raw_distinct_label_count",
    "has_repeated_label",
    "has_label_conflict",
    "duplicate_policy",
    "source_file",
    "source_row",
]
CHOICE_COLUMNS_OUT = [
    "proposal",
    "choice_idx",
    "prop_chid",
    "stance_hl",
    "suggested_stance_hl",
    "issue_hl",
    "n_evaluations",
    "n_non_low_effort_evaluations",
    "n_low_effort_evaluations",
    "n_approve",
    "n_reject",
    "n_abstain",
    "n_other",
    "n_effective_approve",
    "n_effective_reject",
    "n_effective_abstain",
    "n_effective_other",
    "low_effort_only",
    "label_conflict",
    "raw_label_conflict",
    "has_evaluator_label_conflict",
]
ERROR_COLUMNS = ["source_file", "source_row", "reason", "detail"]

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


def write_json_output(path, payload):
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


def write_csv_output(path, df):
    ensure_dir(os.path.dirname(path))
    fd, tmp_path = tempfile.mkstemp(prefix=".tmp_", suffix=".csv", dir=os.path.dirname(path))
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="") as handle:
            df.to_csv(handle, index=False)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp_path, path)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


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
    tmp_root = load_run_meta().get("tmp_root")
    if tmp_root and os.path.isdir(tmp_root):
        return tmp_root
    return None


def is_missing(value):
    if value is None:
        return True
    if isinstance(value, str):
        return value.strip().lower() in MISSING_STRINGS
    if isinstance(value, (dict, list, tuple)):
        return False
    try:
        return bool(pd.isna(value))
    except (TypeError, ValueError):
        return False


def parse_possible_json(value):
    if not isinstance(value, str):
        return value
    stripped = value.strip()
    if not stripped or stripped[0] not in "[{":
        return value
    try:
        return json.loads(stripped)
    except Exception:
        return value


def nested_value(value):
    value = parse_possible_json(value)
    if isinstance(value, dict):
        for key in ["value", "label", "eval", "stance", "text"]:
            if key in value:
                return value[key]
    return value


def first_present(row, candidates):
    _, value = first_present_with_column(row, candidates)
    return value


def first_present_with_column(row, candidates):
    for column in candidates:
        if column in row.index and not is_missing(row[column]):
            return column, row[column]
    return None, None


def normalize_optional_text(value):
    value = nested_value(value)
    if is_missing(value):
        return None
    text = str(value).strip()
    if text.lower() in MISSING_STRINGS:
        return None
    return text


def normalize_label(value):
    value = nested_value(value)
    if is_missing(value):
        return None
    text = str(value).strip()
    key = text.lower().replace("-", " ").replace("_", " ")
    key = " ".join(key.split())
    if key in MISSING_STRINGS:
        return None
    if key in LABEL_ALIASES:
        return LABEL_ALIASES[key]
    title = text[:1].upper() + text[1:].lower()
    if title in LABEL_ORDER:
        return title
    return None


def coerce_bool(value):
    value = nested_value(value)
    if is_missing(value):
        return False
    if isinstance(value, bool):
        return value
    if isinstance(value, (int, float)):
        return bool(value)
    text = str(value).strip().lower()
    if text in {"1", "true", "t", "yes", "y"}:
        return True
    if text in {"0", "false", "f", "no", "n"}:
        return False
    return False


def coerce_seconds(value, source_column=None):
    value = nested_value(value)
    if is_missing(value):
        return pd.NA
    try:
        seconds = float(value)
    except (TypeError, ValueError):
        return pd.NA
    column = str(source_column).lower() if source_column is not None else ""
    if column in {"time", "duration", "eval_t"} or column.endswith("_ms"):
        return seconds / 1000.0
    return seconds


def coerce_float(value):
    value = nested_value(value)
    if is_missing(value):
        return pd.NA
    try:
        return float(value)
    except (TypeError, ValueError):
        return pd.NA


def coerce_choice_idx(value):
    value = nested_value(value)
    if is_missing(value):
        return None
    try:
        choice_idx = int(float(value))
    except (TypeError, ValueError):
        return None
    if choice_idx < 1:
        return None
    return choice_idx


def make_prop_chid(proposal, choice_idx):
    return f"{proposal}{int(choice_idx)}"


def empty_label_df():
    return pd.DataFrame(columns=LABEL_COLUMNS)


def empty_error_df():
    return pd.DataFrame(columns=ERROR_COLUMNS)


def empty_evaluator_df():
    return pd.DataFrame(columns=EVALUATOR_COLUMNS_OUT)


def empty_choice_df():
    return pd.DataFrame(columns=CHOICE_COLUMNS_OUT)


def add_error(errors, source_file, source_row, reason, detail):
    errors.append(
        {
            "source_file": str(source_file),
            "source_row": int(source_row),
            "reason": reason,
            "detail": str(detail),
        }
    )


def extract_choice_label(raw_choice):
    parsed = parse_possible_json(raw_choice)
    if not isinstance(parsed, dict):
        return None, "malformed_choice"
    label = normalize_label(parsed.get("value"))
    if label is None:
        return None, "missing_label"
    return label, None


def is_explicit_missing_choice_cell(raw_choice):
    if raw_choice is None or raw_choice is pd.NA:
        return True
    if isinstance(raw_choice, str) and raw_choice.strip().lower() in MISSING_STRINGS:
        return True
    if isinstance(raw_choice, (dict, list, tuple)):
        return False
    try:
        return bool(pd.isna(raw_choice))
    except (TypeError, ValueError):
        return False
    return False


def normalized_body_length_by_row(raw_df):
    result = pd.Series(pd.NA, index=raw_df.index, dtype="object")
    for column in BODY_LENGTH_COLUMNS:
        if column in raw_df.columns:
            values = pd.to_numeric(raw_df[column].map(coerce_float), errors="coerce")
            result = result.where(result.notna(), values)

    missing = result.isna()
    raw_column = next((column for column in RAW_BODY_LENGTH_COLUMNS if column in raw_df.columns), None)
    if raw_column is None or not missing.any():
        return result

    raw_lengths = pd.to_numeric(raw_df[raw_column].map(coerce_float), errors="coerce")
    if raw_lengths.notna().sum() == 0:
        return result

    global_max = raw_lengths.max(skipna=True)
    if pd.isna(global_max) or global_max <= 0:
        return result

    lang_column = next((column for column in BODY_LANG_COLUMNS if column in raw_df.columns), None)
    if lang_column is not None:
        languages = raw_df[lang_column].map(normalize_optional_text)
        lang_max = raw_lengths.groupby(languages).transform("max")
        denominators = lang_max.fillna(global_max)
    else:
        denominators = pd.Series(global_max, index=raw_df.index)

    normalized = raw_lengths / denominators.where(denominators > 0)
    result = result.where(result.notna(), normalized)
    return result


def evaluator_total_seconds_by_row(raw_df):
    evaluator_ids = []
    seconds = []
    for _, row in raw_df.iterrows():
        evaluator_id = normalize_optional_text(first_present(row, EVALUATOR_COLUMNS))
        evaluator_ids.append(evaluator_id)
        time_column, time_value = first_present_with_column(row, TIME_COLUMNS)
        seconds.append(coerce_seconds(time_value, time_column))

    frame = pd.DataFrame(
        {
            "evaluator_id": evaluator_ids,
            "eval_seconds": pd.to_numeric(pd.Series(seconds), errors="coerce"),
        },
        index=raw_df.index,
    )
    valid = frame.dropna(subset=["evaluator_id", "eval_seconds"])
    if valid.empty:
        return pd.Series(pd.NA, index=raw_df.index, dtype="object")
    totals = valid.groupby("evaluator_id")["eval_seconds"].sum(min_count=1)
    return frame["evaluator_id"].map(totals)


def collect_choice_columns(row):
    choice_columns = []
    for column in row.index:
        match = CHOICE_COL_RE.match(str(column))
        if match:
            choice_columns.append((int(match.group(1)), column))
    choice_columns.sort()
    return choice_columns


def is_news_step(row):
    for column in EVENT_STEP_COLUMNS:
        if column in row.index and normalize_optional_text(row[column]) == "news":
            return True
    return False


def has_label_payload(row, choice_columns):
    if normalize_optional_text(first_present(row, PROPOSAL_COLUMNS)) is not None:
        return True
    if any(not is_missing(row[column]) for _, column in choice_columns):
        return True
    if first_present(row, SINGLE_CHOICE_COLUMNS) is not None:
        return True
    if first_present(row, SINGLE_LABEL_COLUMNS) is not None:
        return True
    return is_news_step(row)


def extract_label_rows(raw_df, source_file=""):
    records = []
    errors = []
    if raw_df is None or raw_df.empty:
        return empty_label_df(), empty_error_df()

    raw_df = raw_df.reset_index(drop=True)
    body_mod_len_n_by_row = normalized_body_length_by_row(raw_df)
    evaluator_total_seconds = evaluator_total_seconds_by_row(raw_df)
    for row_idx, row in raw_df.iterrows():
        source_row = row_idx + 1
        choice_columns = collect_choice_columns(row)
        if not has_label_payload(row, choice_columns):
            continue
        proposal = normalize_optional_text(first_present(row, PROPOSAL_COLUMNS))
        evaluator_id = normalize_optional_text(first_present(row, EVALUATOR_COLUMNS))
        if proposal is None:
            add_error(errors, source_file, source_row, "missing_proposal", "")
            continue
        if evaluator_id is None:
            add_error(errors, source_file, source_row, "missing_evaluator", proposal)
            continue

        time_column, time_value = first_present_with_column(row, TIME_COLUMNS)
        eval_seconds = coerce_seconds(time_value, time_column)
        issue = normalize_optional_text(first_present(row, ISSUE_COLUMNS))
        suggest = normalize_optional_text(first_present(row, SUGGEST_COLUMNS))
        raw_low_effort = coerce_bool(row["low_effort"]) if "low_effort" in row.index else False
        body_mod_len_n = body_mod_len_n_by_row.iloc[row_idx]
        total_seconds = evaluator_total_seconds.iloc[row_idx]
        source_event_id = f"{source_file}:{source_row}:{proposal}:{evaluator_id}"

        if choice_columns:
            for choice_idx, column in choice_columns:
                raw_choice = row[column]
                if is_explicit_missing_choice_cell(raw_choice):
                    label = pd.NA
                elif is_missing(raw_choice):
                    continue
                else:
                    label, error = extract_choice_label(raw_choice)
                    if error is not None:
                        if error == "missing_label":
                            label = pd.NA
                        else:
                            add_error(errors, source_file, source_row, error, column)
                            continue
                records.append(
                    {
                        "proposal": proposal,
                        "choice_idx": choice_idx,
                        "prop_chid": make_prop_chid(proposal, choice_idx),
                        "evaluator_id": evaluator_id,
                        "label": label,
                        "eval_seconds": eval_seconds,
                        "evaluator_total_seconds": total_seconds,
                        "issue": issue,
                        "suggest": suggest,
                        "raw_low_effort": raw_low_effort,
                        "body_mod_len_n": body_mod_len_n,
                        "source_file": str(source_file),
                        "source_row": source_row,
                        "source_event_id": source_event_id,
                    }
                )
            continue

        choice_idx = coerce_choice_idx(first_present(row, SINGLE_CHOICE_COLUMNS))
        label = normalize_label(first_present(row, SINGLE_LABEL_COLUMNS))
        if choice_idx is None:
            add_error(errors, source_file, source_row, "missing_choice_idx", proposal)
            continue
        if label is None:
            add_error(errors, source_file, source_row, "missing_label", proposal)
            continue
        records.append(
            {
                "proposal": proposal,
                "choice_idx": choice_idx,
                "prop_chid": make_prop_chid(proposal, choice_idx),
                "evaluator_id": evaluator_id,
                "label": label,
                "eval_seconds": eval_seconds,
                "evaluator_total_seconds": total_seconds,
                "issue": issue,
                "suggest": suggest,
                "raw_low_effort": raw_low_effort,
                "body_mod_len_n": body_mod_len_n,
                "source_file": str(source_file),
                "source_row": source_row,
                "source_event_id": source_event_id,
            }
        )

    label_df = pd.DataFrame(records, columns=LABEL_COLUMNS)
    error_df = pd.DataFrame(errors, columns=ERROR_COLUMNS)
    return label_df, error_df


def _coerce_label_input(raw_labels):
    df = raw_labels.copy() if raw_labels is not None else empty_label_df()
    for column in LABEL_COLUMNS:
        if column not in df.columns:
            if column == "prop_chid" and {"proposal", "choice_idx"}.issubset(df.columns):
                df[column] = df.apply(lambda row: make_prop_chid(row["proposal"], row["choice_idx"]), axis=1)
            elif column == "raw_low_effort":
                df[column] = False
            elif column == "source_file":
                df[column] = ""
            elif column == "source_row":
                df[column] = range(1, len(df) + 1)
            elif column == "source_event_id":
                df[column] = ""
            else:
                df[column] = pd.NA
    if len(df) and (df["source_event_id"].isna() | (df["source_event_id"].astype(str) == "")).any():
        df["source_event_id"] = (
            df["source_file"].fillna("").astype(str)
            + ":"
            + df["source_row"].fillna("").astype(str)
            + ":"
            + df["proposal"].fillna("").astype(str)
            + ":"
            + df["evaluator_id"].fillna("").astype(str)
        )
    if len(df):
        df["choice_idx"] = df["choice_idx"].astype(int)
        df["prop_chid"] = df.apply(lambda row: make_prop_chid(row["proposal"], row["choice_idx"]), axis=1)
        df["eval_seconds"] = pd.to_numeric(df["eval_seconds"], errors="coerce")
        df["evaluator_total_seconds"] = pd.to_numeric(df["evaluator_total_seconds"], errors="coerce")
        df["body_mod_len_n"] = pd.to_numeric(df["body_mod_len_n"], errors="coerce")
        df["raw_low_effort"] = df["raw_low_effort"].map(coerce_bool)
    return df[LABEL_COLUMNS]


def flag_low_effort_labels(raw_labels, evaluator_seconds_threshold=300, long_body_threshold=0.33, short_eval_seconds=20):
    df = _coerce_label_input(raw_labels)
    if df.empty:
        for column in ["evaluator_total_seconds", "low_effort_evaluator", "low_effort_response", "low_effort"]:
            df[column] = []
        return df

    events = df.drop_duplicates(["evaluator_id", "source_event_id"])[["evaluator_id", "eval_seconds"]].copy()
    event_counts = events.groupby("evaluator_id")["eval_seconds"].count()
    event_totals = events.groupby("evaluator_id")["eval_seconds"].sum(min_count=1)
    fallback_totals = df["evaluator_id"].map(event_totals)
    df["evaluator_total_seconds"] = df["evaluator_total_seconds"].fillna(fallback_totals)
    has_time = df["evaluator_total_seconds"].notna() | (df["evaluator_id"].map(event_counts).fillna(0) > 0)
    df["low_effort_evaluator"] = has_time & (df["evaluator_total_seconds"] < evaluator_seconds_threshold)
    df["low_effort_response"] = (df["body_mod_len_n"] > long_body_threshold) & (df["eval_seconds"] < short_eval_seconds)
    df["low_effort"] = df["raw_low_effort"] | df["low_effort_evaluator"] | df["low_effort_response"]
    return df


def most_common_or_none(values):
    clean_values = [value for value in values if not is_missing(value)]
    if not clean_values:
        return pd.NA
    counts = Counter(clean_values)
    max_count = max(counts.values())
    winners = {value for value, count in counts.items() if count == max_count}
    if len(winners) > 1:
        return pd.NA
    return next(iter(winners))


def choose_repeated_label(group):
    counts = Counter(group["label"].dropna().tolist())
    if not counts:
        return pd.NA
    max_count = max(counts.values())
    winners = {label for label, count in counts.items() if count == max_count}
    for label in group["label"].tolist():
        if label in winners:
            return label
    return sorted(winners)[0]


def deduplicate_evaluator_labels(raw_labels):
    df = flag_low_effort_labels(raw_labels)
    if df.empty:
        return empty_evaluator_df()

    df = df.sort_values(
        ["proposal", "choice_idx", "evaluator_id", "source_file", "source_row", "label"],
        kind="mergesort",
    )
    rows = []
    for key, group in df.groupby(["proposal", "choice_idx", "evaluator_id"], sort=True, dropna=False):
        proposal, choice_idx, evaluator_id = key
        label = choose_repeated_label(group)
        chosen_rows = group[group["label"] == label]
        chosen = chosen_rows.iloc[0] if len(chosen_rows) else group.iloc[0]
        distinct_count = int(group["label"].dropna().nunique())
        rows.append(
            {
                "proposal": proposal,
                "choice_idx": int(choice_idx),
                "prop_chid": make_prop_chid(proposal, choice_idx),
                "evaluator_id": evaluator_id,
                "label": label,
                "eval_seconds": chosen["eval_seconds"],
                "evaluator_total_seconds": group["evaluator_total_seconds"].dropna().iloc[0]
                if group["evaluator_total_seconds"].notna().any()
                else pd.NA,
                "low_effort": bool(group["low_effort"].any()),
                "low_effort_evaluator": bool(group["low_effort_evaluator"].any()),
                "low_effort_response": bool(group["low_effort_response"].any()),
                "raw_low_effort": bool(group["raw_low_effort"].any()),
                "issue_hl": most_common_or_none(group["issue"]),
                "suggested_stance_hl": most_common_or_none(group["suggest"]),
                "raw_label_count": int(len(group)),
                "raw_distinct_label_count": distinct_count,
                "has_repeated_label": bool(len(group) > 1),
                "has_label_conflict": bool(distinct_count > 1),
                "duplicate_policy": "majority_label_then_first_observed",
                "source_file": chosen["source_file"],
                "source_row": int(chosen["source_row"]),
            }
        )

    result = pd.DataFrame(rows, columns=EVALUATOR_COLUMNS_OUT)
    result = result.sort_values(["proposal", "choice_idx", "evaluator_id"], kind="mergesort").reset_index(drop=True)
    if result.duplicated(["proposal", "choice_idx", "evaluator_id"]).any():
        raise ValueError("Evaluator-level human-label output has duplicate keys")
    return result


def issue_text_implies_other(issue_text):
    return not is_missing(issue_text) and str(issue_text).startswith("No")


def apply_notebook_issue_override(evaluator_labels):
    df = evaluator_labels.copy() if evaluator_labels is not None else empty_evaluator_df()
    if df.empty:
        return df
    override = df["issue_hl"].map(issue_text_implies_other)
    df.loc[override, "label"] = "Other"
    return df


def label_counts(df, prefix):
    counts = df["label"].value_counts()
    return {
        f"{prefix}_approve": int(counts.get("Approve", 0)),
        f"{prefix}_reject": int(counts.get("Reject", 0)),
        f"{prefix}_abstain": int(counts.get("Abstain", 0)),
        f"{prefix}_other": int(counts.get("Other", 0)),
    }


def aggregate_choice_labels(evaluator_labels):
    df = evaluator_labels.copy() if evaluator_labels is not None else empty_evaluator_df()
    if df.empty:
        return empty_choice_df()

    rows = []
    for key, group in df.groupby(["proposal", "choice_idx"], sort=True, dropna=False):
        proposal, choice_idx = key
        non_low = group[group["low_effort"] == False]  # noqa: E712
        effective = non_low if len(non_low) else group
        all_counts = label_counts(group, "n")
        effective_counts = label_counts(effective, "n_effective")
        stance = most_common_or_none(effective["label"])
        rows.append(
            {
                "proposal": proposal,
                "choice_idx": int(choice_idx),
                "prop_chid": make_prop_chid(proposal, choice_idx),
                "stance_hl": stance,
                "suggested_stance_hl": most_common_or_none(effective["suggested_stance_hl"]),
                "issue_hl": most_common_or_none(effective["issue_hl"]),
                "n_evaluations": int(len(group)),
                "n_non_low_effort_evaluations": int(len(non_low)),
                "n_low_effort_evaluations": int(group["low_effort"].sum()),
                **all_counts,
                **effective_counts,
                "low_effort_only": bool(len(non_low) == 0),
                "label_conflict": bool(effective["label"].dropna().nunique() > 1),
                "raw_label_conflict": bool(group["label"].dropna().nunique() > 1),
                "has_evaluator_label_conflict": bool(group["has_label_conflict"].any()),
            }
        )

    result = pd.DataFrame(rows, columns=CHOICE_COLUMNS_OUT)
    result = result.sort_values(["proposal", "choice_idx"], kind="mergesort").reset_index(drop=True)
    if result.duplicated(["proposal", "choice_idx"]).any():
        raise ValueError("Choice-level human-label output has duplicate keys")
    return result


def infer_choice_like_labels(choice_labels):
    df = choice_labels.copy() if choice_labels is not None else empty_choice_df()
    if df.empty:
        return df

    for proposal, group in df.groupby("proposal", sort=False, dropna=False):
        del proposal
        labels = [label for label in group["stance_hl"].tolist() if not is_missing(label)]
        if len(labels) != len(group) - 1:
            continue

        missing_idx = group[group["stance_hl"].map(is_missing)].index
        if len(missing_idx) != 1:
            continue

        if all(label in ["Reject", "Abstain", "Other"] for label in labels):
            df.loc[missing_idx[0], "stance_hl"] = "Approve_like"
        elif all(label in ["Approve", "Abstain", "Other"] for label in labels):
            df.loc[missing_idx[0], "stance_hl"] = "Reject_like"
    return df


def check_inferred_choice_labels(choice_labels, evaluator_labels):
    df = choice_labels.copy() if choice_labels is not None else empty_choice_df()
    evaluator_df = evaluator_labels.copy() if evaluator_labels is not None else empty_evaluator_df()
    if df.empty:
        return df

    for idx, row in df.iterrows():
        stance = row["stance_hl"]
        if is_missing(stance) or not str(stance).endswith("_like"):
            continue
        base_label = str(stance).split("_", maxsplit=1)[0]
        matching_raw_label = evaluator_df[
            (evaluator_df["proposal"] == row["proposal"])
            & (evaluator_df["choice_idx"] == row["choice_idx"])
            & (evaluator_df["label"] == base_label)
        ]
        df.loc[idx, "stance_hl"] = base_label if len(matching_raw_label) else pd.NA
    return df


def infer_notebook_choice_labels(choice_labels, evaluator_labels):
    inferred = infer_choice_like_labels(choice_labels)
    return check_inferred_choice_labels(inferred, evaluator_labels)


def apply_inferred_choice_stances(choice_labels, inferred_choice_labels):
    df = choice_labels.copy() if choice_labels is not None else empty_choice_df()
    inferred_df = inferred_choice_labels.copy() if inferred_choice_labels is not None else empty_choice_df()
    if df.empty or inferred_df.empty:
        return df

    inferred_stances = inferred_df.set_index(["proposal", "choice_idx"])["stance_hl"].to_dict()
    for idx, row in df.iterrows():
        if issue_text_implies_other(row["issue_hl"]):
            continue
        stance = inferred_stances.get((row["proposal"], row["choice_idx"]), pd.NA)
        if not is_missing(stance):
            df.loc[idx, "stance_hl"] = stance
    return df


def build_canonical_outputs(raw_labels):
    evaluator_labels = deduplicate_evaluator_labels(raw_labels)
    inferred_choice_labels = infer_notebook_choice_labels(aggregate_choice_labels(evaluator_labels), evaluator_labels)
    evaluator_labels = apply_notebook_issue_override(evaluator_labels)
    choice_labels = aggregate_choice_labels(evaluator_labels)
    choice_labels = apply_inferred_choice_stances(choice_labels, inferred_choice_labels)
    return evaluator_labels, choice_labels


def is_room_news_file(root, path):
    try:
        relative = path.resolve().relative_to(root)
    except ValueError:
        return False
    return len(relative.parts) == 3 and relative.parts[0] == "rooms" and relative.name == "news.ndjson"


def is_explicit_human_label_export(path):
    return bool(EXPLICIT_LABEL_EXPORT_RE.search(path.name))


def discover_human_label_files(input_dir=DATA_DIR):
    root = Path(input_dir).resolve()
    if not root.exists():
        return []

    paths = []
    rooms_dir = root / "rooms"
    if rooms_dir.exists():
        paths.extend(rooms_dir.glob("*/news.ndjson"))

    for pattern in ["*.csv", "*.csv.gz", "*.ndjson", "*.jsonl", "*.json"]:
        for path in root.glob(pattern):
            if not is_explicit_human_label_export(path):
                continue
            paths.append(path)

    unique = sorted({path.resolve() for path in paths if is_room_news_file(root, path) or is_explicit_human_label_export(path)})
    return [path.relative_to(root).as_posix() for path in unique]


def read_human_label_export(path):
    path = Path(path)
    name = path.name.lower()
    if name.endswith(".csv") or name.endswith(".csv.gz"):
        return pd.read_csv(path, low_memory=False)
    if name.endswith(".ndjson") or name.endswith(".jsonl"):
        return pd.read_json(path, lines=True)
    if name.endswith(".json"):
        with open(path, "r", encoding="utf-8") as handle:
            payload = json.load(handle)
        if isinstance(payload, list):
            return pd.DataFrame(payload)
        if isinstance(payload, dict):
            if isinstance(payload.get("rows"), list):
                return pd.DataFrame(payload["rows"])
            return pd.DataFrame([payload])
    raise ValueError(f"Unsupported human-label export: {path}")


def process_human_label_file(work_unit_id, input_dir=DATA_DIR):
    input_path = Path(input_dir) / work_unit_id
    raw_df = read_human_label_export(input_path)
    return extract_label_rows(raw_df, source_file=str(work_unit_id))


def _chunk_path(worker_dir, work_unit_id, kind):
    token = hashlib.sha1(str(work_unit_id).encode("utf-8")).hexdigest()
    subdir = LABEL_CHUNK_DIR if kind == "labels" else ERROR_CHUNK_DIR
    return os.path.join(worker_dir, subdir, f"{token}.csv")


def write_chunk(path, df, columns):
    ensure_dir(os.path.dirname(path))
    if df is None or df.empty:
        df = pd.DataFrame(columns=columns)
    write_csv_output(path, df[columns])


def process_work_unit(work_unit_id, context, worker_dir):
    label_df, error_df = process_human_label_file(work_unit_id, input_dir=context["input_dir"])
    write_chunk(_chunk_path(worker_dir, work_unit_id, "labels"), label_df, LABEL_COLUMNS)
    write_chunk(_chunk_path(worker_dir, work_unit_id, "errors"), error_df, ERROR_COLUMNS)
    return {"label_rows": len(label_df), "error_rows": len(error_df)}


def worker_process_vote_expansion(worker_id, work_unit_id, worker_dir, manifest_path):
    del worker_id, manifest_path
    if not GLOBAL_CONTEXT:
        init_global_context()
    return process_work_unit(work_unit_id, GLOBAL_CONTEXT, worker_dir)


def worker_process_human_labels(worker_id, work_unit_id, worker_dir, manifest_path):
    return worker_process_vote_expansion(worker_id, work_unit_id, worker_dir, manifest_path)


def worker_dir_for_run(worker_id, tmp_root):
    return os.path.join(tmp_root, f"worker_{worker_id:02d}")


def finished_manifest_rows(manifest_path=None, manifest_rows=None):
    import dao_bias.jobs.batch as batch_runner

    manifest_path = manifest_path or MANIFEST_PATH
    if manifest_rows is None:
        if not os.path.exists(manifest_path):
            return []
        manifest_rows = batch_runner.load_manifest(manifest_path)
    return [
        row
        for row in manifest_rows
        if row.get("status") == "finished" and str(row.get("worker_id", "")).isdigit()
    ]


def read_chunk_if_exists(path, columns):
    if not os.path.exists(path):
        return pd.DataFrame(columns=columns)
    return pd.read_csv(path, low_memory=False)


def collect_worker_chunks(tmp_root, manifest_rows=None):
    label_frames = []
    error_frames = []
    if tmp_root and os.path.isdir(tmp_root):
        for row in finished_manifest_rows(manifest_rows=manifest_rows):
            worker_dir = worker_dir_for_run(int(row["worker_id"]), tmp_root)
            work_unit_id = row["work_unit_id"]
            label_frames.append(read_chunk_if_exists(_chunk_path(worker_dir, work_unit_id, "labels"), LABEL_COLUMNS))
            error_frames.append(read_chunk_if_exists(_chunk_path(worker_dir, work_unit_id, "errors"), ERROR_COLUMNS))
    labels = pd.concat(label_frames, ignore_index=True) if label_frames else empty_label_df()
    errors = pd.concat(error_frames, ignore_index=True) if error_frames else empty_error_df()
    return labels, errors


def write_canonical_outputs(raw_labels, parse_errors, clean_output_dir=OUTPUT_DIR):
    ensure_dir(clean_output_dir)
    raw_labels = _coerce_label_input(raw_labels)
    parse_errors = parse_errors.copy() if parse_errors is not None else empty_error_df()
    for column in ERROR_COLUMNS:
        if column not in parse_errors.columns:
            parse_errors[column] = pd.NA
    parse_errors = parse_errors[ERROR_COLUMNS].sort_values(["source_file", "source_row", "reason"], kind="mergesort")

    evaluator_labels, choice_labels = build_canonical_outputs(raw_labels)
    write_csv_output(os.path.join(clean_output_dir, EVALUATOR_LABELS_FILE), evaluator_labels)
    write_csv_output(os.path.join(clean_output_dir, CHOICE_LABELS_FILE), choice_labels)
    write_csv_output(os.path.join(clean_output_dir, PARSE_ERRORS_FILE), parse_errors)

    summary = {
        "job_name": JOB_NAME,
        "generated_at": utc_now_iso(),
        "raw_label_rows": int(len(raw_labels)),
        "evaluator_rows": int(len(evaluator_labels)),
        "choice_rows": int(len(choice_labels)),
        "parse_error_rows": int(len(parse_errors)),
        "evaluator_duplicate_keys": int(evaluator_labels.duplicated(["proposal", "choice_idx", "evaluator_id"]).sum())
        if len(evaluator_labels)
        else 0,
        "choice_duplicate_keys": int(choice_labels.duplicated(["proposal", "choice_idx"]).sum()) if len(choice_labels) else 0,
    }
    write_json_output(os.path.join(clean_output_dir, "run_summary.json"), summary)
    return summary


def run_human_label_cleaning_job(input_dir=DATA_DIR, clean_output_dir=OUTPUT_DIR):
    label_frames = []
    error_frames = []
    for work_unit_id in discover_human_label_files(input_dir):
        labels, errors = process_human_label_file(work_unit_id, input_dir=input_dir)
        label_frames.append(labels)
        error_frames.append(errors)
    raw_labels = pd.concat(label_frames, ignore_index=True) if label_frames else empty_label_df()
    parse_errors = pd.concat(error_frames, ignore_index=True) if error_frames else empty_error_df()
    return write_canonical_outputs(raw_labels, parse_errors, clean_output_dir=clean_output_dir)


def list_manifest_work_units(data_dir=DATA_DIR):
    return discover_human_label_files(data_dir)


def init_global_context(context=None):
    global GLOBAL_CONTEXT
    if context is None:
        context = {
            "input_dir": DATA_DIR,
            "df_proposals": pd.DataFrame(),
            "votes_by_prop": {},
        }
    else:
        context = dict(context)
        context.setdefault("input_dir", DATA_DIR)
        context.setdefault("df_proposals", pd.DataFrame())
        context.setdefault("votes_by_prop", {})
    GLOBAL_CONTEXT = context
    return GLOBAL_CONTEXT


def list_work_units(df_proposals=None, votes_by_prop=None):
    del df_proposals, votes_by_prop
    input_dir = GLOBAL_CONTEXT.get("input_dir", DATA_DIR)
    return list_manifest_work_units(input_dir)


def build_work_manifest(work_unit_ids, batch_runner_module):
    ensure_output_dirs()
    run_id = new_run_id()
    tmp_root = os.path.join(TMP_DIR, run_id)
    ensure_dir(tmp_root)
    write_run_meta(run_id, tmp_root)
    return batch_runner_module.build_manifest(work_unit_ids, MANIFEST_PATH, JOB_NAME)


def reduce_vote_expansion_outputs(clean_output_dir=OUTPUT_DIR, manifest_path=MANIFEST_PATH):
    import dao_bias.jobs.batch as batch_runner

    clean_output_dir = clean_output_dir or OUTPUT_DIR
    manifest_path = manifest_path or MANIFEST_PATH
    manifest_rows, _ = batch_runner.load_manifest_snapshot(manifest_path)
    labels, errors = collect_worker_chunks(current_tmp_root(), manifest_rows=manifest_rows)
    return write_canonical_outputs(labels, errors, clean_output_dir=clean_output_dir)


def reduce_human_label_outputs(clean_output_dir=OUTPUT_DIR, manifest_path=MANIFEST_PATH):
    return reduce_vote_expansion_outputs(clean_output_dir=clean_output_dir, manifest_path=manifest_path)


def sample_work_units(limit, work_unit_ids):
    return sorted(work_unit_ids)[:limit]
