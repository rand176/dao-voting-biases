"""Lossless public crowdsourcing tables; selection is explicit, never a fallback."""

import csv
from pathlib import Path

from dao_bias.data.input_paths import resolve_data_input


COMBINED_COLUMNS = (
    "proposal", "title", "body", "choice", "title_lang", "body_lang",
    "body_mod_len", "title_mod_len", "cluster_id", "choices_len",
    "choices_json", "body_mod_len_n", "choice_idx", "prop_chid",
    *(f"{prefix}_{i}" for prefix in ("eval", "eval_t", "eval_issue", "eval_suggest") for i in range(1, 5)),
)
PROPOSAL_COLUMNS = (
    "proposal", "title", "body", "title_lang", "body_lang", "body_mod_len",
    "title_mod_len", "cluster_id", "choices_len", "choices_json", "body_mod_len_n",
)
LABEL_COLUMNS = tuple(c for c in COMBINED_COLUMNS if c == "proposal" or c not in PROPOSAL_COLUMNS)
PUBLIC_DIRECTORY = Path("humanlabels")
PROPOSALS_PATH = PUBLIC_DIRECTORY / "dao_rnd_proposals.csv"
LABELS_PATH = PUBLIC_DIRECTORY / "dao_rnd_choice_labels.csv"
REPORT_NAME = "dao_rnd.redactions.json"


def _invalid(reason: str) -> ValueError:
    return ValueError(f"Invalid public crowdsourcing CSV: {reason}")


def _validate_rows(rows: list[dict[str, str]]) -> None:
    if not rows:
        raise _invalid("sample is empty")
    seen = set()
    choice_indices = {}
    choice_counts = {}
    for number, row in enumerate(rows, 2):
        if set(row) != set(COMBINED_COLUMNS) or any(v is None for v in row.values()):
            raise _invalid(f"incompatible schema or malformed record {number}")
        proposal = row["proposal"]
        if not proposal.strip():
            raise _invalid(f"missing proposal identifier at record {number}")
        for column in ("choices_len", "choice_idx"):
            value = row[column]
            if not value.isascii() or not value.isdecimal() or int(value) < 1:
                raise _invalid(f"{column} must be a positive integer at record {number}")
        if (int(row["choice_idx"]) > int(row["choices_len"])
                or row["prop_chid"] != proposal + str(int(row["choice_idx"]))):
            raise _invalid(f"inconsistent proposal-choice key at record {number}")
        if row["prop_chid"] in seen:
            raise _invalid(f"duplicate proposal-choice key at record {number}")
        seen.add(row["prop_chid"])
        count = int(row["choices_len"])
        if choice_counts.setdefault(proposal, count) != count:
            raise _invalid(f"Conflicting proposal metadata at record {number}")
        choice_indices.setdefault(proposal, set()).add(int(row["choice_idx"]))
    # Positive bounded indexes and unique keys make this a complete 1..N set.
    for proposal, indices in choice_indices.items():
        if len(indices) != choice_counts[proposal]:
            raise _invalid(f"incomplete choice labels for proposal {proposal}: "
                           f"expected {choice_counts[proposal]}, found {len(indices)}")


def split_crowdsourcing_rows(rows: list[dict[str, str]]) -> tuple[list[dict[str, str]], list[dict[str, str]]]:
    """Split validated combined rows, retaining first-proposal and choice-row order."""
    _validate_rows(rows)
    proposals = {}
    labels = []
    for number, row in enumerate(rows, 2):
        proposal = {c: row[c] for c in PROPOSAL_COLUMNS}
        previous = proposals.setdefault(row["proposal"], proposal)
        if previous != proposal:
            raise _invalid(f"Conflicting proposal metadata at record {number}")
        labels.append({c: row[c] for c in LABEL_COLUMNS})
    return list(proposals.values()), labels


def _read_table(path: Path, columns: tuple[str, ...]) -> list[dict[str, str]]:
    try:
        with path.open(newline="", encoding="utf-8") as handle:
            reader = csv.DictReader(handle, strict=True)
            header = reader.fieldnames or []
            if len(header) != len(set(header)) or set(header) != set(columns):
                raise _invalid(f"missing, duplicate, or unexpected columns in {path.name}")
            rows = list(reader)
    except csv.Error:
        raise _invalid(f"malformed CSV in {path.name}") from None
    if not rows or any(None in row or any(v is None for v in row.values()) for row in rows):
        raise _invalid(f"empty table or malformed records in {path.name}")
    return rows


def load_public_crowdsourcing(analysis_root: Path) -> list[dict[str, str]]:
    """Load both public tables, reject bad joins, and reconstruct the combined rows."""
    proposal_path = resolve_data_input(PROPOSALS_PATH, analysis_root, public_data=True)
    label_path = resolve_data_input(LABELS_PATH, analysis_root, public_data=True)
    proposals = {}
    for row in _read_table(proposal_path, PROPOSAL_COLUMNS):
        key = row["proposal"]
        if not key.strip() or key in proposals:
            raise _invalid("missing or duplicate proposal identifier")
        proposals[key] = row
    reconstructed, used = [], set()
    for row in _read_table(label_path, LABEL_COLUMNS):
        key = row["proposal"]
        if key not in proposals:
            raise _invalid("choice label references a missing proposal")
        combined = {**proposals[key], **row}
        reconstructed.append({c: combined[c] for c in COMBINED_COLUMNS})
        used.add(key)
    if used != set(proposals):
        raise _invalid("proposal table contains proposals without choice labels")
    _validate_rows(reconstructed)
    return reconstructed
