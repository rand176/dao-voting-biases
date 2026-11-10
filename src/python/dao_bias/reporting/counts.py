from __future__ import annotations
from dao_bias.data.paths import PACKAGE_ROOT

from dao_bias.data.input_paths import prepared_analysis_input

import argparse
import os
import csv
import gzip
import json
import math
import re
import statistics
from collections import Counter, defaultdict
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

import dao_bias.reporting.suggested_stance as suggested_stance_diagnostics
from dao_bias.data.public import load_public_crowdsourcing, PROPOSALS_PATH, LABELS_PATH


SCRIPT_PATH = Path(__file__).resolve()
ANALYSIS_ROOT = PACKAGE_ROOT

RELEASE_REGISTRY = Path("metadata/analysis_data_releases.csv")
RAW_PROPOSALS_PATH = Path("data/2024/raw/proposals_0.csv.gz")
RAW_VOTES_DIR = Path("data/2024/raw/votes")
STAGE3_AUDIT_PATH = Path("data/2024/derived/debug/stage3_vote_merge_audit.csv")
PROPOSAL_ISSUE_FLAGS_PATH = Path("data/2024/clean_proposal_flags/proposal_vote_issue_flags.csv.gz")
VOTE_RUN_SUMMARY_PATH = Path("data/2024/clean_vote_outputs/run_summary.json")
AUTHOR_RUN_SUMMARY_PATH = Path("data/2024/clean_author_vote_outputs/run_summary.json")
STANCE_ONE_SIDED_EXCEPTIONS_PATH = Path("metadata/exceptions/stance_one_sided_exceptions.csv")
HUMAN_LABELS_DIR = Path("data/2024/humanlabels")
HUMAN_LABEL_SAMPLE_PATH = HUMAN_LABELS_DIR / "dao_rnd_merged.csv"
HUMAN_LABEL_EVALUATOR_LABELS_PATH = Path(
    "data/2024/clean_human_label_outputs/human_label_evaluator_labels.csv"
)

MACRO_RELATIVE_PATH = Path("sections/macros/analysis_counts.tex")
PROVENANCE_DIR = Path("metadata/analysis_data_provenance")
LATEX_MACRO_NAME_RE = re.compile(r"^[A-Za-z]+$")
LLM_KAPPA_SOURCES = ("llama", "llama33", "oai", "chatgpt")
AUTOMATED_SUGGESTED_SOURCES = ("llama", "llama33", "oai", "chatgpt")

SINGLE_PREFERENCE_TYPES = {"basic", "single-choice"}
VOTING_TYPES = (
    ("basic", "Basic"),
    ("single-choice", "SingleChoice"),
    ("weighted", "Weighted"),
    ("quadratic", "Quadratic"),
    ("ranked-choice", "RankedChoice"),
    ("approval", "Approval"),
)
STANCE_APPROVE = "APPROVE"
STANCE_REJECT = "REJECT"
LABEL_STATS_REQUIRED_COLUMNS = {
    "proposal",
    "space",
    "type",
    "scores_total",
    "n_votes",
    "flagged",
    "stance",
    "stance_oai",
    "suggested_stance_oai",
    "effect_oai",
    "stance_llama",
    "suggested_stance_llama",
    "effect_llama",
    "stance_chatgpt",
    "suggested_stance_chatgpt",
    "effect_chatgpt",
    "stance_hl",
    "suggested_stance_hl",
    "n_evaluations",
    "n_non_low_effort_evaluations",
    "n_low_effort_evaluations",
    "low_effort_only",
}


@dataclass(frozen=True)
class StatValue:
    macro: str
    value: str | int | float
    description: str
    source: str
    latex_kind: str = "num"

    @property
    def latex_value(self) -> str:
        if self.latex_kind == "text":
            return str(self.value)
        return f"\\num{{{self.value}}}"


def open_text(path: Path):
    if path.suffix == ".gz":
        return gzip.open(path, "rt", newline="", encoding="utf-8")
    return path.open("r", newline="", encoding="utf-8")


def require_path(path: Path, label: str) -> Path:
    if not path.exists():
        raise FileNotFoundError(f"Missing {label}: {path}")
    return path


def normalize_id(value: str | None) -> str:
    return (value or "").strip().lower()


def truthy(value: str | int | float | None) -> bool:
    if value is None:
        return False
    if isinstance(value, (int, float)):
        return not math.isclose(float(value), 0.0)
    return value.strip().lower() in {"1", "true", "t", "yes", "y"}


def number_or_zero(value: str | int | float | None) -> float:
    if value is None or value == "":
        return 0.0
    try:
        return float(value)
    except (TypeError, ValueError):
        return 0.0


def format_float(value: float, digits: int = 2) -> str:
    if math.isclose(value, round(value)):
        return str(int(round(value)))
    return f"{value:.{digits}f}".rstrip("0").rstrip(".")


def pct(part: int | float, total: int | float, digits: int = 2) -> str:
    if not total:
        return "0"
    return format_float((float(part) / float(total)) * 100.0, digits=digits)


def reduction_count(raw: int | float | str, final: int | float | str) -> str:
    return format_float(float(raw) - float(final))


def reduction_pct(raw: int | float | str, final: int | float | str) -> str:
    return pct(float(raw) - float(final), float(raw))


def as_optional_number(value: str | int | float | None) -> float | None:
    if value is None or value == "":
        return None
    try:
        out = float(value)
    except (TypeError, ValueError):
        return None
    return out if math.isfinite(out) else None


def summarize_numeric(values: Iterable[int | float]) -> dict[str, str]:
    values = [float(value) for value in values]
    if not values:
        return {"mean": "0", "std": "0", "median": "0"}
    std = statistics.stdev(values) if len(values) > 1 else 0.0
    return {
        "mean": format_float(statistics.mean(values)),
        "std": format_float(std),
        "median": format_float(statistics.median(values)),
    }


def parse_json_list_length(value: str) -> int:
    try:
        parsed = json.loads(value)
    except json.JSONDecodeError:
        return 0
    return len(parsed) if isinstance(parsed, list) else 0


def read_release_registry(analysis_root: Path) -> dict[str, Path]:
    registry_path = require_path(prepared_analysis_input(RELEASE_REGISTRY, analysis_root), "analysis release registry")
    releases: dict[str, Path] = {}
    with registry_path.open("r", newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            name = (row.get("release_name") or "").strip()
            relative_path = (row.get("relative_path") or "").strip()
            if name and relative_path:
                releases[name] = Path(relative_path)
    return releases


def read_csv_fieldnames(path: Path) -> set[str]:
    require_path(path, "CSV data")
    with open_text(path) as handle:
        reader = csv.DictReader(handle)
        return set(reader.fieldnames or [])


def single_option_proposal_ids(rows: Iterable[dict[str, str]]) -> set[str]:
    proposal_rows: Counter[str] = Counter()
    reported_single_option: set[str] = set()
    for row in rows:
        proposal = normalize_id(row.get("proposal"))
        if not proposal:
            continue
        proposal_rows[proposal] += 1
        reported_choices = as_optional_number(row.get("N_choices"))
        if reported_choices is not None and reported_choices <= 1:
            reported_single_option.add(proposal)
    return {proposal for proposal, row_count in proposal_rows.items() if row_count <= 1} | reported_single_option


def resolve_release_path(analysis_root: Path, release: str) -> Path:
    releases = read_release_registry(analysis_root)
    if release not in releases:
        known = ", ".join(sorted(releases)) or "none"
        raise KeyError(f"Unknown analysis release '{release}'. Known releases: {known}")
    return require_path(prepared_analysis_input(releases[release], analysis_root), f"analysis release '{release}'")


def count_raw_proposals(path: Path) -> dict[str, int]:
    require_path(path, "raw proposals")
    proposal_count = 0
    snapshot_flagged_count = 0
    choice_count = 0
    spaces: set[str] = set()
    with open_text(path) as handle:
        for row in csv.DictReader(handle):
            proposal_count += 1
            if truthy(row.get("flagged")):
                snapshot_flagged_count += 1
            space = (row.get("space") or "").strip()
            if space:
                spaces.add(space)
            choice_count += parse_json_list_length(row.get("choices") or "[]")
    return {
        "raw_proposals": proposal_count,
        "raw_snapshot_flagged_proposals": snapshot_flagged_count,
        "raw_spaces": len(spaces),
        "raw_choices": choice_count,
    }


def count_lines_fast(path: Path) -> int:
    opener = gzip.open if path.suffix == ".gz" else open
    count = 0
    with opener(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            count += chunk.count(b"\n")
    return max(count - 1, 0)


def count_raw_votes(votes_dir: Path) -> int:
    require_path(votes_dir, "raw votes directory")
    files = sorted(path for path in votes_dir.iterdir() if path.suffix in {".csv", ".gz"})
    if not files:
        raise FileNotFoundError(f"No raw vote CSV files found in {votes_dir}")
    return sum(count_lines_fast(path) for path in files)


def read_release_stats(path: Path, documented_stance_proposals: dict[str, set[str]]) -> dict[str, object]:
    require_path(path, "current release data")
    proposal_spaces: dict[str, str] = {}
    proposal_types: dict[str, str] = {}
    proposal_flagged: dict[str, bool] = defaultdict(bool)
    proposal_scores_total: dict[str, float] = {}
    proposal_choice_rows: Counter[str] = Counter()
    proposal_stances: dict[str, set[str]] = defaultdict(set)
    vote_mass = 0.0
    snapshot_flagged_proposals: set[str] = set()
    rows: list[dict[str, str]] = []

    with path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        required_columns = {
            "proposal",
            "space",
            "type",
            "scores_total",
            "n_votes",
            "flagged",
            "stance",
        }
        missing_columns = sorted(required_columns.difference(reader.fieldnames or []))
        if missing_columns:
            raise ValueError(f"Release file is missing required columns: {', '.join(missing_columns)}")

        for row in reader:
            proposal = normalize_id(row.get("proposal"))
            if proposal and truthy(row.get("flagged")):
                snapshot_flagged_proposals.add(proposal)
            rows.append(row)

    unflagged_rows = [
        row
        for row in rows
        if normalize_id(row.get("proposal")) and normalize_id(row.get("proposal")) not in snapshot_flagged_proposals
    ]
    single_option_proposals = single_option_proposal_ids(unflagged_rows)
    excluded_single_option_choices = 0

    for row in unflagged_rows:
        proposal = normalize_id(row.get("proposal"))
        if proposal in single_option_proposals:
            excluded_single_option_choices += 1
            continue
        proposal_choice_rows[proposal] += 1
        proposal_spaces.setdefault(proposal, (row.get("space") or "").strip())
        proposal_types.setdefault(proposal, (row.get("type") or "").strip().lower())
        proposal_flagged[proposal] = proposal_flagged[proposal] or truthy(row.get("flagged"))
        if proposal not in proposal_scores_total:
            proposal_scores_total[proposal] = number_or_zero(row.get("scores_total"))
        vote_mass += number_or_zero(row.get("n_votes"))
        stance = (row.get("stance") or "").strip().upper()
        if stance:
            proposal_stances[proposal].add(stance)

    final_proposals = len(proposal_choice_rows)
    proposal_per_space = Counter(space for space in proposal_spaces.values() if space)
    choice_summary = summarize_numeric(proposal_choice_rows.values())
    space_summary = summarize_numeric(proposal_per_space.values())
    single_preference = sum(
        1 for proposal in proposal_choice_rows if proposal_types.get(proposal) in SINGLE_PREFERENCE_TYPES
    )
    flagged = sum(1 for value in proposal_flagged.values() if value)

    known_types = {key for key, _ in VOTING_TYPES}
    total_choice_rows = sum(proposal_choice_rows.values())
    voting_type_proposal_counts: dict[str, int] = {key: 0 for key in known_types}
    voting_type_choice_counts: dict[str, int] = {key: 0 for key in known_types}
    other_type_examples: set[str] = set()
    for proposal, choice_rows in proposal_choice_rows.items():
        vt = proposal_types.get(proposal, "")
        if vt in known_types:
            voting_type_proposal_counts[vt] += 1
            voting_type_choice_counts[vt] += choice_rows
        else:
            other_type_examples.add(vt or "<empty>")
    if other_type_examples:
        raise ValueError(
            "Unexpected voting types in release file: "
            + ", ".join(sorted(other_type_examples))
            + ". Update VOTING_TYPES in dao_bias.reporting.counts."
        )

    single_preference_choices = sum(
        voting_type_choice_counts[key]
        for key in SINGLE_PREFERENCE_TYPES
        if key in voting_type_choice_counts
    )
    multi_preference_proposals = final_proposals - single_preference
    multi_preference_choices = total_choice_rows - single_preference_choices

    approve_only = set()
    reject_only = set()
    proposal_states = {}
    for proposal, stances in proposal_stances.items():
        has_approve = STANCE_APPROVE in stances
        has_reject = STANCE_REJECT in stances
        if has_approve and not has_reject:
            approve_only.add(proposal)
            proposal_states[proposal] = "approve_only"
        elif has_reject and not has_approve:
            reject_only.add(proposal)
            proposal_states[proposal] = "reject_only"
    imbalanced = approve_only | reject_only
    documented_imbalanced = {
        proposal
        for proposal in imbalanced
        if proposal_states[proposal] in documented_stance_proposals.get(proposal, set())
    }

    return {
        "final_choices": total_choice_rows,
        "final_proposals": final_proposals,
        "final_spaces": len(set(space for space in proposal_spaces.values() if space)),
        "final_vote_mass": format_float(vote_mass),
        "final_score_mass": format_float(sum(proposal_scores_total.values())),
        "final_flagged_proposals": flagged,
        "final_flagged_proposal_pct": pct(flagged, final_proposals),
        "excluded_snapshot_flagged_proposals": len(snapshot_flagged_proposals),
        "excluded_single_option_proposals": len(single_option_proposals),
        "excluded_single_option_choices": excluded_single_option_choices,
        "single_preference_proposals": single_preference,
        "single_preference_proposal_pct": pct(single_preference, final_proposals),
        "single_preference_choices": single_preference_choices,
        "single_preference_choice_pct": pct(single_preference_choices, total_choice_rows),
        "multi_preference_proposals": multi_preference_proposals,
        "multi_preference_proposal_pct": pct(multi_preference_proposals, final_proposals),
        "multi_preference_choices": multi_preference_choices,
        "multi_preference_choice_pct": pct(multi_preference_choices, total_choice_rows),
        "choice_per_proposal": choice_summary,
        "proposal_per_space": space_summary,
        "proposal_ids": set(proposal_choice_rows),
        "approve_only_proposals": len(approve_only),
        "reject_only_proposals": len(reject_only),
        "stance_imbalance_proposals": len(imbalanced),
        "stance_imbalance_documented_proposals": len(documented_imbalanced),
        "stance_imbalance_undocumented_proposals": len(imbalanced - documented_imbalanced),
        "stance_imbalance_examples": sorted(imbalanced - documented_imbalanced)[:20],
        "voting_type_proposals": voting_type_proposal_counts,
        "voting_type_choices": voting_type_choice_counts,
        "voting_type_proposal_pct": {
            key: pct(voting_type_proposal_counts[key], final_proposals) for key in known_types
        },
        "voting_type_choice_pct": {
            key: pct(voting_type_choice_counts[key], total_choice_rows) for key in known_types
        },
    }


def read_issue_flag_stats(path: Path, final_proposal_ids: set[str]) -> dict[str, int]:
    if not path.exists():
        return {
            "vote_issue_proposals": 0,
            "invalid_vote_issue_proposals": 0,
            "duplicate_ballot_issue_proposals": 0,
            "final_vote_issue_proposals": 0,
        }

    issue_proposals = set()
    invalid_vote_proposals = set()
    duplicate_ballot_proposals = set()
    with open_text(path) as handle:
        for row in csv.DictReader(handle):
            proposal = normalize_id(row.get("id"))
            if not proposal:
                continue
            if truthy(row.get("flag_any_vote_issues")):
                issue_proposals.add(proposal)
            if truthy(row.get("flag_invalid_votes")):
                invalid_vote_proposals.add(proposal)
            if truthy(row.get("flag_duplicate_ballots")):
                duplicate_ballot_proposals.add(proposal)

    return {
        "vote_issue_proposals": len(issue_proposals),
        "invalid_vote_issue_proposals": len(invalid_vote_proposals),
        "duplicate_ballot_issue_proposals": len(duplicate_ballot_proposals),
        "final_vote_issue_proposals": len(issue_proposals & final_proposal_ids),
    }


def read_stage3_audit(path: Path) -> dict[str, str]:
    defaults = {
        "stage3_loaded_final_proposals": "0",
        "stage3_exclusion_proposals": "0",
        "stage3_invalid_choice_rows_removed": "0",
        "stage3_invalid_choice_vote_mass_removed": "0",
        "stage3_final_choice_rows": "0",
        "stage3_final_proposals": "0",
    }
    if not path.exists():
        return defaults

    metrics: dict[tuple[str, str], str] = {}
    with path.open("r", newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            metrics[(row.get("phase") or "", row.get("metric") or "")] = row.get("value") or "0"

    return {
        "stage3_loaded_final_proposals": format_float(number_or_zero(metrics.get(("load_proposals", "final_proposals")))),
        "stage3_exclusion_proposals": format_float(number_or_zero(metrics.get(("apply_stage_exclusions", "proposals_removed")))),
        "stage3_invalid_choice_rows_removed": format_float(
            number_or_zero(metrics.get(("remove_invalid_choice_idx", "rows_removed")))
        ),
        "stage3_invalid_choice_vote_mass_removed": format_float(
            number_or_zero(metrics.get(("remove_invalid_choice_idx", "invalid_vote_mass")))
        ),
        "stage3_final_choice_rows": format_float(number_or_zero(metrics.get(("final_output", "rows")))),
        "stage3_final_proposals": format_float(number_or_zero(metrics.get(("final_output", "proposals")))),
    }


def read_json(path: Path) -> dict:
    if not path.exists():
        return {}
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def read_run_summary_stats(analysis_root: Path) -> dict[str, str]:
    vote = read_json(analysis_root / VOTE_RUN_SUMMARY_PATH)
    author = read_json(analysis_root / AUTHOR_RUN_SUMMARY_PATH)
    return {
        "vote_expansion_finished_proposals": str(vote.get("finished_proposals", 0)),
        "vote_expansion_favorite_rows": str(vote.get("favorite_choice_counts", {}).get("rows", 0)),
        "vote_expansion_favorite_error_rows": str(vote.get("favorite_choice_counts", {}).get("error_rows", 0)),
        "vote_expansion_support_rows": str(vote.get("support_fraction", {}).get("rows", 0)),
        "vote_expansion_support_error_rows": str(vote.get("support_fraction", {}).get("error_rows", 0)),
        "author_vote_finished_proposals": str(author.get("finished_proposals", 0)),
        "author_vote_favorite_rows": str(author.get("favorite_choice", {}).get("rows", 0)),
        "author_vote_favorite_error_rows": str(author.get("favorite_choice", {}).get("error_rows", 0)),
        "author_vote_support_rows": str(author.get("support_fraction", {}).get("rows", 0)),
        "author_vote_support_error_rows": str(author.get("support_fraction", {}).get("error_rows", 0)),
    }


def read_documented_stance_proposals(analysis_root: Path) -> dict[str, set[str]]:
    proposals: dict[str, set[str]] = defaultdict(set)
    exceptions_path = analysis_root / STANCE_ONE_SIDED_EXCEPTIONS_PATH
    if not exceptions_path.exists():
        return proposals

    allowed_states = {"approve_only", "reject_only"}
    with exceptions_path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            proposal = normalize_id(row.get("proposal"))
            allowed_state = (row.get("allowed_state") or "").strip().lower()
            if not proposal or not allowed_state:
                continue
            if allowed_state not in allowed_states:
                raise ValueError(
                    f"Unknown one-sided stance exception state {allowed_state!r} in {exceptions_path}"
                )
            proposals[proposal].add(allowed_state)
    return proposals


def normalize_label(value: str | int | float | None) -> str:
    if value is None:
        return ""
    label = str(value).strip()
    if not label or label.upper() in {"NA", "N/A", "NULL"}:
        return ""
    return label.upper()


def cohen_kappa(left: Iterable[str], right: Iterable[str]) -> float:
    pairs = [(normalize_label(a), normalize_label(b)) for a, b in zip(left, right)]
    pairs = [(a, b) for a, b in pairs if a and b]
    if not pairs:
        return 0.0

    total = len(pairs)
    observed = sum(1 for a, b in pairs if a == b) / total
    left_counts = Counter(a for a, _ in pairs)
    right_counts = Counter(b for _, b in pairs)
    labels = sorted(set(left_counts) | set(right_counts))
    expected = sum((left_counts[label] / total) * (right_counts[label] / total) for label in labels)
    if math.isclose(expected, 1.0):
        return 1.0 if math.isclose(observed, 1.0) else 0.0
    return (observed - expected) / (1.0 - expected)


def source_macro_prefix(source_name: str) -> str:
    return {
        "oai": "Oai",
        "llama": "Llama",
        "llama33": "LlamaThreePointThree",
        "chatgpt": "Chatgpt",
        "human": "Human",
    }[source_name]


def source_display_label(source_name: str) -> str:
    return {
        "oai": "OAI",
        "llama": "Llama",
        "llama33": "Llama 3.3",
        "chatgpt": "ChatGPT",
        "human": "Human",
    }[source_name]


def paper_source_display_label(source_name: str) -> str:
    return {
        "oai": "OAI",
        "llama": "Llama 3.1",
        "llama33": "Llama 3.3",
        "chatgpt": "ChatGPT",
        "human": "Human",
    }[source_name]


def kappa_agreement_label(value: str | float) -> str:
    score = float(value)
    if score < 0:
        return "less than chance"
    if score <= 0.20:
        return "slight"
    if score <= 0.40:
        return "fair"
    if score <= 0.60:
        return "moderate"
    if score <= 0.80:
        return "substantial"
    return "almost perfect"


def release_has_label_stats(path: Path) -> bool:
    return LABEL_STATS_REQUIRED_COLUMNS.issubset(read_csv_fieldnames(path))


def release_has_suggested_stance_diagnostics(path: Path) -> bool:
    fieldnames = read_csv_fieldnames(path)
    has_winner = "choice_won" in fieldnames
    required_suggested = {
        suggested_stance_diagnostics.MODEL_SPECS[source]["suggested"]
        for source in AUTOMATED_SUGGESTED_SOURCES
    }
    return has_winner and required_suggested.issubset(fieldnames)


def read_candidate_release_stats(path: Path) -> dict[str, object]:
    require_path(path, "candidate release data")
    with path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        fieldnames = set(reader.fieldnames or [])
        missing_columns = sorted(LABEL_STATS_REQUIRED_COLUMNS.difference(fieldnames))
        if missing_columns:
            raise ValueError(f"Candidate release file is missing required columns: {', '.join(missing_columns)}")

        rows = [row for row in reader if normalize_id(row.get("proposal"))]

    single_option_proposals = single_option_proposal_ids(rows)
    rows = [
        row
        for row in rows
        if normalize_id(row.get("proposal")) not in single_option_proposals
    ]

    proposal_rows: Counter[str] = Counter()
    coverage_sources: dict[str, tuple[str, ...]] = {
        "oai": ("stance_oai", "suggested_stance_oai", "effect_oai"),
        "llama": ("stance_llama", "suggested_stance_llama", "effect_llama"),
        "chatgpt": ("stance_chatgpt", "suggested_stance_chatgpt", "effect_chatgpt"),
        "human": ("stance_hl", "suggested_stance_hl"),
    }
    optional_sources: dict[str, tuple[str, ...]] = {
        "llama33": ("stance_llama33", "suggested_stance_llama33", "effect_llama33"),
    }
    for source_name, cols in optional_sources.items():
        if all(col in fieldnames for col in cols):
            coverage_sources[source_name] = cols

    all_sources = ("oai", "llama", "llama33", "chatgpt", "human")
    source_labeled_rows: dict[str, int] = {source: 0 for source in all_sources}
    source_labeled_proposals: dict[str, set[str]] = {source: set() for source in all_sources}
    source_labeled_by_proposal: dict[str, Counter[str]] = {source: Counter() for source in all_sources}
    source_kappa_pairs: dict[str, tuple[list[str], list[str]]] = {
        source: ([], []) for source in all_sources
    }

    choice_rows = len(rows)
    stance_values = [normalize_label(row.get("stance")) for row in rows]
    total_score_mass = sum(number_or_zero(row.get("scores_total")) for row in rows)
    total_vote_mass = sum(number_or_zero(row.get("n_votes")) for row in rows)
    flagged_proposals = 0
    single_preference_proposals = 0

    for row in rows:
        proposal = normalize_id(row.get("proposal"))
        if not proposal:
            continue
        proposal_rows[proposal] += 1
        if truthy(row.get("flagged")):
            flagged_proposals += 1
        if (row.get("type") or "").strip().lower() in SINGLE_PREFERENCE_TYPES:
            single_preference_proposals += 1

        for source_name, cols in coverage_sources.items():
            has_label = any(normalize_label(row.get(col)) for col in cols)
            if has_label:
                source_labeled_rows[source_name] += 1
                source_labeled_proposals[source_name].add(proposal)
                source_labeled_by_proposal[source_name][proposal] += 1

        for source_name, stance_col in {
            "oai": "stance_oai",
            "llama": "stance_llama",
            "llama33": "stance_llama33",
            "chatgpt": "stance_chatgpt",
            "human": "stance_hl",
        }.items():
            if stance_col not in fieldnames:
                continue
            left_values, right_values = source_kappa_pairs[source_name]
            left_values.append(normalize_label(row.get("stance")))
            right_values.append(normalize_label(row.get(stance_col)))

    coverage_rows: dict[str, dict[str, int]] = {}
    for source_name in all_sources:
        proposal_counts = source_labeled_by_proposal[source_name]
        total_rows_by_proposal = proposal_rows
        coverage_rows[source_name] = {
            "labeled_rows": source_labeled_rows[source_name],
            "labeled_proposals": len(source_labeled_proposals[source_name]),
            "complete_proposals": sum(
                1 for proposal, total in total_rows_by_proposal.items() if proposal_counts[proposal] == total
            ),
            "partial_proposals": sum(
                1
                for proposal, total in total_rows_by_proposal.items()
                if 0 < proposal_counts[proposal] < total
            ),
            "missing_proposals": sum(1 for proposal in total_rows_by_proposal if proposal_counts[proposal] == 0),
        }

    n_evaluations = int(sum(number_or_zero(row.get("n_evaluations")) for row in rows))
    n_non_low_effort_evaluations = int(sum(number_or_zero(row.get("n_non_low_effort_evaluations")) for row in rows))
    n_low_effort_evaluations = int(sum(number_or_zero(row.get("n_low_effort_evaluations")) for row in rows))
    low_effort_only_choices = sum(1 for row in rows if truthy(row.get("low_effort_only")))
    kappa = {
        source_name: format_float(cohen_kappa(*pairs))
        for source_name, pairs in source_kappa_pairs.items()
    }

    return {
        "candidate_choices": choice_rows,
        "candidate_proposals": len(proposal_rows),
        "candidate_retained_pct": 100,
        "candidate_score_mass": format_float(total_score_mass),
        "candidate_vote_mass": format_float(total_vote_mass),
        "candidate_flagged_proposals": flagged_proposals,
        "candidate_single_preference_proposals": single_preference_proposals,
        "coverage_rows": coverage_rows,
        "n_evaluations": n_evaluations,
        "n_non_low_effort_evaluations": n_non_low_effort_evaluations,
        "n_low_effort_evaluations": n_low_effort_evaluations,
        "low_effort_only_choices": low_effort_only_choices,
        "kappa": kappa,
    }


def int_or_zero(value: str | int | float | None) -> int:
    if value is None or value == "":
        return 0
    try:
        return int(float(value))
    except (TypeError, ValueError):
        return 0


def read_human_sample_stats(analysis_root: Path, *, public_data: bool = False) -> tuple[dict[str, str | int], dict[str, str]]:
    if public_data:
        rows = load_public_crowdsourcing(analysis_root)
    else:
        sample_path = analysis_root / HUMAN_LABEL_SAMPLE_PATH
        if not sample_path.exists():
            return {}, {}
        with sample_path.open("r", newline="", encoding="utf-8") as handle:
            rows = list(csv.DictReader(handle))

    proposal_language: dict[str, str] = {}
    proposal_choice_counts: dict[str, int] = {}
    choice_rows = 0
    for row in rows:
        proposal = normalize_id(row.get("proposal"))
        if not proposal:
            continue
        choice_rows += 1
        language = (row.get("body_lang") or row.get("title_lang") or "").strip().lower()
        proposal_language.setdefault(proposal, language)
        proposal_choice_counts.setdefault(proposal, int_or_zero(row.get("choices_len")))

    language_counts = Counter(proposal_language.values())
    values: dict[str, str | int] = {
        "evaluated_proposals": len(proposal_language),
        "evaluated_choices": choice_rows,
        "english_proposals": language_counts["en"],
        "chinese_proposals": language_counts["zh-cn"] + language_counts["zh"],
        "japanese_proposals": language_counts["ja"],
        "spanish_proposals": language_counts["es"],
        "max_choices_per_proposal": max(proposal_choice_counts.values(), default=0),
    }
    sources = {
        key: str(Path("public_data") / (LABELS_PATH if key == "evaluated_choices" else PROPOSALS_PATH))
        if public_data else str(HUMAN_LABEL_SAMPLE_PATH)
        for key in values
    }
    return values, sources


def read_human_reviewer_stats(analysis_root: Path) -> tuple[dict[str, str | int], dict[str, str]]:
    evaluator_path = analysis_root / HUMAN_LABEL_EVALUATOR_LABELS_PATH
    if not evaluator_path.exists():
        return {}, {}

    proposal_evaluators: dict[str, set[str]] = defaultdict(set)
    with evaluator_path.open("r", newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            proposal = normalize_id(row.get("proposal"))
            evaluator = normalize_id(row.get("evaluator_id"))
            if proposal and evaluator:
                proposal_evaluators[proposal].add(evaluator)

    reviewer_counts = [len(evaluators) for evaluators in proposal_evaluators.values()]
    values: dict[str, str | int] = {
        "proposal_review_min": min(reviewer_counts, default=0),
        "proposal_at_least_three_reviews": sum(1 for count in reviewer_counts if count >= 3),
    }
    sources = {key: str(HUMAN_LABEL_EVALUATOR_LABELS_PATH) for key in values}
    return values, sources


def read_human_event_stats(analysis_root: Path) -> tuple[dict[str, str | int], dict[str, str]]:
    rooms_dir = analysis_root / HUMAN_LABELS_DIR / "rooms"
    if not rooms_dir.exists():
        return {}, {}

    players_by_step: dict[str, set[str]] = defaultdict(set)
    evaluator_players: set[str] = set()
    total_time_by_player: dict[str, float] = defaultdict(float)
    for path in sorted(rooms_dir.glob("room*/news.ndjson")):
        with path.open("r", encoding="utf-8") as handle:
            for line in handle:
                if not line.strip():
                    continue
                event = json.loads(line)
                player = normalize_id(event.get("player"))
                if not player:
                    continue
                players_by_step[(event.get("stepId") or "").strip()].add(player)
                if event.get("news") is not None:
                    evaluator_players.add(player)
                total_time_by_player[player] += number_or_zero(event.get("time"))

    evaluator_minutes = {
        player: total_time_by_player[player] / 1000 / 60
        for player in evaluator_players
        if player in total_time_by_player
    }
    demographic_players = players_by_step["demo_initial"]
    demographic_minutes = [
        evaluator_minutes[player]
        for player in demographic_players
        if player in evaluator_minutes
    ]

    values: dict[str, str | int] = {
        "task_evaluators": len(players_by_step["loadnews"]),
        "demographic_evaluators": len(demographic_players),
        "low_effort_evaluators": sum(1 for minutes in demographic_minutes if minutes < 5),
        "average_task_minutes": format_float(statistics.mean(demographic_minutes), digits=2)
        if demographic_minutes
        else "0",
    }
    sources = {key: str(HUMAN_LABELS_DIR / "rooms") for key in values}
    return values, sources


def read_human_evaluation_stats(
    analysis_root: Path,
    label_stats: dict[str, object] | None,
    release_path: Path,
) -> dict[str, object] | None:
    if label_stats is None:
        return None

    values: dict[str, str | int] = {}
    sources: dict[str, str] = {}

    for read_stats in (
        read_human_sample_stats,
        read_human_reviewer_stats,
        read_human_event_stats,
    ):
        stat_values, stat_sources = read_stats(analysis_root)
        values.update(stat_values)
        sources.update(stat_sources)

    coverage_rows = label_stats["coverage_rows"]
    kappas = label_stats["kappa"]
    assert isinstance(coverage_rows, dict)
    assert isinstance(kappas, dict)
    human_coverage = coverage_rows["human"]
    assert isinstance(human_coverage, dict)

    release_source = release_path.as_posix()
    release_values: dict[str, str | int] = {
        "release_labeled_choices": human_coverage["labeled_rows"],
        "release_labeled_proposals": human_coverage["labeled_proposals"],
        "choice_evaluations": label_stats["n_evaluations"],
        "non_low_effort_choice_evaluations": label_stats["n_non_low_effort_evaluations"],
        "low_effort_choice_evaluations": label_stats["n_low_effort_evaluations"],
        "kappa": kappas["human"],
        "kappa_agreement": kappa_agreement_label(kappas["human"]),
    }
    values.update(release_values)
    sources.update({key: release_source for key in release_values})

    return {"values": values, "sources": sources}


def build_llm_agreement_stat_values(label_stats: dict[str, object], release_path: Path) -> list[StatValue]:
    rows: list[StatValue] = []
    kappas = label_stats["kappa"]
    assert isinstance(kappas, dict)

    for source_name in LLM_KAPPA_SOURCES:
        prefix = source_macro_prefix(source_name)
        label = paper_source_display_label(source_name)
        kappa_value = kappas[source_name]
        rows.extend(
            [
                StatValue(
                    f"DaoBiasLlmKappa{prefix}",
                    kappa_value,
                    f"Heuristic-vs-{label} Cohen kappa in analysis release",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasLlmKappa{prefix}Agreement",
                    kappa_agreement_label(kappa_value),
                    f"Agreement label for heuristic-vs-{label} Cohen kappa",
                    release_path.as_posix(),
                    "text",
                ),
            ]
        )

    highest_source = max(LLM_KAPPA_SOURCES, key=lambda source: float(kappas[source]))
    rows.append(
        StatValue(
            "DaoBiasLlmKappaHighestSource",
            paper_source_display_label(highest_source),
            "LLM source with the highest heuristic-vs-LLM Cohen kappa",
            release_path.as_posix(),
            "text",
        )
    )
    return rows


def build_human_evaluation_stat_values(human_stats: dict[str, object]) -> list[StatValue]:
    values = human_stats["values"]
    sources = human_stats["sources"]
    assert isinstance(values, dict)
    assert isinstance(sources, dict)

    specs = [
        ("evaluated_proposals", "DaoBiasHumanEvaluatedProposalN", "Evaluated proposals in the human-evaluation sample", "num"),
        ("evaluated_choices", "DaoBiasHumanEvaluatedChoiceN", "Choice rows in the human-evaluation sample", "num"),
        ("english_proposals", "DaoBiasHumanEnglishProposalN", "English proposals in the human-evaluation sample", "num"),
        ("chinese_proposals", "DaoBiasHumanChineseProposalN", "Chinese proposals in the human-evaluation sample", "num"),
        ("japanese_proposals", "DaoBiasHumanJapaneseProposalN", "Japanese proposals in the human-evaluation sample", "num"),
        ("spanish_proposals", "DaoBiasHumanSpanishProposalN", "Spanish proposals in the human-evaluation sample", "num"),
        ("max_choices_per_proposal", "DaoBiasHumanMaxChoiceN", "Maximum choices per evaluated human proposal", "num"),
        ("proposal_review_min", "DaoBiasHumanProposalReviewMinN", "Minimum proposal-level human reviews", "num"),
        (
            "proposal_at_least_three_reviews",
            "DaoBiasHumanProposalAtLeastThreeReviewN",
            "Human-evaluation proposals with at least three reviewers",
            "num",
        ),
        ("task_evaluators", "DaoBiasHumanEvaluatorN", "Human evaluators who reached proposal-labeling tasks", "num"),
        ("demographic_evaluators", "DaoBiasHumanDemographicEvaluatorN", "Human evaluators with demographic/feedback data", "num"),
        ("low_effort_evaluators", "DaoBiasHumanLowEffortEvaluatorN", "Low-effort human evaluators in demographic/feedback data", "num"),
        ("average_task_minutes", "DaoBiasHumanAverageTaskMinutes", "Average human-evaluation task duration in minutes", "num"),
        ("release_labeled_choices", "DaoBiasHumanLabeledChoiceN", "Human-labeled current-release choice rows", "num"),
        ("release_labeled_proposals", "DaoBiasHumanLabeledProposalN", "Human-labeled current-release proposals", "num"),
        ("choice_evaluations", "DaoBiasHumanChoiceEvaluationN", "Human choice-level evaluations in current release", "num"),
        (
            "non_low_effort_choice_evaluations",
            "DaoBiasHumanNonLowEffortChoiceEvaluationN",
            "Non-low-effort human choice-level evaluations in current release",
            "num",
        ),
        (
            "low_effort_choice_evaluations",
            "DaoBiasHumanLowEffortChoiceEvaluationN",
            "Low-effort human choice-level evaluations in current release",
            "num",
        ),
        ("kappa", "DaoBiasHumanKappa", "Heuristic-vs-human Cohen kappa in analysis release", "num"),
        ("kappa_agreement", "DaoBiasHumanKappaAgreement", "Agreement label for heuristic-vs-human Cohen kappa", "text"),
    ]

    rows = []
    for key, macro, description, latex_kind in specs:
        if key not in values:
            continue
        rows.append(StatValue(macro, values[key], description, sources[key], latex_kind))
    return rows


def build_suggested_stance_stat_values(
    diagnostics: suggested_stance_diagnostics.Diagnostics,
    release_path: Path,
) -> list[StatValue]:
    rows: list[StatValue] = []
    summaries = diagnostics.model_summary
    distributions = diagnostics.distributions
    source_no_suggestion_pct: list[tuple[str, float]] = []

    for source_name in AUTOMATED_SUGGESTED_SOURCES:
        summary = summaries.get(source_name)
        if not summary:
            continue
        denominator = summary["positive_suggested_stance_proposals"] + summary["no_suggestion_proposals"]
        if denominator <= 0:
            continue
        prefix = source_macro_prefix(source_name)
        label = paper_source_display_label(source_name)
        no_suggestion_pct = (summary["no_suggestion_proposals"] / denominator) * 100.0
        source_no_suggestion_pct.append((source_name, no_suggestion_pct))
        rows.append(
            StatValue(
                f"DaoBiasSuggestedNoSuggestionPct{prefix}",
                format_float(no_suggestion_pct, digits=1),
                f"No-suggestion share among reviewed, nonconflict {label} proposal-level outputs",
                release_path.as_posix(),
            )
        )

    if source_no_suggestion_pct:
        min_source, min_pct = min(source_no_suggestion_pct, key=lambda item: item[1])
        max_source, max_pct = max(source_no_suggestion_pct, key=lambda item: item[1])
        rows.extend(
            [
                StatValue(
                    "DaoBiasSuggestedNoSuggestionPctMin",
                    format_float(min_pct, digits=1),
                    "Minimum no-suggestion share across automated suggested-stance sources",
                    release_path.as_posix(),
                ),
                StatValue(
                    "DaoBiasSuggestedNoSuggestionPctMinSource",
                    paper_source_display_label(min_source),
                    "Automated source with the minimum no-suggestion share",
                    release_path.as_posix(),
                    "text",
                ),
                StatValue(
                    "DaoBiasSuggestedNoSuggestionPctMax",
                    format_float(max_pct, digits=1),
                    "Maximum no-suggestion share across automated suggested-stance sources",
                    release_path.as_posix(),
                ),
                StatValue(
                    "DaoBiasSuggestedNoSuggestionPctMaxSource",
                    paper_source_display_label(max_source),
                    "Automated source with the maximum no-suggestion share",
                    release_path.as_posix(),
                    "text",
                ),
            ]
        )

    llama33_summary = summaries.get("llama33")
    if llama33_summary:
        for macro_suffix, key, description in (
            ("ReviewedProposalN", "reviewed_proposals", "Llama 3.3 reviewed proposals"),
            ("NotReviewedProposalN", "not_reviewed_proposals", "Llama 3.3 not-reviewed proposals"),
            (
                "BasicExclusionProposalN",
                "basic_exclusion_proposals",
                "Llama 3.3 Basic-proposal coverage exclusions",
            ),
            ("ConflictProposalN", "conflict_proposals", "Llama 3.3 proposal-level label conflicts"),
            ("NoSuggestionProposalN", "no_suggestion_proposals", "Llama 3.3 no-suggestion proposals"),
            (
                "PositiveProposalN",
                "positive_suggested_stance_proposals",
                "Llama 3.3 positive suggested-stance proposals",
            ),
        ):
            rows.append(
                StatValue(
                    f"DaoBiasSuggestedLlamaThreePointThree{macro_suffix}",
                    llama33_summary[key],
                    description,
                    release_path.as_posix(),
                )
            )

    approve_positive_shares = []
    for source_name in AUTOMATED_SUGGESTED_SOURCES:
        summary = summaries.get(source_name)
        distribution = distributions.get(source_name, {})
        if not summary or summary["positive_suggested_stance_proposals"] <= 0:
            continue
        approve_positive_shares.append(
            (
                source_name,
                (distribution.get(STANCE_APPROVE, 0) / summary["positive_suggested_stance_proposals"]) * 100.0,
            )
        )
    if approve_positive_shares:
        min_source, min_pct = min(approve_positive_shares, key=lambda item: item[1])
        rows.extend(
            [
                StatValue(
                    "DaoBiasSuggestedApprovePositivePctMin",
                    format_float(min_pct, digits=1),
                    "Minimum Approve share among positive suggested stances across automated sources",
                    release_path.as_posix(),
                ),
                StatValue(
                    "DaoBiasSuggestedApprovePositivePctMinSource",
                    paper_source_display_label(min_source),
                    "Automated source with the minimum Approve share among positive suggested stances",
                    release_path.as_posix(),
                    "text",
                ),
            ]
        )

    approve_metrics = [
        row
        for row in diagnostics.metrics
        if row["model_key"] in AUTOMATED_SUGGESTED_SOURCES and row["stance"] == STANCE_APPROVE
    ]
    metric_specs = (
        ("precision", "Precision", "precision"),
        ("recall", "Recall", "recall"),
        ("f1", "FOne", "F1"),
    )
    for field, macro_label, prose_label in metric_specs:
        values = [float(row[field]) for row in approve_metrics]
        if not values:
            continue
        rows.extend(
            [
                StatValue(
                    f"DaoBiasSuggestedApprove{macro_label}Min",
                    format_float(min(values), digits=2),
                    f"Minimum Approve {prose_label} across automated suggested-stance sources",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasSuggestedApprove{macro_label}Max",
                    format_float(max(values), digits=2),
                    f"Maximum Approve {prose_label} across automated suggested-stance sources",
                    release_path.as_posix(),
                ),
            ]
        )

    return rows


def build_candidate_release_stat_values(candidate_stats: dict[str, object], release_path: Path) -> list[StatValue]:
    rows = [
        StatValue(
            "DaoBiasCandidateProposalN",
            candidate_stats["candidate_proposals"],
            "Unique proposals in candidate release",
            release_path.as_posix(),
        ),
        StatValue(
            "DaoBiasCandidateChoiceN",
            candidate_stats["candidate_choices"],
            "Choice rows in candidate release",
            release_path.as_posix(),
        ),
        StatValue(
            "DaoBiasCandidateRetainedPct",
            candidate_stats["candidate_retained_pct"],
            "Share of stage-5 candidate rows retained in candidate release",
            release_path.as_posix(),
        ),
    ]

    for source_name in ("oai", "llama", "llama33", "chatgpt", "human"):
        prefix = source_macro_prefix(source_name)
        label = source_display_label(source_name)
        coverage = candidate_stats["coverage_rows"][source_name]
        rows.extend(
            [
                StatValue(
                    f"DaoBiasCandidate{prefix}LabeledChoiceN",
                    coverage["labeled_rows"],
                    f"{label} labeled choice rows in candidate release",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasCandidate{prefix}LabeledProposalN",
                    coverage["labeled_proposals"],
                    f"{label} labeled proposals in candidate release",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasCandidate{prefix}CompleteProposalN",
                    coverage["complete_proposals"],
                    f"{label} complete proposals in candidate release",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasCandidate{prefix}PartialProposalN",
                    coverage["partial_proposals"],
                    f"{label} partial proposals in candidate release",
                    release_path.as_posix(),
                ),
                StatValue(
                    f"DaoBiasCandidate{prefix}MissingProposalN",
                    coverage["missing_proposals"],
                    f"{label} missing proposals in candidate release",
                    release_path.as_posix(),
                ),
            ]
        )

    rows.extend(
        [
            StatValue(
                "DaoBiasCandidateHumanChoiceN",
                candidate_stats["coverage_rows"]["human"]["labeled_rows"],
                "Human-labeled choice rows in candidate release",
                release_path.as_posix(),
            ),
            StatValue(
                "DaoBiasCandidateHumanProposalN",
                candidate_stats["coverage_rows"]["human"]["labeled_proposals"],
                "Human-labeled proposals in candidate release",
                release_path.as_posix(),
            ),
            StatValue(
                "DaoBiasCandidateHumanEvaluatorN",
                candidate_stats["n_evaluations"],
                "Human evaluator labels in candidate release",
                release_path.as_posix(),
            ),
            StatValue(
                "DaoBiasCandidateHumanNonLowEffortEvaluatorN",
                candidate_stats["n_non_low_effort_evaluations"],
                "Non-low-effort human evaluator labels in candidate release",
                release_path.as_posix(),
            ),
            StatValue(
                "DaoBiasCandidateHumanLowEffortEvaluatorN",
                candidate_stats["n_low_effort_evaluations"],
                "Low-effort human evaluator labels in candidate release",
                release_path.as_posix(),
            ),
            StatValue(
                "DaoBiasCandidateHumanLowEffortOnlyChoiceN",
                candidate_stats["low_effort_only_choices"],
                "Human-labeled choices with only low-effort evaluations in candidate release",
                release_path.as_posix(),
            ),
        ]
    )

    for source_name, kappa_value in candidate_stats["kappa"].items():
        prefix = source_macro_prefix(source_name)
        label = source_display_label(source_name)
        rows.append(
            StatValue(
                f"DaoBiasCandidateKappa{prefix}",
                kappa_value,
                f"Stance vs {label} Cohen kappa in candidate release",
                release_path.as_posix(),
            )
        )

    return rows


def build_stat_values(
    *,
    release: str,
    release_path: Path,
    raw_stats: dict[str, int],
    raw_vote_count: int,
    release_stats: dict[str, object],
    issue_stats: dict[str, int],
    stage3_stats: dict[str, str],
    run_summary_stats: dict[str, str],
    label_stats: dict[str, object] | None = None,
    human_eval_stats: dict[str, object] | None = None,
    suggested_stance_stats: suggested_stance_diagnostics.Diagnostics | None = None,
    candidate_stats: dict[str, object] | None = None,
) -> list[StatValue]:
    choice_summary = release_stats["choice_per_proposal"]
    space_summary = release_stats["proposal_per_space"]
    assert isinstance(choice_summary, dict)
    assert isinstance(space_summary, dict)
    excluded_snapshot_flagged_pct = pct(
        release_stats["excluded_snapshot_flagged_proposals"],
        raw_stats["raw_snapshot_flagged_proposals"],
    )
    proposal_and_release_source = f"{RAW_PROPOSALS_PATH}; {release_path.as_posix()}"
    vote_and_release_source = f"{RAW_VOTES_DIR}; {release_path.as_posix()}"

    rows = [
        StatValue("DaoBiasReleaseName", release, "Release alias used for generated paper counts", str(RELEASE_REGISTRY), "text"),
        StatValue(
            "DaoBiasReleasePath",
            release_path.as_posix(),
            "Canonical analysis release path",
            str(RELEASE_REGISTRY),
            "text",
        ),
        StatValue("DaoBiasRawProposalN", raw_stats["raw_proposals"], "Raw Snapshot proposal rows", str(RAW_PROPOSALS_PATH)),
        StatValue(
            "DaoBiasRawSnapshotFlaggedProposalN",
            raw_stats["raw_snapshot_flagged_proposals"],
            "Raw Snapshot proposals marked by Snapshot's flagged field",
            str(RAW_PROPOSALS_PATH),
        ),
        StatValue("DaoBiasRawSpaceN", raw_stats["raw_spaces"], "Raw Snapshot spaces in proposal data", str(RAW_PROPOSALS_PATH)),
        StatValue("DaoBiasRawChoiceN", raw_stats["raw_choices"], "Raw Snapshot choices parsed from proposal choice arrays", str(RAW_PROPOSALS_PATH)),
        StatValue("DaoBiasRawVoteN", raw_vote_count, "Raw Snapshot vote rows", str(RAW_VOTES_DIR)),
        StatValue("DaoBiasRawVoteMillionN", format_float(raw_vote_count / 1_000_000), "Raw Snapshot vote rows in millions", str(RAW_VOTES_DIR)),
        StatValue("DaoBiasFinalProposalN", release_stats["final_proposals"], "Unique proposals in current release", release_path.as_posix()),
        StatValue("DaoBiasFinalSpaceN", release_stats["final_spaces"], "Unique spaces in current release", release_path.as_posix()),
        StatValue("DaoBiasFinalChoiceN", release_stats["final_choices"], "Choice rows in current release", release_path.as_posix()),
        StatValue("DaoBiasFinalVoteMassN", release_stats["final_vote_mass"], "Favorite-mode ballot mass in current release", release_path.as_posix()),
        StatValue(
            "DaoBiasDatasetSpaceRemovedN",
            reduction_count(raw_stats["raw_spaces"], release_stats["final_spaces"]),
            "Raw Snapshot spaces not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetSpaceReductionPct",
            reduction_pct(raw_stats["raw_spaces"], release_stats["final_spaces"]),
            "Share of raw Snapshot spaces not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetProposalRemovedN",
            reduction_count(raw_stats["raw_proposals"], release_stats["final_proposals"]),
            "Raw Snapshot proposals not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetProposalReductionPct",
            reduction_pct(raw_stats["raw_proposals"], release_stats["final_proposals"]),
            "Share of raw Snapshot proposals not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetChoiceRemovedN",
            reduction_count(raw_stats["raw_choices"], release_stats["final_choices"]),
            "Raw Snapshot choices not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetChoiceReductionPct",
            reduction_pct(raw_stats["raw_choices"], release_stats["final_choices"]),
            "Share of raw Snapshot choices not retained in the paper-facing current release",
            proposal_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetVoteRemovedN",
            reduction_count(raw_vote_count, release_stats["final_vote_mass"]),
            "Raw Snapshot vote rows not retained as favorite-mode ballot mass in the paper-facing current release",
            vote_and_release_source,
        ),
        StatValue(
            "DaoBiasDatasetVoteReductionPct",
            reduction_pct(raw_vote_count, release_stats["final_vote_mass"]),
            "Share of raw Snapshot vote rows not retained as favorite-mode ballot mass in the paper-facing current release",
            vote_and_release_source,
        ),
        StatValue("DaoBiasFinalScoreMassN", release_stats["final_score_mass"], "Snapshot voting-power score mass in current release", release_path.as_posix()),
        StatValue("DaoBiasFinalFlaggedProposalN", release_stats["final_flagged_proposals"], "Snapshot-flagged proposals retained after the paper-facing exclusion", release_path.as_posix()),
        StatValue("DaoBiasFinalFlaggedProposalPct", release_stats["final_flagged_proposal_pct"], "Share of paper-facing current-release proposals flagged by Snapshot", release_path.as_posix()),
        StatValue(
            "DaoBiasExcludedSnapshotFlaggedProposalN",
            release_stats["excluded_snapshot_flagged_proposals"],
            "Snapshot-flagged current-release proposals excluded from paper-facing counts",
            release_path.as_posix(),
        ),
        StatValue(
            "DaoBiasExcludedSnapshotFlaggedProposalPct",
            excluded_snapshot_flagged_pct,
            "Share of all raw Snapshot-flagged proposals excluded from paper-facing current-release counts",
            str(RAW_PROPOSALS_PATH),
        ),
        StatValue(
            "DaoBiasExcludedSingleOptionProposalN",
            release_stats["excluded_single_option_proposals"],
            "Single-option current-release proposals excluded from paper-facing analysis counts",
            release_path.as_posix(),
        ),
        StatValue(
            "DaoBiasExcludedSingleOptionChoiceN",
            release_stats["excluded_single_option_choices"],
            "Choice rows in single-option current-release proposals excluded from paper-facing analysis counts",
            release_path.as_posix(),
        ),
        StatValue("DaoBiasSinglePreferenceProposalN", release_stats["single_preference_proposals"], "Current-release proposals using basic or single-choice voting", release_path.as_posix()),
        StatValue("DaoBiasSinglePreferenceProposalPct", release_stats["single_preference_proposal_pct"], "Share of current-release proposals using basic or single-choice voting", release_path.as_posix()),
        StatValue("DaoBiasSinglePreferenceChoiceN", release_stats["single_preference_choices"], "Current-release choice rows in basic or single-choice proposals", release_path.as_posix()),
        StatValue("DaoBiasSinglePreferenceChoicePct", release_stats["single_preference_choice_pct"], "Share of current-release choice rows in basic or single-choice proposals", release_path.as_posix()),
        StatValue("DaoBiasMultiPreferenceProposalN", release_stats["multi_preference_proposals"], "Current-release proposals using weighted, quadratic, ranked-choice, or approval voting", release_path.as_posix()),
        StatValue("DaoBiasMultiPreferenceProposalPct", release_stats["multi_preference_proposal_pct"], "Share of current-release proposals using multi-preference voting systems", release_path.as_posix()),
        StatValue("DaoBiasMultiPreferenceChoiceN", release_stats["multi_preference_choices"], "Current-release choice rows in multi-preference voting proposals", release_path.as_posix()),
        StatValue("DaoBiasMultiPreferenceChoicePct", release_stats["multi_preference_choice_pct"], "Share of current-release choice rows in multi-preference voting proposals", release_path.as_posix()),
        *[
            StatValue(
                f"DaoBiasVotingType{macro_suffix}{kind}",
                release_stats[stat_key][type_key],
                description,
                release_path.as_posix(),
            )
            for type_key, macro_suffix in VOTING_TYPES
            for kind, stat_key, description in (
                ("ProposalN", "voting_type_proposals", f"Current-release proposals using {type_key} voting"),
                ("ProposalPct", "voting_type_proposal_pct", f"Share of current-release proposals using {type_key} voting"),
                ("ChoiceN", "voting_type_choices", f"Current-release choice rows in {type_key} proposals"),
                ("ChoicePct", "voting_type_choice_pct", f"Share of current-release choice rows in {type_key} proposals"),
            )
        ],
        StatValue("DaoBiasFinalChoicePerProposalMean", choice_summary["mean"], "Mean choice rows per current-release proposal", release_path.as_posix()),
        StatValue("DaoBiasFinalChoicePerProposalStd", choice_summary["std"], "Standard deviation of choice rows per current-release proposal", release_path.as_posix()),
        StatValue("DaoBiasFinalChoicePerProposalMedian", choice_summary["median"], "Median choice rows per current-release proposal", release_path.as_posix()),
        StatValue("DaoBiasFinalProposalPerSpaceMean", space_summary["mean"], "Mean current-release proposals per space", release_path.as_posix()),
        StatValue("DaoBiasFinalProposalPerSpaceStd", space_summary["std"], "Standard deviation of current-release proposals per space", release_path.as_posix()),
        StatValue("DaoBiasFinalProposalPerSpaceMedian", space_summary["median"], "Median current-release proposals per space", release_path.as_posix()),
        StatValue("DaoBiasVoteIssueProposalN", issue_stats["vote_issue_proposals"], "Raw proposals with duplicate-ballot or invalid-vote issue flags", str(PROPOSAL_ISSUE_FLAGS_PATH)),
        StatValue("DaoBiasInvalidVoteIssueProposalN", issue_stats["invalid_vote_issue_proposals"], "Raw proposals with invalid-vote issue flags", str(PROPOSAL_ISSUE_FLAGS_PATH)),
        StatValue("DaoBiasDuplicateBallotIssueProposalN", issue_stats["duplicate_ballot_issue_proposals"], "Raw proposals with duplicate-ballot issue flags", str(PROPOSAL_ISSUE_FLAGS_PATH)),
        StatValue("DaoBiasFinalVoteIssueProposalN", issue_stats["final_vote_issue_proposals"], "Current-release proposals with vote issue flags", str(PROPOSAL_ISSUE_FLAGS_PATH)),
        StatValue("DaoBiasStageThreeLoadedFinalProposalN", stage3_stats["stage3_loaded_final_proposals"], "Stage-3 audit final proposals loaded before vote merge", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasStageThreeExclusionProposalN", stage3_stats["stage3_exclusion_proposals"], "Stage-3 audit proposals removed by stage exclusions", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasStageThreeInvalidChoiceRowsRemovedN", stage3_stats["stage3_invalid_choice_rows_removed"], "Stage-3 audit rows removed for invalid choice index", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasStageThreeInvalidChoiceVoteMassRemovedN", stage3_stats["stage3_invalid_choice_vote_mass_removed"], "Stage-3 audit vote mass removed for invalid choice index", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasStageThreeFinalChoiceRowN", stage3_stats["stage3_final_choice_rows"], "Stage-3 audit final choice rows", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasStageThreeFinalProposalN", stage3_stats["stage3_final_proposals"], "Stage-3 audit final proposals", str(STAGE3_AUDIT_PATH)),
        StatValue("DaoBiasVoteExpansionFinishedProposalN", run_summary_stats["vote_expansion_finished_proposals"], "Finished proposals in vote-expansion run", str(VOTE_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasVoteExpansionFavoriteRowN", run_summary_stats["vote_expansion_favorite_rows"], "Favorite-mode rows in vote-expansion run", str(VOTE_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasVoteExpansionFavoriteErrorRowN", run_summary_stats["vote_expansion_favorite_error_rows"], "Favorite-mode parse error rows in vote-expansion run", str(VOTE_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasVoteExpansionSupportRowN", run_summary_stats["vote_expansion_support_rows"], "Support-fraction rows in vote-expansion run", str(VOTE_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasVoteExpansionSupportErrorRowN", run_summary_stats["vote_expansion_support_error_rows"], "Support-fraction parse error rows in vote-expansion run", str(VOTE_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasAuthorVoteFinishedProposalN", run_summary_stats["author_vote_finished_proposals"], "Finished proposals in author-vote run", str(AUTHOR_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasAuthorVoteFavoriteRowN", run_summary_stats["author_vote_favorite_rows"], "Favorite-mode rows in author-vote run", str(AUTHOR_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasAuthorVoteFavoriteErrorRowN", run_summary_stats["author_vote_favorite_error_rows"], "Favorite-mode parse error rows in author-vote run", str(AUTHOR_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasAuthorVoteSupportRowN", run_summary_stats["author_vote_support_rows"], "Support-fraction rows in author-vote run", str(AUTHOR_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasAuthorVoteSupportErrorRowN", run_summary_stats["author_vote_support_error_rows"], "Support-fraction parse error rows in author-vote run", str(AUTHOR_RUN_SUMMARY_PATH)),
        StatValue("DaoBiasStanceApproveOnlyProposalN", release_stats["approve_only_proposals"], "Current-release proposals with Approve labels but no Reject labels", release_path.as_posix()),
        StatValue("DaoBiasStanceRejectOnlyProposalN", release_stats["reject_only_proposals"], "Current-release proposals with Reject labels but no Approve labels", release_path.as_posix()),
        StatValue("DaoBiasStanceImbalanceProposalN", release_stats["stance_imbalance_proposals"], "Current-release proposals with one-sided approve/reject stance labels", release_path.as_posix()),
        StatValue("DaoBiasStanceImbalanceDocumentedProposalN", release_stats["stance_imbalance_documented_proposals"], "One-sided stance-label proposals documented in stance metadata", str(STANCE_ONE_SIDED_EXCEPTIONS_PATH)),
        StatValue("DaoBiasStanceImbalanceUndocumentedProposalN", release_stats["stance_imbalance_undocumented_proposals"], "One-sided stance-label proposals not documented in stance metadata", str(STANCE_ONE_SIDED_EXCEPTIONS_PATH)),
    ]
    if label_stats is not None:
        rows.extend(build_llm_agreement_stat_values(label_stats, release_path))
    if human_eval_stats is not None:
        rows.extend(build_human_evaluation_stat_values(human_eval_stats))
    if suggested_stance_stats is not None:
        rows.extend(build_suggested_stance_stat_values(suggested_stance_stats, release_path))
    if release == "candidate":
        candidate_stats = candidate_stats or label_stats or {}
        rows.extend(build_candidate_release_stat_values(candidate_stats, release_path))
    return rows


def render_macro_file(rows: list[StatValue], generated_at: str) -> str:
    lines = [
        "% Generated by dao_bias.reporting.counts.",
        f"% Generated at {generated_at}.",
        "% Do not edit by hand; rerun the generator after changing the analysis release.",
        "",
    ]
    for row in rows:
        if not LATEX_MACRO_NAME_RE.fullmatch(row.macro):
            raise ValueError(
                f"Invalid LaTeX macro name {row.macro!r}; command names must contain letters only."
            )
        lines.append(f"\\newcommand{{\\{row.macro}}}{{{row.latex_value}}}")
    lines.append("")
    return "\n".join(lines)


def write_outputs(
    *,
    analysis_root: Path,
    paper_root: Path | None,
    release: str,
    release_path: Path,
    rows: list[StatValue],
    release_stats: dict[str, object],
    write_macro_output: bool = True,
) -> None:
    generated_at = datetime.now(timezone.utc).isoformat()

    if write_macro_output:
        if paper_root is None:
            raise ValueError("paper_root is required when write_macro_output is True")
        macro_path = paper_root / MACRO_RELATIVE_PATH
        macro_path.parent.mkdir(parents=True, exist_ok=True)
        macro_path.write_text(render_macro_file(rows, generated_at), encoding="utf-8")

    selected = os.environ.get("DAO_BIAS_DATA_ROOT")
    output = os.environ.get("DAO_BIAS_OUTPUT_ROOT")
    output_root = Path(output) if selected and output and Path(selected).resolve() == analysis_root.resolve() else analysis_root
    provenance_dir = output_root / PROVENANCE_DIR
    provenance_dir.mkdir(parents=True, exist_ok=True)
    csv_path = provenance_dir / f"latex_counts_{release}.csv"
    json_path = provenance_dir / f"latex_counts_{release}.json"

    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=["macro", "value", "latex_value", "description", "source"],
            lineterminator="\n",
        )
        writer.writeheader()
        for row in rows:
            writer.writerow(
                {
                    "macro": row.macro,
                    "value": row.value,
                    "latex_value": row.latex_value,
                    "description": row.description,
                    "source": row.source,
                }
            )

    payload = {
        "generated_at": generated_at,
        "analysis_root": analysis_root.as_posix(),
        "paper_root": paper_root.as_posix() if paper_root is not None else None,
        "release": release,
        "release_path": release_path.as_posix(),
        "counts": {
            row.macro: {
                "value": row.value,
                "latex_value": row.latex_value,
                "description": row.description,
                "source": row.source,
            }
            for row in rows
        },
        "diagnostics": {
            "stance_imbalance_examples": release_stats.get("stance_imbalance_examples", []),
        },
    }
    json_path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")


def generate_latex_counts(
    *,
    release: str,
    paper_root: Path | None = None,
    analysis_root: Path = ANALYSIS_ROOT,
    write_macro_output: bool = True,
) -> list[StatValue]:
    if write_macro_output and paper_root is None:
        raise ValueError("paper_root is required when write_macro_output is True")
    release_path = resolve_release_path(analysis_root, release)
    documented_stance_proposals = read_documented_stance_proposals(analysis_root)
    raw_stats = count_raw_proposals(analysis_root / RAW_PROPOSALS_PATH)
    raw_vote_count = count_raw_votes(analysis_root / RAW_VOTES_DIR)
    release_stats = read_release_stats(release_path, documented_stance_proposals)
    label_stats = read_candidate_release_stats(release_path) if release_has_label_stats(release_path) else None
    human_eval_stats = read_human_evaluation_stats(analysis_root, label_stats, release_path)
    suggested_stance_stats = (
        suggested_stance_diagnostics.build_diagnostics(analysis_root, release)
        if release_has_suggested_stance_diagnostics(release_path)
        else None
    )
    candidate_stats = label_stats if release == "candidate" else None
    issue_stats = read_issue_flag_stats(
        analysis_root / PROPOSAL_ISSUE_FLAGS_PATH,
        release_stats["proposal_ids"],
    )
    stage3_stats = read_stage3_audit(analysis_root / STAGE3_AUDIT_PATH)
    run_summary_stats = read_run_summary_stats(analysis_root)
    rows = build_stat_values(
        release=release,
        release_path=release_path,
        raw_stats=raw_stats,
        raw_vote_count=raw_vote_count,
        release_stats=release_stats,
        issue_stats=issue_stats,
        stage3_stats=stage3_stats,
        run_summary_stats=run_summary_stats,
        label_stats=label_stats,
        human_eval_stats=human_eval_stats,
        suggested_stance_stats=suggested_stance_stats,
        candidate_stats=candidate_stats,
    )
    write_outputs(
        analysis_root=analysis_root,
        paper_root=paper_root,
        release=release,
        release_path=release_path,
        rows=rows,
        release_stats=release_stats,
        write_macro_output=write_macro_output,
    )
    return rows


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Write generated LaTeX macros for analysis data counts.")
    parser.add_argument("--public-data", action="store_true", help="Summarize only the shared crowdsourcing sample; do not generate full paper counts.")
    parser.add_argument("--output-json", type=Path, help="Required in public mode; new JSON output for crowdsourcing sample counts.")
    parser.add_argument("--release", default="current", help="Release alias in metadata/analysis_data_releases.csv.")
    parser.add_argument(
        "--paper-root",
        type=Path,
        help="Path to the paper repository root where sections/macros/analysis_counts.tex is written.",
    )
    parser.add_argument(
        "--analysis-root",
        type=Path,
        default=ANALYSIS_ROOT,
        help="Path to the analysis repository root. Defaults to the package root.",
    )
    parser.add_argument(
        "--skip-macro-output",
        action="store_true",
        help="Write provenance outputs only and skip sections/macros/analysis_counts.tex.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.public_data:
        if args.output_json is None or args.paper_root is not None or args.release != "current":
            raise SystemExit("Public mode requires --output-json and does not support --paper-root or other releases.")
        try:
            values, sources = read_human_sample_stats(args.analysis_root.resolve(), public_data=True)
        except (FileNotFoundError, ValueError, csv.Error) as error:
            raise SystemExit(str(error)) from None
        args.output_json.parent.mkdir(parents=True, exist_ok=True)
        try:
            with args.output_json.open("x", encoding="utf-8") as handle:
                json.dump({"data_mode": "public", "sample": values, "sources": sources}, handle, indent=2)
                handle.write("\n")
        except FileExistsError:
            raise SystemExit(
                f"Output already exists: {args.output_json}; choose a new --output-root with "
                "run.py human-summary (or --output-json when invoking dao_bias.reporting.counts directly)."
            ) from None
        print(f"Wrote crowdsourcing sample counts to {args.output_json}; full paper counts require private data.")
        return
    if args.output_json is not None:
        raise SystemExit("--output-json is only supported with --public-data.")
    if not args.skip_macro_output and args.paper_root is None:
        raise SystemExit("--paper-root is required unless --skip-macro-output is set.")
    paper_root = args.paper_root.resolve() if args.paper_root is not None else None
    try:
        rows = generate_latex_counts(
            release=args.release,
            paper_root=paper_root,
            analysis_root=args.analysis_root.resolve(),
            write_macro_output=not args.skip_macro_output,
        )
    except FileNotFoundError as error:
        raise SystemExit(
            f"Input unavailable: {error}\nPrivate mode requires original analysis inputs. "
            "Use --public-data --output-json PATH for the published crowdsourcing summary."
        ) from None
    if args.skip_macro_output:
        print(f"Wrote {len(rows)} analysis-count provenance rows for release {args.release}")
    else:
        macro_path = paper_root / MACRO_RELATIVE_PATH
        print(f"Wrote {len(rows)} LaTeX analysis-count macros to {macro_path}")


if __name__ == "__main__":
    main()
