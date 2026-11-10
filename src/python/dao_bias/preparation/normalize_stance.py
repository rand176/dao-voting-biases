from __future__ import annotations

import argparse
import csv
from collections import Counter, defaultdict
from pathlib import Path


STANCE_APPROVE = "APPROVE"
STANCE_REJECT = "REJECT"
STANCE_ABSTAIN = "ABSTAIN"
STANCE_OTHER = "OTHER"
KNOWN_STANCES = {STANCE_APPROVE, STANCE_REJECT, STANCE_ABSTAIN, STANCE_OTHER}
ALLOWED_EXCEPTION_STATES = {"approve_only", "reject_only"}


def normalize_id(value: str | None) -> str:
    return (value or "").strip().lower()


def normalize_stance(value: str | None) -> str:
    stance = (value or "").strip().upper()
    return stance if stance in KNOWN_STANCES else STANCE_OTHER


def proposal_state(stances: set[str]) -> str:
    has_approve = STANCE_APPROVE in stances
    has_reject = STANCE_REJECT in stances
    if has_approve and has_reject:
        return "both"
    if has_approve:
        return "approve_only"
    if has_reject:
        return "reject_only"
    return "neither"


def load_one_sided_exceptions(path: Path, stage: str) -> dict[str, set[str]]:
    exceptions: dict[str, set[str]] = defaultdict(set)
    if not path.is_file():
        raise FileNotFoundError(f"Required curated exceptions file is missing: {path}")

    with path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        required = {"stage", "proposal", "allowed_state"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise ValueError(f"Missing exception column(s) in {path}: {', '.join(sorted(missing))}")
        for row in reader:
            if (row.get("stage") or "").strip() != stage:
                continue
            proposal = normalize_id(row.get("proposal"))
            allowed_state = (row.get("allowed_state") or "").strip().lower()
            if not proposal or not allowed_state:
                continue
            if allowed_state not in ALLOWED_EXCEPTION_STATES:
                raise ValueError(f"Unknown one-sided stance exception state {allowed_state!r} in {path}")
            exceptions[proposal].add(allowed_state)
    return exceptions


def collect_proposal_states(input_path: Path) -> tuple[dict[str, str], dict[str, Counter], int]:
    proposal_stances: dict[str, set[str]] = defaultdict(set)
    proposal_counts: dict[str, Counter] = defaultdict(Counter)
    row_count = 0

    with input_path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        required = {"proposal", "stance"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise ValueError(f"Missing release column(s) in {input_path}: {', '.join(sorted(missing))}")
        for row in reader:
            row_count += 1
            proposal = normalize_id(row.get("proposal"))
            stance = normalize_stance(row.get("stance"))
            if not proposal:
                continue
            proposal_stances[proposal].add(stance)
            proposal_counts[proposal]["rows"] += 1
            proposal_counts[proposal][stance.lower()] += 1

    states = {proposal: proposal_state(stances) for proposal, stances in proposal_stances.items()}
    return states, proposal_counts, row_count


def write_audit(
    audit_path: Path,
    states: dict[str, str],
    proposal_counts: dict[str, Counter],
    documented_exceptions: dict[str, set[str]],
) -> dict[str, set[str]]:
    audit_path.parent.mkdir(parents=True, exist_ok=True)
    one_sided = {
        proposal: state
        for proposal, state in states.items()
        if state in ALLOWED_EXCEPTION_STATES
    }
    documented = {
        proposal
        for proposal, state in one_sided.items()
        if state in documented_exceptions.get(proposal, set())
    }
    normalized = set(one_sided) - documented

    fieldnames = [
        "proposal",
        "original_stance_state",
        "action",
        "rows",
        "approve_rows",
        "reject_rows",
        "abstain_rows",
        "other_rows",
    ]
    with audit_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for proposal in sorted(one_sided):
            counts = proposal_counts[proposal]
            action = "kept_documented_exception" if proposal in documented else "normalized_to_other"
            writer.writerow(
                {
                    "proposal": proposal,
                    "original_stance_state": one_sided[proposal],
                    "action": action,
                    "rows": counts["rows"],
                    "approve_rows": counts["approve"],
                    "reject_rows": counts["reject"],
                    "abstain_rows": counts["abstain"],
                    "other_rows": counts["other"],
                }
            )

    return {
        "one_sided": set(one_sided),
        "documented": documented,
        "normalized": normalized,
    }


def normalize_row_to_other(row: dict[str, str]) -> None:
    row["stance"] = STANCE_OTHER
    for col, value in {
        "is_infavor": "0",
        "is_against": "0",
        "is_abstain": "0",
        "is_other": "1",
        "author_multi_approve": "0",
    }.items():
        if col in row:
            row[col] = value

    for col in ("author_choice_stance", "author_choice_stance_compact"):
        if col in row and row[col].strip():
            row[col] = STANCE_OTHER


def write_normalized_release(input_path: Path, output_path: Path, normalized_proposals: set[str]) -> int:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    normalized_rows = 0
    with input_path.open("r", newline="", encoding="utf-8") as source, output_path.open(
        "w", newline="", encoding="utf-8"
    ) as target:
        reader = csv.DictReader(source)
        if reader.fieldnames is None:
            raise ValueError(f"Missing header in {input_path}")
        writer = csv.DictWriter(target, fieldnames=reader.fieldnames)
        writer.writeheader()
        for row in reader:
            if normalize_id(row.get("proposal")) in normalized_proposals:
                normalize_row_to_other(row)
                normalized_rows += 1
            writer.writerow(row)
    return normalized_rows


def normalize_release_stance(
    *,
    input_path: Path,
    output_path: Path,
    exceptions_path: Path,
    audit_path: Path,
    stage: str,
) -> dict[str, int]:
    documented_exceptions = load_one_sided_exceptions(exceptions_path, stage)
    states, proposal_counts, row_count = collect_proposal_states(input_path)
    for path in (audit_path, output_path):
        if path.exists():
            print(f"Overwriting existing normalization output: {path}")
    audit_sets = write_audit(audit_path, states, proposal_counts, documented_exceptions)
    normalized_rows = write_normalized_release(input_path, output_path, audit_sets["normalized"])
    return {
        "input_rows": row_count,
        "output_rows": row_count,
        "one_sided_proposals": len(audit_sets["one_sided"]),
        "normalized_proposals": len(audit_sets["normalized"]),
        "documented_exception_proposals": len(audit_sets["documented"]),
        "normalized_rows": normalized_rows,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description="Normalize undocumented one-sided stance labels in a release CSV.")
    parser.add_argument("--input", type=Path, required=True, help="Input release CSV.")
    parser.add_argument("--output", type=Path, required=True, help="Output normalized release CSV.")
    parser.add_argument(
        "--exceptions",
        type=Path,
        required=True,
        help="metadata/exceptions/stance_one_sided_exceptions.csv.",
    )
    parser.add_argument("--audit-output", type=Path, required=True, help="Audit CSV for one-sided proposal actions.")
    parser.add_argument("--stage", default="2_stance_v5", help="Exception stage to use.")
    args = parser.parse_args()

    summary = normalize_release_stance(
        input_path=args.input,
        output_path=args.output,
        exceptions_path=args.exceptions,
        audit_path=args.audit_output,
        stage=args.stage,
    )
    for key, value in summary.items():
        print(f"{key}={value}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as error:
        raise SystemExit(str(error)) from None
