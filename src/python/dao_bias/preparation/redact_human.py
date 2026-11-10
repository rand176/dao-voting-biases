from __future__ import annotations
"""Prepare a shareable crowdsourcing CSV, preserving pipeline join keys.

Usage (from this package root; the original input CSV is not bundled):
    PYTHONPATH=src/python python -B -m dao_bias.preparation.redact_human \
        INPUT/data/2024/humanlabels/dao_rnd_merged.csv \
        /tmp/crowdsourcing_review --split

Use a new temporary destination, review the result, then update the public copy.
The example's private input is intentionally absent from the standalone package.
Split mode writes proposal metadata once and choice labels separately. Omitting
--split retains the combined-CSV export for source-side compatibility checks.

Redacts contact emails and phone numbers in Snapshot text, including contact
link targets. Keeps evaluations, timings, original text-length variables, and
Ethereum addresses. Converts Unicode line/paragraph separators (U+2028/U+2029)
to ordinary newlines for editor compatibility.
Removes the exported row index. The input has no participant-ID columns.
Original proposal and proposal-choice IDs are retained for pipeline joins;
this is contact redaction, not anonymisation. A value-free companion JSON report
counts replacements and preserved numeric candidates requiring manual review.
Unexpected columns require review, and existing output files are never replaced.
"""


import argparse
import csv
import json
import re
from collections import Counter, defaultdict
from contextlib import ExitStack
from pathlib import Path

from dao_bias.data.public import COMBINED_COLUMNS, PROPOSAL_COLUMNS, LABEL_COLUMNS, PROPOSALS_PATH, LABELS_PATH, REPORT_NAME, split_crowdsourcing_rows

ALLOWED_COLUMNS = set(COMBINED_COLUMNS)
INDEX_COLUMNS = {"", "Unnamed: 0"}
TEXT_COLUMNS = {"title", "body", "choice", "choices_json"}
ETHEREUM_OR_HASH = re.compile(r"\b0x[0-9a-fA-F]{40,64}\b")
CONTACT_LINK = re.compile(
    r"\b(mailto|tel):(?:[^\s<>()\[\]\"']+|\([^\s<>()\[\]\"']*\))+", re.IGNORECASE
)
EMAIL = re.compile(
    r"[A-Z0-9.!#$%&'*+/=?^_`{|}~-]+(?:@|%40)"
    r"[A-Z0-9](?:[A-Z0-9-]*[A-Z0-9])?(?:\.[A-Z0-9](?:[A-Z0-9-]*[A-Z0-9])?)*"
    r"\.[A-Z]{2,63}(?![A-Z0-9-])",
    re.IGNORECASE,
)
PHONE_CANDIDATE = re.compile(
    r"(?<![\w.])(?:\+[ \t]*|\()?[0-9](?:[ \t().-]*[0-9]){6,14}"
    r"(?:[ \t]*(?:ext\.?|x)[ \t]*[0-9]{1,6})?(?!\w|[ \t().-]*[0-9])",
    re.IGNORECASE,
)
NATIONAL_PHONE = re.compile(r"(?:\(?[0-9]{3}\)?[ .-])[0-9]{3}[ .-][0-9]{4}|0[0-9]{1,4}[ .-][0-9]{3,4}[ .-][0-9]{3,4}")
PHONE_CONTEXT = re.compile(r"(?:phone|telephone|mobile|whatsapp|call|tel)(?:\s+number)?\s*[:=]?\s*$", re.IGNORECASE)


def redact_contacts(text: str) -> tuple[str, Counter, int]:
    """Conservative detection; uncertain numeric strings are counted for review."""
    replacements = Counter()
    review_candidates = 0

    def redact_part(part: str) -> str:
        nonlocal review_candidates

        def contact_link(match: re.Match) -> str:
            kind = "email" if match[1].lower() == "mailto" else "phone"
            replacements[kind] += 1
            return f"[{kind.upper()}]"

        part = CONTACT_LINK.sub(contact_link, part)

        def email(match: re.Match) -> str:
            replacements["email"] += 1
            return "[EMAIL]"

        part = EMAIL.sub(email, part)

        def phone(match: re.Match) -> str:
            nonlocal review_candidates
            value = match.group()
            number = re.split(r"\s*(?:ext\.?|x)\s*", value, flags=re.IGNORECASE)[0]
            context = part[max(0, match.start() - 40):match.start()]
            if (value.startswith("+") or NATIONAL_PHONE.fullmatch(number)
                    or PHONE_CONTEXT.search(context)):
                replacements["phone"] += 1
                return "[PHONE]"
            review_candidates += 1
            return value

        return PHONE_CANDIDATE.sub(phone, part)

    # Never allow numeric patterns to consume an address or a transaction hash.
    parts, start = [], 0
    for match in ETHEREUM_OR_HASH.finditer(text):
        parts.extend((redact_part(text[start:match.start()]), match.group()))
        start = match.end()
    parts.append(redact_part(text[start:]))
    return "".join(parts), replacements, review_candidates


def export_crowdsourcing(source: Path, destination: Path, *, report_path: Path | None = None,
                        split: bool = False) -> tuple[int, int]:
    csv_paths = [destination / PROPOSALS_PATH.name, destination / LABELS_PATH.name] if split else [destination]
    report_path = report_path or (destination / REPORT_NAME if split else destination.with_suffix(".redactions.json"))
    if report_path.resolve() in {source.resolve(), *(p.resolve() for p in csv_paths)}:
        raise ValueError("The report must be separate from the input and output CSV")
    for path in [*csv_paths, report_path]:
        if path.exists():
            raise FileExistsError("CSV and report destinations must all be new files")
    with source.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        columns = reader.fieldnames or []
        if len(columns) != len(set(columns)):
            raise ValueError("Duplicate column names in input")
        missing = {"proposal", "choice_idx", "prop_chid"} - set(columns)
        if missing:
            raise ValueError(f"Missing required columns: {', '.join(sorted(missing))}")
        unexpected = set(columns) - ALLOWED_COLUMNS - INDEX_COLUMNS
        if unexpected:
            raise ValueError(
                "Unexpected columns; review for identifying information before export: "
                + ", ".join(sorted(unexpected))
            )
        rows = list(reader)

    public_columns = [column for column in columns if column not in INDEX_COLUMNS]
    proposals: set[str] = set()
    replacements = Counter(email=0, phone=0)
    by_field = defaultdict(Counter)
    review_by_field = Counter()
    for number, row in enumerate(rows, start=2):
        if None in row or any(value is None for value in row.values()):
            raise ValueError(f"Malformed CSV record {number}")
        proposal = row["proposal"]
        choice = row["choice_idx"]
        if not proposal.strip() or not choice.isascii() or not choice.isdecimal() or int(choice) < 1:
            raise ValueError(f"Missing proposal or invalid choice index at record {number}")
        if row["prop_chid"] != proposal + str(int(choice)):
            raise ValueError(f"Inconsistent proposal-choice identifier at record {number}")
        for column in public_columns:
            row[column] = row[column].replace("\u2028", "\n").replace("\u2029", "\n")
            if column in TEXT_COLUMNS:
                row[column], counts, candidates = redact_contacts(row[column])
                replacements.update(counts)
                if counts:
                    by_field[column].update(counts)
                if candidates:
                    review_by_field[column] += candidates
        proposals.add(proposal)

    if split:
        proposal_rows, label_rows = split_crowdsourcing_rows([
            {column: row[column] for column in public_columns} for row in rows
        ])
        tables = [(csv_paths[0], PROPOSAL_COLUMNS, proposal_rows),
                  (csv_paths[1], LABEL_COLUMNS, label_rows)]
    else:
        tables = [(destination, public_columns, rows)]
    report = {
        "policy": "contact-redacted; not anonymous",
        "rows": len(rows), "proposals": len(proposals),
        "replacements": dict(replacements),
        "replacements_by_field": {field: dict(counts) for field, counts in sorted(by_field.items())},
        "numeric_candidates_for_review_by_field": dict(sorted(review_by_field.items())),
        "review_note": "Automatic detection is incomplete. Review preserved numeric candidates and text before sharing. Counts include repeated proposal text across choice rows; no removed values are stored.",
        "text_length_variables": "Retained from the original, unredacted text.",
    }
    if split:
        report.update(layout="split-proposals-and-choice-labels", proposal_rows=len(proposal_rows),
                      choice_label_rows=len(label_rows))
    for path in [*csv_paths, report_path]:
        path.parent.mkdir(parents=True, exist_ok=True)
    created = []
    try:
        with ExitStack() as stack:
            handles = []
            for path, fields, records in tables:
                handle = stack.enter_context(path.open("x", newline="", encoding="utf-8"))
                created.append(path)
                handles.append((handle, fields, records))
            report_handle = stack.enter_context(report_path.open("x", encoding="utf-8"))
            created.append(report_path)
            for handle, fields, records in handles:
                writer = csv.DictWriter(handle, fieldnames=fields, extrasaction="ignore")
                writer.writeheader()
                writer.writerows(records)
            json.dump(report, report_handle, indent=2, sort_keys=True)
            report_handle.write("\n")
    except BaseException:
        for path in created:
            path.unlink()
        raise
    return len(rows), len(proposals)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path, help="Combined dao_rnd_merged.csv input")
    parser.add_argument("destination", type=Path, help="New combined CSV, or a directory for --split outputs")
    parser.add_argument("--split", action="store_true", help="Write proposal and choice-label CSVs plus a report; refuse existing files")
    parser.add_argument("--report", type=Path, help="New value-free report; defaults to dao_rnd.redactions.json in split mode, otherwise the CSV with suffix .redactions.json")
    args = parser.parse_args()
    try:
        rows, proposals = export_crowdsourcing(args.source, args.destination, report_path=args.report, split=args.split)
    except (OSError, ValueError, csv.Error) as error:
        parser.exit(1, f"Export failed: {error}\n")
    print(f"Wrote {rows} rows across {proposals} proposals to {args.destination}")


if __name__ == "__main__":
    main()
