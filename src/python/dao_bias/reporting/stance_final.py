"""Render final-sample stance figures from an original registered analytical release.

From the package root:
    python run.py reports --kind stance-final --data-root INPUT --output-root /tmp/figure7
Or invoke the module directly:
    PYTHONPATH=src/python python -B -m dao_bias.reporting.stance_final --analysis-root INPUT --output-dir /tmp/figure7

Writes llms_final_sample.png, .pdf, and .json; requires --overwrite to replace
existing outputs. The analytical release is not bundled in the public package.
"""
from __future__ import annotations
from dao_bias.data.paths import PACKAGE_ROOT, OUTPUT_ROOT

import argparse
import csv
import hashlib
import math
from pathlib import Path

import dao_bias.reporting.stance_figure as figure
from dao_bias.reporting.counts import normalize_id, single_option_proposal_ids, truthy


def build_final_payload(analysis_root: Path, release: str = "current") -> dict[str, object]:
    source, registry = figure.resolve_release(analysis_root, release)
    # Retain only sample/label columns, not the large proposal body fields.
    columns = {"proposal", "choice_idx", "prop_chid", "N_choices", "flagged"}
    columns.update(spec.column for spec in figure.MODEL_SPECS)
    csv.field_size_limit(max(csv.field_size_limit(), 20_000_000))
    with source.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        missing = columns - set(reader.fieldnames or [])
        if missing:
            raise ValueError(f"Release is missing required columns: {', '.join(sorted(missing))}")
        rows = [{col: row[col] for col in columns} for row in reader]

    for row in rows:
        row["proposal"] = normalize_id(row["proposal"])
    if any(not row["proposal"] for row in rows):
        raise ValueError("Release contains a missing proposal identifier")
    input_proposals = {row["proposal"] for row in rows}
    # Same ordering and proposal-level guards as dao_bias.reporting.counts.read_release_stats.
    flagged = {row["proposal"] for row in rows if truthy(row["flagged"])}
    unflagged = [row for row in rows if row["proposal"] not in flagged]
    single = single_option_proposal_ids(unflagged)
    retained = [row for row in unflagged if row["proposal"] not in single]

    keys: set[tuple[str, int]] = set()
    prop_chids: set[str] = set()
    for row in retained:
        index = float(row["choice_idx"])
        if not math.isfinite(index) or index < 1 or not index.is_integer():
            raise ValueError(f"Invalid choice index for proposal {row['proposal']}")
        key = (row["proposal"], int(index))
        if key in keys or row["prop_chid"] in prop_chids:
            raise ValueError(f"Duplicate proposal-choice identifier: {key}")
        if row["prop_chid"] != f"{key[0]}{key[1]}":
            raise ValueError(f"prop_chid does not match proposal-choice pair: {key}")
        keys.add(key)
        prop_chids.add(row["prop_chid"])

    counts, sample_rows = figure.count_stance_rows(retained, figure.MODEL_SPECS)
    totals = {name: sum(values.values()) for name, values in counts.items()}
    if not sample_rows or totals["heuristic"] != sample_rows:
        raise ValueError("Final sample must be nonempty with one heuristic label per choice")
    with source.open("rb") as handle:
        source_hash = hashlib.file_digest(handle, "sha256").hexdigest()
    return {
        "counts": counts,
        "totals": totals,
        "sample": "Final analytic sample after Snapshot-flagged and single-option proposal exclusions",
        "sample_rows": sample_rows,
        "sample_proposals": len({row["proposal"] for row in retained}),
        "input_rows": len(rows),
        "input_proposals": len(input_proposals),
        "source": str(source.resolve()),
        "source_sha256": source_hash,
        "release": {**registry, "release_name": release},
        "source_columns": {spec.key: spec.column for spec in figure.MODEL_SPECS},
        "counting_rule": "Source-specific nonmissing canonical choice-level labels in the final analytic sample; totals measure coverage, not agreement.",
        "exclusions": {
            "snapshot_flagged": {"proposals": len(flagged), "choices": len(rows) - len(unflagged)},
            "remaining_single_option": {"proposals": len(single), "choices": len(unflagged) - sample_rows},
        },
        "display_options": {"bottom_note": False},
        "generator": "dao_bias.reporting.stance_final",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--analysis-root", type=Path, default=PACKAGE_ROOT)
    parser.add_argument("--release", default="current")
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--overwrite", action="store_true")
    args = parser.parse_args()
    root = args.analysis_root.resolve()
    output = args.output_dir or OUTPUT_ROOT / "stance-final"
    outputs = {ext: output / f"llms_final_sample.{ext}" for ext in ("png", "pdf", "json")}
    if not args.overwrite and any(path.exists() for path in outputs.values()):
        parser.error("Output already exists; choose another --output-dir or pass --overwrite")
    try:
        payload = build_final_payload(root, args.release)
    except (OSError, ValueError) as error:
        parser.exit(1, f"Cannot build final-sample stance counts: {error}\n"
                    "This workflow requires the original registered analytical release. "
                    "Use python run.py human-summary for the bundled human-label sample.\n")
    for ext in ("png", "pdf"):
        figure.write_figure(outputs[ext], payload, include_note=False)
    figure.write_json(outputs["json"], payload)
    print(f"Final sample: {payload['sample_rows']:,} choices; {payload['sample_proposals']:,} proposals")
    for path in outputs.values():
        print(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
