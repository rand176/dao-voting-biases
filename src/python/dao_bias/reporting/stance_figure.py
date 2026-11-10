from __future__ import annotations
#!/usr/bin/env python3
"""Generate the manuscript stance-agreement count figure from a release alias."""


from dao_bias.data.paths import OUTPUT_ROOT

import argparse
import csv
import json
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


CANONICAL_STANCES = ("APPROVE", "REJECT", "ABSTAIN", "OTHER")
PLOT_STANCE_ORDER = ("APPROVE", "REJECT", "ABSTAIN", "OTHER")
MISSING_LABELS = {"", "NA", "N/A", "NONE", "NULL"}


@dataclass(frozen=True)
class ModelSpec:
    key: str
    label: str
    column: str


MODEL_SPECS = (
    ModelSpec("heuristic", "Heuristic", "stance"),
    ModelSpec("llama", "Llama 3.1", "stance_llama"),
    ModelSpec("llama33", "Llama 3.3", "stance_llama33"),
    ModelSpec("oai", "OAI", "stance_oai"),
    ModelSpec("chatgpt", "ChatGPT", "stance_chatgpt"),
)

PLOT_COLORS = {
    "APPROVE": "#4C78A8",
    "REJECT": "#E45756",
    "ABSTAIN": "#72B7B2",
    "OTHER": "#54A24B",
}


def clean_label(value: object) -> str:
    return str(value or "").strip().strip('"').upper()


def resolve_release(analysis_root: Path, release: str) -> tuple[Path, dict[str, str]]:
    registry_path = analysis_root / "metadata" / "analysis_data_releases.csv"
    with registry_path.open(newline="", encoding="utf-8") as handle:
        rows = [row for row in csv.DictReader(handle) if row.get("release_name") == release]
    if len(rows) != 1:
        raise ValueError(f"Expected exactly one release alias for {release!r} in {registry_path}")
    return analysis_root / rows[0]["relative_path"], rows[0]


def empty_counts() -> dict[str, int]:
    return {stance: 0 for stance in sorted(CANONICAL_STANCES)}


def count_stance_rows(input_rows: Iterable[dict[str, str]], model_specs: Iterable[ModelSpec]) -> tuple[dict[str, dict[str, int]], int]:
    specs = tuple(model_specs)
    counts = {spec.key: Counter() for spec in specs}
    rows = 0
    for row_number, row in enumerate(input_rows, start=2):
        rows += 1
        for spec in specs:
            label = clean_label(row.get(spec.column))
            if label in MISSING_LABELS:
                continue
            if label not in CANONICAL_STANCES:
                expected = ", ".join(CANONICAL_STANCES)
                raise ValueError(
                    f"Unknown stance label in column {spec.column} at row {row_number}: "
                    f"{row.get(spec.column)!r}; expected one of {expected} or a missing label"
                )
            counts[spec.key][label] += 1
    return (
        {
            spec.key: {
                stance: int(counts[spec.key].get(stance, 0))
                for stance in sorted(CANONICAL_STANCES)
            }
            for spec in specs
        },
        rows,
    )


def count_stances(release_path: Path, model_specs: Iterable[ModelSpec]) -> tuple[dict[str, dict[str, int]], int]:
    specs = tuple(model_specs)
    with release_path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        fieldnames = set(reader.fieldnames or [])
        missing_columns = [spec.column for spec in specs if spec.column not in fieldnames]
        if missing_columns:
            raise ValueError(
                "Release is missing required stance column(s): "
                + ", ".join(sorted(missing_columns))
            )
        return count_stance_rows(reader, specs)


def build_payload(analysis_root: Path, release: str) -> dict[str, object]:
    release_path, release_row = resolve_release(analysis_root, release)
    counts, rows = count_stances(release_path, MODEL_SPECS)
    release_payload = dict(release_row)
    release_payload.update({
        "release_name": release,
        "path": str(release_path),
        "relative_path": release_row["relative_path"],
        "rows": rows,
    })
    return {
        "counts": counts,
        "counting_rule": (
            "Source-specific non-missing choice-level stance labels; only "
            "APPROVE, REJECT, ABSTAIN, and OTHER are counted."
        ),
        "model_set": "manuscript",
        "plotted_models": [spec.label for spec in MODEL_SPECS],
        "release": release_payload,
        "requested_models": [spec.key for spec in MODEL_SPECS],
        "stance_order": list(PLOT_STANCE_ORDER),
        "totals": {key: sum(value.values()) for key, value in counts.items()},
    }


def write_json(path: Path, payload: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_figure(path: Path, payload: dict[str, object], *, include_note: bool = True) -> None:
    import matplotlib.pyplot as plt  # type: ignore
    from matplotlib.ticker import FuncFormatter  # type: ignore

    path.parent.mkdir(parents=True, exist_ok=True)
    counts = payload["counts"]
    totals = payload["totals"]
    assert isinstance(counts, dict)
    assert isinstance(totals, dict)

    max_count = max(
        int(counts[spec.key][stance])
        for spec in MODEL_SPECS
        for stance in PLOT_STANCE_ORDER
    )
    y_limit = max_count * 1.18 if max_count else 1
    fig, axes = plt.subplots(1, len(MODEL_SPECS), figsize=(12.2, 5.2), sharey=True)
    fig.patch.set_facecolor("white")

    for axis, spec in zip(axes, MODEL_SPECS):
        source_counts = counts[spec.key]
        values = [int(source_counts[stance]) for stance in PLOT_STANCE_ORDER]
        bars = axis.bar(
            range(len(PLOT_STANCE_ORDER)),
            values,
            color=[PLOT_COLORS[stance] for stance in PLOT_STANCE_ORDER],
            edgecolor="white",
            linewidth=1,
            width=0.72,
        )
        axis.set_title(f"{spec.label}\nn={int(totals[spec.key]):,}", fontsize=12, fontweight="bold")
        axis.set_xticks(range(len(PLOT_STANCE_ORDER)), ["Approve", "Reject", "Abstain", "Other"], rotation=35, ha="right")
        axis.set_ylim(0, y_limit)
        axis.grid(axis="y", color="#D9D9D9", linewidth=0.8)
        axis.set_axisbelow(True)
        axis.set_facecolor("#FBFBFB")
        for spine in ("top", "right"):
            axis.spines[spine].set_visible(False)
        axis.spines["left"].set_color("#BDBDBD")
        axis.spines["bottom"].set_color("#BDBDBD")
        for bar, value in zip(bars, values):
            if value <= 0:
                continue
            axis.text(
                bar.get_x() + bar.get_width() / 2,
                value + y_limit * 0.015,
                f"{value:,}",
                ha="center",
                va="bottom",
                rotation=90,
                fontsize=8,
                color="#333333",
            )

    axes[0].set_ylabel("Choice-level labels")
    axes[0].yaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{int(value):,}"))
    fig.suptitle("Stance label counts by source", x=0.01, ha="left", y=0.99, fontsize=16, fontweight="bold")
    if include_note:
        fig.text(
            0.01,
            0.02,
            "Counts are source-specific non-missing choice-level labels in canonical stance categories.",
            ha="left",
            va="bottom",
            fontsize=9,
            color="#4A4A4A",
        )
    fig.subplots_adjust(left=0.08, right=0.99, bottom=0.22, top=0.82, wspace=0.16)
    fig.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(fig)


def default_outputs(analysis_root: Path, paper_root: Path | None = None) -> dict[str, Path]:
    resolved_paper_root = paper_root or OUTPUT_ROOT
    return {
        "json": OUTPUT_ROOT / "metadata" / "analysis_data_provenance" / "stance_agreement_figure__latest.json",
        "figure": resolved_paper_root / "figures" / "images25" / "llms" / "llms.png",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--analysis-root", type=Path, default=Path("."))
    parser.add_argument("--release", default="current")
    parser.add_argument("--output-json", type=Path)
    parser.add_argument("--figure-output", type=Path)
    parser.add_argument("--paper-root", type=Path)
    args = parser.parse_args()

    analysis_root = args.analysis_root.resolve()
    paper_root = args.paper_root.resolve() if args.paper_root else None
    outputs = default_outputs(analysis_root, paper_root=paper_root)
    try:
        payload = build_payload(analysis_root, args.release)
    except FileNotFoundError as error:
        parser.exit(1, f"Input unavailable: {error}\nPrivate mode requires original analysis inputs. "
                    "Only human-label data are bundled; provide the original release inputs.\n")
    write_json(args.output_json or outputs["json"], payload)
    write_figure(args.figure_output or outputs["figure"], payload)
    print(f"release={args.release}")
    print(f"rows={payload['release']['rows']}")
    print(f"models={len(payload['requested_models'])}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
