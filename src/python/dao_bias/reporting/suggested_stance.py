from __future__ import annotations
#!/usr/bin/env python3
"""Generate suggested-stance diagnostics from an analysis release alias."""


from dao_bias.data.paths import OUTPUT_ROOT

import argparse
import base64
import csv
import json
from collections import Counter, defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable


POSITIVE_STANCES = ("APPROVE", "REJECT", "ABSTAIN", "OTHER")
PLOTTED_DISTRIBUTION_STATES = ("no_suggestion",) + POSITIVE_STANCES
DISTRIBUTION_DENOMINATOR_LABEL = "reviewed, nonconflict proposals"
PLOTTED_DISTRIBUTION_LABELS = {
    "no_suggestion": "No suggestion",
    "APPROVE": "Approve",
    "REJECT": "Reject",
    "ABSTAIN": "Abstain",
    "OTHER": "Other",
}
PLOTTED_DISTRIBUTION_FIGURE_ORDER = POSITIVE_STANCES + ("no_suggestion",)
PLOTTED_DISTRIBUTION_COLORS = {
    "no_suggestion": "#BAB0AC",
    "APPROVE": "#4C78A8",
    "REJECT": "#E45756",
    "ABSTAIN": "#72B7B2",
    "OTHER": "#54A24B",
}
NO_SUGGESTION_LABELS = {"", "NA", "N/A", "NONE", "NULL", "NO_SUGGESTION", "NO SUGGESTION"}
NONCANONICAL_LABELS = {
    "APPROVE_LIKE": "APPROVE",
    "APPROVE-LIKE": "APPROVE",
    "REJECT_LIKE": "REJECT",
    "REJECT-LIKE": "REJECT",
    "ABSTAIN_LIKE": "ABSTAIN",
    "ABSTAIN-LIKE": "ABSTAIN",
    "OTHER_LIKE": "OTHER",
    "OTHER-LIKE": "OTHER",
}
MODEL_SPECS = {
    "oai": {
        "label": "OpenAI GPT-4o",
        "suggested": "suggested_stance_oai",
        "stance": "stance_oai",
        "effect": "effect_oai",
    },
    "llama": {
        "label": "Llama 3.1",
        "suggested": "suggested_stance_llama",
        "stance": "stance_llama",
        "effect": "effect_llama",
    },
    "llama33": {
        "label": "Llama 3.3",
        "suggested": "suggested_stance_llama33",
        "stance": "stance_llama33",
        "effect": "effect_llama33",
        "reviewed": "llama33_reviewed",
        "not_reviewed": "llama33_not_reviewed",
        "conflict": "llama33_label_conflict",
        "basic_exclusion": "llama33_basic_proposal_exclusion",
        "source": "source_file_llama33",
    },
    "chatgpt": {
        "label": "ChatGPT",
        "suggested": "suggested_stance_chatgpt",
        "stance": "stance_chatgpt",
        "effect": "effect_chatgpt",
    },
    "human": {
        "label": "human labels",
        "suggested": "suggested_stance_hl",
        "stance": "stance_hl",
    },
}
WINNER_MATCHING_MODEL_KEYS = ("oai", "llama", "llama33", "chatgpt")
SPARSE_METRIC_POSITIVE_THRESHOLD = 30
SPARSE_METRIC_TEX = "\\multicolumn{3}{c}{Sparse ($n<30$)}"
SUGGESTED_STANCE_WINNER_TABLE_INPUT = (
    r"\input{tables/2026/stats/table_suggested_stance_precision_recall.tex}"
)
SUGGESTED_STANCE_WINNER_TABLE_CAPTION = (
    r"\caption{\textbf{Detected suggested stance compared with the winning stance.} "
    r"For each source, we treat the detected suggested stance as a prediction "
    r"(column Pred.) and we count how many times the prediction was actually true "
    r"(column TP) over all the time stance a given stance won (column Winner). "
    r"With this framework, we can compute precision, recall, and F1 to capture "
    r"alignment between textual suggestions and realized voting outcomes.}"
)


@dataclass
class ProposalModelState:
    model_key: str
    model_label: str
    proposal: str
    state: str
    suggested_stance: str | None
    winner_stance: str | None
    basic_exclusion: bool = False


@dataclass
class Diagnostics:
    metrics: list[dict[str, object]]
    model_states: list[ProposalModelState]
    model_summary: dict[str, dict[str, int]]
    distributions: dict[str, dict[str, int]]
    distribution_plot_data: list[dict[str, object]]
    label_normalization: dict[str, dict[str, object]]
    release: dict[str, object]
    matched_metrics: list[dict[str, object]] = field(default_factory=list)


def truthy(value: object) -> bool:
    return str(value or "").strip().upper() in {"1", "TRUE", "T", "YES", "Y"}


def clean(value: object) -> str:
    return str(value or "").strip().strip('"').upper()


def normalize_id(value: object) -> str:
    return str(value or "").strip().lower()


def as_optional_number(value: object) -> float | None:
    if value is None or value == "":
        return None
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


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


def normalize_suggested_label(value: object, normalization_counts: Counter[str]) -> str | None:
    label = clean(value)
    if label in NO_SUGGESTION_LABELS:
        return None
    if label in POSITIVE_STANCES:
        return label
    if label in NONCANONICAL_LABELS:
        normalization_counts[label] += 1
        return NONCANONICAL_LABELS[label]
    raise ValueError(f"Unknown suggested-stance label: {value!r}")


def normalize_winner_stance(value: object) -> str | None:
    label = clean(value)
    if label in POSITIVE_STANCES:
        return label
    return None


def resolve_release(analysis_root: Path, release: str) -> tuple[Path, dict[str, str]]:
    registry_path = analysis_root / "metadata" / "analysis_data_releases.csv"
    with registry_path.open(newline="", encoding="utf-8") as handle:
        rows = [row for row in csv.DictReader(handle) if row.get("release_name") == release]
    if len(rows) != 1:
        raise ValueError(f"Expected exactly one release alias for {release!r} in {registry_path}")
    release_path = analysis_root / rows[0]["relative_path"]
    return release_path, rows[0]


def read_release_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


def choose_winner_column(fieldnames: Iterable[str]) -> str:
    fields = set(fieldnames)
    if "choice_won" in fields:
        return "choice_won"
    raise ValueError("Release is missing the observed winner column: expected choice_won")


def proposal_winner(rows: list[dict[str, str]], winner_col: str) -> str | None:
    winners = {
        normalize_winner_stance(row.get("stance"))
        for row in rows
        if truthy(row.get(winner_col))
    }
    winners.discard(None)
    if len(winners) == 1:
        return next(iter(winners))
    return None


def row_has_model_value(row: dict[str, str], spec: dict[str, str]) -> bool:
    for col in (spec.get("suggested"), spec.get("stance"), spec.get("effect"), spec.get("source")):
        if col and clean(row.get(col)) not in NO_SUGGESTION_LABELS:
            return True
    return False


def collapse_model_state(
    proposal: str,
    rows: list[dict[str, str]],
    model_key: str,
    spec: dict[str, str],
    winner_stance: str | None,
    normalization_counts: Counter[str],
) -> ProposalModelState | None:
    if spec["suggested"] not in rows[0]:
        return None
    reviewed_col = spec.get("reviewed")
    not_reviewed_col = spec.get("not_reviewed")
    conflict_col = spec.get("conflict")
    basic_exclusion_col = spec.get("basic_exclusion")

    conflict = any(truthy(row.get(conflict_col)) for row in rows) if conflict_col else False
    reviewed = any(truthy(row.get(reviewed_col)) for row in rows) if reviewed_col else any(
        row_has_model_value(row, spec) for row in rows
    )
    if not_reviewed_col and any(truthy(row.get(not_reviewed_col)) for row in rows):
        reviewed = False if not any(row_has_model_value(row, spec) for row in rows) else reviewed
    basic_exclusion = any(truthy(row.get(basic_exclusion_col)) for row in rows) if basic_exclusion_col else False

    normalized_values = {
        normalize_suggested_label(row.get(spec["suggested"]), normalization_counts)
        for row in rows
    }
    normalized_values.discard(None)
    if conflict or len(normalized_values) > 1:
        return ProposalModelState(model_key, spec["label"], proposal, "conflict", None, winner_stance, basic_exclusion)
    if len(normalized_values) == 1:
        return ProposalModelState(
            model_key,
            spec["label"],
            proposal,
            "positive",
            next(iter(normalized_values)),
            winner_stance,
            basic_exclusion,
        )
    if reviewed:
        return ProposalModelState(model_key, spec["label"], proposal, "no_suggestion", None, winner_stance, basic_exclusion)
    return ProposalModelState(model_key, spec["label"], proposal, "not_reviewed", None, winner_stance, basic_exclusion)


def metric_rows(states: list[ProposalModelState], matched_only: set[tuple[str, str]] | None = None) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for model_key in MODEL_SPECS:
        model_states = [state for state in states if state.model_key == model_key]
        if matched_only is not None:
            model_states = [state for state in model_states if (state.proposal, state.model_key) in matched_only]
        if not model_states:
            continue
        model_label = MODEL_SPECS[model_key]["label"]
        reviewed_nonconflict = [
            state for state in model_states
            if state.state in {"positive", "no_suggestion"} and state.winner_stance in POSITIVE_STANCES
        ]
        positive_predictions = [state for state in reviewed_nonconflict if state.state == "positive"]
        for stance in POSITIVE_STANCES:
            predicted = [state for state in positive_predictions if state.suggested_stance == stance]
            actual = [state for state in reviewed_nonconflict if state.winner_stance == stance]
            true_positive = [
                state for state in predicted
                if state.winner_stance == stance
            ]
            precision = len(true_positive) / len(predicted) if predicted else 0.0
            recall = len(true_positive) / len(actual) if actual else 0.0
            f1 = 2 * precision * recall / (precision + recall) if precision + recall else 0.0
            rows.append({
                "model_key": model_key,
                "model_label": model_label,
                "stance": stance,
                "eligible_positive_proposals": len(reviewed_nonconflict),
                "predicted_positive_proposals": len(predicted),
                "winner_positive_proposals": len(actual),
                "true_positive_proposals": len(true_positive),
                "precision": f"{precision:.6f}",
                "recall": f"{recall:.6f}",
                "f1": f"{f1:.6f}",
            })
    return rows


def distribution_plot_rows(
    model_summary: dict[str, dict[str, int]],
    distributions: dict[str, dict[str, int]],
) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for model_key in MODEL_SPECS:
        summary = model_summary.get(model_key)
        dist = distributions.get(model_key)
        if summary is None or dist is None:
            continue
        # Conflicts are reviewed, but they do not map to a single plotted state.
        denominator = summary["positive_suggested_stance_proposals"] + summary["no_suggestion_proposals"]
        if denominator <= 0:
            continue
        model_label = MODEL_SPECS[model_key]["label"]
        for state in PLOTTED_DISTRIBUTION_STATES:
            count = int(dist.get(state, 0))
            rows.append({
                "model_key": model_key,
                "model_label": model_label,
                "state": state,
                "state_label": PLOTTED_DISTRIBUTION_LABELS[state],
                "count": count,
                "share": count / denominator,
                "reviewed_nonconflict_proposals": denominator,
            })
    return rows


def build_diagnostics(analysis_root: Path, release: str, matched_min_overlap: int = 30) -> Diagnostics:
    release_path, release_row = resolve_release(analysis_root, release)
    rows = read_release_rows(release_path)
    single_option_proposals = single_option_proposal_ids(rows)
    rows = [
        row
        for row in rows
        if normalize_id(row.get("proposal")) not in single_option_proposals
    ]
    if not rows:
        raise ValueError(f"Release has no rows: {release_path}")
    winner_col = choose_winner_column(rows[0].keys())
    by_proposal: dict[str, list[dict[str, str]]] = defaultdict(list)
    for row in rows:
        proposal = normalize_id(row.get("proposal"))
        if proposal:
            by_proposal[proposal].append(row)

    normalization_counts: Counter[str] = Counter()
    states: list[ProposalModelState] = []
    for proposal, proposal_rows in by_proposal.items():
        winner_stance = proposal_winner(proposal_rows, winner_col)
        for model_key, spec in MODEL_SPECS.items():
            state = collapse_model_state(
                proposal,
                proposal_rows,
                model_key,
                spec,
                winner_stance,
                normalization_counts,
            )
            if state is not None:
                states.append(state)

    summary: dict[str, dict[str, int]] = {}
    distributions: dict[str, dict[str, int]] = {}
    for model_key in MODEL_SPECS:
        model_states = [state for state in states if state.model_key == model_key]
        if not model_states:
            continue
        state_counts = Counter(state.state for state in model_states)
        summary[model_key] = {
            "total_proposals": len(model_states),
            "reviewed_proposals": state_counts["positive"] + state_counts["no_suggestion"] + state_counts["conflict"],
            "not_reviewed_proposals": state_counts["not_reviewed"],
            "positive_suggested_stance_proposals": state_counts["positive"],
            "no_suggestion_proposals": state_counts["no_suggestion"],
            "conflict_proposals": state_counts["conflict"],
            "basic_exclusion_proposals": sum(1 for state in model_states if state.basic_exclusion),
        }
        dist = Counter(
            state.suggested_stance if state.state == "positive" else state.state
            for state in model_states
        )
        distributions[model_key] = dict(sorted(dist.items()))

    matched_metrics: list[dict[str, object]] = []
    positive_by_proposal: dict[str, set[str]] = defaultdict(set)
    active_models = {state.model_key for state in states}
    for state in states:
        if state.state == "positive" and state.winner_stance in POSITIVE_STANCES:
            positive_by_proposal[state.proposal].add(state.model_key)
    matched_proposals = {
        proposal for proposal, models in positive_by_proposal.items()
        if active_models and active_models.issubset(models)
    }
    if len(matched_proposals) >= matched_min_overlap:
        matched_keys = {
            (state.proposal, state.model_key)
            for state in states
            if state.proposal in matched_proposals
        }
        matched_metrics = metric_rows(states, matched_only=matched_keys)

    label_normalization = {
        raw: {"canonical": NONCANONICAL_LABELS[raw], "count": count}
        for raw, count in sorted(normalization_counts.items())
    }
    release_payload = dict(release_row)
    release_payload.update({
        "release_name": release,
        "path": str(release_path),
        "rows": len(rows),
        "proposals": len(by_proposal),
        "winner_column": winner_col,
    })
    plot_rows = distribution_plot_rows(summary, distributions)
    return Diagnostics(
        metrics=metric_rows(states),
        model_states=states,
        model_summary=summary,
        distributions=distributions,
        distribution_plot_data=plot_rows,
        label_normalization=label_normalization,
        release=release_payload,
        matched_metrics=matched_metrics,
    )


def write_metrics_csv(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "model_key",
        "model_label",
        "stance",
        "eligible_positive_proposals",
        "predicted_positive_proposals",
        "winner_positive_proposals",
        "true_positive_proposals",
        "precision",
        "recall",
        "f1",
    ]
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def tex_escape(value: object) -> str:
    return str(value).replace("_", "\\_").replace("%", "\\%")


def winner_matching_table_rows(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    return [row for row in rows if row["model_key"] in WINNER_MATCHING_MODEL_KEYS]


def row_uses_sparse_metric_cells(row: dict[str, object]) -> bool:
    return int(row["predicted_positive_proposals"]) < SPARSE_METRIC_POSITIVE_THRESHOLD


def write_tex_table(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        "% Generated by dao_bias.reporting.suggested_stance",
        "\\begingroup",
        "\\small",
        "\\setlength{\\tabcolsep}{3pt}",
        "\\begin{tabularx}{\\linewidth}{@{}>{\\RaggedRight\\arraybackslash}Xlrrrrrr@{}}",
        "\\toprule",
        "Source & Stance & Pred. & Winner & TP & Precision & Recall & F1 \\\\",
        "\\midrule",
    ]
    for row in rows:
        metric_cells = (
            [SPARSE_METRIC_TEX]
            if row_uses_sparse_metric_cells(row)
            else [str(row["precision"]), str(row["recall"]), str(row["f1"])]
        )
        lines.append(
            " & ".join([
                tex_escape(row["model_label"]),
                tex_escape(row["stance"]),
                str(row["predicted_positive_proposals"]),
                str(row["winner_positive_proposals"]),
                str(row["true_positive_proposals"]),
            ] + metric_cells) + " \\\\"
        )
    lines.extend([
        "\\midrule",
        (
            "\\multicolumn{8}{@{}>{\\RaggedRight\\arraybackslash}p{\\linewidth}@{}}{"
            "\\footnotesize Counts compare automated-source detected positive suggested stances "
            "with the uniquely identified ex post winning stance by observed voting power. "
            "Precision, recall, and F1 are descriptive diagnostics, not causal estimates, and are "
            "shown only for rows with at least 30 positive suggested-stance predictions. Sparse "
            "rows retain counts but mark metric cells unstable.} \\\\"
        ),
        "\\bottomrule",
        "\\end{tabularx}",
        "\\endgroup",
        "",
    ])
    path.write_text("\n".join(lines), encoding="utf-8")


def sync_winner_matching_table_caption(paper_root: Path) -> bool:
    appendix_path = paper_root / "sections" / "Data_Appendix.tex"
    if not appendix_path.exists():
        return False

    text = appendix_path.read_text(encoding="utf-8")
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if line.strip() != SUGGESTED_STANCE_WINNER_TABLE_INPUT:
            continue

        caption_index = index + 1
        if caption_index >= len(lines) or not lines[caption_index].lstrip().startswith("\\caption{"):
            raise ValueError(
                f"Expected suggested-stance table caption immediately after "
                f"{SUGGESTED_STANCE_WINNER_TABLE_INPUT} in {appendix_path}"
            )

        indent = lines[caption_index][:len(lines[caption_index]) - len(lines[caption_index].lstrip())]
        lines[caption_index] = f"{indent}{SUGGESTED_STANCE_WINNER_TABLE_CAPTION}"
        trailing_newline = "\n" if text.endswith("\n") else ""
        appendix_path.write_text("\n".join(lines) + trailing_newline, encoding="utf-8")
        return True

    raise ValueError(f"Could not find suggested-stance winner table input in {appendix_path}")


def write_json(path: Path, diagnostics: Diagnostics) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "release": diagnostics.release,
        "models": diagnostics.model_summary,
        "distributions": diagnostics.distributions,
        "distribution_plot_data": diagnostics.distribution_plot_data,
        "label_normalization": diagnostics.label_normalization,
        "metrics": diagnostics.metrics,
        "matched_metrics": diagnostics.matched_metrics,
    }
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def write_distribution_figure(path: Path, plot_rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    try:
        import matplotlib.pyplot as plt  # type: ignore
        from matplotlib.ticker import PercentFormatter  # type: ignore
    except Exception:
        path.write_bytes(base64.b64decode(
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+/p9sAAAAASUVORK5CYII="
        ))
        return

    model_keys = [
        model_key for model_key in MODEL_SPECS
        if any(row["model_key"] == model_key for row in plot_rows)
    ]
    if not model_keys:
        model_keys = ["none"]
        labels = ["No reviewed\nproposals"]
        stacked = {state: [0.0] for state in PLOTTED_DISTRIBUTION_STATES}
        reviewed_counts = [0]
    else:
        labels = [
            "Human labels" if model_key == "human" else MODEL_SPECS[model_key]["label"]
            for model_key in model_keys
        ]
        stacked = {
            state: [
                next(
                    row["share"]
                    for row in plot_rows
                    if row["model_key"] == model_key and row["state"] == state
                )
                for model_key in model_keys
            ]
            for state in PLOTTED_DISTRIBUTION_STATES
        }
        reviewed_counts = [
            next(
                int(row["reviewed_nonconflict_proposals"])
                for row in plot_rows
                if row["model_key"] == model_key
            )
            for model_key in model_keys
        ]
    fig_width = max(7.5, min(10.5, 1.5 * len(labels)))
    fig_height = max(3.8, 0.7 * len(labels) + 1.6)
    fig, ax = plt.subplots(figsize=(fig_width, fig_height))
    fig.patch.set_facecolor("white")
    ax.set_facecolor("#FBFBFB")
    positions = list(range(len(labels)))
    left = [0.0] * len(labels)
    for state in PLOTTED_DISTRIBUTION_FIGURE_ORDER:
        shares = stacked[state]
        bars = ax.barh(
            positions,
            shares,
            left=left,
            label=PLOTTED_DISTRIBUTION_LABELS[state],
            color=PLOTTED_DISTRIBUTION_COLORS[state],
            height=0.72,
            edgecolor="white",
            linewidth=1.1,
        )
        for index, (bar, share, start) in enumerate(zip(bars, shares, left)):
            if share < 0.08:
                continue
            text_color = "#1F1F1F" if state in {"ABSTAIN", "no_suggestion"} else "white"
            ax.text(
                start + share / 2,
                positions[index],
                f"{share:.0%}",
                ha="center",
                va="center",
                fontsize=9,
                fontweight="bold",
                color=text_color,
            )
        left = [base + share for base, share in zip(left, shares)]

    ax.set_yticks(positions, labels=labels)
    ax.invert_yaxis()
    ax.set_xlim(0, 1.14)
    ax.set_xticks([0.0, 0.25, 0.5, 0.75, 1.0])
    ax.xaxis.set_major_formatter(PercentFormatter(1.0))
    ax.set_xlabel(f"Share of {DISTRIBUTION_DENOMINATOR_LABEL}")
    ax.grid(axis="x", color="#D9D9D9", linewidth=0.8)
    ax.grid(axis="y", visible=False)
    ax.set_axisbelow(True)
    for spine in ("top", "right", "left"):
        ax.spines[spine].set_visible(False)
    ax.spines["bottom"].set_color("#BDBDBD")
    ax.tick_params(axis="y", length=0)

    for index, count in enumerate(reviewed_counts):
        ax.text(
            1.02,
            positions[index],
            f"n={count:,}",
            ha="left",
            va="center",
            fontsize=9,
            color="#4A4A4A",
        )
    ax.text(
        1.02,
        -0.62,
        "Reviewed proposals",
        ha="left",
        va="bottom",
        fontsize=9,
        color="#4A4A4A",
        fontweight="bold",
    )

    handles, legend_labels = ax.get_legend_handles_labels()
    legend = fig.legend(
        handles,
        legend_labels,
        frameon=False,
        ncols=5,
        loc="upper left",
        bbox_to_anchor=(0.0, 0.96),
        borderaxespad=0.0,
        columnspacing=1.8,
        handlelength=1.8,
    )
    for text in legend.get_texts():
        text.set_fontsize(9)

    fig.suptitle("Suggested stance distribution by source", x=0.0, ha="left", y=0.995, fontsize=16, fontweight="bold")
    fig.subplots_adjust(left=0.21, right=0.92, bottom=0.16, top=0.86)
    fig.savefig(path, dpi=220, bbox_inches="tight")
    plt.close(fig)


def default_outputs(analysis_root: Path, paper_root: Path | None = None) -> dict[str, Path]:
    table_root = paper_root or OUTPUT_ROOT
    return {
        "csv": OUTPUT_ROOT / "metadata" / "analysis_data_provenance" / "suggested_stance_diagnostics__latest.csv",
        "json": OUTPUT_ROOT / "metadata" / "analysis_data_provenance" / "suggested_stance_diagnostics__latest.json",
        "tex": table_root / "tables" / "2026" / "stats" / "table_suggested_stance_precision_recall.tex",
        "figure": table_root / "figures" / "images25" / "llms" / "suggested_stance_distribution_by_model.png",
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--analysis-root", type=Path, default=Path("."))
    parser.add_argument("--release", default="current")
    parser.add_argument("--output-csv", type=Path)
    parser.add_argument("--output-json", type=Path)
    parser.add_argument("--output-tex", type=Path)
    parser.add_argument("--figure-output", type=Path)
    parser.add_argument("--matched-output-tex", type=Path)
    parser.add_argument("--matched-min-overlap", type=int, default=30)
    parser.add_argument("--paper-root", type=Path)
    args = parser.parse_args()

    analysis_root = args.analysis_root.resolve()
    paper_root = args.paper_root.resolve() if args.paper_root else None
    outputs = default_outputs(analysis_root, paper_root=paper_root)
    try:
        diagnostics = build_diagnostics(analysis_root, args.release, matched_min_overlap=args.matched_min_overlap)
    except FileNotFoundError as error:
        parser.exit(1, f"Input unavailable: {error}\nPrivate mode requires original analysis inputs. "
                    "Use python run.py human-summary --output-root PATH for the "
                    "published crowdsourcing summary; suggested-stance diagnostics require private data.\n")
    write_metrics_csv(args.output_csv or outputs["csv"], diagnostics.metrics)
    write_json(args.output_json or outputs["json"], diagnostics)
    output_tex = args.output_tex or outputs["tex"]
    write_tex_table(output_tex, winner_matching_table_rows(diagnostics.metrics))
    if args.output_tex is None and paper_root is not None:
        sync_winner_matching_table_caption(paper_root)
    write_distribution_figure(args.figure_output or outputs["figure"], diagnostics.distribution_plot_data)
    if args.matched_output_tex and diagnostics.matched_metrics:
        write_tex_table(args.matched_output_tex, winner_matching_table_rows(diagnostics.matched_metrics))
    print(f"release={args.release}")
    print(f"metrics_rows={len(diagnostics.metrics)}")
    print(f"models={len(diagnostics.model_summary)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
