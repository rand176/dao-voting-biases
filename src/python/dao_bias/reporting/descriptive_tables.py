from __future__ import annotations
from dao_bias.data.paths import PACKAGE_ROOT

from dao_bias.data.input_paths import prepared_analysis_input

import argparse
import csv
import math
from collections import Counter, defaultdict
from pathlib import Path


SCRIPT_PATH = Path(__file__).resolve()
ANALYSIS_ROOT = PACKAGE_ROOT
PAPER_ROOT = ANALYSIS_ROOT / "latex_paper"
RELEASE_REGISTRY = Path("metadata/analysis_data_releases.csv")
RANK_ORDER = [str(value) for value in range(1, 11)] + ["10+"]
VOTING_TYPE_ORDER = [
    ("basic", "Basic"),
    ("single-choice", "Single-choice"),
    ("approval", "Approval"),
    ("quadratic", "Quadratic"),
    ("ranked-choice", "Ranked-choice"),
    ("weighted", "Weighted"),
]
FIRST_CHOICE_BY_TYPE_CAPTION = (
    r"\caption{\textbf{Voting power, frequency of approval and author choices of the first choice by voting type.} "
    r"The sample is the final cleaned dataset with \DaoBiasFinalChoiceN{} choices.}"
)


def truthy(value: str | None) -> bool:
    return str(value or "").strip().lower() in {"1", "true", "t", "yes", "y"}


def optional_float(value: str | int | float | None) -> float | None:
    if value is None:
        return None
    text = str(value).strip()
    if not text or text.upper() == "NA":
        return None
    try:
        out = float(text)
    except ValueError:
        return None
    return out if math.isfinite(out) else None


def required_float(row: dict[str, str], column: str) -> float:
    value = optional_float(row.get(column))
    if value is None:
        raise ValueError(f"Missing numeric value in column {column!r}")
    return value


def fmt_num(value: float) -> str:
    return f"\\num{{{value:.2f}}}"


def fmt_n(value: int) -> str:
    return f"{value:,}"


def read_release_registry(analysis_root: Path) -> dict[str, Path]:
    path = prepared_analysis_input(RELEASE_REGISTRY, analysis_root)
    with path.open(newline="", encoding="utf-8") as handle:
        return {
            row["release_name"]: Path(row["relative_path"])
            for row in csv.DictReader(handle)
            if row.get("release_name") and row.get("relative_path")
        }


def current_release_path(analysis_root: Path, release: str) -> Path:
    registry = read_release_registry(analysis_root)
    if release not in registry:
        known = ", ".join(sorted(registry)) or "none"
        raise KeyError(f"Unknown release {release!r}. Known releases: {known}")
    path = prepared_analysis_input(registry[release], analysis_root)
    if not path.exists():
        raise FileNotFoundError(path)
    return path


def read_choice_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        rows = [row for row in csv.DictReader(handle) if (row.get("proposal") or "").strip()]

    proposal_counts = Counter((row["proposal"] or "").strip().lower() for row in rows)
    flagged = {
        (row["proposal"] or "").strip().lower()
        for row in rows
        if truthy(row.get("flagged"))
    }
    reported_single_option = {
        (row["proposal"] or "").strip().lower()
        for row in rows
        if (optional_float(row.get("N_choices")) or math.inf) <= 1
    }
    single_option = {proposal for proposal, count in proposal_counts.items() if count <= 1}
    excluded = flagged | reported_single_option | single_option

    return [
        row
        for row in rows
        if (row["proposal"] or "").strip().lower() not in excluded
    ]


def mean_sd(values: list[float]) -> tuple[float, float]:
    if not values:
        return 0.0, 0.0
    mean = sum(values) / len(values)
    if len(values) == 1:
        return mean, 0.0
    variance = sum((value - mean) ** 2 for value in values) / (len(values) - 1)
    return mean, math.sqrt(variance)


def grouped_summary(
    rows: list[dict[str, str]],
    variables: list[tuple[str, str, bool]],
) -> dict[str, dict[str, object]]:
    grouped: dict[str, dict[str, list[float]]] = defaultdict(
        lambda: {name: [] for name, _, _ in variables}
    )
    counts: Counter[str] = Counter()

    for row in rows:
        rank = (row.get("choice_idx10") or row.get("choice_idx") or "").strip()
        counts[rank] += 1
        for name, column, allow_missing in variables:
            value = optional_float(row.get(column)) if allow_missing else required_float(row, column)
            if value is not None:
                grouped[rank][name].append(value)

    total = sum(counts.values())
    out: dict[str, dict[str, object]] = {}
    for rank in RANK_ORDER:
        if counts[rank] == 0:
            continue
        row_summary: dict[str, object] = {
            "pct": counts[rank] / total * 100 if total else 0.0,
            "n": counts[rank],
        }
        for name, _, _ in variables:
            mean, sd = mean_sd(grouped[rank][name])
            row_summary[f"{name}_mean"] = mean
            row_summary[f"{name}_sd"] = sd
        out[rank] = row_summary
    return out


def row_cells(rank: str, stats: dict[str, object], variables: list[str]) -> str:
    cells = [rank]
    for variable in variables:
        cells.extend([
            fmt_num(float(stats[f"{variable}_mean"])),
            fmt_num(float(stats[f"{variable}_sd"])),
        ])
    cells.extend([fmt_num(float(stats["pct"])), fmt_n(int(stats["n"]))])
    return " & ".join(cells) + r" \\"


def grouped_summary_by_type(
    rows: list[dict[str, str]],
    variables: list[tuple[str, str, bool]],
) -> dict[str, dict[str, object]]:
    grouped: dict[str, dict[str, list[float]]] = defaultdict(
        lambda: {name: [] for name, _, _ in variables}
    )
    counts: Counter[str] = Counter()

    for row in rows:
        vote_type = (row.get("type") or "").strip()
        if not vote_type:
            continue
        counts[vote_type] += 1
        for name, column, allow_missing in variables:
            value = optional_float(row.get(column)) if allow_missing else required_float(row, column)
            if value is not None:
                grouped[vote_type][name].append(value)

    total = sum(counts.values())
    out: dict[str, dict[str, object]] = {}
    for vote_type, _ in VOTING_TYPE_ORDER:
        if counts[vote_type] == 0:
            continue
        row_summary: dict[str, object] = {
            "pct": counts[vote_type] / total * 100 if total else 0.0,
            "n": counts[vote_type],
        }
        for name, _, _ in variables:
            mean, sd = mean_sd(grouped[vote_type][name])
            row_summary[f"{name}_mean"] = mean
            row_summary[f"{name}_sd"] = sd
        out[vote_type] = row_summary
    return out


def type_row_cells(label: str, stats: dict[str, object], variables: list[str]) -> str:
    cells = [label]
    for variable in variables:
        cells.extend([
            fmt_num(float(stats[f"{variable}_mean"])),
            fmt_num(float(stats[f"{variable}_sd"])),
        ])
    cells.extend([fmt_num(float(stats["pct"])), fmt_n(int(stats["n"]))])
    return " & ".join(cells) + r" \\"


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    print(f"Wrote {path}")


def choice_position_table(rows: list[dict[str, str]]) -> str:
    variables = [
        ("vp", "choice_share_2", False),
        ("votes", "n_votes_share_2", False),
    ]
    summary = grouped_summary(rows, variables)
    body = "\n".join(row_cells(rank, summary[rank], ["vp", "votes"]) for rank in summary)
    hline_bottom = len(summary) + 3
    return f"""\\begin{{table}}
\\centering
\\begin{{tblr}}{{
colspec={{
Q[l]
Q[r, colsep=6pt]
Q[r, colsep=6pt]
Q[r, colsep=20pt]
Q[r, colsep=6pt]
Q[r, colsep=10pt]
Q[r]
}},
hline{{1}}={{1-7}}{{solid, black, 0.1em}},
hline{{2}}={{2-3}}{{solid, black, 0.05em}},
hline{{2}}={{4-5}}{{solid, black, 0.05em}},
hline{{3}}={{1-7}}{{solid, black, 0.05em}},
hline{{{hline_bottom}}}={{1-7}}{{solid, black, 0.1em}},
cell{{1}}{{2}}={{c=2}}{{halign=c}},
cell{{1}}{{4}}={{c=2}}{{halign=c}},
}}
& Voting Power &  & Number of votes &  & Percent & N \\\\
Choice Pos. & Mean & SD & Mean & SD &  &  \\\\
{body}
\\end{{tblr}}
\\caption{{\\textbf{{Choice position, voting-power share, and individual-vote share.}} The sample is the paper's final cleaned Snapshot proposal-choice dataset: \\DaoBiasFinalChoiceN{{}} choices from \\DaoBiasFinalProposalN{{}} final proposals in \\DaoBiasFinalSpaceN{{}} DAO spaces, observed from Oct.~2020 to Nov.~2023, after excluding pending or invalid proposals, proposals with fewer than two choices, malformed or unavailable-choice votes, and Snapshot-flagged proposals. Outcomes are mean choice-level voting-power share and mean favorite-choice vote share by choice position; N indicates the number of choice rows at each position, with positions above 10 grouped as 10+.}}
\\label{{table_stats_vp_n}}
\\end{{table}}
"""


def overlap_table(rows: list[dict[str, str]]) -> str:
    variables = [
        ("vp", "choice_share_2", False),
        ("approve", "is_infavor", False),
        ("author", "is_author_choice", True),
    ]
    all_summary = grouped_summary(rows, variables)
    upto3_rows = [row for row in rows if required_float(row, "N_choices") <= 3]
    upto3_summary = grouped_summary(upto3_rows, variables)

    panel_a = "\n".join(row_cells(rank, all_summary[rank], ["vp", "approve", "author"]) for rank in all_summary)
    panel_b = "\n".join(
        row_cells(rank, upto3_summary[rank], ["vp", "approve", "author"])
        for rank in ["1", "2", "3"]
        if rank in upto3_summary
    )
    return f"""\\begin{{table}}[!tbp]
\\centering
\\caption{{\\textbf{{Overlap among position, approval, and author-choice indicators}}. Mean voting-power share, approval indicator, and author-choice indicator by choice rank. Panel A uses all proposals in the active choice-level data after release guards; Panel B restricts the same statistics to proposals with up to three choices. Author-choice means omit rows without reconstructed author-choice information.}}
\\label{{table_stats_3biases}}
\\small
\\setlength{{\\tabcolsep}}{{3pt}}
\\renewcommand{{\\arraystretch}}{{1.08}}
\\begin{{tabular*}}{{\\linewidth}}{{@{{\\extracolsep{{\\fill}}}}lrrrrrrrr@{{}}}}
\\toprule
& \\multicolumn{{2}}{{c}}{{Voting power}} & \\multicolumn{{2}}{{c}}{{Approve}} & \\multicolumn{{2}}{{c}}{{Author choice}} & \\multicolumn{{2}}{{c}}{{All choices}} \\\\
\\cmidrule(lr){{2-3}}\\cmidrule(lr){{4-5}}\\cmidrule(lr){{6-7}}\\cmidrule(l){{8-9}}
Choice rank & Mean & SD & Mean & SD & Mean & SD & Share (\\%) & N \\\\
\\midrule
\\multicolumn{{9}}{{@{{}}l}}{{\\textit{{Panel A: all proposals}}}} \\\\
{panel_a}
\\addlinespace[0.35em]
\\multicolumn{{9}}{{@{{}}l}}{{\\textit{{Panel B: proposals with up to three choices}}}} \\\\
{panel_b}
\\bottomrule
\\end{{tabular*}}
\\end{{table}}
    """


def first_choice_by_type_table(rows: list[dict[str, str]]) -> str:
    variables = [
        ("vp", "choice_share_2", False),
        ("approve", "is_infavor", False),
        ("author", "is_author_choice", True),
    ]
    first_choice_rows = [row for row in rows if (row.get("choice_idx10") or "").strip() == "1"]
    summary = grouped_summary_by_type(first_choice_rows, variables)
    body_rows = []
    for vote_type, label in VOTING_TYPE_ORDER:
        if vote_type not in summary:
            continue
        body_rows.append(type_row_cells(label, summary[vote_type], ["vp", "approve", "author"]))
        if vote_type == "single-choice":
            body_rows.append(r"\hline")
    body = "\n".join(body_rows)
    return f"""\\begin{{table}}
\\centering
\\begin{{tblr}}[         %% tabularray outer open
]                     %% tabularray outer close
{{                     %% tabularray inner open
colspec={{Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]}},
hline{{2}}={{9}}{{solid, black, 0.05em}},
hline{{3}}={{1-9}}{{solid, black, 0.05em}},
hline{{2}}={{2,4,6,8}}{{solid, black, 0.05em, l=-0.5}},
hline{{2}}={{3,5,7}}{{solid, black, 0.05em, r=-0.5}},
hline{{1}}={{1-9}}{{solid, black, 0.1em}},
hline{{9}}={{1-9}}{{solid, black, 0.1em}},
cell{{1}}{{1}}={{}}{{halign=c}},
cell{{1}}{{2}}={{c=2}}{{halign=c}},
cell{{1}}{{3}}={{}}{{halign=c}},
cell{{1}}{{4}}={{c=2}}{{halign=c}},
cell{{1}}{{5}}={{}}{{halign=c}},
cell{{1}}{{6}}={{c=2}}{{halign=c}},
cell{{1}}{{7}}={{}}{{halign=c}},
cell{{1}}{{8}}={{c=2}}{{halign=c}},
cell{{1}}{{9}}={{}}{{halign=c}},
cell{{2-8}}{{1}}={{}}{{halign=l}},
cell{{2-8}}{{2}}={{}}{{halign=r}},
cell{{2-8}}{{3}}={{}}{{halign=r}},
cell{{2-8}}{{4}}={{}}{{halign=r}},
cell{{2-8}}{{5}}={{}}{{halign=r}},
cell{{2-8}}{{6}}={{}}{{halign=r}},
cell{{2-8}}{{7}}={{}}{{halign=r}},
cell{{2-8}}{{8}}={{}}{{halign=r}},
cell{{2-8}}{{9}}={{}}{{halign=r}},
}}                     %% tabularray inner close
& Voting Power &  & Approve &  & Author Choice &  & All &  \\\\
type & Mean & SD & Mean & SD & Mean & SD & Percent & N \\\\
{body}
\\end{{tblr}}
{FIRST_CHOICE_BY_TYPE_CAPTION}
\\label{{table_stats_3biases_bytype_firstchoice}}
\\end{{table}}
"""


def stance_choice_table(rows: list[dict[str, str]]) -> str:
    variables = [
        ("approve", "is_infavor", False),
        ("reject", "is_against", False),
        ("abstain", "is_abstain", False),
        ("other", "is_other", False),
    ]
    summary = grouped_summary(rows, variables)
    body = "\n".join(row_cells(rank, summary[rank], ["approve", "reject", "abstain", "other"]) for rank in summary)
    hline_bottom = len(summary) + 3
    return f"""\\begin{{table}}
\\centering
\\begin{{tblr}}{{
colspec={{Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]}},
hline{{2}}={{11}}{{solid, black, 0.05em}},
hline{{3}}={{1-11}}{{solid, black, 0.05em}},
hline{{1}}={{1-11}}{{solid, black, 0.1em}},
hline{{{hline_bottom}}}={{1-11}}{{solid, black, 0.1em}},
cell{{1}}{{2}}={{c=2}}{{halign=c}},
cell{{1}}{{4}}={{c=2}}{{halign=c}},
cell{{1}}{{6}}={{c=2}}{{halign=c}},
cell{{1}}{{8}}={{c=2}}{{halign=c}},
cell{{1}}{{10}}={{c=2}}{{halign=c}},
cell{{2-{hline_bottom - 1}}}{{1}}={{}}{{halign=l}},
cell{{2-{hline_bottom - 1}}}{{2-11}}={{}}{{halign=r}},
}}
& Approve &  & Reject &  & Abstain &  & Other &  & All &  \\\\
Choice Position & Mean & SD & Mean & SD & Mean & SD & Mean & SD & Percent & N \\\\
{body}
\\end{{tblr}}
\\caption{{\\textbf{{Stance distribution by choice position}}. The sample is the active Snapshot choice-level analytic dataset after release guards. Outcomes are mean stance indicators by choice position; the final columns report each position's share of all choice rows and row count.}}
\\label{{table_stats_stance_choice_idx}}
\\end{{table}}
"""


def stance_author_table(rows: list[dict[str, str]]) -> str:
    variables = [
        ("vp", "choice_share_2", False),
        ("approve", "is_infavor", False),
        ("reject", "is_against", False),
        ("abstain", "is_abstain", False),
        ("other", "is_other", False),
        ("author", "is_author_choice", True),
    ]
    summary = grouped_summary(rows, variables)
    body_rows = []
    for rank in summary:
        stats = summary[rank]
        cells = [rank]
        cells.extend([fmt_num(float(stats["vp_mean"])), fmt_num(float(stats["vp_sd"]))])
        cells.extend([fmt_num(float(stats["pct"])), fmt_n(int(stats["n"]))])
        for variable in ["approve", "reject", "abstain", "other", "author"]:
            cells.extend([fmt_num(float(stats[f"{variable}_mean"])), fmt_num(float(stats[f"{variable}_sd"]))])
        body_rows.append(" & ".join(cells) + r" \\")
    body = "\n".join(body_rows)
    hline_bottom = len(summary) + 3
    return f"""\\begin{{table}}
\\centering
\\begin{{tblr}}{{
colspec={{Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]Q[]}},
hline{{2}}={{3-4,15}}{{solid, black, 0.05em}},
hline{{3}}={{1-15}}{{solid, black, 0.05em}},
hline{{1}}={{1-15}}{{solid, black, 0.1em}},
hline{{{hline_bottom}}}={{1-15}}{{solid, black, 0.1em}},
cell{{1}}{{2}}={{c=4}}{{halign=c}},
cell{{1}}{{6}}={{c=2}}{{halign=c}},
cell{{1}}{{8}}={{c=2}}{{halign=c}},
cell{{1}}{{10}}={{c=2}}{{halign=c}},
cell{{1}}{{12}}={{c=2}}{{halign=c}},
cell{{1}}{{14}}={{c=2}}{{halign=c}},
cell{{2-{hline_bottom - 1}}}{{1}}={{}}{{halign=l}},
cell{{2-{hline_bottom - 1}}}{{2-15}}={{}}{{halign=r}},
}}
& Voting Power &  &  &  & Approve &  & Reject &  & Abstain &  & Other &  & Author Choice &  \\\\
\\# & Mean & SD & Percent & N & Mean & SD & Mean & SD & Mean & SD & Mean & SD & Mean & SD \\\\
{body}
\\end{{tblr}}
\\caption{{\\textbf{{Choice position, stance, and author selection.}} The sample is the active Snapshot choice-level analytic dataset after release guards. Outcomes are mean voting-power share, stance indicators, author-choice indicator, and row counts by choice position. Author-choice means omit rows without reconstructed author-choice information.}}
\\label{{table_stats_stance_author}}
\\end{{table}}
"""


def write_tables(release: str, analysis_root: Path, paper_root: Path, output_dir: Path | None = None) -> None:
    target = output_dir or paper_root / "tables/2026/stats"
    rows = read_choice_rows(current_release_path(analysis_root, release))
    write_text(
        target / "table_stat_n_votes.tex",
        choice_position_table(rows),
    )
    write_text(
        target / "table_stats_3biases.tex",
        overlap_table(rows),
    )
    write_text(
        target / "table_stats_3biases_bytype_firstchoice.tex",
        first_choice_by_type_table(rows),
    )
    write_text(
        target / "table_stance_choice_idx.tex",
        stance_choice_table(rows),
    )
    write_text(
        target / "table_stance_and_author.tex",
        stance_author_table(rows),
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--release", default="current")
    parser.add_argument("--analysis-root", type=Path, default=ANALYSIS_ROOT)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--paper-root", type=Path, default=PAPER_ROOT)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    write_tables(args.release, args.analysis_root, args.paper_root, args.output_dir)


if __name__ == "__main__":
    main()
