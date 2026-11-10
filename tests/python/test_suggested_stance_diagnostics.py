import csv
import json
import subprocess
import sys
from pathlib import Path


SCRIPT = Path("src/python/dao_bias/reporting/suggested_stance.py")
SUGGESTED_STANCE_WINNER_TABLE_CAPTION = (
    r"\caption{\textbf{Detected suggested stance compared with the winning stance.} "
    r"For each source, we treat the detected suggested stance as a prediction "
    r"(column Pred.) and we count how many times the prediction was actually true "
    r"(column TP) over all the time stance a given stance won (column Winner). "
    r"With this framework, we can compute precision, recall, and F1 to capture "
    r"alignment between textual suggestions and realized voting outcomes.}"
)


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def make_analysis_root(
    tmp_path: Path,
    rows: list[dict[str, object]],
    *,
    include_current: bool = False,
) -> Path:
    root = tmp_path / "analysis"
    release_path = root / "data" / "candidate.csv"
    write_csv(release_path, rows)
    release_rows = [
        {
            "release_name": "candidate",
            "relative_path": "data/candidate.csv",
            "is_frozen": "FALSE",
            "notes": "test candidate",
        }
    ]
    if include_current:
        current_path = root / "data" / "current.csv"
        write_csv(current_path, rows)
        release_rows.insert(0, {
            "release_name": "current",
            "relative_path": "data/current.csv",
            "is_frozen": "FALSE",
            "notes": "test current",
        })
    write_csv(root / "metadata" / "analysis_data_releases.csv", release_rows)
    return root


def run_diagnostics(
    root: Path,
    tmp_path: Path,
    *,
    release: str | None = "candidate",
) -> tuple[Path, Path, Path, Path]:
    out_csv = tmp_path / "diagnostics.csv"
    out_json = tmp_path / "diagnostics.json"
    out_tex = tmp_path / "table.tex"
    out_png = tmp_path / "distribution.png"
    command = [
        sys.executable,
        str(SCRIPT),
        "--analysis-root",
        str(root),
        "--output-csv",
        str(out_csv),
        "--output-json",
        str(out_json),
        "--output-tex",
        str(out_tex),
        "--figure-output",
        str(out_png),
    ]
    if release is not None:
        command.extend(["--release", release])
    subprocess.run(command, check=True)
    return out_csv, out_json, out_tex, out_png


def test_suggested_stance_diagnostics_use_release_alias_and_model_denominators(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "proposal": "p1",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_llama": "APPROVE",
                "suggested_stance_llama33": "APPROVE",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p1",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_llama": "APPROVE",
                "suggested_stance_llama33": "APPROVE",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p2",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE_LIKE",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p2",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE_LIKE",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p3",
                "choice_idx": 1,
                "type": "basic",
                "stance": "ABSTAIN",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "",
                "source_file_llama33": "",
                "llama33_reviewed": "FALSE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p4",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "OTHER",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "OTHER",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "OTHER",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "TRUE",
            },
            {
                "proposal": "p5",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "",
                "source_file_llama33": "",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p5",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "",
                "suggested_stance_llama": "",
                "suggested_stance_llama33": "",
                "source_file_llama33": "",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p-observed-diff",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_llama": "APPROVE",
                "suggested_stance_llama33": "APPROVE",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
            {
                "proposal": "p-observed-diff",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_llama": "APPROVE",
                "suggested_stance_llama33": "APPROVE",
                "source_file_llama33": "llama33.csv",
                "llama33_reviewed": "TRUE",
                "llama33_label_conflict": "FALSE",
            },
        ],
    )

    out_csv, out_json, out_tex, out_png = run_diagnostics(root, tmp_path)

    rows = list(csv.DictReader(out_csv.open(newline="", encoding="utf-8")))
    oai_approve = next(row for row in rows if row["model_key"] == "oai" and row["stance"] == "APPROVE")
    llama33_approve = next(row for row in rows if row["model_key"] == "llama33" and row["stance"] == "APPROVE")

    assert oai_approve["predicted_positive_proposals"] == "3"
    assert oai_approve["winner_positive_proposals"] == "2"
    assert oai_approve["true_positive_proposals"] == "2"
    assert oai_approve["precision"] == "0.666667"
    assert oai_approve["recall"] == "1.000000"
    assert oai_approve["f1"] == "0.800000"
    assert llama33_approve["predicted_positive_proposals"] == "2"
    assert llama33_approve["winner_positive_proposals"] == "3"
    assert llama33_approve["true_positive_proposals"] == "2"
    assert llama33_approve["precision"] == "1.000000"
    assert llama33_approve["recall"] == "0.666667"
    assert llama33_approve["f1"] == "0.800000"

    payload = json.loads(out_json.read_text(encoding="utf-8"))
    llama33 = payload["models"]["llama33"]
    assert llama33["reviewed_proposals"] == 4
    assert llama33["not_reviewed_proposals"] == 0
    assert llama33["no_suggestion_proposals"] == 2
    assert llama33["conflict_proposals"] == 0
    assert payload["label_normalization"]["APPROVE_LIKE"]["canonical"] == "APPROVE"
    assert payload["release"]["relative_path"] == "data/candidate.csv"
    assert payload["release"]["winner_column"] == "choice_won"
    assert payload["release"]["rows"] == 8
    assert payload["release"]["proposals"] == 4
    llama33_plot_rows = [
        row for row in payload["distribution_plot_data"]
        if row["model_key"] == "llama33"
    ]
    assert [row["state"] for row in llama33_plot_rows] == [
        "no_suggestion",
        "APPROVE",
        "REJECT",
        "ABSTAIN",
        "OTHER",
    ]
    no_suggestion_row = next(row for row in llama33_plot_rows if row["state"] == "no_suggestion")
    assert no_suggestion_row["count"] == 2
    assert no_suggestion_row["share"] == 2 / 4
    tex = out_tex.read_text(encoding="utf-8")
    assert tex.startswith("% Generated by dao_bias.reporting.suggested_stance")
    assert "\\begin{tabularx}{\\linewidth}" in tex
    assert "Precision & Recall & F1" in tex
    assert (
        "Llama 3.3 & APPROVE & 2 & 3 & 2 & "
        "\\multicolumn{3}{c}{Sparse ($n<30$)}"
    ) in tex
    assert "Sparse rows retain counts but mark metric cells unstable" in tex
    assert b"\r\n" not in out_csv.read_bytes()
    assert out_png.exists()


def test_suggested_stance_diagnostics_defaults_to_current_release(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "proposal": "p1",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE",
            },
            {
                "proposal": "p1",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE",
            },
        ],
        include_current=True,
    )
    _, out_json, _, _ = run_diagnostics(root, tmp_path, release=None)
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["release"]["release_name"] == "current"
    assert payload["release"]["relative_path"] == "data/current.csv"


def test_winner_matching_table_excludes_human_validation_rows(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "proposal": "p1",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_hl": "APPROVE",
            },
            {
                "proposal": "p1",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE",
                "suggested_stance_hl": "APPROVE",
            },
        ],
    )

    out_csv, out_json, out_tex, _ = run_diagnostics(root, tmp_path)

    rows = list(csv.DictReader(out_csv.open(newline="", encoding="utf-8")))
    assert any(row["model_key"] == "human" for row in rows)
    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert "human" in payload["models"]
    tex = out_tex.read_text(encoding="utf-8")
    assert "OpenAI GPT-4o & APPROVE" in tex
    assert "human labels" not in tex


def test_suggested_stance_diagnostics_syncs_paper_caption_for_default_tex_output(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "proposal": "p1",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "APPROVE",
            },
            {
                "proposal": "p1",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "APPROVE",
            },
        ],
    )
    paper_root = tmp_path / "paper"
    appendix = paper_root / "sections" / "Data_Appendix.tex"
    appendix.parent.mkdir(parents=True)
    appendix.write_text(
        "\n".join([
            "Before.",
            r"\begin{table}[ht]",
            r"\centering",
            r"\input{tables/2026/stats/table_suggested_stance_precision_recall.tex}",
            r"\caption{Old generated caption.}",
            r"\label{tab:stance_suggest_recall_precision_f1}",
            r"\end{table}",
            "After.",
            "",
        ]),
        encoding="utf-8",
    )

    subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--analysis-root",
            str(root),
            "--paper-root",
            str(paper_root),
            "--release",
            "candidate",
            "--output-csv",
            str(tmp_path / "diagnostics.csv"),
            "--output-json",
            str(tmp_path / "diagnostics.json"),
            "--figure-output",
            str(tmp_path / "distribution.png"),
        ],
        check=True,
    )

    appendix_text = appendix.read_text(encoding="utf-8")
    assert SUGGESTED_STANCE_WINNER_TABLE_CAPTION in appendix_text
    assert "Old generated caption" not in appendix_text
    assert (
        paper_root
        / "tables"
        / "2026"
        / "stats"
        / "table_suggested_stance_precision_recall.tex"
    ).exists()


def test_suggested_stance_diagnostics_reject_unknown_labels(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "proposal": "p1",
                "choice_idx": 1,
                "type": "single-choice",
                "stance": "APPROVE",
                "choice_won": 1,
                "choice_won_no_author_vp": 1,
                "suggested_stance_oai": "MAYBE",
            },
            {
                "proposal": "p1",
                "choice_idx": 2,
                "type": "single-choice",
                "stance": "REJECT",
                "choice_won": 0,
                "choice_won_no_author_vp": 0,
                "suggested_stance_oai": "MAYBE",
            },
        ],
    )
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--analysis-root",
            str(root),
            "--release",
            "candidate",
            "--output-csv",
            str(tmp_path / "diagnostics.csv"),
            "--output-json",
            str(tmp_path / "diagnostics.json"),
            "--output-tex",
            str(tmp_path / "table.tex"),
            "--figure-output",
            str(tmp_path / "distribution.png"),
        ],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    assert result.returncode != 0
    assert "Unknown suggested-stance label" in result.stderr
