"""Synthetic CLI coverage for the manuscript table generator; no paper/data needed."""
import csv
import subprocess
import sys
from pathlib import Path


def test_descriptive_table_cli_applies_sample_guards_and_choice_denominators(tmp_path):
    root = tmp_path / "analysis"
    (root / "metadata").mkdir(parents=True)
    (root / "metadata/analysis_data_releases.csv").write_text(
        "release_name,relative_path\ncurrent,choices.csv\n"
    )
    rows = []
    for proposal, shares, flagged in [
        ("a", [0.8, 0.2], 0), ("b", [0.6, 0.3, 0.1], 0),
        ("flagged", [0.99, 0.01], 1), ("single", [1.0], 0),
    ]:
        for index, share in enumerate(shares, 1):
            rows.append(dict(proposal=proposal, choice_idx=index, choice_idx10=index,
                N_choices=len(shares), type="single-choice", flagged=flagged,
                choice_share_2=share, n_votes_share_2=share,
                is_infavor=int(index==1), is_against=int(index==2),
                is_abstain=0, is_other=int(index>2),
                is_author_choice=int((proposal=="a" and index==2) or
                                     (proposal=="b" and index==1))))
    data = root / "choices.csv"
    with data.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    original = data.read_bytes()
    output = tmp_path / "paper-output"
    script = Path(__file__).resolve().parents[2] / "src/python/dao_bias/reporting/descriptive_tables.py"
    result = subprocess.run([sys.executable, str(script), "--analysis-root", str(root),
        "--paper-root", str(output)], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    table = (output / "tables/2026/stats/table_stat_n_votes.tex").read_text()
    # Five retained choice rows: position 1 has N=2 (40%), mean .70, sample SD .14.
    assert r"1 & \num{0.70} & \num{0.14} & \num{0.70} & \num{0.14} & \num{40.00} & 2" in table
    assert data.read_bytes() == original
