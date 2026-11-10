import csv
import json
import subprocess
import sys
from pathlib import Path

import tempfile
import unittest

SCRIPT = Path("src/python/dao_bias/reporting/stance_figure.py")


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = sorted({key for row in rows for key in row})
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def make_analysis_root(tmp_path: Path, rows: list[dict[str, object]]) -> Path:
    root = tmp_path / "analysis"
    write_csv(root / "data" / "current.csv", rows)
    write_csv(root / "data" / "candidate.csv", [{"stance": "REJECT"}])
    write_csv(
        root / "metadata" / "analysis_data_releases.csv",
        [
            {
                "release_name": "current",
                "relative_path": "data/current.csv",
                "is_frozen": "FALSE",
                "notes": "test current release",
            },
            {
                "release_name": "candidate",
                "relative_path": "data/candidate.csv",
                "is_frozen": "FALSE",
                "notes": "test candidate release",
            },
        ],
    )
    return root


def run_generator(root: Path, tmp_path: Path) -> tuple[Path, Path]:
    out_json = tmp_path / "stance_agreement.json"
    out_png = tmp_path / "llms.png"
    subprocess.run(
        [
            sys.executable,
            str(SCRIPT),
            "--analysis-root",
            str(root),
            "--release",
            "current",
            "--output-json",
            str(out_json),
            "--figure-output",
            str(out_png),
        ],
        check=True,
    )
    return out_json, out_png


def test_stance_agreement_figure_resolves_current_and_counts_all_plotted_sources(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "stance": "APPROVE",
                "stance_llama": "APPROVE",
                "stance_llama33": "APPROVE",
                "stance_oai": "",
                "stance_chatgpt": "NA",
            },
            {
                "stance": "REJECT",
                "stance_llama": "",
                "stance_llama33": "REJECT",
                "stance_oai": "REJECT",
                "stance_chatgpt": "OTHER",
            },
            {
                "stance": "ABSTAIN",
                "stance_llama": "NA",
                "stance_llama33": "",
                "stance_oai": "N/A",
                "stance_chatgpt": "",
            },
            {
                "stance": "OTHER",
                "stance_llama": "OTHER",
                "stance_oai": "OTHER",
                "stance_chatgpt": "APPROVE",
            },
        ],
    )

    out_json, out_png = run_generator(root, tmp_path)

    payload = json.loads(out_json.read_text(encoding="utf-8"))
    assert payload["release"]["release_name"] == "current"
    assert payload["release"]["relative_path"] == "data/current.csv"
    assert payload["release"]["rows"] == 4
    assert payload["requested_models"] == ["heuristic", "llama", "llama33", "oai", "chatgpt"]
    assert payload["plotted_models"] == ["Heuristic", "Llama 3.1", "Llama 3.3", "OAI", "ChatGPT"]
    assert payload["counts"]["heuristic"] == {
        "ABSTAIN": 1,
        "APPROVE": 1,
        "OTHER": 1,
        "REJECT": 1,
    }
    assert payload["counts"]["llama"] == {
        "ABSTAIN": 0,
        "APPROVE": 1,
        "OTHER": 1,
        "REJECT": 0,
    }
    assert payload["counts"]["llama33"] == {
        "ABSTAIN": 0,
        "APPROVE": 1,
        "OTHER": 0,
        "REJECT": 1,
    }
    assert payload["counts"]["oai"] == {
        "ABSTAIN": 0,
        "APPROVE": 0,
        "OTHER": 1,
        "REJECT": 1,
    }
    assert payload["counts"]["chatgpt"] == {
        "ABSTAIN": 0,
        "APPROVE": 1,
        "OTHER": 1,
        "REJECT": 0,
    }
    assert payload["totals"]["llama33"] == 2
    assert out_png.exists()


def test_stance_agreement_figure_rejects_unknown_non_empty_stance_labels(tmp_path):
    root = make_analysis_root(
        tmp_path,
        [
            {
                "stance": "APPROVE",
                "stance_llama": "APPROVE",
                "stance_llama33": "MAYBE",
                "stance_oai": "APPROVE",
                "stance_chatgpt": "APPROVE",
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
            "current",
            "--output-json",
            str(tmp_path / "stance_agreement.json"),
            "--figure-output",
            str(tmp_path / "llms.png"),
        ],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    assert result.returncode != 0
    assert "Unknown stance label in column stance_llama33" in result.stderr


def final_sample_row(proposal, index, stance="OTHER", **overrides):
    return {
        "proposal": proposal,
        "choice_idx": index,
        "prop_chid": f"{proposal.strip().lower()}{index}",
        "N_choices": 2,
        "flagged": "0",
        "stance": stance,
        "stance_llama": "",
        "stance_llama33": "",
        "stance_oai": "",
        "stance_chatgpt": "",
        **overrides,
    }


class FinalSampleFigureTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.tmp_path = Path(self.directory.name)
        previous_path = sys.path[:]
        self.addCleanup(lambda: sys.path.__setitem__(slice(None), previous_path))
        from dao_bias.reporting.stance_final import build_final_payload
        self.build_payload = build_final_payload

    def test_exclusions_and_source_specific_coverage(self):
        root = make_analysis_root(self.tmp_path, [
            final_sample_row(" Keep ", 1, "APPROVE", stance_llama="APPROVE"),
            final_sample_row("keep", 2, "REJECT", stance_oai="REJECT"),
            final_sample_row("flagged", 1, flagged="TRUE"),
            final_sample_row("flagged", 2),  # Exclude entire proposal.
            final_sample_row("singleton", 1, N_choices=5),
            final_sample_row("reported-single", 1, N_choices=1),
            final_sample_row("reported-single", 2),
            final_sample_row("flagged-single", 1, flagged="1", N_choices=1),
            final_sample_row("other", 1),
            final_sample_row("other", 2, "ABSTAIN"),
        ])
        payload = self.build_payload(root)
        self.assertEqual((payload["sample_rows"], payload["sample_proposals"]), (4, 2))
        self.assertEqual(payload["exclusions"], {
            "snapshot_flagged": {"proposals": 2, "choices": 3},
            "remaining_single_option": {"proposals": 2, "choices": 3},
        })
        self.assertEqual(payload["counts"]["heuristic"], {
            "APPROVE": 1, "REJECT": 1, "ABSTAIN": 1, "OTHER": 1,
        })
        self.assertEqual(payload["totals"], {
            "heuristic": 4, "llama": 1, "llama33": 0, "oai": 1, "chatgpt": 0,
        })

    def test_invalid_choice_coverage(self):
        cases = [
            ({"choice_idx": 1, "prop_chid": "keep1"}, "Duplicate proposal-choice"),
            ({"stance": "NA"}, "one heuristic label per choice"),
            ({"prop_chid": "wrong"}, "prop_chid does not match"),
        ]
        for overrides, error in cases:
            with self.subTest(overrides=overrides):
                root = make_analysis_root(self.tmp_path, [
                    final_sample_row("keep", 1, "APPROVE"),
                    {**final_sample_row("keep", 2, "REJECT"), **overrides},
                ])
                with self.assertRaisesRegex(ValueError, error):
                    self.build_payload(root)
