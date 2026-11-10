import csv
import subprocess
import sys
from pathlib import Path

import pytest




def sample_rows():
    # The published combined schema is an independent fixture contract.
    fields = ["proposal", "title", "body", "choice", "title_lang", "body_lang",
              "body_mod_len", "title_mod_len", "cluster_id", "choices_len",
              "choices_json", "body_mod_len_n", "choice_idx", "prop_chid"]
    fields += [f"{prefix}_{i}" for prefix in ("eval", "eval_t", "eval_issue", "eval_suggest") for i in range(1, 5)]
    rows = []
    for proposal, index, count in [("a", 2, 2), ("b", 1, 1), ("a", 1, 2)]:
        row = dict.fromkeys(fields, "")
        row.update(proposal=proposal, choice_idx=str(index), prop_chid=f"{proposal}{index}",
                   title="Shared title", body="Text, with\nnewlines", title_lang="en", body_lang="en",
                   choices_len=str(count), choice=f"Choice {index}", eval_1="Approve", eval_t_1="12.50",
                   choices_json=repr([f"Choice {i}" for i in range(1, count + 1)]), eval_suggest_1="N/A")
        rows.append(row)
    return rows


def write_csv(path, rows, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields or list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


def test_split_export_roundtrips_order_values_and_missingness(tmp_path):
    source, destination = tmp_path / "input.csv", tmp_path / "public_data/humanlabels"
    rows = sample_rows()
    write_csv(source, rows)
    original = source.read_bytes()
    result = subprocess.run([sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/preparation/redact_human.py")),
                             str(source), str(destination), "--split"], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    from dao_bias.data.public import load_public_crowdsourcing
    assert load_public_crowdsourcing(tmp_path) == rows
    with (destination / "dao_rnd_proposals.csv").open(newline="") as handle:
        proposals = list(csv.DictReader(handle))
    assert [r["proposal"] for r in proposals] == ["a", "b"]
    assert set(proposals[0]) == {"proposal", "title", "body", "title_lang", "body_lang",
                                 "body_mod_len", "title_mod_len", "cluster_id", "choices_len",
                                 "choices_json", "body_mod_len_n"}
    with (destination / "dao_rnd_choice_labels.csv").open(newline="") as handle:
        labels = list(csv.DictReader(handle))
    assert [r["prop_chid"] for r in labels] == ["a2", "b1", "a1"]
    assert "body" not in labels[0] and labels[0]["eval_4"] == ""
    assert source.read_bytes() == original
    published = {p.name: p.read_bytes() for p in destination.iterdir()}
    repeated = subprocess.run([sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/preparation/redact_human.py")),
                               str(source), str(destination), "--split"], capture_output=True, text=True)
    assert repeated.returncode != 0
    assert {p.name: p.read_bytes() for p in destination.iterdir()} == published


@pytest.mark.parametrize("fault", [
    "duplicate_proposal", "duplicate_choice", "partial_labels", "missing_middle_label", "orphan_choice", "unused_proposal", "bad_key",
    "bad_count", "zero_count", "bad_index", "zero_index", "fractional_index", "blank_proposal",
    "missing_column", "unexpected_column", "duplicate_header",
    "malformed_record", "empty_labels", "missing_proposals", "missing_labels",
])
def test_public_summary_rejects_invalid_split_tables_without_fallback(tmp_path, fault):
    from dao_bias.data.public import PROPOSAL_COLUMNS, LABEL_COLUMNS
    directory = tmp_path / "public_data/humanlabels"
    source_rows = sample_rows()
    proposals = [{k: r[k] for k in PROPOSAL_COLUMNS} for r in source_rows[:2]]
    labels = [{k: r[k] for k in LABEL_COLUMNS} for r in source_rows]
    proposal_fields, label_fields = list(PROPOSAL_COLUMNS), list(LABEL_COLUMNS)
    if fault == "duplicate_proposal": proposals.append(proposals[0].copy())
    elif fault == "duplicate_choice": labels.append(labels[0].copy())
    elif fault == "orphan_choice": labels[0]["proposal"] = "unknown"
    elif fault == "unused_proposal": labels = [r for r in labels if r["proposal"] != "b"]
    elif fault in {"partial_labels", "missing_middle_label"}:
        proposals[0]["choices_len"] = "3"
        proposals[0]["choices_json"] = "['Choice 1', 'Choice 2', 'Choice 3']"
        if fault == "missing_middle_label":
            labels[0]["choice_idx"] = "3"
            labels[0]["prop_chid"] = "a3"
    elif fault == "bad_key": labels[0]["prop_chid"] = "wrong"
    elif fault == "bad_count": proposals[0]["choices_len"] = "broken"
    elif fault == "zero_count": proposals[0]["choices_len"] = "0"
    elif fault == "bad_index": labels[0]["choice_idx"] = "3"
    elif fault == "zero_index": labels[0]["choice_idx"] = "0"
    elif fault == "fractional_index": labels[0]["choice_idx"] = "1.5"
    elif fault == "blank_proposal": proposals[0]["proposal"] = ""
    elif fault == "missing_column":
        proposal_fields.remove("body")
        for r in proposals: del r["body"]
    elif fault == "unexpected_column":
        label_fields.append("participant_id")
        for r in labels: r["participant_id"] = "synthetic"
    elif fault == "duplicate_header": label_fields.append("choice_idx")
    elif fault == "empty_labels": labels = []
    write_csv(directory / "dao_rnd_proposals.csv", proposals, proposal_fields)
    write_csv(directory / "dao_rnd_choice_labels.csv", labels, label_fields)
    if fault == "malformed_record":
        with (directory / "dao_rnd_choice_labels.csv").open("a") as handle: handle.write("too,few,columns\n")
    if fault == "missing_proposals": (directory / "dao_rnd_proposals.csv").unlink()
    if fault == "missing_labels": (directory / "dao_rnd_choice_labels.csv").unlink()
    # Neither the old public CSV nor private data may rescue a broken split.
    write_csv(directory / "dao_rnd_merged.csv", source_rows)
    write_csv(tmp_path / "data/2024/humanlabels/dao_rnd_merged.csv", source_rows)
    output = tmp_path / "outputs/counts.json"
    result = subprocess.run([sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/reporting/counts.py")),
                             "--analysis-root", str(tmp_path), "--public-data", "--output-json", str(output)],
                            capture_output=True, text=True)
    assert result.returncode != 0
    assert "Invalid public crowdsourcing CSV" in result.stderr or "Public input is unavailable" in result.stderr
    assert "Traceback" not in result.stderr
    assert not output.parent.exists()


@pytest.mark.parametrize("field, conflicting", [("body", "Conflicting body"), ("choices_len", "3")])
def test_split_export_rejects_conflicting_proposal_metadata_before_writing(tmp_path, field, conflicting):
    rows = sample_rows()
    rows[2][field] = conflicting
    source, destination = tmp_path / "input.csv", tmp_path / "export"
    write_csv(source, rows)
    result = subprocess.run([sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/preparation/redact_human.py")),
                             str(source), str(destination), "--split"], capture_output=True, text=True)
    assert result.returncode != 0
    assert "Conflicting proposal metadata" in result.stderr
    assert not destination.exists()


@pytest.mark.parametrize("existing", ["dao_rnd_proposals.csv", "dao_rnd_choice_labels.csv", "dao_rnd.redactions.json"])
def test_split_export_refuses_any_existing_output_without_partial_files(tmp_path, existing):
    source, directory = tmp_path / "input.csv", tmp_path / "export"
    write_csv(source, sample_rows())
    directory.mkdir()
    (directory / existing).write_text("existing output")
    result = subprocess.run([sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/preparation/redact_human.py")),
                             str(source), str(directory), "--split"], capture_output=True, text=True)
    assert result.returncode != 0
    assert "must all be new files" in result.stderr
    assert [p.name for p in directory.iterdir()] == [existing]
    assert (directory / existing).read_text() == "existing output"
