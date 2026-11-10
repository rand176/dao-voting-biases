SCRIPT_PATHS = {'src/python/dao_bias/preparation/tvl.py': 'src/python/dao_bias/preparation/tvl.py', 'dao_bias.data.paths.py': 'src/python/dao_bias/data/paths.py', 'dao_bias.data.public.py': 'src/python/dao_bias/data/public.py', 'dao_bias.data.input_paths.py': 'src/python/dao_bias/data/input_paths.py', 'dao_bias.data.readers.py': 'src/python/dao_bias/data/readers.py', 'dao_bias.preparation.fake_proposals.py': 'src/python/dao_bias/preparation/fake_proposals.py', 'dao_bias.preparation.duplicate_ballots.py': 'src/python/dao_bias/preparation/duplicate_ballots.py', 'dao_bias.preparation.vote_issues.py': 'src/python/dao_bias/preparation/vote_issues.py', 'dao_bias.preparation.normalize_stance.py': 'src/python/dao_bias/preparation/normalize_stance.py', 'dao_bias.preparation.redact_human.py': 'src/python/dao_bias/preparation/redact_human.py', 'dao_bias.jobs.runner.py': 'src/python/dao_bias/jobs/runner.py', 'dao_bias.jobs.batch.py': 'src/python/dao_bias/jobs/batch.py', 'dao_bias.jobs.__init__.py': 'src/python/dao_bias/jobs/__init__.py', 'dao_bias.jobs.author_votes.py': 'src/python/dao_bias/jobs/author_votes.py', 'dao_bias.jobs.human_labels.py': 'src/python/dao_bias/jobs/human_labels.py', 'dao_bias.jobs.cumulative.py': 'src/python/dao_bias/jobs/cumulative.py', 'dao_bias.jobs.votes.py': 'src/python/dao_bias/jobs/votes.py', 'dao_bias.reporting.counts.py': 'src/python/dao_bias/reporting/counts.py', 'dao_bias.reporting.descriptive_tables.py': 'src/python/dao_bias/reporting/descriptive_tables.py', 'dao_bias.reporting.stance_figure.py': 'src/python/dao_bias/reporting/stance_figure.py', 'dao_bias.reporting.stance_final.py': 'src/python/dao_bias/reporting/stance_final.py', 'dao_bias.reporting.suggested_stance.py': 'src/python/dao_bias/reporting/suggested_stance.py'}
import csv
import json
import subprocess
import sys
from pathlib import Path

import pytest


from dao_bias.reporting.counts import read_human_sample_stats


def write_sample(path, proposals):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["proposal", "body_lang", "choices_len", "choice_idx", "prop_chid"])
        writer.writeheader()
        writer.writerows({"proposal": p, "body_lang": "en", "choices_len": "2",
                          "choice_idx": "1", "prop_chid": p + "1"} for p in proposals)


def write_public_sample(root, proposals):
    from dao_bias.data.public import PROPOSAL_COLUMNS, LABEL_COLUMNS, PROPOSALS_PATH, LABELS_PATH
    for path, columns in [(PROPOSALS_PATH, PROPOSAL_COLUMNS), (LABELS_PATH, LABEL_COLUMNS)]:
        output = root / "public_data" / path
        output.parent.mkdir(parents=True, exist_ok=True)
        with output.open("w", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=columns)
            writer.writeheader()
            for proposal in proposals:
                row = dict.fromkeys(columns, "")
                row["proposal"] = proposal
                if "body_lang" in row:
                    row.update(body_lang="en", choices_len="2")
                else:
                    row.update(choice_idx="1", prop_chid=proposal + "1")
                if "choice_idx" in row:
                    for index in (1, 2):
                        row.update(choice_idx=str(index), prop_chid=proposal + str(index))
                        writer.writerow(row)
                else:
                    writer.writerow(row)


def test_public_crowdsourcing_cli_selects_public_input_and_keeps_private_default(tmp_path):
    relative = Path("data/2024/humanlabels/dao_rnd_merged.csv")
    write_sample(tmp_path / relative, ["private"])
    write_public_sample(tmp_path, ["public-a", "public-b"])
    default, _ = read_human_sample_stats(tmp_path)
    assert default["evaluated_proposals"] == 1
    output = tmp_path / "counts.json"
    result = subprocess.run([
        sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/reporting/counts.py")),
        "--analysis-root", str(tmp_path), "--public-data", "--output-json", str(output),
    ], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    payload = json.loads(output.read_text())
    assert payload["sample"]["evaluated_proposals"] == 2
    assert payload["sample"]["evaluated_choices"] == 4
    assert set(payload["sources"].values()) == {
        "public_data/humanlabels/dao_rnd_proposals.csv",
        "public_data/humanlabels/dao_rnd_choice_labels.csv",
    }
    assert not (tmp_path / "metadata").exists()
    original_output = output.read_bytes()
    repeated = subprocess.run([
        sys.executable, str((Path(__file__).resolve().parents[2] / "src/python/dao_bias/reporting/counts.py")),
        "--analysis-root", str(tmp_path), "--public-data", "--output-json", str(output),
    ], capture_output=True, text=True)
    assert repeated.returncode != 0
    assert "Output already exists" in repeated.stderr
    assert "Traceback" not in repeated.stderr
    assert output.read_bytes() == original_output
    (tmp_path / "public_data/humanlabels/dao_rnd_choice_labels.csv").unlink()
    with pytest.raises(FileNotFoundError, match="Public input is unavailable"):
        read_human_sample_stats(tmp_path, public_data=True)


def test_private_crowdsourcing_reader_retains_permissive_legacy_behavior(tmp_path):
    sample = tmp_path / "data/2024/humanlabels/dao_rnd_merged.csv"
    sample.parent.mkdir(parents=True)
    sample.write_text("proposal,body_lang,choices_len\np,en,broken\n,en,2\n")
    values, _ = read_human_sample_stats(tmp_path)
    assert values["evaluated_proposals"] == values["evaluated_choices"] == 1
    assert values["max_choices_per_proposal"] == 0


@pytest.mark.parametrize("script, extra_args", [
    ("dao_bias.reporting.counts.py", ["--skip-macro-output"]),
    ("dao_bias.reporting.suggested_stance.py", []),
])
@pytest.mark.parametrize("registry_present", [False, True])
def test_private_commands_explain_missing_inputs_without_traceback(tmp_path, script, extra_args, registry_present):
    if registry_present:
        registry = tmp_path / "metadata/analysis_data_releases.csv"
        registry.parent.mkdir()
        registry.write_text("release_name,relative_path\ncurrent,data/private.csv\n")
    result = subprocess.run([
        sys.executable, str(Path(__file__).resolve().parents[2] / SCRIPT_PATHS[script]),
        "--analysis-root", str(tmp_path), *extra_args,
    ], capture_output=True, text=True)
    assert result.returncode != 0
    assert "Private mode requires" in result.stderr
    assert ("--public-data" if "counts" in script else "run.py human-summary") in result.stderr
    assert ("private.csv" if registry_present else "analysis_data_releases.csv") in result.stderr
    assert "Traceback" not in result.stderr
    assert not (tmp_path / "outputs").exists()
    assert not (tmp_path / "figures").exists()
    assert not (tmp_path / "metadata/analysis_data_provenance").exists()


@pytest.mark.parametrize("public_data", [False, True])
def test_missing_input_guidance_matches_selected_mode(tmp_path, public_data):
    from dao_bias.data.input_paths import resolve_data_input
    with pytest.raises(FileNotFoundError) as caught:
        resolve_data_input("missing.csv", tmp_path, public_data=public_data)
    message = str(caught.value)
    assert message.startswith("Public input is unavailable" if public_data else "Private input is unavailable")
    if public_data:
        assert "no private fallback" in message
        assert "Private mode requires" not in message
    else:
        assert "Private mode requires original analysis inputs" in message
        assert "Public data cannot run" not in message
