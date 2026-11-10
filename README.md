# Voting Biases in DAO Governance

Analysis code for the paper:

Balietti, Saggese, and Strohmaier, "Voting Biases in Decentralized
Autonomous Organization (DAO) Governance"
<https://papers.ssrn.com/sol3/papers.cfm?abstract_id=7096118>

The pipeline turns raw Snapshot exports into analytical releases, estimates
the paper's models, and produces its figures and tables. Python prepares
Snapshot records and labels; R builds releases, estimates models, and
generates the reporting artifacts.

## Data

### What is in this repository

`public_data/humanlabels/` contains a contact-redacted sample of the human
labels collected for the paper: 590 proposals and 1,645 choice-label rows.
This is the raw crowdsourcing input behind the human/LLM agreement analysis.
See [public_data/README.md](public_data/README.md) for the file layout,
redaction policy, and privacy notes.

### What is not in this repository

Full empirical replication requires private inputs that are not distributed:

- raw Snapshot exports (proposals, spaces, votes, follows)
- analytical data releases and curated exception tables
- LLM label exports and TVL matching inputs

Without them, every analysis command exits with
`--data-root is required; Snapshot inputs are not bundled`. The bundled
human-label sample alone cannot substitute for these inputs.

### Requesting access

To request the private inputs, contact the authors — their contact details
are listed on the paper's
[SSRN page](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=7096118).
Once you have an input tree, verify it with
`python run.py check-inputs --data-root /path/to/inputs`; the expected
layout is documented under [Configuration](#configuration).

## Quick start

Requirements:

- Python 3.11+
- Python packages for tests and preparation: pytest, pandas, numpy,
  matplotlib, langdetect
- R with: data.table, fixest, ggplot2, marginaleffects, modelsummary
  (models); plus foreign, gridExtra, psych, scales, reshape2, plyr, dplyr,
  ineq, texreg, zoo, lme4, lmerTest, mediation, tidyverse, Rmisc, nnet, car
  for the full analysis. `hhi` is optional.

No installation step is needed: everything runs through `run.py`.

```sh
python run.py --help
python run.py human-summary          # summarize the bundled label sample
python run.py test                   # Python + R suites, self-contained fixtures
```

## Command reference

Every command accepts `--data-root`, `--output-root`, `--release`,
`--config` and `--workers`; see `python run.py COMMAND --help`.

### Runs without the private inputs

| Command | Purpose |
| --- | --- |
| `human-summary` | Summarize the bundled public label sample |
| `test [--language python\|r]` | Run the self-contained test suites |
| `tables --kind heuristics` | Stance-pattern tables; needs R but no data root |
| `fetch-tvl [--cache-only]` | Fetch DefiLlama inputs into the output tree |
| `author-timing --timing-file FILE` | Replay a saved timing table without data |

### Requires the private inputs (`--data-root`)

| Command | Purpose |
| --- | --- |
| `check-inputs` | Verify required raw input files exist |
| `prepare --stage NAME` | Run one preparation stage (see below) |
| `models [--kind additional]` | Main robustness models, effects, model tables, forest figures |
| `bootstrap --replicates N --seed S` | Three-bias bootstrap inference |
| `figures --kind all\|NAME` | Figure workflows (`NAME` is a filename under `src/r/reporting/figures/`) |
| `tables --kind descriptive` | Descriptive LaTeX tables |
| `reports --kind counts` | Manuscript count macros and source evidence |
| `reports --kind stance` | Human/LLM agreement figure and supporting JSON |
| `reports --kind stance-final` | Agreement on the final analytical sample |
| `reports --kind suggested-stance` | Suggested-stance metrics, table and figure |
| `descriptive` | Supplemental descriptive calculations |
| `metric-slopes` | OLS slopes used for bias-metric tests |
| `stance-comparison` | Comparison of historical and revised stance heuristics |
| `tvl-review` | Protocol-to-Snapshot matching review artifacts |
| `tvl-memberships` | Rebuild curated top-TVL sample memberships |
| `author-timing` | Author voting-timing analysis |

#### Preparation stages

Python stages: `snapshot` (runs language, lengths, fake, maturity, choices,
delegates, dates together — or each individually), `vote-issues`,
`duplicate-ballots`, `votes`, `author-votes`, `human-labels`, `cumulative`,
`author-power` and `normalize` (the last two take `--input-file`).

R stages, run in order: `dataset`, `stance`, `merge-votes`, `features`,
`labels`, `candidate`. The final stage builds a candidate release; it does
not promote it to `current`.

#### Full pipeline example

```sh
# Check that an input tree has the required raw files
python run.py check-inputs --data-root /path/to/inputs

# Run the Python preparation stages
python run.py prepare --stage snapshot --data-root INPUT --output-root outputs
python run.py prepare --stage votes    --data-root INPUT --output-root outputs --workers 8

# Run the R preparation stages, in this order
for stage in dataset stance merge-votes features labels candidate; do
  python run.py prepare --stage "$stage" --data-root INPUT --output-root outputs
done

# Estimate models, bootstrap inference, and produce figures/tables
python run.py models    --data-root INPUT --output-root outputs
python run.py bootstrap --replicates 199 --seed 20260518 --data-root INPUT --output-root outputs
python run.py figures --kind all         --data-root INPUT --output-root outputs
python run.py tables --kind descriptive  --data-root INPUT --output-root outputs
python run.py reports --kind counts      --data-root INPUT --output-root outputs
```

Use a fresh output tree when inputs change: saved outputs deliberately
refuse overwrites.

## Configuration

`--data-root` names the analysis input tree and is required for Snapshot
workflows. `--output-root` defaults to `outputs/` inside this package. CLI
paths are relative to the caller; paths in `--config config.json` are
relative to that JSON file. Copy `config.example.json` and set `data_root`,
`output_root`, and optionally `llm_root`. Environment equivalents are
`DAO_BIAS_DATA_ROOT`, `DAO_BIAS_OUTPUT_ROOT` and `LLM_DATA_DIR`. CLI values
take precedence over config, then environment values.

Expected input layout:

```text
INPUT/
  data/2024/raw/{proposals_0.csv.gz,spaces_0.csv.gz,votes/,follows/}
  data/2024/humanlabels/                original label inputs for batch jobs
  data/2024/derived/{active/,debug/,archive_large/}
  data/2024/clean_*_outputs/            optional supplied intermediates
  metadata/analysis_data_releases.csv   current/candidate -> relative_path
  metadata/exceptions/                  curated exclusions and overrides
  metadata/analysis_data_provenance/    TVL matching inputs and inference
  r/storage/                            curated GPT4o proposal lists
  llms/                                 external model label exports
```

## Repository layout

```text
run.py                    command interface
src/python/dao_bias/      data, preparation, jobs, reporting
src/r/                    runtime, data, preparation, models, inference, reporting
public_data/humanlabels/  published human-label inputs
tests/python/, tests/r/   self-contained verification
```
