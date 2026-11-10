# Public data

This folder contains only the contact-redacted human-label sample and its
redaction report. Crowdsourcing data is split into proposal and choice-label
tables under `humanlabels/`; the public path has no year directory.
Select it explicitly with `python run.py human-summary`.
In the source analysis repository, the default pipeline uses private inputs
outside this folder; those inputs are absent from this standalone package.
Missing public inputs cause an error; there is no automatic private fallback.

## Included files

| Path relative to this folder | Contents |
|---|---|
| `humanlabels/dao_rnd_proposals.csv` | 590 unique proposals, with contact-redacted Snapshot text, language, original text-length variables, and choice-list metadata. Original `proposal` keys are preserved. |
| `humanlabels/dao_rnd_choice_labels.csv` | 1,645 proposal-choice rows, with choice text, original `proposal`, `choice_idx`, and `prop_chid`, plus the existing reviewer label, timing, issue and suggestion columns. |
| `humanlabels/dao_rnd.redactions.json` | Value-free contact replacement and review counts over the reconstructed choice-level sample, plus split-table row counts. Replacement counts include repeated proposal text before splitting for comparability with the previous export. |

The crowdsourcing export drops the exported row index, converts Unicode
line/paragraph separators to ordinary newlines, and replaces detected contact
emails and phone numbers in `title`, `body`, `choice`, and `choices_json` with
`[EMAIL]` and `[PHONE]`. Entire `mailto:` and `tel:` link targets, including query
parameters, are replaced. Ethereum addresses and original proposal identifiers
are retained. All other values, including evaluations, timings, and numeric
research variables, remain unchanged. Text-length variables describe the original
unredacted text and are not recomputed from the published text.

`--public-data` calls `src/python/dao_bias/data/public.py` to join the two tables by
`proposal`, preserving choice-label row order and reconstructing all original
combined row values. Proposal-level fields are stored only once. The loader
requires both published CSVs, rejects incompatible schemas, duplicate keys,
orphaned labels, proposals without labels, and incomplete per-proposal choice-label sets, and does not fall back to a
combined CSV or private inputs. It runs only when public mode is selected, not
when private information is missing. Private mode keeps the original combined
input. This split layout is the only supported public crowdsourcing format.

Inherited value conventions are preserved: `choices_json` contains Python-repr
lists (often with single quotes), not strict JSON. `eval_suggest_*` can contain
the literal string `N/A`, while unused `eval_*` slots contain empty cells. The
loader preserves these strings and empty fields without missing-value coercion;
it does not parse or reserialize the choice-list text.


Personal names, social-media `@handles`, and Discord `name#NNNN` identifiers in
Snapshot text and choice values are retained; the contact redaction does not
remove them. This retention is intentional: the sample preserves original
ballot text and choices apart from the disclosed email and phone replacements.

This is a contact-redacted dataset, not an anonymous one. Proposal identifiers
allow retrieval of original Snapshot text, and Ethereum addresses may link to
identities. It has no participant-ID columns. Reviewer slots (`eval_1` to `eval_4`)
are not global participant identifiers; precise timings may nevertheless support
linkage with independently obtained logs.
Raw participant events, demographics, payment records, and the full Snapshot
data are not included here.

## Supported commands

Run from the repository root with Python 3.11 or newer (standard library only):

```bash
python run.py human-summary --output-root outputs
```

The command reports sample size and language/choice-count summaries. It does not
calculate reviewer participation or full paper statistics. It refuses existing JSON outputs; choose a new
`--output-root` for each run. Generated `outputs/` are ignored by Git.

The crowdsourcing summary rejects missing or duplicate required headers, empty
samples, malformed records, invalid choice counts, inconsistent or duplicate
proposal-choice keys, and conflicting choice counts within a proposal before
creating its output. These checks apply only to public mode.

Public mode cannot run the full pipeline: the shared sample does
not substitute for raw votes, the registered choice-level release, or room logs.
Joins to other datasets require those datasets to be obtained independently.
Use the source analysis repository for the private analysis workflow.

## Refreshing the crowdsourcing export

From this package root, supply the original combined CSV separately and use
a new output path for review:

```bash
PYTHONPATH=src/python python -B -m dao_bias.preparation.redact_human \
  INPUT/data/2024/humanlabels/dao_rnd_merged.csv \
  /tmp/crowdsourcing_review --split
```

With `--split`, the destination is a review directory. The generator writes
`dao_rnd_proposals.csv`, `dao_rnd_choice_labels.csv`, and `dao_rnd.redactions.json`;
all three files must be new. Use `--report PATH` to choose another report
location. The report contains counts and field names, never removed values.
Redaction is applied before splitting, and conflicting repeated proposal metadata
causes an error instead of silently selecting one version. The original combined
CSV exporter remains available without `--split` for source-side compatibility
checks, but that combined file is no longer included or read in the public package.

Email detection covers ordinary and percent-encoded `@` addresses and contact
links. Phone detection covers international `+` numbers, common US/UK national
formats, and contact-labelled numeric strings. Ambiguous numeric candidates are
preserved and counted for review, since dates, amounts, timestamps, and URL IDs
can resemble phone numbers. Automatic detection is incomplete, especially for
obfuscated contacts. Review both changed text and preserved candidates before
sharing; the report is not an anonymity certificate.

The generator refuses overwrites and unexpected columns. Review the result
before replacing the published tables. Original proposal identifiers are retained
for compatibility. Do not rerun the raw
human-label job just to refresh this export. The stored crowd evaluations and
their missing values are preserved; publishing this sample does not correct
or rerun the production human-label aggregation.
