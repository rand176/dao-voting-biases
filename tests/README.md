# Tests

Run `python run.py test`, or select `--language python` / `--language r`.
Direct commands from the package root are `python -B -m pytest -q -p no:cacheprovider` and
`Rscript tests/r/run_r_tests.R`.

`run.py test` uses a temporary workspace for synthetic outputs, pytest files and
caches, and removes it on exit. Analysis data/output options do not redirect test
fixtures. Direct pytest invocations also isolate synthetic output paths. The documented
`-B` prevents parent-process bytecode; test configuration propagates suppression
to subprocesses. `-p no:cacheprovider` avoids the local pytest cache.

The suites use synthetic fixtures and the published human-label sample. They
cover preparation rules, ballot jobs, release/normalization guards, stance
classification, model helpers, path selection and packaging. They require no
private input tree. Full-data assertions and historical missing-file checks
are not part of this suite. Tests do not prove full empirical replication.
