"""Small command interface; importing this module does not load research data."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory

ROOT = Path(__file__).resolve().parents[3]
R_STAGES = {"dataset", "stance", "merge-votes", "features", "labels", "candidate"}


# command -> (help text, needs --data-root)
COMMAND_HELP = {
    "check-inputs": ("verify a Snapshot input tree has the required raw files", True),
    "prepare": ("run one preparation stage; see prepare --help for stage order", True),
    "models": ("estimate models, effects, model tables, forest figures", True),
    "bootstrap": ("three-bias bootstrap inference", True),
    "figures": ("figure workflows; --kind all or a filename under src/r/reporting/figures/", True),
    "tables": ("descriptive LaTeX tables", True),
    "reports": ("manuscript count macros and stance-agreement artifacts", True),
    "descriptive": ("supplemental descriptive calculations", True),
    "metric-slopes": ("OLS slopes used for bias-metric tests", True),
    "stance-comparison": ("compare historical and revised stance heuristics", True),
    "author-timing": ("author voting-timing analysis; --timing-file replays a saved table", True),
    "tvl-review": ("protocol-to-Snapshot matching review artifacts", True),
    "tvl-memberships": ("rebuild curated top-TVL sample memberships", True),
    "human-summary": ("summarize the bundled public label sample", False),
    "fetch-tvl": ("fetch DefiLlama inputs into the output tree", False),
    "test": ("run the self-contained Python and R test suites", False),
}

PREPARE_EPILOG = """\
Python stages: snapshot (bundles language, lengths, fake, maturity, choices,
delegates, dates — or run each individually), vote-issues, duplicate-ballots,
votes, author-votes, human-labels, cumulative, author-power, normalize.
author-power and normalize take --input-file.

R stages, run in this order: dataset, stance, merge-votes, features, labels,
candidate. The final stage builds a candidate release; it does not promote it.
"""


def commands_epilog():
    def block(title, entries):
        width = max(len(name) for name, _ in entries)
        rows = [title] + ["  %-*s  %s" % (width, name, text) for name, text in entries]
        return "\n".join(rows)
    needed = [(name, text) for name, (text, required) in COMMAND_HELP.items() if required]
    free = [(name, text) for name, (text, required) in COMMAND_HELP.items() if not required]
    free.append(("tables --kind heuristics", "stance-pattern tables"))
    return "\n\n".join([
        block("Commands that work without --data-root:", free),
        block("Commands that need --data-root (private Snapshot inputs, not distributed):", needed),
        'Run "run.py COMMAND --help" for per-command options.',
    ])


def parser():
    p = argparse.ArgumentParser(
        description="DAO voting-bias analysis and replication tools",
        epilog=commands_epilog(),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    commands = p.add_subparsers(dest="command", required=True, metavar="COMMAND",
                                help="one of the commands listed below")
    for command in COMMAND_HELP:
        kwargs = {}
        if command == "prepare":
            kwargs.update(description="Run one preparation stage.",
                          epilog=PREPARE_EPILOG,
                          formatter_class=argparse.RawDescriptionHelpFormatter)
        q = commands.add_parser(command, **kwargs)
        q.add_argument("--config", type=Path)
        q.add_argument("--data-root", "--input-root", type=Path)
        q.add_argument("--output-root", "--output-dir", type=Path)
        q.add_argument("--release", default="current")
        q.add_argument("--workers", type=int, default=4)
        if command == "prepare":
            q.add_argument("--stage", default="snapshot", choices=["snapshot", "language", "lengths", "fake", "maturity", "choices", "delegates", "dates", "vote-issues", "duplicate-ballots", "votes", "author-votes", "human-labels", "cumulative", "author-power", "normalize", *sorted(R_STAGES)])
            q.add_argument("--input-file", type=Path)
        if command == "reports":
            q.add_argument("--kind", choices=["counts","stance","stance-final","suggested-stance"], default="counts")
        if command == "fetch-tvl":
            q.add_argument("--cache-only", action="store_true")
        if command in {"figures", "tables", "models"}:
            if command == "tables":
                q.add_argument("--kind", choices=["descriptive", "heuristics"], default="descriptive")
            else:
                q.add_argument("--kind", default={"figures": "all", "models": "main"}[command])
        if command == "bootstrap":
            q.add_argument("--replicates", type=int)
            q.add_argument("--seed", type=int)
        if command == "author-timing":
            q.add_argument("--timing-file", type=Path)
        if command == "test":
            q.add_argument("--language", choices=["all", "python", "r"], default="all")
    return p


def settings(args):
    config = {}
    if args.config:
        config = json.loads(args.config.read_text())
        extra = set(config) - {"data_root", "output_root", "llm_root"}
        if extra:
            raise ValueError("Unknown config fields: " + ", ".join(sorted(extra)))
    base = args.config.resolve().parent if args.config else ROOT
    def path(value):
        value = Path(value)
        return (base / value).resolve() if not value.is_absolute() else value.resolve()
    data = args.data_root or config.get("data_root") or os.environ.get("DAO_BIAS_DATA_ROOT")
    output = args.output_root or config.get("output_root") or os.environ.get("DAO_BIAS_OUTPUT_ROOT") or ROOT / "outputs"
    # CLI paths are relative to the caller; config paths are relative to the config.
    data = args.data_root.resolve() if args.data_root else path(data) if data else None
    output = args.output_root.resolve() if args.output_root else path(output)
    env = os.environ.copy()
    env.update(DAO_BIAS_ROOT=str(ROOT), DAO_BIAS_DATA_ROOT=str(data or ROOT),
               DAO_BIAS_OUTPUT_ROOT=str(output), DAO_BIAS_RELEASE=args.release,
               PYTHONPATH=str(ROOT / "src/python") + os.pathsep + env.get("PYTHONPATH", ""))
    if config.get("llm_root"):
        env["LLM_DATA_DIR"] = str(path(config["llm_root"]))
    return data, output, env


def invoke(argv, env):
    # Interpreter flags do not propagate to child processes; suppress their caches too.
    child_env = dict(env, PYTHONDONTWRITEBYTECODE="1")
    return subprocess.run(argv, cwd=ROOT, env=child_env).returncode


def main(argv=None):
    args = parser().parse_args(argv)
    try:
        data, output, env = settings(args)
        if args.workers < 1:
            raise ValueError("--workers must be positive")
        if args.command == "test":
            with TemporaryDirectory(prefix="dao-bias-tests-") as workspace:
                temporary = Path(workspace)
                env.update(DAO_BIAS_DATA_ROOT=str(ROOT),
                           DAO_BIAS_OUTPUT_ROOT=str(temporary/"outputs"),
                           MPLCONFIGDIR=str(temporary/"matplotlib"),
                           XDG_CACHE_HOME=str(temporary/"cache"))
                if args.language in {"all", "python"}:
                    code = invoke([sys.executable, "-m", "pytest", "-q",
                                   "-p", "no:cacheprovider", "--basetemp", str(temporary/"pytest")], env)
                    if code:
                        return code
                return invoke(["Rscript", str(ROOT/"tests/r/run_r_tests.R")], env) if args.language in {"all","r"} else 0
        if args.command == "human-summary":
            if args.data_root is not None or args.release != "current":
                raise ValueError(
                    "human-summary summarizes the bundled public sample; "
                    "--data-root and --release are not accepted"
                )
            return invoke([sys.executable,"-m","dao_bias.reporting.counts","--public-data",
                "--analysis-root",str(ROOT),"--output-json",str(output/"human_summary.json")],env)
        if args.command == "tables" and args.kind == "heuristics":
            return invoke(["Rscript",str(ROOT/"src/r/main.R"),"tables","--kind","heuristics"],env)
        if args.command == "check-inputs":
            if data is None:
                raise ValueError("--data-root is required for Snapshot input checks")
            required = ["data/2024/raw/proposals_0.csv.gz", "data/2024/raw/votes", "data/2024/raw/spaces_0.csv.gz"]
            missing = [name for name in required if not (data/name).exists()]
            for name in required:
                print(("MISSING " if name in missing else "OK ") + str(data/name))
            return int(bool(missing))
        if args.command == "fetch-tvl":
            cmd = [sys.executable,"-m","dao_bias.preparation.tvl","--output-dir",str(output/"data/contributors"),
                   "--provenance-dir",str(output/"metadata/analysis_data_provenance")]
            if args.cache_only:
                cmd.extend(["--cache-only", "--reuse-raw"])
            return invoke(cmd,env)
        replay = args.command == "author-timing" and args.timing_file is not None
        if data is None and not replay:
            raise ValueError("--data-root is required; Snapshot inputs are not bundled")
        if data is not None and not data.is_dir():
            raise ValueError("Input root does not exist: " + str(data))
        if args.command == "reports":
            common = ["--analysis-root",str(data),"--release",args.release]
            if args.kind == "counts":
                module = "counts"
                extra = ["--paper-root",str(output)]
            elif args.kind == "stance-final":
                module = "stance_final"
                extra = ["--output-dir",str(output/"stance-final")]
            elif args.kind == "stance":
                module = "stance_figure"
                extra = ["--output-json",str(output/"stance-agreement.json"),"--figure-output",str(output/"figures/stance-agreement.png")]
            else:
                module = "suggested_stance"
                meta = output/"metadata/analysis_data_provenance"
                extra = ["--output-csv",str(meta/"suggested_stance_diagnostics__latest.csv"),
                    "--output-json",str(meta/"suggested_stance.json"),"--output-tex",str(output/"tables/suggested-stance.tex"),
                    "--figure-output",str(output/"figures/suggested-stance.png")]
            return invoke([sys.executable,"-m","dao_bias.reporting."+module,*common,*extra],env)
        if args.command == "prepare" and args.stage not in R_STAGES:
            cmd = [sys.executable,"-m","dao_bias.preparation.workflow","--stage",args.stage,"--workers",str(args.workers)]
            if args.input_file:
                cmd += ["--input-file",str(args.input_file.resolve())]
            return invoke(cmd,env)
        if args.command == "tables" and args.kind == "descriptive":
            return invoke([sys.executable,"-m","dao_bias.reporting.descriptive_tables","--analysis-root",str(data),
                "--output-dir",str(output/"tables"),"--release",args.release],env)
        cmd = ["Rscript",str(ROOT/"src/r/main.R"),args.command,"--workers",str(args.workers)]
        for key in ["kind","stage","timing_file","replicates","seed"]:
            value = getattr(args,key,None)
            if value is not None:
                cmd += ["--"+key.replace("_","-"),str(value)]
        return invoke(cmd,env)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(str(error),file=sys.stderr)
        return 2
