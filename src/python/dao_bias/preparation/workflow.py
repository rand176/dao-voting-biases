"""Explicit preparation stages; all writes go beneath the selected output root."""
import argparse
from pathlib import Path
import sys

from dao_bias.data.paths import INPUT_DATA_DIR, DATA_2024_DIR, prepared_input


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--stage",required=True)
    parser.add_argument("--workers",type=int,default=4)
    parser.add_argument("--input-file",type=Path)
    args = parser.parse_args(argv)
    import pandas as pd
    from dao_bias.preparation import snapshot
    from dao_bias.data.readers import pd_read_dir

    def read(relative):
        path = prepared_input(*Path(relative).parts)
        if not path.exists():
            raise FileNotFoundError("Required preparation input: " + str(path))
        return pd.read_csv(path,low_memory=False)
    def write(frame,name):
        path=DATA_2024_DIR/"derived/active"/name
        path.parent.mkdir(parents=True,exist_ok=True)
        frame.to_csv(path,index=False)
        print(path)
    def stage(name):
        if name in {"votes","author-votes","human-labels","cumulative"}:
            from dao_bias.jobs.runner import main as jobs
            job={"votes":"vote_expansion","author-votes":"author_votes","human-labels":"human_labels","cumulative":"vote_cumulative"}[name]
            for command in ["build-manifest","run","reduce"]:
                command_args=[command,job]
                if command=="run":command_args += ["--workers",str(args.workers)]
                jobs(command_args)
            return
        if name=="fake":
            from dao_bias.preparation.fake_proposals import build_and_write_fake_proposal_outputs
            build_and_write_fake_proposal_outputs();return
        if name=="duplicate-ballots":
            from dao_bias.preparation.duplicate_ballots import build_and_write_duplicate_ballot_outputs
            build_and_write_duplicate_ballot_outputs();return
        if name=="vote-issues":
            from dao_bias.preparation.vote_issues import build_and_write_proposal_vote_issue_flags
            build_and_write_proposal_vote_issue_flags();return
        if name=="language":
            proposals=read("raw/proposals_0.csv.gz").drop(columns=["Unnamed: 0"],errors="ignore")
            proposals=proposals.loc[proposals["title"].notna()]
            write(snapshot.prepare_language(proposals),"proposals_lang.csv.gz")
        elif name=="lengths":
            write(snapshot.prepare_lengths(read("derived/active/proposals_lang.csv.gz")),"proposals_lang_len.csv.gz")
        elif name=="maturity":
            follows=pd_read_dir(str(INPUT_DATA_DIR/"raw/follows"))
            proposals,spaces=snapshot.prepare_maturity(read("derived/active/proposals_lang_len_fake.csv.gz"),read("raw/spaces_0.csv.gz"),follows)
            write(proposals,"proposals_lang_len_fake_paper.csv.gz");write(spaces,"spaces_lang_len_fake_paper.csv.gz")
        elif name=="choices":
            write(snapshot.prepare_choices(read("derived/active/proposals_lang_len_fake_paper.csv.gz")),"choices_lang_len_fake_paper.csv.gz")
        elif name=="delegates":
            proposals,spaces=snapshot.prepare_delegates(read("raw/proposals_0.csv.gz"),read("raw/spaces_0.csv.gz"))
            write(proposals,"proposals_delegates.csv.gz");write(spaces,"spaces_settings.csv.gz")
        elif name=="dates":
            write(snapshot.prepare_dates(read("raw/proposals_0.csv.gz"),read("raw/spaces_0.csv.gz")),"proposals_dates.csv.gz")
        elif name=="author-power":
            if args.input_file is None:raise ValueError("author-power requires --input-file; no release is selected implicitly")
            from dao_bias.preparation.author_power import remove_author_power
            write(remove_author_power(pd.read_csv(args.input_file,low_memory=False)),"no_author_power.csv")
        elif name=="normalize":
            if args.input_file is None:raise ValueError("normalize requires --input-file")
            from dao_bias.preparation.normalize_stance import normalize_release_stance
            target=DATA_2024_DIR/"derived/active/stance_normalized.csv"
            normalize_release_stance(input_path=args.input_file, output_path=target,
                exceptions_path=INPUT_DATA_DIR.parents[1]/"metadata/exceptions/stance_one_sided_exceptions.csv",
                audit_path=target.with_name("stance_normalization_audit.csv"), stage="2_stance_v5")
        else:raise ValueError("Unknown preparation stage: "+name)
    if args.stage=="snapshot":
        for name in ["language","lengths","fake","maturity","choices","delegates","dates"]:stage(name)
    else:stage(args.stage)
    return 0


if __name__=="__main__":
    try:raise SystemExit(main())
    except (OSError,ValueError,ImportError) as error:raise SystemExit(str(error)) from None
