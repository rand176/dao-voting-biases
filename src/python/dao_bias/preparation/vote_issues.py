from __future__ import annotations

from pathlib import Path

import pandas as pd

from dao_bias.data.paths import CLEAN_PROPOSAL_FLAGS_DIR, CLEAN_VOTE_OUTPUTS_DIR, raw_data_path


PROPOSALS_RAW_FILE = raw_data_path("proposals_0.csv.gz")
DEFAULT_DUPLICATE_FLAGS_PATH = CLEAN_PROPOSAL_FLAGS_DIR / "proposal_duplicate_ballot_flags.csv.gz"
DEFAULT_INVALID_VOTE_ERRORS_PATH = CLEAN_VOTE_OUTPUTS_DIR / "prop_choices_favorite_counts_ERR.csv"
CANONICAL_ISSUE_FLAGS_FILE = CLEAN_PROPOSAL_FLAGS_DIR / "proposal_vote_issue_flags.csv.gz"

ISSUE_FLAGS_COLUMNS = [
    "id",
    "flag_duplicate_ballots",
    "duplicate_pairs",
    "duplicate_ballot_rows",
    "duplicate_wallets",
    "flag_invalid_votes",
    "invalid_vote_rows",
    "invalid_vote_reason_count",
    "flag_any_vote_issues",
    "issue_categories",
    "flag_reason",
]


def normalize_id_series(series: pd.Series) -> pd.Series:
    return series.fillna("").astype(str).str.strip().str.lower()


def load_proposal_ids(path: Path = PROPOSALS_RAW_FILE) -> pd.Series:
    df_props = pd.read_csv(path, usecols=["id"], low_memory=False)
    return normalize_id_series(df_props["id"])


def load_duplicate_ballot_flags(path: Path | str = DEFAULT_DUPLICATE_FLAGS_PATH) -> pd.DataFrame:
    path = Path(path)
    columns = [
        "id",
        "flag_duplicate_ballots",
        "duplicate_pairs",
        "duplicate_ballot_rows",
        "duplicate_wallets",
        "flag_category",
        "flag_reason",
    ]
    if not path.exists():
        return pd.DataFrame(columns=columns)

    df = pd.read_csv(path, low_memory=False)
    if "id" not in df.columns:
        return pd.DataFrame(columns=columns)

    df["id"] = normalize_id_series(df["id"])
    for column in ["flag_duplicate_ballots", "duplicate_pairs", "duplicate_ballot_rows", "duplicate_wallets"]:
        if column not in df.columns:
            df[column] = 0
        df[column] = pd.to_numeric(df[column], errors="coerce").fillna(0).astype(int)
    if "flag_reason" not in df.columns:
        df["flag_reason"] = ""
    return df.loc[:, columns].copy()


def load_invalid_vote_errors(path: Path | str = DEFAULT_INVALID_VOTE_ERRORS_PATH) -> pd.DataFrame:
    path = Path(path)
    columns = ["proposal", "vote_mode", "type", "reason", "detail", "choice"]
    if not path.exists():
        return pd.DataFrame(columns=columns)

    df = pd.read_csv(path, low_memory=False)
    if "proposal" not in df.columns:
        return pd.DataFrame(columns=columns)

    df["proposal"] = normalize_id_series(df["proposal"])
    if "reason" not in df.columns:
        df["reason"] = ""
    return df.loc[:, [column for column in columns if column in df.columns]].copy()


def summarize_invalid_vote_errors(df_errors: pd.DataFrame) -> pd.DataFrame:
    if df_errors is None or df_errors.empty:
        return pd.DataFrame(columns=["id", "invalid_vote_rows", "invalid_vote_reason_count"])

    df = df_errors.copy()
    df["proposal"] = normalize_id_series(df["proposal"])
    if "reason" not in df.columns:
        df["reason"] = ""
    df = df.loc[df["proposal"] != ""]
    if df.empty:
        return pd.DataFrame(columns=["id", "invalid_vote_rows", "invalid_vote_reason_count"])

    summary = (
        df.groupby("proposal", as_index=False)
        .agg(
            invalid_vote_rows=("proposal", "size"),
            invalid_vote_reason_count=("reason", "nunique"),
        )
        .rename(columns={"proposal": "id"})
    )
    summary["invalid_vote_rows"] = summary["invalid_vote_rows"].astype(int)
    summary["invalid_vote_reason_count"] = summary["invalid_vote_reason_count"].astype(int)
    return summary.sort_values("id").reset_index(drop=True)


def _issue_categories_for_row(row) -> str:
    categories = []
    if int(row["flag_duplicate_ballots"]) == 1:
        categories.append("duplicate_ballots")
    if int(row["flag_invalid_votes"]) == 1:
        categories.append("invalid_votes")
    return "|".join(categories) if categories else "not_flagged"


def _flag_reason_for_row(row) -> str:
    has_duplicates = int(row["flag_duplicate_ballots"]) == 1
    has_invalid_votes = int(row["flag_invalid_votes"]) == 1
    if has_duplicates and has_invalid_votes:
        return "Contains duplicate ballot histories and malformed vote rows"
    if has_duplicates:
        return "Contains repeated proposal+voter ballot histories in raw Snapshot votes"
    if has_invalid_votes:
        return "Contains malformed vote rows in canonical vote-expansion errors"
    return "No vote issue signal"


def build_proposal_vote_issue_flags(
    proposal_ids=None,
    duplicate_flags: pd.DataFrame | None = None,
    invalid_vote_errors: pd.DataFrame | None = None,
) -> pd.DataFrame:
    if proposal_ids is None:
        proposal_ids = load_proposal_ids()

    ids = pd.Series(proposal_ids, dtype="object")
    df_flags = pd.DataFrame({"id": normalize_id_series(ids)})
    df_flags = df_flags.loc[df_flags["id"] != ""].drop_duplicates().sort_values("id").reset_index(drop=True)

    df_flags["flag_duplicate_ballots"] = 0
    df_flags["duplicate_pairs"] = 0
    df_flags["duplicate_ballot_rows"] = 0
    df_flags["duplicate_wallets"] = 0
    df_flags["flag_invalid_votes"] = 0
    df_flags["invalid_vote_rows"] = 0
    df_flags["invalid_vote_reason_count"] = 0
    df_flags["flag_any_vote_issues"] = 0
    df_flags["issue_categories"] = "not_flagged"
    df_flags["flag_reason"] = "No vote issue signal"

    if duplicate_flags is not None and not duplicate_flags.empty:
        dup = duplicate_flags.copy()
        dup["id"] = normalize_id_series(dup["id"])
        merge_cols = ["id", "flag_duplicate_ballots", "duplicate_pairs", "duplicate_ballot_rows", "duplicate_wallets"]
        df_flags = df_flags.merge(dup.loc[:, merge_cols], on="id", how="left", suffixes=("", "_dup"))
        for column in ["flag_duplicate_ballots", "duplicate_pairs", "duplicate_ballot_rows", "duplicate_wallets"]:
            dup_column = f"{column}_dup"
            df_flags[column] = pd.to_numeric(df_flags[dup_column], errors="coerce").fillna(df_flags[column]).astype(int)
            df_flags.drop(columns=[dup_column], inplace=True)

    invalid_summary = summarize_invalid_vote_errors(
        invalid_vote_errors if invalid_vote_errors is not None else pd.DataFrame()
    )
    if not invalid_summary.empty:
        df_flags = df_flags.merge(invalid_summary, on="id", how="left", suffixes=("", "_invalid"))
        for column in ["invalid_vote_rows", "invalid_vote_reason_count"]:
            invalid_column = f"{column}_invalid"
            df_flags[column] = pd.to_numeric(df_flags[invalid_column], errors="coerce").fillna(df_flags[column]).astype(int)
            df_flags.drop(columns=[invalid_column], inplace=True)

    df_flags["flag_invalid_votes"] = (df_flags["invalid_vote_rows"] > 0).astype(int)
    df_flags["flag_any_vote_issues"] = (
        (df_flags["flag_duplicate_ballots"] == 1) | (df_flags["flag_invalid_votes"] == 1)
    ).astype(int)
    df_flags["issue_categories"] = df_flags.apply(_issue_categories_for_row, axis=1)
    df_flags["flag_reason"] = df_flags.apply(_flag_reason_for_row, axis=1)
    return df_flags.loc[:, ISSUE_FLAGS_COLUMNS].sort_values("id").reset_index(drop=True)


def write_proposal_vote_issue_flags(
    df_flags: pd.DataFrame,
    output_path: Path | str = CANONICAL_ISSUE_FLAGS_FILE,
) -> pd.DataFrame:
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    df_flags.to_csv(output_path, index=False, compression="gzip")
    return df_flags


def build_and_write_proposal_vote_issue_flags(
    proposal_ids=None,
    duplicate_flags: pd.DataFrame | None = None,
    invalid_vote_errors: pd.DataFrame | None = None,
    duplicate_flags_path: Path | str = DEFAULT_DUPLICATE_FLAGS_PATH,
    invalid_vote_errors_path: Path | str = DEFAULT_INVALID_VOTE_ERRORS_PATH,
    output_path: Path | str = CANONICAL_ISSUE_FLAGS_FILE,
) -> pd.DataFrame:
    if duplicate_flags is None:
        duplicate_flags = load_duplicate_ballot_flags(duplicate_flags_path)
    if invalid_vote_errors is None:
        invalid_vote_errors = load_invalid_vote_errors(invalid_vote_errors_path)
    df_flags = build_proposal_vote_issue_flags(
        proposal_ids=proposal_ids,
        duplicate_flags=duplicate_flags,
        invalid_vote_errors=invalid_vote_errors,
    )
    return write_proposal_vote_issue_flags(df_flags, output_path=output_path)


def load_proposal_issue_details(path: Path | str = CANONICAL_ISSUE_FLAGS_FILE) -> dict[str, dict[str, str]]:
    path = Path(path)
    if not path.exists():
        return {}

    df = pd.read_csv(path, low_memory=False)
    if "id" not in df.columns:
        return {}

    df["id"] = normalize_id_series(df["id"])
    if "flag_any_vote_issues" not in df.columns:
        return {}
    df = df.loc[pd.to_numeric(df["flag_any_vote_issues"], errors="coerce").fillna(0).astype(int) == 1]
    details = {}
    for row in df.to_dict("records"):
        proposal_id = row["id"]
        details[proposal_id] = {
            "issue_categories": str(row.get("issue_categories", "vote_issues")),
            "flag_reason": str(row.get("flag_reason", "")),
        }
    return details


def main() -> None:
    df_flags = build_and_write_proposal_vote_issue_flags()
    flagged = int(df_flags["flag_any_vote_issues"].sum())
    duplicate_flagged = int(df_flags["flag_duplicate_ballots"].sum())
    invalid_flagged = int(df_flags["flag_invalid_votes"].sum())
    print(f"Wrote {len(df_flags)} proposal vote issue flags to {CANONICAL_ISSUE_FLAGS_FILE}")
    print(f"Flagged proposals: {flagged}")
    print(f"  duplicate ballot proposals: {duplicate_flagged}")
    print(f"  invalid-vote proposals: {invalid_flagged}")


if __name__ == "__main__":
    main()
