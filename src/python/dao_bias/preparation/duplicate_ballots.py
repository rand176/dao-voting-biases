from __future__ import annotations

import csv
import gzip
import os
from pathlib import Path
import tempfile
import zlib

import pandas as pd

from dao_bias.data.paths import CLEAN_PROPOSAL_FLAGS_DIR, raw_data_path


OUTPUT_DIR = CLEAN_PROPOSAL_FLAGS_DIR
PROPOSALS_RAW_FILE = raw_data_path("proposals_0.csv.gz")
VOTES_DIR = raw_data_path("votes")
CANONICAL_FLAGS_FILE = OUTPUT_DIR / "proposal_duplicate_ballot_flags.csv.gz"
CANONICAL_PAIRS_FILE = OUTPUT_DIR / "proposal_duplicate_ballot_pairs.csv.gz"
DEFAULT_BUCKET_COUNT = 256

FLAGS_COLUMNS = [
    "id",
    "flag_duplicate_ballots",
    "duplicate_pairs",
    "duplicate_ballot_rows",
    "duplicate_wallets",
    "flag_category",
    "flag_reason",
]

PAIRS_COLUMNS = [
    "proposal",
    "voter",
    "n_ballots",
    "first_created",
    "last_created",
    "first_vote_id",
    "last_vote_id",
]


def normalize_id_series(series: pd.Series) -> pd.Series:
    return series.fillna("").astype(str).str.strip().str.lower()


def _coerce_created(series: pd.Series) -> pd.Series:
    created = pd.to_numeric(series, errors="coerce")
    return created.where(created.notna(), pd.NA)


def summarize_duplicate_ballot_pairs(df_votes: pd.DataFrame) -> pd.DataFrame:
    if df_votes.empty:
        return pd.DataFrame(columns=PAIRS_COLUMNS)

    df = df_votes.copy()
    df["proposal"] = normalize_id_series(df["proposal"])
    df["voter"] = normalize_id_series(df["voter"])
    df["id"] = df["id"].fillna("").astype(str)
    df["created"] = _coerce_created(df["created"])
    df = df.loc[(df["proposal"] != "") & (df["voter"] != "")]
    if df.empty:
        return pd.DataFrame(columns=PAIRS_COLUMNS)

    df = df.sort_values(["proposal", "voter", "created", "id"], kind="mergesort").reset_index(drop=True)
    grouped = df.groupby(["proposal", "voter"], sort=False, dropna=False)
    pairs = grouped.agg(
        n_ballots=("proposal", "size"),
        first_created=("created", "first"),
        last_created=("created", "last"),
        first_vote_id=("id", "first"),
        last_vote_id=("id", "last"),
    ).reset_index()
    pairs = pairs.loc[pairs["n_ballots"] > 1].copy()
    if pairs.empty:
        return pd.DataFrame(columns=PAIRS_COLUMNS)

    pairs["first_created"] = pd.to_numeric(pairs["first_created"], errors="coerce").astype("Int64")
    pairs["last_created"] = pd.to_numeric(pairs["last_created"], errors="coerce").astype("Int64")
    return pairs.loc[:, PAIRS_COLUMNS].sort_values(["proposal", "voter"]).reset_index(drop=True)


def build_duplicate_ballot_flags(proposal_ids, duplicate_pairs: pd.DataFrame) -> pd.DataFrame:
    proposal_ids = pd.Series(proposal_ids, dtype="object")
    df_flags = pd.DataFrame({"id": normalize_id_series(proposal_ids)})
    df_flags = df_flags.loc[df_flags["id"] != ""].drop_duplicates().sort_values("id").reset_index(drop=True)
    df_flags["flag_duplicate_ballots"] = 0
    df_flags["duplicate_pairs"] = 0
    df_flags["duplicate_ballot_rows"] = 0
    df_flags["duplicate_wallets"] = 0
    df_flags["flag_category"] = "not_flagged"
    df_flags["flag_reason"] = "No duplicate ballot signal"

    if duplicate_pairs.empty:
        return df_flags.loc[:, FLAGS_COLUMNS]

    proposal_counts = duplicate_pairs.groupby("proposal", as_index=False).agg(
        duplicate_pairs=("voter", "size"),
        duplicate_ballot_rows=("n_ballots", "sum"),
        duplicate_wallets=("voter", "nunique"),
    )
    proposal_counts = proposal_counts.rename(columns={"proposal": "id"})
    df_flags = df_flags.merge(proposal_counts, on="id", how="left", suffixes=("", "_dup"))

    for column in ["duplicate_pairs", "duplicate_ballot_rows", "duplicate_wallets"]:
        merged_col = f"{column}_dup"
        if merged_col in df_flags.columns:
            df_flags[column] = df_flags[merged_col].fillna(df_flags[column]).astype(int)
            df_flags.drop(columns=[merged_col], inplace=True)

    flagged_mask = df_flags["duplicate_pairs"] > 0
    df_flags.loc[flagged_mask, "flag_duplicate_ballots"] = 1
    df_flags.loc[flagged_mask, "flag_category"] = "duplicate_ballots"
    df_flags.loc[flagged_mask, "flag_reason"] = (
        "Contains repeated proposal+voter ballot histories in raw Snapshot votes"
    )

    return df_flags.loc[:, FLAGS_COLUMNS].sort_values("id").reset_index(drop=True)


def build_duplicate_ballot_outputs(
    df_votes: pd.DataFrame,
    proposal_ids=None,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    if proposal_ids is None:
        proposal_ids = pd.Series(dtype="object")
    duplicate_pairs = summarize_duplicate_ballot_pairs(df_votes)
    df_flags = build_duplicate_ballot_flags(proposal_ids, duplicate_pairs)
    return df_flags, duplicate_pairs


def _iter_vote_rows():
    for vote_path in sorted(VOTES_DIR.glob("*")):
        if vote_path.suffix not in {".csv", ".gz"} and not vote_path.name.endswith(".csv.gz"):
            continue
        opener = gzip.open if vote_path.name.endswith(".gz") else open
        with opener(vote_path, "rt", encoding="utf-8", newline="") as handle:
            reader = csv.DictReader(handle)
            for row in reader:
                yield {
                    "proposal": (row.get("proposal") or "").strip().lower(),
                    "voter": (row.get("voter") or "").strip().lower(),
                    "created": row.get("created"),
                    "id": row.get("id"),
                }


def _load_proposal_ids() -> pd.Series:
    df_props = pd.read_csv(PROPOSALS_RAW_FILE, usecols=["id"], low_memory=False)
    return normalize_id_series(df_props["id"])


def _bucket_index(proposal: str, voter: str, bucket_count: int) -> int:
    key = f"{proposal}\t{voter}".encode("utf-8")
    return zlib.crc32(key) % bucket_count


def _coerce_created_value(value) -> tuple[int, float]:
    try:
        if pd.isna(value):
            return (1, float("inf"))
    except Exception:
        pass

    try:
        return (0, float(value))
    except Exception:
        return (1, float("inf"))


def build_and_write_duplicate_ballot_outputs(bucket_count: int = DEFAULT_BUCKET_COUNT) -> tuple[pd.DataFrame, pd.DataFrame]:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    proposal_ids = _load_proposal_ids()
    duplicate_rows = []
    proposal_summary = {}

    with tempfile.TemporaryDirectory(prefix="duplicate_ballots_", dir=str(OUTPUT_DIR)) as tmpdir:
        bucket_paths = [Path(tmpdir) / f"bucket_{idx:03d}.csv" for idx in range(bucket_count)]
        bucket_handles = []
        bucket_writers = []
        try:
            for bucket_path in bucket_paths:
                handle = open(bucket_path, "w", encoding="utf-8", newline="")
                writer = csv.writer(handle)
                writer.writerow(["proposal", "voter", "created", "id"])
                bucket_handles.append(handle)
                bucket_writers.append(writer)

            for row in _iter_vote_rows():
                proposal = row["proposal"]
                voter = row["voter"]
                if proposal == "" or voter == "":
                    continue
                bucket_idx = _bucket_index(proposal, voter, bucket_count)
                bucket_writers[bucket_idx].writerow([proposal, voter, row["created"], row["id"]])
        finally:
            for handle in bucket_handles:
                handle.close()

        with gzip.open(CANONICAL_PAIRS_FILE, "wt", encoding="utf-8", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=PAIRS_COLUMNS)
            writer.writeheader()

            for bucket_path in bucket_paths:
                if not bucket_path.exists() or bucket_path.stat().st_size == 0:
                    continue

                bucket_df = pd.read_csv(bucket_path, low_memory=False)
                if bucket_df.empty:
                    continue

                pair_state = {}
                for row in bucket_df.to_dict("records"):
                    proposal = str(row.get("proposal", "")).strip().lower()
                    voter = str(row.get("voter", "")).strip().lower()
                    if proposal == "" or voter == "":
                        continue

                    created_value = row.get("created")
                    created_key = _coerce_created_value(created_value)
                    vote_id = "" if pd.isna(row.get("id")) else str(row.get("id"))
                    key = (proposal, voter)

                    if key not in pair_state:
                        pair_state[key] = {
                            "proposal": proposal,
                            "voter": voter,
                            "n_ballots": 0,
                            "first_created": created_value,
                            "last_created": created_value,
                            "first_vote_id": vote_id,
                            "last_vote_id": vote_id,
                            "_first_key": (created_key, vote_id),
                            "_last_key": (created_key, vote_id),
                        }

                    state = pair_state[key]
                    state["n_ballots"] += 1

                    if (created_key, vote_id) < state["_first_key"]:
                        state["first_created"] = created_value
                        state["first_vote_id"] = vote_id
                        state["_first_key"] = (created_key, vote_id)

                    if (created_key, vote_id) > state["_last_key"]:
                        state["last_created"] = created_value
                        state["last_vote_id"] = vote_id
                        state["_last_key"] = (created_key, vote_id)

                for key in sorted(pair_state):
                    state = pair_state[key]
                    if state["n_ballots"] <= 1:
                        continue

                    out_row = {
                        "proposal": state["proposal"],
                        "voter": state["voter"],
                        "n_ballots": state["n_ballots"],
                        "first_created": pd.to_numeric(state["first_created"], errors="coerce"),
                        "last_created": pd.to_numeric(state["last_created"], errors="coerce"),
                        "first_vote_id": state["first_vote_id"],
                        "last_vote_id": state["last_vote_id"],
                    }
                    writer.writerow(out_row)
                    duplicate_rows.append(out_row)

                    summary = proposal_summary.setdefault(
                        state["proposal"],
                        {
                            "duplicate_pairs": 0,
                            "duplicate_ballot_rows": 0,
                            "duplicate_wallets": set(),
                        },
                    )
                    summary["duplicate_pairs"] += 1
                    summary["duplicate_ballot_rows"] += int(state["n_ballots"])
                    summary["duplicate_wallets"].add(state["voter"])

        duplicate_pairs = pd.DataFrame(duplicate_rows, columns=PAIRS_COLUMNS)
        if not duplicate_pairs.empty:
            duplicate_pairs["first_created"] = pd.to_numeric(duplicate_pairs["first_created"], errors="coerce").astype("Int64")
            duplicate_pairs["last_created"] = pd.to_numeric(duplicate_pairs["last_created"], errors="coerce").astype("Int64")

    df_flags = build_duplicate_ballot_flags(proposal_ids, duplicate_pairs)

    df_flags.to_csv(CANONICAL_FLAGS_FILE, index=False, compression="gzip")
    return df_flags, duplicate_pairs


def main() -> None:
    df_flags, duplicate_pairs = build_and_write_duplicate_ballot_outputs()
    print(f"Wrote {len(df_flags)} proposal duplicate-ballot flags to {CANONICAL_FLAGS_FILE}")
    print(f"Flagged proposals: {int(df_flags['flag_duplicate_ballots'].sum())}")
    print(f"Wrote {len(duplicate_pairs)} duplicate proposal-voter histories to {CANONICAL_PAIRS_FILE}")


if __name__ == "__main__":
    main()
