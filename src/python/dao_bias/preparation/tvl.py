from __future__ import annotations
from dao_bias.data.paths import OUTPUT_ROOT
#!/usr/bin/env python3
"""Fetch and derive DefiLlama Ethereum TVL robustness inputs."""


import argparse
import csv
import datetime as dt
import hashlib
import json
import math
import re
import statistics
import time
import urllib.parse
import urllib.request
from pathlib import Path


BASE_URL = "https://api.llama.fi"
PROTOCOLS_ENDPOINT = f"{BASE_URL}/protocols"
PROTOCOL_ENDPOINT_TEMPLATE = f"{BASE_URL}/protocol/{{slug}}"
# Undocumented endpoint backing the DefiLlama website; used for parent governance IDs.
# Shape may change without notice — parsing degrades gracefully rather than crashing.
PARENT_PROTOCOLS_ENDPOINT = f"{BASE_URL}/lite/protocols2"
PARENT_PROTOCOLS_CACHE_FILENAME = "defillama_parent_protocols_raw.json"
DEFAULT_PERIOD_START = dt.date(2020, 10, 1)
DEFAULT_PERIOD_END = dt.date(2023, 11, 30)
DEFAULT_CHAIN = "Ethereum"
DEFAULT_OUTPUT_DIR = Path("data/contributors")
DEFAULT_PROVENANCE_DIR = Path("metadata/analysis_data_provenance")
DEFAULT_TOP_N = 100
DEFAULT_TVL_COVERAGE_THRESHOLD = 0.80
PROTOCOL_TOP100_FILENAME = "defillama_protocol_ethereum_tvl_top100.csv"
PROTOCOL_DIAGNOSTICS_FILENAME = "defillama_protocol_ethereum_tvl_diagnostics.csv"
PROTOCOL_MANIFEST_FILENAME = "defillama_protocol_tvl_manifest.json"
SNAPSHOT_GOVERNANCE_RE = re.compile(r"snapshot:([A-Za-z0-9_.-]+)")
GOVERNANCE_FIELDS = (
    "governanceID",
    "governanceId",
    "governanceIDs",
    "governanceIds",
    "governance_ids",
    "governance",
)
PROTOCOL_FIELDNAMES = [
    "protocol_rank",
    "protocol_slug",
    "protocol_name",
    "protocol_id",
    "category",
    "parent_protocol",
    "snapshot_governance_ids",
    "parent_snapshot_governance_ids",
    "mean_ethereum_tvl_usd",
    "median_ethereum_tvl_usd",
    "tvl_day_count",
    "expected_period_days",
    "tvl_coverage_ratio",
    "first_tvl_date",
    "last_tvl_date",
    "cache_file",
    "cache_checksum",
    "included_in_top100",
    "excluded_reason",
    "tvl_eligible_for_ranking",
    "ranking_exclusion_reason",
    "fetch_error",
]


def normalize_snapshot_space(space: str) -> str:
    return space.strip().lower()


def build_parent_governance_lookup(parent_protocols_payload) -> dict[str, list[str]]:
    """Build parent_id -> [snapshot_space, ...] from the lite/protocols2 payload.

    Degrades gracefully: missing/empty parentProtocols array, missing governance
    fields per entry, and non-dict entries all produce empty results rather than
    raising. This is intentional — the endpoint is undocumented and may change shape.
    """
    lookup: dict[str, list[str]] = {}
    if not isinstance(parent_protocols_payload, dict):
        return lookup
    entries = parent_protocols_payload.get("parentProtocols")
    if not entries or not isinstance(entries, list):
        return lookup
    for entry in entries:
        if not isinstance(entry, dict):
            continue
        parent_id = entry.get("id")
        if not parent_id or not isinstance(parent_id, str):
            continue
        ids = parse_snapshot_governance_ids(entry)
        if ids:
            lookup[parent_id] = ids
    return lookup


def parse_snapshot_governance_ids(protocol: dict) -> list[str]:
    spaces = set()
    for field in GOVERNANCE_FIELDS:
        value = protocol.get(field)
        if value is None:
            continue
        values = value if isinstance(value, list) else [value]
        for item in values:
            if item is None:
                continue
            for match in SNAPSHOT_GOVERNANCE_RE.findall(str(item)):
                normalized = normalize_snapshot_space(match)
                if normalized:
                    spaces.add(normalized)
    return sorted(spaces)


def parse_date(value) -> dt.date | None:
    if value is None:
        return None
    if isinstance(value, dt.date):
        return value
    if isinstance(value, (int, float)):
        return dt.datetime.fromtimestamp(value, tz=dt.timezone.utc).date()
    text = str(value).strip()
    if not text:
        return None
    if text.isdigit():
        return dt.datetime.fromtimestamp(int(text), tz=dt.timezone.utc).date()
    try:
        return dt.datetime.fromisoformat(text.replace("Z", "+00:00")).date()
    except ValueError:
        return None


def tvl_value(row: dict) -> float | None:
    for key in ("totalLiquidityUSD", "totalLiquidityUsd", "tvl", "totalLiquidity"):
        value = row.get(key)
        if value is None or value == "":
            continue
        try:
            return float(value)
        except (TypeError, ValueError):
            continue
    return None


def extract_ethereum_tvl_rows(
    protocol_history: dict,
    start_date: dt.date = DEFAULT_PERIOD_START,
    end_date: dt.date = DEFAULT_PERIOD_END,
) -> list[dict]:
    chain_tvls = protocol_history.get("chainTvls") or {}
    ethereum_key = next(
        (key for key in chain_tvls if str(key).lower() == DEFAULT_CHAIN.lower()),
        None,
    )
    if ethereum_key is None:
        return []

    ethereum_payload = chain_tvls[ethereum_key]
    tvl_rows = ethereum_payload.get("tvl", ethereum_payload) if isinstance(ethereum_payload, dict) else ethereum_payload
    if not isinstance(tvl_rows, list):
        return []

    by_date = {}
    for row in tvl_rows:
        if not isinstance(row, dict):
            continue
        row_date = parse_date(row.get("date"))
        if row_date is None or row_date < start_date or row_date > end_date:
            continue
        value = tvl_value(row)
        if value is None:
            continue
        by_date[row_date.isoformat()] = value
    return [
        {"date": row_date, "tvl_usd": by_date[row_date]}
        for row_date in sorted(by_date)
    ]


def median_tvl(rows: list[dict]) -> float | None:
    values = [float(row["tvl_usd"]) for row in rows if row.get("tvl_usd") is not None]
    if not values:
        return None
    return float(statistics.median(values))


def mean_tvl(rows: list[dict]) -> float | None:
    values = [float(row["tvl_usd"]) for row in rows if row.get("tvl_usd") is not None]
    if not values:
        return None
    return float(statistics.fmean(values))


def expected_period_days(start_date: dt.date, end_date: dt.date) -> int:
    if end_date < start_date:
        raise ValueError("period end must be on or after period start")
    return (end_date - start_date).days + 1


def minimum_tvl_day_count(expected_days: int, coverage_threshold: float) -> int:
    return int(math.ceil(expected_days * coverage_threshold))


def is_cex_protocol(protocol: dict) -> bool:
    category = str(protocol.get("category") or "").strip().lower()
    return category == "cex" or "centralized exchange" in category


def has_ethereum_history(protocol_history: dict) -> bool:
    chain_tvls = protocol_history.get("chainTvls") or {}
    return any(str(key).lower() == DEFAULT_CHAIN.lower() for key in chain_tvls)


def protocol_diagnostic_row(
    protocol: dict,
    expected_days: int,
    cache_file: str = "",
    cache_checksum: str = "",
    fetch_error: str = "",
    parent_governance_lookup: dict[str, list[str]] | None = None,
) -> dict:
    parent_id = protocol.get("parentProtocol") or ""
    parent_ids: list[str] = []
    if parent_id and parent_governance_lookup is not None:
        parent_ids = parent_governance_lookup.get(parent_id, [])
    return {
        "protocol_rank": "",
        "protocol_slug": str(protocol.get("slug") or "").strip(),
        "protocol_name": protocol.get("name") or "",
        "protocol_id": protocol.get("id") or "",
        "category": protocol.get("category") or "",
        "parent_protocol": parent_id,
        "snapshot_governance_ids": ";".join(parse_snapshot_governance_ids(protocol)),
        "parent_snapshot_governance_ids": ";".join(parent_ids),
        "mean_ethereum_tvl_usd": "",
        "median_ethereum_tvl_usd": "",
        "tvl_day_count": 0,
        "expected_period_days": expected_days,
        "tvl_coverage_ratio": 0.0,
        "first_tvl_date": "",
        "last_tvl_date": "",
        "cache_file": cache_file,
        "cache_checksum": cache_checksum,
        "included_in_top100": False,
        "excluded_reason": "",
        "tvl_eligible_for_ranking": False,
        "ranking_exclusion_reason": "",
        "fetch_error": fetch_error,
    }


def derive_protocol_tvl_tables(
    protocols: list[dict],
    protocol_histories: dict[str, dict],
    top_n: int = DEFAULT_TOP_N,
    start_date: dt.date = DEFAULT_PERIOD_START,
    end_date: dt.date = DEFAULT_PERIOD_END,
    coverage_threshold: float = DEFAULT_TVL_COVERAGE_THRESHOLD,
    cache_metadata: dict[str, dict] | None = None,
    fetch_errors: list[dict] | None = None,
    exclude_cex: bool = True,
    parent_governance_lookup: dict[str, list[str]] | None = None,
) -> tuple[list[dict], list[dict]]:
    expected_days = expected_period_days(start_date, end_date)
    min_days = minimum_tvl_day_count(expected_days, coverage_threshold)
    cache_metadata = cache_metadata or {}
    fetch_errors_by_slug = {
        str(row.get("protocol_slug") or ""): str(row.get("error") or "")
        for row in (fetch_errors or [])
    }
    diagnostics = []

    for protocol in protocols:
        slug = str(protocol.get("slug") or "").strip()
        if not slug:
            continue
        cache_row = cache_metadata.get(slug, {})
        row = protocol_diagnostic_row(
            protocol,
            expected_days,
            cache_file=cache_row.get("cache_file", ""),
            cache_checksum=cache_row.get("cache_checksum", ""),
            fetch_error=fetch_errors_by_slug.get(slug, ""),
            parent_governance_lookup=parent_governance_lookup,
        )

        if exclude_cex and is_cex_protocol(protocol):
            row["ranking_exclusion_reason"] = "cex_category"
            row["excluded_reason"] = "cex_category"
            diagnostics.append(row)
            continue

        if slug in fetch_errors_by_slug:
            row["ranking_exclusion_reason"] = "fetch_error"
            row["excluded_reason"] = "fetch_error"
            diagnostics.append(row)
            continue

        history = protocol_histories.get(slug) or {}
        if not has_ethereum_history(history):
            row["ranking_exclusion_reason"] = "no_ethereum_history"
            row["excluded_reason"] = "no_ethereum_history"
            diagnostics.append(row)
            continue

        tvl_rows = extract_ethereum_tvl_rows(history, start_date=start_date, end_date=end_date)
        if not tvl_rows:
            row["ranking_exclusion_reason"] = "no_period_tvl"
            row["excluded_reason"] = "no_period_tvl"
            diagnostics.append(row)
            continue

        row["mean_ethereum_tvl_usd"] = mean_tvl(tvl_rows)
        row["median_ethereum_tvl_usd"] = median_tvl(tvl_rows)
        row["tvl_day_count"] = len(tvl_rows)
        row["tvl_coverage_ratio"] = len(tvl_rows) / expected_days
        row["first_tvl_date"] = tvl_rows[0]["date"]
        row["last_tvl_date"] = tvl_rows[-1]["date"]

        if len(tvl_rows) < min_days:
            row["ranking_exclusion_reason"] = "insufficient_tvl_coverage"
            row["excluded_reason"] = "insufficient_tvl_coverage"
            diagnostics.append(row)
            continue

        row["tvl_eligible_for_ranking"] = True
        diagnostics.append(row)

    eligible = sorted(
        [row for row in diagnostics if row["tvl_eligible_for_ranking"]],
        key=lambda row: (-float(row["median_ethereum_tvl_usd"]), row["protocol_slug"]),
    )
    for rank, row in enumerate(eligible, start=1):
        row["protocol_rank"] = rank
        if rank <= top_n:
            row["included_in_top100"] = True
            row["ranking_exclusion_reason"] = ""
            row["excluded_reason"] = ""
        else:
            row["ranking_exclusion_reason"] = "outside_top100"
            row["excluded_reason"] = "outside_top100"

    top_rows = [row.copy() for row in eligible if row["included_in_top100"]]
    diagnostics = sorted(
        diagnostics,
        key=lambda row: (
            row["protocol_rank"] == "",
            row["protocol_rank"] if row["protocol_rank"] != "" else 10**9,
            row["protocol_slug"],
        ),
    )
    return diagnostics, top_rows


def write_json(path: Path, payload) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding="utf-8")


def read_json(path: Path):
    return json.loads(path.read_text(encoding="utf-8"))


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def write_csv(path: Path, rows: list[dict], fieldnames: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def resolve_analysis_path(path: Path, analysis_root: Path) -> Path:
    return path if path.is_absolute() else analysis_root / path


def manifest_analysis_path(path: Path, analysis_root: Path) -> str:
    try:
        return str(path.relative_to(analysis_root))
    except ValueError:
        return str(path)


def fetch_json(url: str, attempts: int = 3):
    request = urllib.request.Request(url, headers={"User-Agent": "daos-bias-tvl-robustness/1.0"})
    last_error = None
    for attempt in range(1, attempts + 1):
        try:
            with urllib.request.urlopen(request, timeout=60) as response:
                return json.loads(response.read().decode("utf-8"))
        except Exception as err:
            last_error = err
            if attempt == attempts:
                break
            time.sleep(2 * attempt)
    raise last_error


def fetch_protocol_history(slug: str):
    encoded_slug = urllib.parse.quote(slug, safe="")
    return fetch_json(PROTOCOL_ENDPOINT_TEMPLATE.format(slug=encoded_slug))


def build_manifest(
    protocol_slugs: list[str],
    period_start: dt.date = DEFAULT_PERIOD_START,
    period_end: dt.date = DEFAULT_PERIOD_END,
    top_n: int = DEFAULT_TOP_N,
    expected_period_days: int | None = None,
    coverage_threshold: float = DEFAULT_TVL_COVERAGE_THRESHOLD,
    fetch_date: str | None = None,
    fetch_errors: list[dict] | None = None,
    cache_checksums: dict[str, str] | None = None,
    allow_partial: bool = False,
    cache_only: bool = False,
    exclude_cex: bool = True,
    raw_cache_dir: Path | str | None = None,
    final_artifacts: list[str] | None = None,
    parent_protocols_fetch_date: str | None = None,
) -> dict:
    expected_days = expected_period_days or globals()["expected_period_days"](period_start, period_end)
    today = dt.date.today().isoformat()
    return {
        "fetch_date": fetch_date or today,
        "fetch_timestamp_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
        "source": "DefiLlama free API",
        "source_docs": {
            "defillama": "https://defillama.com/docs/api",
            "coingecko_tvl_note": "https://support.coingecko.com/hc/en-us/articles/48323807003545-How-to-update-TVL-on-my-token-page",
        },
        "endpoints": {
            "protocols": PROTOCOLS_ENDPOINT,
            "protocol": PROTOCOL_ENDPOINT_TEMPLATE,
            "parent_protocols": {
                "url": PARENT_PROTOCOLS_ENDPOINT,
                "status": "undocumented_website_backing",
                "note": (
                    "Undocumented endpoint backing the DefiLlama website UI. "
                    "Used to inherit parent-level Snapshot governance IDs into child protocol rows. "
                    "Shape may change without notice; parsing degrades gracefully."
                ),
            },
        },
        "period": {
            "start": period_start.isoformat(),
            "end": period_end.isoformat(),
        },
        "chain": DEFAULT_CHAIN,
        "top_n": top_n,
        "ranking_metric": "median_ethereum_tvl_usd",
        "expected_period_days": expected_days,
        "tvl_coverage_threshold": coverage_threshold,
        "minimum_tvl_day_count": minimum_tvl_day_count(expected_days, coverage_threshold),
        "snapshot_input_used": False,
        "snapshot_input_files": [],
        "raw_cache_dir": str(raw_cache_dir) if raw_cache_dir else None,
        "final_artifacts": final_artifacts or [
            PROTOCOL_TOP100_FILENAME,
            PROTOCOL_DIAGNOSTICS_FILENAME,
            PROTOCOL_MANIFEST_FILENAME,
        ],
        "duplicate_date_policy": "Deduplicate Ethereum TVL observations by UTC date before statistics; last_observation_wins.",
        "missing_tvl_policy": "Do not zero-fill or forward-fill missing TVL days; rank on observed historical Ethereum rows only.",
        "cex_policy": (
            "Exclude CEX protocols before fetching protocol history and before ranking."
            if exclude_cex else
            "Include CEX protocols during fetch and ranking; apply any CEX filtering in downstream subsets."
        ),
        "aggregation_rules": (
            "Compute each non-CEX protocol slug's observed daily Ethereum TVL over the study period, "
            "deduplicate by UTC date, require the configured coverage threshold, rank eligible "
            "protocols by period median Ethereum TVL, and keep the Top N protocol-level rows. "
            "Snapshot-space matching is intentionally deferred to the R robustness script. "
            "Parent-level Snapshot governance IDs are inherited from the parentProtocol field via "
            "the lite/protocols2 endpoint and stored in parent_snapshot_governance_ids (separate "
            "from child-level snapshot_governance_ids) so match provenance stays distinct."
        ),
        "protocol_slugs_fetched": sorted(protocol_slugs),
        "fetch_errors": fetch_errors or [],
        "partial_results": {
            "allowed": bool(allow_partial),
            "cache_only": bool(cache_only),
            "error_count": len(fetch_errors or []),
            "policy": "--allow-partial must be set before final artifacts are written with fetch errors.",
        },
        "cache_checksums": cache_checksums or {},
        "parent_governance_vintage": {
            "protocols_list_fetch_date": fetch_date or today,
            "parent_protocols_fetch_date": parent_protocols_fetch_date or today,
            "note": (
                "Child protocol governance IDs and TVL stats were fetched on protocols_list_fetch_date. "
                "Parent protocol governance IDs (parent_snapshot_governance_ids column) were fetched "
                "on parent_protocols_fetch_date. When these dates differ, the two sources have "
                "different vintages; both are auditable via cache_checksums."
            ),
        },
    }


def parse_args(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    parser.add_argument("--provenance-dir", type=Path, default=DEFAULT_PROVENANCE_DIR)
    parser.add_argument("--period-start", type=dt.date.fromisoformat, default=DEFAULT_PERIOD_START)
    parser.add_argument("--period-end", type=dt.date.fromisoformat, default=DEFAULT_PERIOD_END)
    parser.add_argument("--top-n", type=int, default=DEFAULT_TOP_N)
    parser.add_argument("--tvl-coverage-threshold", type=float, default=DEFAULT_TVL_COVERAGE_THRESHOLD)
    parser.add_argument("--reuse-raw", action="store_true")
    parser.add_argument("--cache-only", action="store_true")
    parser.add_argument("--allow-partial", action="store_true")
    parser.add_argument("--include-cex", action="store_true")
    return parser.parse_args(argv)


def main(argv=None) -> int:
    args = parse_args(argv)
    analysis_root = OUTPUT_ROOT
    output_dir = resolve_analysis_path(args.output_dir, analysis_root)
    provenance_dir = resolve_analysis_path(args.provenance_dir, analysis_root)
    protocols_path = output_dir / "defillama_protocols_raw.json"
    histories_dir = output_dir / "defillama_protocol_history"
    top_path = provenance_dir / PROTOCOL_TOP100_FILENAME
    diagnostics_path = provenance_dir / PROTOCOL_DIAGNOSTICS_FILENAME
    manifest_path = provenance_dir / PROTOCOL_MANIFEST_FILENAME

    parent_protocols_path = output_dir / PARENT_PROTOCOLS_CACHE_FILENAME

    if args.cache_only and not args.reuse_raw:
        raise ValueError("--cache-only requires --reuse-raw so live DefiLlama requests are not made.")
    if args.cache_only and not protocols_path.exists():
        raise RuntimeError(f"--cache-only requested but cached protocols file is missing: {protocols_path}")
    if args.cache_only and not parent_protocols_path.exists():
        raise RuntimeError(
            f"--cache-only requested but cached parent protocols file is missing: {parent_protocols_path}"
        )

    if args.reuse_raw and protocols_path.exists():
        protocols = read_json(protocols_path)
    else:
        protocols = fetch_json(PROTOCOLS_ENDPOINT)
        write_json(protocols_path, protocols)
    cache_checksums = {
        manifest_analysis_path(protocols_path, analysis_root): sha256_file(protocols_path)
    }

    if args.reuse_raw and parent_protocols_path.exists():
        parent_protocols_payload = read_json(parent_protocols_path)
        print(f"Loaded parent protocols from cache: {parent_protocols_path}")
    else:
        try:
            parent_protocols_payload = fetch_json(PARENT_PROTOCOLS_ENDPOINT)
            write_json(parent_protocols_path, parent_protocols_payload)
            print(f"Fetched parent protocols from {PARENT_PROTOCOLS_ENDPOINT}")
        except Exception as err:
            print(f"Warning: failed to fetch parent protocols ({err}); parent governance IDs will be empty.")
            parent_protocols_payload = {}
    parent_governance_lookup = build_parent_governance_lookup(parent_protocols_payload)
    if parent_protocols_path.exists():
        cache_checksums[manifest_analysis_path(parent_protocols_path, analysis_root)] = sha256_file(
            parent_protocols_path
        )
    print(f"Built parent governance lookup with {len(parent_governance_lookup)} parent protocol(s)")

    candidate_protocols = [
        protocol for protocol in protocols
        if str(protocol.get("slug") or "").strip()
    ]
    exclude_cex = not args.include_cex
    fetch_protocols = [
        protocol for protocol in candidate_protocols
        if args.include_cex or not is_cex_protocol(protocol)
    ]
    print(
        f"Loaded {len(candidate_protocols)} DefiLlama protocol slug(s); "
        f"fetching {len(fetch_protocols)} {'all' if args.include_cex else 'non-CEX'} history payload(s)"
    )

    histories_dir.mkdir(parents=True, exist_ok=True)
    histories = {}
    cache_metadata = {}
    fetch_errors = []
    for protocol in fetch_protocols:
        slug = str(protocol["slug"])
        history_path = histories_dir / f"{slug}.json"
        if args.reuse_raw and history_path.exists():
            history = read_json(history_path)
        elif args.cache_only:
            fetch_errors.append({"protocol_slug": slug, "error": "missing_cached_history"})
            continue
        else:
            try:
                history = fetch_protocol_history(slug)
            except Exception as err:
                fetch_errors.append({"protocol_slug": slug, "error": str(err)})
                print(f"Skipping {slug}: {err}")
                continue
            write_json(history_path, history)
        histories[slug] = history
        history_manifest_path = manifest_analysis_path(history_path, analysis_root)
        history_checksum = sha256_file(history_path)
        cache_checksums[history_manifest_path] = history_checksum
        cache_metadata[slug] = {
            "cache_file": history_manifest_path,
            "cache_checksum": history_checksum,
        }
        if len(histories) % 10 == 0:
            print(f"Loaded {len(histories)} protocol history payload(s)")

    if fetch_errors and not args.allow_partial:
        error_summary = "; ".join(
            f"{row['protocol_slug']}: {row['error']}" for row in fetch_errors[:5]
        )
        raise RuntimeError(
            "DefiLlama protocol history fetch failed before final artifacts were written. "
            "Rerun with --allow-partial to write explicitly partial artifacts. "
            f"First error(s): {error_summary}"
        )

    diagnostics, top_rows = derive_protocol_tvl_tables(
        candidate_protocols,
        histories,
        top_n=args.top_n,
        start_date=args.period_start,
        end_date=args.period_end,
        coverage_threshold=args.tvl_coverage_threshold,
        cache_metadata=cache_metadata,
        fetch_errors=fetch_errors,
        exclude_cex=exclude_cex,
        parent_governance_lookup=parent_governance_lookup,
    )
    write_csv(top_path, top_rows, PROTOCOL_FIELDNAMES)
    write_csv(diagnostics_path, diagnostics, PROTOCOL_FIELDNAMES)
    manifest = build_manifest(
        protocol_slugs=sorted(histories),
        period_start=args.period_start,
        period_end=args.period_end,
        top_n=args.top_n,
        expected_period_days=expected_period_days(args.period_start, args.period_end),
        coverage_threshold=args.tvl_coverage_threshold,
        fetch_errors=fetch_errors,
        cache_checksums=cache_checksums,
        allow_partial=args.allow_partial,
        cache_only=args.cache_only,
        exclude_cex=exclude_cex,
        raw_cache_dir=manifest_analysis_path(output_dir, analysis_root),
        final_artifacts=[
            manifest_analysis_path(top_path, analysis_root),
            manifest_analysis_path(diagnostics_path, analysis_root),
            manifest_analysis_path(manifest_path, analysis_root),
        ],
        parent_protocols_fetch_date=dt.date.today().isoformat(),
    )
    write_json(manifest_path, manifest)

    print(f"Wrote {top_path}")
    print(f"Wrote {diagnostics_path}")
    print(f"Wrote {manifest_path}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError, RuntimeError) as error:
        raise SystemExit(str(error)) from None
