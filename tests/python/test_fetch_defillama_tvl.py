import datetime as dt
import json
import sys
import tempfile
import unittest
from pathlib import Path


ANALYSIS_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.preparation.tvl as fetch_defillama_tvl


def ts(date_text):
    return int(dt.datetime.fromisoformat(date_text).replace(tzinfo=dt.timezone.utc).timestamp())


def period_rows(start, values):
    start_date = dt.date.fromisoformat(start)
    return [
        {
            "date": int(dt.datetime.combine(
                start_date + dt.timedelta(days=index),
                dt.time(),
                tzinfo=dt.timezone.utc,
            ).timestamp()),
            "totalLiquidityUSD": value,
        }
        for index, value in enumerate(values)
    ]


def protocol(slug, category="Dexes", chains=None, governance_id=None, name=None):
    row = {
        "id": slug,
        "slug": slug,
        "name": name or slug.title(),
        "category": category,
    }
    if chains is not None:
        row["chains"] = chains
    if governance_id is not None:
        row["governanceID"] = governance_id
    return row


class DefiLlamaTvlTests(unittest.TestCase):
    def test_parse_snapshot_governance_ids_from_mixed_fields(self):
        protocol = {
            "slug": "alpha",
            "governanceID": ["snapshot:Space-A.eth", "eip155:1:0x0000"],
            "governanceIds": "snapshot:space-b.eth, snapshot:space-a.eth",
            "other": {"governanceID": "snapshot:ignored.eth"},
        }

        self.assertEqual(
            fetch_defillama_tvl.parse_snapshot_governance_ids(protocol),
            ["space-a.eth", "space-b.eth"],
        )

    def test_extracts_ethereum_tvl_inside_period(self):
        history = {
            "chainTvls": {
                "Ethereum": {
                    "tvl": [
                        {"date": ts("2020-09-30"), "totalLiquidityUSD": 10},
                        {"date": ts("2020-10-01"), "totalLiquidityUSD": 100},
                        {"date": "2021-01-02", "tvl": 300},
                        {"date": ts("2023-12-01"), "totalLiquidityUSD": 900},
                    ]
                },
                "Arbitrum": {
                    "tvl": [{"date": ts("2021-01-02"), "totalLiquidityUSD": 700}]
                },
            }
        }

        rows = fetch_defillama_tvl.extract_ethereum_tvl_rows(
            history,
            start_date=dt.date(2020, 10, 1),
            end_date=dt.date(2023, 11, 30),
        )

        self.assertEqual([row["date"] for row in rows], ["2020-10-01", "2021-01-02"])
        self.assertEqual(fetch_defillama_tvl.median_tvl(rows), 200)

    def test_deduplicates_same_day_ethereum_tvl_before_statistics(self):
        history = {
            "chainTvls": {
                "Ethereum": {
                    "tvl": [
                        {"date": ts("2021-01-01"), "totalLiquidityUSD": 100},
                        {"date": ts("2021-01-01"), "totalLiquidityUSD": 900},
                        {"date": ts("2021-01-02"), "totalLiquidityUSD": 300},
                    ]
                }
            }
        }

        rows = fetch_defillama_tvl.extract_ethereum_tvl_rows(
            history,
            start_date=dt.date(2021, 1, 1),
            end_date=dt.date(2021, 1, 2),
        )

        self.assertEqual([row["date"] for row in rows], ["2021-01-01", "2021-01-02"])
        self.assertEqual(fetch_defillama_tvl.median_tvl(rows), 600)

    def test_protocol_level_ranking_uses_historical_ethereum_tvl_not_current_chain_gate(self):
        protocols = [
            protocol("cex-alpha", category="CEX", chains=["Ethereum"], governance_id="snapshot:cex.eth"),
            protocol("alpha", chains=["Arbitrum"], governance_id="snapshot:alpha.eth"),
            protocol("beta", chains=["Ethereum"], governance_id="snapshot:beta.eth"),
            protocol("gamma", chains=["Ethereum"], governance_id="snapshot:gamma.eth"),
            protocol("delta", chains=["Ethereum"], governance_id="snapshot:delta.eth"),
        ]
        histories = {
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 8)}}},
            "beta": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [200] * 7)}}},
            "gamma": {"chainTvls": {"Polygon": {"tvl": period_rows("2021-01-01", [300] * 10)}}},
            "delta": {"chainTvls": {"Ethereum": {"tvl": [{"date": ts("2020-12-31"), "totalLiquidityUSD": 400}]}}},
        }

        diagnostics, top_rows = fetch_defillama_tvl.derive_protocol_tvl_tables(
            protocols,
            histories,
            top_n=100,
            start_date=dt.date(2021, 1, 1),
            end_date=dt.date(2021, 1, 10),
            coverage_threshold=0.80,
        )

        self.assertEqual([row["protocol_slug"] for row in top_rows], ["alpha"])
        alpha = next(row for row in diagnostics if row["protocol_slug"] == "alpha")
        beta = next(row for row in diagnostics if row["protocol_slug"] == "beta")
        cex = next(row for row in diagnostics if row["protocol_slug"] == "cex-alpha")
        gamma = next(row for row in diagnostics if row["protocol_slug"] == "gamma")
        delta = next(row for row in diagnostics if row["protocol_slug"] == "delta")
        self.assertTrue(alpha["tvl_eligible_for_ranking"])
        self.assertEqual(alpha["tvl_day_count"], 8)
        self.assertEqual(alpha["expected_period_days"], 10)
        self.assertEqual(alpha["tvl_coverage_ratio"], 0.8)
        self.assertEqual(alpha["snapshot_governance_ids"], "alpha.eth")
        self.assertEqual(beta["ranking_exclusion_reason"], "insufficient_tvl_coverage")
        self.assertEqual(cex["ranking_exclusion_reason"], "cex_category")
        self.assertEqual(gamma["ranking_exclusion_reason"], "no_ethereum_history")
        self.assertEqual(delta["ranking_exclusion_reason"], "no_period_tvl")

    def test_include_cex_keeps_cex_in_fetch_and_ranking(self):
        protocols = [
            protocol("cex-alpha", category="CEX", governance_id="snapshot:cex.eth"),
            protocol("alpha", governance_id="snapshot:alpha.eth"),
        ]
        histories = {
            "cex-alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [500] * 10)}}},
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 10)}}},
        }

        diagnostics, top_rows = fetch_defillama_tvl.derive_protocol_tvl_tables(
            protocols,
            histories,
            top_n=100,
            start_date=dt.date(2021, 1, 1),
            end_date=dt.date(2021, 1, 10),
            coverage_threshold=0.80,
            exclude_cex=False,
        )

        self.assertEqual([row["protocol_slug"] for row in top_rows], ["cex-alpha", "alpha"])
        cex = next(row for row in diagnostics if row["protocol_slug"] == "cex-alpha")
        self.assertTrue(cex["tvl_eligible_for_ranking"])
        self.assertEqual(cex["ranking_exclusion_reason"], "")

    def test_writes_manifest_with_protocol_level_provenance_fields(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)
            manifest = fetch_defillama_tvl.build_manifest(
                protocol_slugs=["alpha", "beta"],
                period_start=dt.date(2020, 10, 1),
                period_end=dt.date(2023, 11, 30),
                top_n=100,
                expected_period_days=1156,
                coverage_threshold=0.80,
                fetch_date="2026-05-18",
                cache_checksums={"defillama_protocol_history/alpha.json": "abc123"},
                allow_partial=False,
            )
            fetch_defillama_tvl.write_json(output_dir / "manifest.json", manifest)

            loaded = json.loads((output_dir / "manifest.json").read_text(encoding="utf-8"))
            self.assertEqual(loaded["fetch_date"], "2026-05-18")
            self.assertEqual(loaded["period"], {"start": "2020-10-01", "end": "2023-11-30"})
            self.assertEqual(loaded["chain"], "Ethereum")
            self.assertEqual(loaded["top_n"], 100)
            self.assertEqual(loaded["ranking_metric"], "median_ethereum_tvl_usd")
            self.assertEqual(loaded["expected_period_days"], 1156)
            self.assertEqual(loaded["tvl_coverage_threshold"], 0.8)
            self.assertEqual(loaded["minimum_tvl_day_count"], 925)
            self.assertFalse(loaded["snapshot_input_used"])
            self.assertEqual(loaded["endpoints"]["protocols"], "https://api.llama.fi/protocols")
            self.assertIn("last_observation_wins", loaded["duplicate_date_policy"])
            self.assertEqual(loaded["protocol_slugs_fetched"], ["alpha", "beta"])
            self.assertEqual(
                loaded["cache_checksums"],
                {"defillama_protocol_history/alpha.json": "abc123"},
            )
            self.assertFalse(loaded["partial_results"]["allowed"])

    def test_fetch_errors_fail_closed_before_final_artifacts_unless_partial_allowed(self):
        protocols = [
            protocol("alpha", governance_id="snapshot:alpha.eth"),
            protocol("beta", governance_id="snapshot:beta.eth"),
        ]
        histories = {
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 10)}}},
        }

        old_fetch_json = fetch_defillama_tvl.fetch_json
        old_fetch_protocol_history = fetch_defillama_tvl.fetch_protocol_history
        try:
            fetch_defillama_tvl.fetch_json = lambda url: protocols

            def fake_history(slug):
                if slug == "beta":
                    raise RuntimeError("boom")
                return histories[slug]

            fetch_defillama_tvl.fetch_protocol_history = fake_history
            with tempfile.TemporaryDirectory() as tmpdir:
                output_dir = Path(tmpdir) / "raw"
                provenance_dir = Path(tmpdir) / "prov"
                with self.assertRaises(RuntimeError):
                    fetch_defillama_tvl.main([
                        "--output-dir", str(output_dir),
                        "--provenance-dir", str(provenance_dir),
                        "--period-start", "2021-01-01",
                        "--period-end", "2021-01-10",
                    ])
                self.assertFalse(
                    (provenance_dir / fetch_defillama_tvl.PROTOCOL_TOP100_FILENAME).exists()
                )

                exit_code = fetch_defillama_tvl.main([
                    "--output-dir", str(output_dir),
                    "--provenance-dir", str(provenance_dir),
                    "--period-start", "2021-01-01",
                    "--period-end", "2021-01-10",
                    "--allow-partial",
                    "--reuse-raw",
                ])
                self.assertEqual(exit_code, 0)
                manifest = json.loads(
                    (provenance_dir / fetch_defillama_tvl.PROTOCOL_MANIFEST_FILENAME).read_text(
                        encoding="utf-8"
                    )
                )
                self.assertTrue(manifest["partial_results"]["allowed"])
                self.assertEqual(manifest["fetch_errors"][0]["protocol_slug"], "beta")
        finally:
            fetch_defillama_tvl.fetch_json = old_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = old_fetch_protocol_history

    def test_cache_only_reuse_records_missing_history_without_live_fetching(self):
        protocols = [
            protocol("alpha", governance_id="snapshot:alpha.eth"),
            protocol("beta", governance_id="snapshot:beta.eth"),
        ]
        histories = {
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 10)}}},
        }
        parent_protocols_payload = {"parentProtocols": []}

        old_fetch_json = fetch_defillama_tvl.fetch_json
        old_fetch_protocol_history = fetch_defillama_tvl.fetch_protocol_history
        try:
            fetch_defillama_tvl.fetch_json = lambda url: protocols

            def fail_live_fetch(slug):
                raise AssertionError(f"live fetch should not run for {slug}")

            fetch_defillama_tvl.fetch_protocol_history = fail_live_fetch
            with tempfile.TemporaryDirectory() as tmpdir:
                output_dir = Path(tmpdir) / "raw"
                provenance_dir = Path(tmpdir) / "prov"
                output_dir.mkdir(parents=True)
                fetch_defillama_tvl.write_json(output_dir / "defillama_protocols_raw.json", protocols)
                history_dir = output_dir / "defillama_protocol_history"
                history_dir.mkdir()
                fetch_defillama_tvl.write_json(history_dir / "alpha.json", histories["alpha"])

                # Missing parent protocols cache → hard error under --cache-only (before artifact write)
                with self.assertRaises(RuntimeError) as ctx:
                    fetch_defillama_tvl.main([
                        "--output-dir", str(output_dir),
                        "--provenance-dir", str(provenance_dir),
                        "--period-start", "2021-01-01",
                        "--period-end", "2021-01-10",
                        "--reuse-raw",
                        "--cache-only",
                    ])
                self.assertIn("parent protocols file is missing", str(ctx.exception))
                self.assertFalse(
                    (provenance_dir / fetch_defillama_tvl.PROTOCOL_TOP100_FILENAME).exists()
                )

                # Create parent protocols cache so the subsequent run can proceed
                fetch_defillama_tvl.write_json(
                    output_dir / fetch_defillama_tvl.PARENT_PROTOCOLS_CACHE_FILENAME,
                    parent_protocols_payload,
                )

                # Now missing beta history → recorded as fetch error (missing_cached_history)
                exit_code = fetch_defillama_tvl.main([
                    "--output-dir", str(output_dir),
                    "--provenance-dir", str(provenance_dir),
                    "--period-start", "2021-01-01",
                    "--period-end", "2021-01-10",
                    "--reuse-raw",
                    "--cache-only",
                    "--allow-partial",
                ])
                self.assertEqual(exit_code, 0)
                manifest = json.loads(
                    (provenance_dir / fetch_defillama_tvl.PROTOCOL_MANIFEST_FILENAME).read_text(
                        encoding="utf-8"
                    )
                )
                self.assertTrue(manifest["partial_results"]["allowed"])
                self.assertEqual(
                    manifest["fetch_errors"],
                    [{"protocol_slug": "beta", "error": "missing_cached_history"}],
                )
        finally:
            fetch_defillama_tvl.fetch_json = old_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = old_fetch_protocol_history



    def test_relative_paths_resolve_against_analysis_root_for_reproducibility(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            analysis_root = Path(tmpdir)
            self.assertEqual(
                fetch_defillama_tvl.resolve_analysis_path(Path("data/contributors"), analysis_root),
                analysis_root / "data/contributors",
            )
            self.assertEqual(
                fetch_defillama_tvl.manifest_analysis_path(
                    analysis_root / "data/2024/derived/current.csv",
                    analysis_root,
                ),
                "data/2024/derived/current.csv",
            )


class ParentProtocolTests(unittest.TestCase):
    def _parent_payload(self, entries):
        return {"parentProtocols": entries}

    def test_build_parent_governance_lookup_extracts_snapshot_ids(self):
        payload = self._parent_payload([
            {
                "id": "parent#curve-finance",
                "governanceID": ["snapshot:curve.eth"],
                "name": "Curve Finance",
            },
            {
                "id": "parent#aave",
                "governanceID": "snapshot:aave.eth",
                "name": "Aave",
            },
        ])
        lookup = fetch_defillama_tvl.build_parent_governance_lookup(payload)
        self.assertEqual(lookup.get("parent#curve-finance"), ["curve.eth"])
        self.assertEqual(lookup.get("parent#aave"), ["aave.eth"])

    def test_build_parent_governance_lookup_empty_payload(self):
        self.assertEqual(fetch_defillama_tvl.build_parent_governance_lookup({}), {})

    def test_build_parent_governance_lookup_missing_parentprotocols_key(self):
        self.assertEqual(fetch_defillama_tvl.build_parent_governance_lookup({"other": []}), {})

    def test_build_parent_governance_lookup_non_dict_payload(self):
        self.assertEqual(fetch_defillama_tvl.build_parent_governance_lookup([]), {})
        self.assertEqual(fetch_defillama_tvl.build_parent_governance_lookup(None), {})

    def test_build_parent_governance_lookup_non_dict_entry(self):
        payload = self._parent_payload([
            "not-a-dict",
            None,
            {"id": "parent#good", "governanceID": "snapshot:good.eth"},
        ])
        lookup = fetch_defillama_tvl.build_parent_governance_lookup(payload)
        self.assertEqual(list(lookup.keys()), ["parent#good"])

    def test_build_parent_governance_lookup_entry_missing_governance(self):
        payload = self._parent_payload([
            {"id": "parent#no-gov", "name": "No Governance"},
            {"id": "parent#with-gov", "governanceID": "snapshot:with.eth"},
        ])
        lookup = fetch_defillama_tvl.build_parent_governance_lookup(payload)
        self.assertNotIn("parent#no-gov", lookup)
        self.assertIn("parent#with-gov", lookup)

    def test_build_parent_governance_lookup_entry_missing_id(self):
        payload = self._parent_payload([
            {"name": "No ID", "governanceID": "snapshot:noid.eth"},
            {"id": "parent#good", "governanceID": "snapshot:good.eth"},
        ])
        lookup = fetch_defillama_tvl.build_parent_governance_lookup(payload)
        self.assertEqual(list(lookup.keys()), ["parent#good"])

    def test_protocol_diagnostic_row_inherits_parent_governance_ids(self):
        p = {
            "slug": "curve-dex",
            "name": "Curve DEX",
            "id": "curve-dex-id",
            "category": "Dexes",
            "parentProtocol": "parent#curve-finance",
        }
        lookup = {"parent#curve-finance": ["curve.eth"]}
        row = fetch_defillama_tvl.protocol_diagnostic_row(p, 100, parent_governance_lookup=lookup)
        self.assertEqual(row["parent_snapshot_governance_ids"], "curve.eth")
        self.assertEqual(row["snapshot_governance_ids"], "")
        self.assertEqual(row["parent_protocol"], "parent#curve-finance")

    def test_protocol_diagnostic_row_no_parent_lookup_is_empty(self):
        p = {
            "slug": "curve-dex",
            "name": "Curve DEX",
            "id": "curve-dex-id",
            "category": "Dexes",
            "parentProtocol": "parent#curve-finance",
        }
        row = fetch_defillama_tvl.protocol_diagnostic_row(p, 100)
        self.assertEqual(row["parent_snapshot_governance_ids"], "")

    def test_protocol_diagnostic_row_parent_not_in_lookup(self):
        p = {
            "slug": "unknown-child",
            "name": "Unknown",
            "id": "unk",
            "category": "Dexes",
            "parentProtocol": "parent#unknown",
        }
        lookup = {"parent#curve-finance": ["curve.eth"]}
        row = fetch_defillama_tvl.protocol_diagnostic_row(p, 100, parent_governance_lookup=lookup)
        self.assertEqual(row["parent_snapshot_governance_ids"], "")

    def test_derive_protocol_tvl_tables_populates_parent_ids(self):
        protocols = [
            {
                "id": "curve-dex-id",
                "slug": "curve-dex",
                "name": "Curve DEX",
                "category": "Dexes",
                "parentProtocol": "parent#curve-finance",
            },
        ]
        histories = {
            "curve-dex": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [500] * 10)}}},
        }
        lookup = {"parent#curve-finance": ["curve.eth"]}
        diagnostics, top_rows = fetch_defillama_tvl.derive_protocol_tvl_tables(
            protocols, histories,
            top_n=100,
            start_date=dt.date(2021, 1, 1),
            end_date=dt.date(2021, 1, 10),
            coverage_threshold=0.80,
            parent_governance_lookup=lookup,
        )
        self.assertEqual(len(top_rows), 1)
        self.assertEqual(top_rows[0]["parent_snapshot_governance_ids"], "curve.eth")
        self.assertEqual(top_rows[0]["snapshot_governance_ids"], "")

    def test_manifest_includes_parent_protocols_endpoint_and_vintage(self):
        manifest = fetch_defillama_tvl.build_manifest(
            protocol_slugs=["curve-dex"],
            period_start=dt.date(2020, 10, 1),
            period_end=dt.date(2023, 11, 30),
            top_n=100,
            expected_period_days=1156,
            fetch_date="2026-06-10",
            parent_protocols_fetch_date="2026-06-11",
        )
        self.assertIn("parent_protocols", manifest["endpoints"])
        parent_ep = manifest["endpoints"]["parent_protocols"]
        self.assertEqual(parent_ep["status"], "undocumented_website_backing")
        vintage = manifest["parent_governance_vintage"]
        self.assertEqual(vintage["protocols_list_fetch_date"], "2026-06-10")
        self.assertEqual(vintage["parent_protocols_fetch_date"], "2026-06-11")
        self.assertIn("note", vintage)

    def test_main_fetches_parent_protocols_on_first_run(self):
        protocols = [
            protocol("alpha", governance_id="snapshot:alpha.eth"),
        ]
        parent_payload = {
            "parentProtocols": [
                {"id": "parent#alpha", "governanceID": "snapshot:alpha-parent.eth"},
            ]
        }
        histories = {
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 10)}}},
        }

        old_fetch_json = fetch_defillama_tvl.fetch_json
        old_fetch_protocol_history = fetch_defillama_tvl.fetch_protocol_history
        try:
            def mock_fetch_json(url):
                if "protocols2" in url:
                    return parent_payload
                return protocols

            fetch_defillama_tvl.fetch_json = mock_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = lambda slug: histories[slug]

            with tempfile.TemporaryDirectory() as tmpdir:
                output_dir = Path(tmpdir) / "raw"
                provenance_dir = Path(tmpdir) / "prov"
                exit_code = fetch_defillama_tvl.main([
                    "--output-dir", str(output_dir),
                    "--provenance-dir", str(provenance_dir),
                    "--period-start", "2021-01-01",
                    "--period-end", "2021-01-10",
                ])
                self.assertEqual(exit_code, 0)
                # Parent protocols cache file must be written
                parent_cache = output_dir / fetch_defillama_tvl.PARENT_PROTOCOLS_CACHE_FILENAME
                self.assertTrue(parent_cache.exists())
                # Parent protocols cache checksum must be in manifest
                manifest = json.loads(
                    (provenance_dir / fetch_defillama_tvl.PROTOCOL_MANIFEST_FILENAME).read_text(
                        encoding="utf-8"
                    )
                )
                checksums = manifest["cache_checksums"]
                self.assertTrue(
                    any(fetch_defillama_tvl.PARENT_PROTOCOLS_CACHE_FILENAME in k for k in checksums)
                )
        finally:
            fetch_defillama_tvl.fetch_json = old_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = old_fetch_protocol_history

    def test_main_reuses_parent_protocols_cache_under_reuse_raw(self):
        protocols = [
            protocol("alpha", governance_id="snapshot:alpha.eth"),
        ]
        parent_payload = {"parentProtocols": []}
        histories = {
            "alpha": {"chainTvls": {"Ethereum": {"tvl": period_rows("2021-01-01", [100] * 10)}}},
        }

        old_fetch_json = fetch_defillama_tvl.fetch_json
        old_fetch_protocol_history = fetch_defillama_tvl.fetch_protocol_history
        fetch_json_calls = []
        try:
            def mock_fetch_json(url):
                fetch_json_calls.append(url)
                return protocols

            fetch_defillama_tvl.fetch_json = mock_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = lambda slug: histories[slug]

            with tempfile.TemporaryDirectory() as tmpdir:
                output_dir = Path(tmpdir) / "raw"
                provenance_dir = Path(tmpdir) / "prov"
                output_dir.mkdir(parents=True)
                fetch_defillama_tvl.write_json(output_dir / "defillama_protocols_raw.json", protocols)
                fetch_defillama_tvl.write_json(
                    output_dir / fetch_defillama_tvl.PARENT_PROTOCOLS_CACHE_FILENAME,
                    parent_payload,
                )
                history_dir = output_dir / "defillama_protocol_history"
                history_dir.mkdir()
                fetch_defillama_tvl.write_json(history_dir / "alpha.json", histories["alpha"])

                exit_code = fetch_defillama_tvl.main([
                    "--output-dir", str(output_dir),
                    "--provenance-dir", str(provenance_dir),
                    "--period-start", "2021-01-01",
                    "--period-end", "2021-01-10",
                    "--reuse-raw",
                ])
                self.assertEqual(exit_code, 0)
                # Neither protocols list nor parent protocols should be fetched live
                self.assertEqual(fetch_json_calls, [])
        finally:
            fetch_defillama_tvl.fetch_json = old_fetch_json
            fetch_defillama_tvl.fetch_protocol_history = old_fetch_protocol_history

    def test_parent_protocols_field_in_protocol_fieldnames(self):
        self.assertIn(
            "parent_snapshot_governance_ids",
            fetch_defillama_tvl.PROTOCOL_FIELDNAMES,
        )
        # Must appear after snapshot_governance_ids (positional adjacency requirement)
        idx_child = fetch_defillama_tvl.PROTOCOL_FIELDNAMES.index("snapshot_governance_ids")
        idx_parent = fetch_defillama_tvl.PROTOCOL_FIELDNAMES.index("parent_snapshot_governance_ids")
        self.assertEqual(idx_parent, idx_child + 1)


if __name__ == "__main__":
    unittest.main()
