import csv
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "src/python/dao_bias/preparation/redact_human.py"


class AnonymizeCrowdsourcingTests(unittest.TestCase):
    def run_export(self, source, destination):
        return subprocess.run(
            [sys.executable, str(SCRIPT), str(source), str(destination)],
            capture_output=True, text=True,
        )

    def write_source(self, path, rows):
        with path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
            writer.writeheader()
            writer.writerows(rows)

    def test_export_preserves_join_keys_responses_and_text(self):
        rows = [
            {"": "0", "proposal": "private-a", "choice_idx": "1", "prop_chid": "private-a1",
             "body": "Text, with\nnewlines", "eval_1": "Approve", "eval_2": "", "eval_t_1": "12.50"},
            {"": "1", "proposal": "private-a", "choice_idx": "2", "prop_chid": "private-a2",
             "body": "Text, with\nnewlines", "eval_1": "Reject", "eval_2": "Other", "eval_t_1": "12.50"},
            {"": "2", "proposal": "private-b", "choice_idx": "1", "prop_chid": "private-b1",
             "body": "Different text", "eval_1": "Abstain", "eval_2": "", "eval_t_1": "0"},
        ]
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, rows)
            original = source.read_bytes()
            result = self.run_export(source, destination)
            self.assertEqual(result.returncode, 0, result.stderr)
            with destination.open(newline="", encoding="utf-8") as handle:
                exported = list(csv.DictReader(handle))
            self.assertEqual(len(exported), 3)
            self.assertEqual(exported[0]["proposal"], exported[1]["proposal"])
            self.assertNotEqual(exported[0]["proposal"], exported[2]["proposal"])
            for before, after in zip(rows, exported):
                self.assertEqual(after, {key: value for key, value in before.items() if key != ""})
                self.assertEqual(after["prop_chid"], after["proposal"] + after["choice_idx"])
            repeated = Path(folder) / "repeated.csv"
            self.assertEqual(self.run_export(source, repeated).returncode, 0)
            self.assertEqual(repeated.read_bytes(), destination.read_bytes())
            self.assertEqual(source.read_bytes(), original)
            published = destination.read_bytes()
            self.assertNotEqual(self.run_export(source, destination).returncode, 0)
            self.assertEqual(destination.read_bytes(), published)
            self.assertNotEqual(self.run_export(source, source).returncode, 0)
            self.assertEqual(source.read_bytes(), original)

    def test_unreviewed_columns_and_inconsistent_keys_fail_without_output(self):
        base = {"proposal": "private-a", "choice_idx": "1", "prop_chid": "private-a1"}
        for changes in ({"participant_id": "sensitive"}, {"prop_chid": "wrong"}, {"choice_idx": "0"}):
            with self.subTest(changes=changes), tempfile.TemporaryDirectory() as folder:
                source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
                self.write_source(source, [{**base, **changes}])
                result = self.run_export(source, destination)
                self.assertNotEqual(result.returncode, 0)
                self.assertFalse(destination.exists())

    def test_unicode_line_separators_become_regular_newlines(self):
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, [{
                "proposal": "private-a", "choice_idx": "1", "prop_chid": "private-a1",
                "body": "First\u2028second\u2029third\nlast", "eval_1": "Approve",
            }])
            original = source.read_bytes()
            result = self.run_export(source, destination)
            self.assertEqual(result.returncode, 0, result.stderr)
            with destination.open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0]["body"], "First\nsecond\nthird\nlast")
            self.assertEqual(rows[0]["eval_1"], "Approve")
            self.assertNotIn("\u2028", destination.read_text())
            self.assertNotIn("\u2029", destination.read_text())
            self.assertEqual(source.read_bytes(), original)

    def test_contacts_are_redacted_in_every_text_field_with_value_free_report(self):
        email, phone = "person@example.org", "+1 202 555 0100"
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, [{
                "proposal": "p", "choice_idx": "1", "prop_chid": "p1",
                "title": email, "body": f"Email {email}; call {phone}.",
                "choice": phone, "choices_json": json.dumps([email, phone]),
                "eval_1": "Approve", "eval_t_1": "2025550100", "body_mod_len": "1234",
            }])
            original = source.read_bytes()
            result = self.run_export(source, destination)
            self.assertEqual(result.returncode, 0, result.stderr)
            with destination.open(newline="") as handle:
                row = next(csv.DictReader(handle))
            self.assertEqual(row["title"], "[EMAIL]")
            self.assertEqual(row["body"], "Email [EMAIL]; call [PHONE].")
            self.assertEqual(row["choice"], "[PHONE]")
            self.assertEqual(json.loads(row["choices_json"]), ["[EMAIL]", "[PHONE]"])
            self.assertEqual(row["eval_t_1"], "2025550100")
            self.assertEqual(row["body_mod_len"], "1234")
            report_text = destination.with_suffix(".redactions.json").read_text()
            report = json.loads(report_text)
            self.assertEqual(report["replacements"], {"email": 3, "phone": 3})
            self.assertEqual(set(report["replacements_by_field"]), {"title", "body", "choice", "choices_json"})
            self.assertNotIn(email, report_text)
            self.assertNotIn(phone, report_text)
            self.assertEqual(source.read_bytes(), original)

    def test_contact_links_and_national_phone_formats_are_redacted(self):
        text = ("[Mail](mailto:person%40example.org?subject=hello) "
                "[Call](tel:%2B1%20202%20555%200100) "
                "[Call](tel:+1(202)555-0100) "
                "(202) 555-0100; 020 7946 0958; phone: 2025550100")
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, [{"proposal": "p", "choice_idx": "1", "prop_chid": "p1", "body": text}])
            result = self.run_export(source, destination)
            self.assertEqual(result.returncode, 0, result.stderr)
            with destination.open(newline="") as handle:
                self.assertEqual(next(csv.DictReader(handle))["body"],
                                 "[Mail]([EMAIL]) [Call]([PHONE]) [Call]([PHONE]) [PHONE]; [PHONE]; phone: [PHONE]")

    def test_noncontact_numbers_and_ethereum_addresses_remain_unchanged(self):
        text = ("2024-01-31; 31.01.2024; 2020-2024; $1,000,000; 1 000 000; "
                "timestamp 1700000000; 0.12345 - 0.67890; "
                "https://example.org/1234567890123; "
                "https://cdn.example.org/lib/package@1.2.3; "
                "0x1234567890123456789012345678901234567890")
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, [{"proposal": "p", "choice_idx": "1", "prop_chid": "p1", "body": text}])
            result = self.run_export(source, destination)
            self.assertEqual(result.returncode, 0, result.stderr)
            with destination.open(newline="") as handle:
                self.assertEqual(next(csv.DictReader(handle))["body"], text)
            report = json.loads(destination.with_suffix(".redactions.json").read_text())
            self.assertEqual(report["replacements"], {"email": 0, "phone": 0})
            self.assertGreater(report["numeric_candidates_for_review_by_field"]["body"], 0)

    def test_existing_report_prevents_creation_of_csv(self):
        with tempfile.TemporaryDirectory() as folder:
            source, destination = Path(folder) / "input.csv", Path(folder) / "output.csv"
            self.write_source(source, [{"proposal": "p", "choice_idx": "1", "prop_chid": "p1"}])
            report = destination.with_suffix(".redactions.json")
            report.write_text("existing report")
            self.assertNotEqual(self.run_export(source, destination).returncode, 0)
            self.assertFalse(destination.exists())
            self.assertEqual(report.read_text(), "existing report")


if __name__ == "__main__":
    unittest.main()
