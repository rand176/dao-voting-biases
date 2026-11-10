import csv
import sys
import tempfile
import unittest
from pathlib import Path


ANALYSIS_ROOT = Path(__file__).resolve().parents[2]

import dao_bias.preparation.normalize_stance as normalize_release_stance


def write_csv(path, rows, fieldnames):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def read_csv(path):
    with path.open("r", newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle))


class NormalizeReleaseStanceTests(unittest.TestCase):
    def test_normalizes_undocumented_one_sided_proposals_only(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            source = root / "current.csv"
            output = root / "normalized.csv"
            exceptions = root / "stance_one_sided_exceptions.csv"
            audit = root / "audit.csv"
            fieldnames = [
                "proposal",
                "choice_idx",
                "stance",
                "is_infavor",
                "is_against",
                "is_abstain",
                "is_other",
                "author_choice_stance",
                "author_choice_stance_compact",
                "author_multi_approve",
            ]
            write_csv(
                source,
                [
                    {
                        "proposal": "prop-both",
                        "choice_idx": "1",
                        "stance": "APPROVE",
                        "is_infavor": "1",
                        "is_against": "0",
                        "is_abstain": "0",
                        "is_other": "0",
                        "author_choice_stance": "APPROVE",
                        "author_choice_stance_compact": "APPROVE",
                        "author_multi_approve": "1",
                    },
                    {
                        "proposal": "prop-both",
                        "choice_idx": "2",
                        "stance": "REJECT",
                        "is_infavor": "0",
                        "is_against": "1",
                        "is_abstain": "0",
                        "is_other": "0",
                        "author_choice_stance": "APPROVE",
                        "author_choice_stance_compact": "APPROVE",
                        "author_multi_approve": "1",
                    },
                    {
                        "proposal": "prop-undoc",
                        "choice_idx": "1",
                        "stance": "APPROVE",
                        "is_infavor": "1",
                        "is_against": "0",
                        "is_abstain": "0",
                        "is_other": "0",
                        "author_choice_stance": "APPROVE",
                        "author_choice_stance_compact": "APPROVE",
                        "author_multi_approve": "1",
                    },
                    {
                        "proposal": "prop-doc",
                        "choice_idx": "1",
                        "stance": "REJECT",
                        "is_infavor": "0",
                        "is_against": "1",
                        "is_abstain": "0",
                        "is_other": "0",
                        "author_choice_stance": "REJECT",
                        "author_choice_stance_compact": "REJECT",
                        "author_multi_approve": "0",
                    },
                ],
                fieldnames,
            )
            write_csv(
                exceptions,
                [
                    {
                        "stage": "2_stance_v5",
                        "proposal": "prop-doc",
                        "allowed_state": "reject_only",
                        "source_line": "1",
                        "note": "documented exception",
                    }
                ],
                ["stage", "proposal", "allowed_state", "source_line", "note"],
            )

            summary = normalize_release_stance.normalize_release_stance(
                input_path=source,
                output_path=output,
                exceptions_path=exceptions,
                audit_path=audit,
                stage="2_stance_v5",
            )

            self.assertEqual(summary["one_sided_proposals"], 2)
            self.assertEqual(summary["normalized_proposals"], 1)
            self.assertEqual(summary["documented_exception_proposals"], 1)

            rows = read_csv(output)
            by_key = {(row["proposal"], row["choice_idx"]): row for row in rows}
            self.assertEqual(by_key[("prop-undoc", "1")]["stance"], "OTHER")
            self.assertEqual(by_key[("prop-undoc", "1")]["is_infavor"], "0")
            self.assertEqual(by_key[("prop-undoc", "1")]["is_other"], "1")
            self.assertEqual(by_key[("prop-undoc", "1")]["author_choice_stance"], "OTHER")
            self.assertEqual(by_key[("prop-undoc", "1")]["author_multi_approve"], "0")
            self.assertEqual(by_key[("prop-doc", "1")]["stance"], "REJECT")
            self.assertEqual(by_key[("prop-both", "1")]["stance"], "APPROVE")

            audit_rows = read_csv(audit)
            actions = {row["proposal"]: row["action"] for row in audit_rows}
            self.assertEqual(actions["prop-undoc"], "normalized_to_other")
            self.assertEqual(actions["prop-doc"], "kept_documented_exception")


if __name__ == "__main__":
    unittest.main()
