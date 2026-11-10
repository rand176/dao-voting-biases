from __future__ import annotations
from dao_bias.data.paths import INPUT_ROOT, OUTPUT_ROOT, prepared_input

import re
from pathlib import Path

import pandas as pd

from dao_bias.data.paths import CLEAN_PROPOSAL_FLAGS_DIR, active_data_path, raw_data_path

ROOT_DIR = INPUT_ROOT
OUTPUT_DIR = CLEAN_PROPOSAL_FLAGS_DIR
EXCEPTIONS_DIR = ROOT_DIR / "metadata" / "exceptions"
REVIEWS_DIR = OUTPUT_ROOT / "metadata" / "reviews"

PROPOSALS_LANG_FILE = prepared_input("derived", "active", "proposals_lang_len.csv.gz")
PROPOSALS_RAW_FILE = raw_data_path("proposals_0.csv.gz")
PROPOSALS_LANG_FAKE_FILE = active_data_path("proposals_lang_len_fake.csv.gz")
CANONICAL_OUTPUT_FILE = OUTPUT_DIR / "proposal_fake_heuristics.csv.gz"
STRICT_FLAGGED_ONLY_FILE = REVIEWS_DIR / "proposal_fake_strict_flagged_only.csv"

LATIN_TEXT_1 = (
    "Sed suscipit ligula nec turpis volutpat, non tempus nisi auctor. "
    "Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; "
    "Proin tincidunt, nisl a tristique dictum, libero neque varius felis, eu vulputate dolor "
    "sapien ac erat. Fusce vel odio et libero eleifend hendrerit vel in odio."
)
LATIN_TEXT_2 = (
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc ac ligula sit amet turpis "
    "vehicula ultricies et posuere tortor. Curabitur feugiat tempus ex, ac vestibulum velit "
    "consectetur id. Nunc feugiat semper augue tristique dignissim. Aliquam sollicitudin dui "
    "quis justo elementum, sed tincidunt libero vehicula."
)

BROAD_FAKE_TITLE_TERMS = [
    "asd",
    "test",
    "huhuhu",
    "yhyyy",
    "one",
    "hi",
    "i use my hands to eat.",
    "eidjt",
    "john5",
    "hello",
    "aaa",
    "45t3",
    "dvz",
    "wwwww",
]

LEGACY_R_JUNK_SPACES = {
    "pooryia.eth",
    "weenus",
    "hsedefi.eth",
    "zimoli.eth",
    "raidentest.eth",
    "onchainlabs.eth",
    "116trickery.eth",
    "parubook.eth",
    "diskarma.eth",
    "xunfa.eth",
    "chenl.eth",
    "echeng.eth",
    "swaeio.eth",
    "estfor.eth",
    "personar.eth",
    "samuv.eth",
    "sexyrabbit.eth",
    "07544570.eth",
    "lifetree.eth",
    "matheww.eth",
    "dao2.eth",
    "332334.eth",
    "eth88888888.eth",
    "niceaccount.eth",
    "peoplepay.eth",
    "syncseason0test.eth",
    "080329.eth",
    "trubka.eth",
}

LEGACY_R_TEST_TITLE_EXACT = {
    "test",
    "test proposal",
    "testing",
    "tester",
    "testerr",
    "snapshot",
    "proposal",
    "123",
    "321",
    "dummy",
    "测试",
    "テスト",
    "시험",
    "testy",
    "test vote",
    "testt",
    "proposal test",
    "vote power testing",
    "delegation test",
    "testvote",
    "test#",
    "test:",
    "test*",
    "test!",
    "test.",
    "test?",
    "testa",
    "testo",
    "is this working",
    "final testing",
    "just testing",
    "testd",
    "testetstetstetest",
    "just test the vote",
    "first test",
    "testingss",
    "quorum test",
    "さとtest",
    "test提案",
    "testsdsadas",
    "delegate test",
    "snapshot test",
    "gnosis test",
    "test-proposal",
    "lets test",
    "test again",
    "i need to test",
    "poap test",
    "testingproposal",
    "proposal 1 test",
    "test 4 approval voting",
    "im testing this",
    "testproppposal",
    "testuuu",
    "testprop",
    "tests",
    "dev_test",
    "tester vote",
    "i want to test",
    "test round",
    "is this a test",
    "testinng",
    "another test",
    "teste",
    "proposal testing",
    "quadratic test",
    "hi!test",
    "text count test",
    "update test",
    "test_vote",
    "w3cr test",
    "voting test",
    "title test",
    "test vp",
    "vote test",
    "testq",
    "testasd",
    "ceci est un test",
    "hi this is test",
    "voting power test",
    "does this work?",
    "a test for vote",
    "a test vote",
    "public test",
    "test for weighted voting",
    "testprod",
    "discord test",
    "test_",
    "demo test",
    "testtt",
    "testttt",
    "testtttt",
    "testright",
    "test snapshot",
    "let's test our new space!",
    "test_proposal",
    "just a try to test",
    "test quorum",
    "test weighted vote",
    "for testing voting",
    "sip-test voting power",
    "testq2",
    "test voting power",
    "提案test",
    "a test case",
    "proposition test",
    "war dieser test erfolgreich?",
    "new test dynamic",
    "testkurageスナップショット",
    "z8test",
    "[test prop 2",
    "fn test 2",
    "testn+",
    "testerooni",
    "poll test with votes",
    "testing vote",
    "vote strategy test",
    "test propsal",
    "[testing]",
    "bond test",
    "prop test",
    "safesnap test",
    "i am testing gov",
}

LEGACY_R_TEST_BODY_EXACT = {
    "test",
    "testing",
    "this is a test",
    "test, plz ignore. will be deleted.",
    "asd",
    "123",
    "321",
    "dummy",
    "snapshot",
    "proposal",
    "测试",
    "テスト",
    "시험",
}

LEGACY_R_TEST_TITLE_PATTERNS = [
    r"^test(?:\s|$|[-:!?.#*_])",
    r"^testing(?:\s|$|[-:!?.#*_])",
    r"\btest proposal(?:s)?\b",
    r"\bproposal test\b",
    r"\btest vote\b",
    r"\btestingproposal\b",
    r"\btestproposal\b",
    r"\bdummy\b",
    r"^test-",
    r"^test\d",
    r"^testing\d",
    r"(?:^|[\s\[(])test(?:ing)?(?:$|[\s\])])",
]

LEGACY_R_TEST_BODY_PATTERNS = [
    r"^test(?:\s|$|[-:!?.#*_])",
    r"^testing(?:\s|$|[-:!?.#*_])",
    r"^this is a test\b",
    r"\btest proposal(?:s)?\b",
    r"\bmock proposal\b",
    r"\bdummy\b",
]


def normalize_text(series: pd.Series) -> pd.Series:
    return (
        series.fillna("")
        .astype(str)
        .str.replace(r"\s+", " ", regex=True)
        .str.strip()
    )


def normalize_id(series: pd.Series) -> pd.Series:
    return series.fillna("").astype(str).str.strip().str.lower()


def load_manual_fake_flags() -> pd.DataFrame:
    path = EXCEPTIONS_DIR / "proposal_fake_manual_flags.csv"
    if not path.exists():
        return pd.DataFrame(
            columns=[
                "proposal",
                "flag_fake_broad",
                "flag_fake_strict",
                "flag_category",
                "flag_reason",
                "source",
            ]
        )

    df = pd.read_csv(path, dtype="object")
    df["proposal"] = normalize_id(df["proposal"])
    return df


def build_broad_fake_fields(df: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    title_mod = normalize_text(df.get("title_mod", df["title"])).str.lower()
    body_mod = normalize_text(df.get("body_mod", df["body"])).str.lower()
    title_lang = df.get("title_lang", pd.Series("", index=df.index)).fillna("").astype(str)
    body_lang = df.get("body_lang", pd.Series("", index=df.index)).fillna("").astype(str)

    broad_flag = pd.Series(0, index=df.index, dtype="int64")
    broad_reason = pd.Series("", index=df.index, dtype="object")

    def mark(mask: pd.Series, reason: str) -> None:
        new_hits = mask & broad_reason.eq("")
        broad_flag.loc[new_hits] = 1
        broad_reason.loc[new_hits] = reason

    title_terms_re = "|".join(re.escape(term) for term in BROAD_FAKE_TITLE_TERMS)
    mark(
        title_mod.str.contains(title_terms_re, regex=True, na=False),
        "Broad compatibility rule: title_mod matches legacy fake-token list from cleanup_2024_2_fake.ipynb",
    )
    mark(body_mod.eq("test"), "Broad compatibility rule: body_mod equals 'test'")
    mark(body_mod.eq("asd"), "Broad compatibility rule: body_mod equals 'asd'")
    mark(body_lang.eq("unknown"), "Broad compatibility rule: body_lang equals 'unknown'")
    mark(title_lang.eq("unknown"), "Broad compatibility rule: title_lang equals 'unknown'")

    broad_reason = broad_reason.mask(broad_reason.eq(""), "No broad fake signal")
    return broad_flag, broad_reason


def build_strict_fake_fields(df: pd.DataFrame) -> tuple[pd.Series, pd.Series]:
    title_l = df["title_norm"].str.lower()
    body_l = df["body_norm"].str.lower()
    choices_l = df["choices_norm"].str.lower()
    space_l = normalize_text(df.get("space", pd.Series("", index=df.index))).str.lower()
    combined_l = title_l + " " + body_l + " " + choices_l

    categories = pd.Series("", index=df.index, dtype="object")
    reasons = pd.Series("", index=df.index, dtype="object")

    def mark(mask: pd.Series, category: str, reason: str) -> None:
        new_hits = mask & reasons.eq("")
        categories.loc[new_hits] = category
        reasons.loc[new_hits] = reason

    legacy_title_mask = title_l.isin(LEGACY_R_TEST_TITLE_EXACT)
    for pattern in LEGACY_R_TEST_TITLE_PATTERNS:
        legacy_title_mask = legacy_title_mask | title_l.str.contains(pattern, regex=True, na=False)
    mark(
        legacy_title_mask,
        "legacy_r_test_title",
        "Matches a legacy high-confidence R-stage title test/demo removal pattern",
    )

    legacy_body_mask = body_l.isin(LEGACY_R_TEST_BODY_EXACT)
    for pattern in LEGACY_R_TEST_BODY_PATTERNS:
        legacy_body_mask = legacy_body_mask | body_l.str.contains(pattern, regex=True, na=False)
    mark(
        legacy_body_mask,
        "legacy_r_test_body",
        "Matches a legacy high-confidence R-stage body test/demo removal pattern",
    )

    legacy_junk_space_mask = space_l.isin(LEGACY_R_JUNK_SPACES)
    mark(
        legacy_junk_space_mask,
        "legacy_r_junk_space",
        "Space was previously removed wholesale inline in the R stance pipeline as fake/test-heavy",
    )

    latin_placeholder_mask = (
        body_l.str.contains(LATIN_TEXT_1.lower(), regex=False)
        | body_l.str.contains(LATIN_TEXT_2.lower(), regex=False)
    )
    mark(
        latin_placeholder_mask,
        "latin_placeholder",
        "Contains one of the canonical Latin placeholder bodies previously removed inline in the R pipeline",
    )

    placeholder_mask = (
        combined_l.str.contains("lorem ipsum", regex=False)
        | combined_l.str.contains("vokalia and consonantia", regex=False)
        | combined_l.str.contains("blind texts", regex=False)
        | combined_l.str.contains("far far away, behind the word mountains", regex=False)
    )
    mark(
        placeholder_mask,
        "placeholder_boilerplate",
        "Contains canonical lorem ipsum / dummy-text boilerplate",
    )

    markdown_demo_mask = (
        combined_l.str.contains("markdown-it", regex=False)
        | combined_l.str.contains("stormtroopocat", regex=False)
        | combined_l.str.contains("h1 heading", regex=False)
        | combined_l.str.contains("babelfish", regex=False)
        | combined_l.str.contains("localhost:8000/proposal", regex=False)
    )
    mark(
        markdown_demo_mask,
        "demo_template",
        "Contains markdown/demo boilerplate or localhost placeholder content",
    )

    template_mask = (
        title_l.str.contains(r"\[template\]", regex=True, na=False)
        | combined_l.str.contains("[author]", regex=False)
        | combined_l.str.contains("requested budget: ?", regex=False)
        | combined_l.str.contains("article length: ?", regex=False)
    )
    mark(
        template_mask,
        "template_placeholder",
        "Contains explicit template placeholders rather than proposal content",
    )

    generic_choice_mask = choices_l.str.contains(
        r"^\[\s*\"choice 1\"(?:,\s*\"choice \d+\")+\s*\]$",
        regex=True,
        na=False,
    )
    snapshot_sample_mask = (
        title_l.str.contains(
            r"^(?:\d+\s*-\s*)?(?:quadratic|weighted) voting \| strategy:",
            regex=True,
            na=False,
        )
        & generic_choice_mask
    )
    mark(
        snapshot_sample_mask,
        "snapshot_sample_proposal",
        "Matches the generic Snapshot sample voting template with placeholder choice labels",
    )

    latest_proposal_mask = title_l.str.fullmatch(r"latest proposal \d+", na=False)
    mark(
        latest_proposal_mask,
        "explicit_test_demo",
        "Title matches a generic 'Latest proposal N' test/demo naming pattern",
    )

    explicit_test_mask = (
        title_l.str.fullmatch(r"(test|testing|proposal demo|demo)", na=False)
        | body_l.str.fullmatch(r"(test|testing|proposal demo|demo)", na=False)
        | title_l.str.contains(r"\btest proposal(?:s)?\b", regex=True, na=False)
        | body_l.str.contains(r"\btest proposal(?:s)?\b", regex=True, na=False)
        | title_l.str.contains(r"\bdemo\b", regex=True, na=False)
        | body_l.str.contains(r"\btest quadratic voting\b", regex=True, na=False)
        | title_l.str.contains(r"\btest\b", regex=True, na=False) & body_l.str.len().lt(40)
    )
    mark(
        explicit_test_mask,
        "explicit_test_demo",
        "Contains explicit test/demo wording with no substantive proposal text",
    )

    snapshot_first_vote_mask = title_l.eq("snapshot first vote")
    mark(
        snapshot_first_vote_mask,
        "snapshot_sample_proposal",
        "Matches the generic 'Snapshot first vote' sample proposal pattern",
    )

    random_char_mask = pd.Series(False, index=df.index)
    mark(
        random_char_mask,
        "random_chars",
        "Title is only short random-looking characters with non-substantive body text",
    )

    categories = categories.mask(categories.eq(""), "not_flagged")
    reasons = reasons.mask(reasons.eq(""), "No high-confidence fake/test/demo signal")
    return categories, reasons


def apply_manual_fake_flags(df_flags: pd.DataFrame, manual_flags: pd.DataFrame) -> pd.DataFrame:
    if manual_flags.empty:
        return df_flags

    overrides = manual_flags.drop_duplicates("proposal", keep="last").set_index("proposal")
    df_flags = df_flags.copy()
    df_flags["id"] = normalize_id(df_flags["id"])

    overlap = df_flags["id"].isin(overrides.index)
    if not overlap.any():
        return df_flags

    override_ids = df_flags.loc[overlap, "id"]
    df_flags.loc[overlap, "flag_fake_broad"] = overrides.loc[override_ids, "flag_fake_broad"].fillna(
        df_flags.loc[overlap, "flag_fake_broad"]
    ).astype(int).to_numpy()
    df_flags.loc[overlap, "flag_fake_strict"] = overrides.loc[override_ids, "flag_fake_strict"].fillna(
        df_flags.loc[overlap, "flag_fake_strict"]
    ).astype(int).to_numpy()

    category_override = overrides.loc[override_ids, "flag_category"].fillna("").astype(str).to_numpy()
    reason_override = overrides.loc[override_ids, "flag_reason"].fillna("").astype(str).to_numpy()

    category_mask = category_override != ""
    reason_mask = reason_override != ""
    if category_mask.any():
        df_flags.loc[df_flags.index[overlap][category_mask], "flag_category"] = category_override[category_mask]
    if reason_mask.any():
        df_flags.loc[df_flags.index[overlap][reason_mask], "flag_reason"] = reason_override[reason_mask]

    broad_reason_override = []
    broad_value_override = (
        overrides.loc[override_ids, "flag_fake_broad"].fillna(df_flags.loc[overlap, "flag_fake_broad"]).astype(int).to_numpy()
    )
    for proposal_id, broad_value in zip(override_ids, broad_value_override):
        source = overrides.loc[proposal_id, "source"] if "source" in overrides.columns else ""
        if int(broad_value) == 1:
            reason = f"Manual fake-flag override{': ' + source if source else ''}"
        else:
            reason = f"Manual fake-flag keep override{': ' + source if source else ''}"
        broad_reason_override.append(reason)
    df_flags.loc[df_flags.index[overlap], "flag_fake_broad_reason"] = broad_reason_override

    return df_flags


def build_fake_proposal_flags() -> tuple[pd.DataFrame, pd.DataFrame]:
    df_lang = pd.read_csv(PROPOSALS_LANG_FILE, low_memory=False)
    df_lang["id"] = normalize_id(df_lang["id"])
    df_lang["title_norm"] = normalize_text(df_lang["title"])
    df_lang["body_norm"] = normalize_text(df_lang["body"])
    df_lang["choices_norm"] = normalize_text(df_lang["choices"])

    broad_flag, broad_reason = build_broad_fake_fields(df_lang)
    strict_category, strict_reason = build_strict_fake_fields(df_lang)

    df_flags = pd.DataFrame(
        {
            "id": df_lang["id"],
            "flag_fake_broad": broad_flag.astype(int),
            "flag_fake_broad_reason": broad_reason,
            "flag_fake_strict": strict_reason.ne("No high-confidence fake/test/demo signal").astype(int),
            "flag_category": strict_category,
            "flag_reason": strict_reason,
        }
    )

    df_flags = apply_manual_fake_flags(df_flags, load_manual_fake_flags())

    df_lang_out = df_lang.copy()
    df_lang_out["fake"] = df_flags["flag_fake_broad"].astype(float).to_numpy()
    return df_flags, df_lang_out


def build_and_write_fake_proposal_outputs() -> tuple[pd.DataFrame, pd.DataFrame]:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    REVIEWS_DIR.mkdir(parents=True, exist_ok=True)

    df_flags, df_lang_out = build_fake_proposal_flags()
    df_flags.sort_values("id").to_csv(CANONICAL_OUTPUT_FILE, index=False, compression="gzip")
    df_lang_out.to_csv(PROPOSALS_LANG_FAKE_FILE, index=False, compression="gzip")
    df_flags[df_flags["flag_fake_strict"] == 1].to_csv(STRICT_FLAGGED_ONLY_FILE, index=False)
    return df_flags, df_lang_out


def main() -> None:
    df_flags, df_lang_out = build_and_write_fake_proposal_outputs()
    print(f"Wrote {len(df_flags)} proposal fake flags to {CANONICAL_OUTPUT_FILE}")
    print(
        "Broad fake count:",
        int(df_flags["flag_fake_broad"].sum()),
        "| strict fake count:",
        int(df_flags["flag_fake_strict"].sum()),
    )
    print(f"Wrote compatibility dataset with fake column to {PROPOSALS_LANG_FAKE_FILE}")
    print(f"Wrote strict flagged-only review file to {STRICT_FLAGGED_ONLY_FILE}")
    print(f"Compatibility fake count in proposals dataset: {int(df_lang_out['fake'].sum())}")


if __name__ == "__main__":
    main()
