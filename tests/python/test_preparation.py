"""Small fixtures guard the transformations formerly embedded in notebooks."""
import pandas as pd
import numpy as np
import warnings
from dao_bias.preparation import snapshot
from dao_bias.preparation.author_power import remove_author_power


def test_language_lengths_preserve_rows_and_thresholds():
    raw = pd.DataFrame({"id": ["a", "b", "c"], "title": ["  ABCD  ", "abcdefghij", ""],
                        "body": [" " + "x" * 50 + " ", "x" * 9, float("nan")]})
    result = snapshot.prepare_lengths(snapshot.prepare_language(raw, detect=lambda text: "en"))
    assert result.id.tolist() == ["a", "b", "c"]
    assert result.title_mod_len.tolist() == [4, 10, 0]
    assert result.title_is_short.tolist() == [1, 0, 1]
    assert result.title_is_very_short.tolist() == [1, 0, 1]
    assert result.body_mod_len.tolist() == [50, 9, 0]
    assert result.body_is_short.tolist() == [0, 1, 1]
    assert result.body_is_very_short.tolist() == [0, 1, 1]
    assert "title_mod" not in raw


def test_maturity_marks_all_proposals_without_dropping_excluded_rows():
    proposals = pd.DataFrame({"id": list("abcde"), "space": ["mature", "mature", "single", "small", "small"], "votes": [3, 10, 100, 1, 2]})
    spaces = pd.DataFrame({"id": ["mature", "single", "small"]})
    follows = pd.DataFrame({"space": ["mature"] * 5 + ["single"] * 5 + ["small"] * 5})
    result, selected = snapshot.prepare_maturity(proposals, spaces, follows)
    assert result.id.tolist() == list("abcde")
    assert result.paper.tolist() == [1, 1, 0, 0, 0]
    assert selected.id.tolist() == ["mature"]
    assert "paper" not in proposals


def test_choices_keep_ties_and_original_position_convention():
    proposals = pd.DataFrame({"id": ["tie", "first", "pending"], "space": ["dao"] * 3,
        "choices": ['["Yes", "No"]'] * 3, "scores": ['[5,5]', '[6.5,3.5]', '[1,0]'],
        "scores_state": ["final", "final", "pending"]})
    with warnings.catch_warnings():
        warnings.simplefilter("error", FutureWarning)
        result = snapshot.prepare_choices(proposals)
    assert result.proposal.tolist() == ["tie", "tie", "first", "first"]
    assert result.choice_idx.tolist() == [1, 2, 1, 2]
    assert result.choice_won.tolist() == [1, 1, 1, 0]
    assert result.choice_score.tolist() == [5, 5, 6.5, 3.5]
    # The original preparation field tests membership of 1 in zero-based winners.
    # Preserve this legacy field here; downstream release winners are recomputed.
    assert result.first_choice_won.tolist() == [1, 1, 0, 0]


def test_delegates_and_dates_have_stable_keys_and_order():
    proposals = pd.DataFrame({"id": ["p2", "p1"], "space": ["dao"] * 2,
        "strategies": ['[{"name":"delegation"}]', '[]'], "start": [86400, 0],
        "end": [172800, 86400], "created": [100, 10]})
    spaces = pd.DataFrame({"id": ["dao"], "settings": ['{}'], "created": [50]})
    delegates, settings = snapshot.prepare_delegates(proposals, spaces)
    assert delegates.has_deleg_strategies.tolist() == [1, 0]
    assert settings.id.tolist() == ["dao"]
    assert settings.vo_type.tolist() == ["NA"]
    dates = snapshot.prepare_dates(proposals, spaces)
    assert dates.id.tolist() == ["p2", "p1"]
    assert dates.YMD_start.tolist() == ["1970-01-02", "1970-01-01"]
    assert dates.created_relative.tolist() == [50, -40]
    assert dates.prop_counter.tolist() == [1, 2]


def test_author_power_uses_support_fractions_and_handles_zero_total():
    choices = pd.DataFrame({"proposal": ["p", "p", "q"], "type": ["weighted"] * 3,
        "is_author_choice": [1, 0, 1], "author_support_fraction": [.75, .25, 1.],
        "author_vp": [4, 4, 2], "choice_score": [8, 4, 2], "choice_idx": [1, 2, 1], "quorum": [1, 1, 0]})
    result = remove_author_power(choices)
    assert result.choice_score_no_author_vp.tolist() == [5, 3, 0]
    np.testing.assert_allclose(result.choice_share_no_author_vp, [5/8, 3/8, np.nan])
    assert result.choice_won_no_author_vp.tolist() == [1, 0, 0]
    assert "choice_score_no_author_vp" not in choices
