source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
source("src/r/preparation/choice_helpers.R")

choices <- data.frame(
  proposal = c("prop-zero", "prop-zero", "prop-live", "prop-live", "prop-live"),
  choice_idx = c(1, 2, 1, 2, 3),
  choice_score = c(0, 0, 3, 1, 0),
  scores_total = c(0, 0, 4, 4, 4),
  author_vp = c(NA, NA, 2, 2, 2),
  stringsAsFactors = FALSE
)

choices <- addStage1ChoiceShare(choices)
choices <- addStage1ChoiceCounts(choices)
choices <- addStage1RandomAndExcessShare(choices)
choices <- addStage1AuthorShare(choices)

stopifnot(identical(choices$N_choices, c(2L, 2L, 3L, 3L, 3L)))
stopifnot(all(is.na(choices$choice_share[choices$proposal == "prop-zero"])))
stopifnot(all(choices$rnd_share[choices$proposal == "prop-zero"] == 0.5))
stopifnot(all(is.na(choices$excess_share[choices$proposal == "prop-zero"])))
stopifnot(all(choices$choice_share[choices$proposal == "prop-live"] == c(0.75, 0.25, 0)))
stopifnot(all(round(choices$rnd_share[choices$proposal == "prop-live"], 10) == round(1 / 3, 10)))
stopifnot(all(round(choices$author_share[choices$proposal == "prop-live"], 10) == 0.5))

choices_with_single_option <- data.frame(
  proposal = c("prop-one", "prop-two", "prop-two", "prop-reported-one", "prop-reported-one"),
  choice_idx = c(1, 1, 2, 1, 2),
  N_choices = c(1, 2, 2, 1, 1),
  stringsAsFactors = FALSE
)

choices_without_single_option <- dropSingleOptionProposals(choices_with_single_option)
stopifnot(identical(choices_without_single_option$proposal, c("prop-two", "prop-two")))

bad_author_share <- data.frame(
  proposal = c("prop-bad", "prop-bad"),
  author_vp = c(1, 0),
  scores_total = c(0, 0),
  stringsAsFactors = FALSE
)
bad_author_share <- addStage1AuthorShare(bad_author_share)
stopifnot(all(is.na(bad_author_share$author_share)))

author_choices <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b", "prop-c"),
  author_choice = c(2, 4, 1, NA),
  stringsAsFactors = FALSE
)
choice_rows <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-a", "prop-b", "prop-b", "prop-c", "prop-c"),
  choice_idx = c(1, 2, 4, 1, 2, 1, 2),
  stringsAsFactors = FALSE
)

flag <- stage1AuthorChoiceFlag(choice_rows, author_choices)
stopifnot(identical(flag, c(0L, 1L, 1L, 1L, 0L, NA_integer_, NA_integer_)))

malformed_author_choices <- data.frame(
  proposal = "prop-d",
  author_choice = 9,
  stringsAsFactors = FALSE
)
malformed_choice_rows <- data.frame(
  proposal = c("prop-d", "prop-d"),
  choice_idx = c(1, 2),
  stringsAsFactors = FALSE
)

malformed_flag <- stage1AuthorChoiceFlag(malformed_choice_rows, malformed_author_choices)
stopifnot(identical(malformed_flag, c(NA_integer_, NA_integer_)))
