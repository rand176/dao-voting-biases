source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
source("src/r/preparation/author_votes.R")

df_choices <- data.frame(
  proposal = c("prop-1", "prop-1", "prop-2", "prop-2", "prop-2", "prop-3", "prop-3"),
  choice_idx = c(2, 4, 1, 2, 3, 1, 2),
  prop_chid = c("prop-12", "prop-14", "prop-21", "prop-22", "prop-23", "prop-31", "prop-32"),
  type = c("approval", "approval", "weighted", "weighted", "weighted", "approval", "approval"),
  stringsAsFactors = FALSE
)

favorite <- data.frame(
  proposal = c("prop-1", "prop-1", "prop-2", "prop-3"),
  author = c("0xauthor", "0xauthor", "0xwriter", "0xbroken"),
  author_choice = c(4, 2, 3, NA),
  author_choice_weight = c(0.5, 0.5, 1.0, NA),
  author_vp = c(5, 5, 7, 9),
  author_pos = c(1, 1, 2, 3),
  author_rank = c(0.5, 0.5, 1.0, 1.0),
  vote_mode = c("favorite_choice", "favorite_choice", "favorite_choice", "favorite_choice"),
  stringsAsFactors = FALSE
)

support <- data.frame(
  proposal = c("prop-1", "prop-1", "prop-2", "prop-2", "prop-3"),
  author = c("0xauthor", "0xauthor", "0xwriter", "0xwriter", "0xbroken"),
  author_choice = c(2, 4, 1, 3, NA),
  author_support_fraction = c(0.5, 0.5, 0.25, 0.75, NA),
  author_vp = c(5, 5, 7, 7, 9),
  author_pos = c(1, 1, 2, 2, 3),
  author_rank = c(0.5, 0.5, 1.0, 1.0, 1.0),
  vote_mode = c("support_fraction", "support_fraction", "support_fraction", "support_fraction", "support_fraction"),
  stringsAsFactors = FALSE
)

merged <- mergeAuthorVoteOutputs(df_choices, favorite, support)

stopifnot(nrow(merged) == 7)
stopifnot(sum(merged$is_author_choice, na.rm = TRUE) == 3)
stopifnot(all(merged$is_author_choice[merged$proposal == "prop-1"] == c(1, 1)))
stopifnot(all(merged$is_author_choice[merged$proposal == "prop-2"] == c(0, 0, 1)))
stopifnot(all(is.na(merged$is_author_choice[merged$proposal == "prop-3"])))
stopifnot(all(merged$author_multi[merged$proposal == "prop-1"] == 1))
stopifnot(merged$author_choice[merged$proposal == "prop-1"][1] == 4)
stopifnot(all(merged$author_support_fraction[merged$proposal == "prop-1"] == c(0.5, 0.5)))
stopifnot(all(merged$author_support_fraction[merged$proposal == "prop-2"] == c(0.25, 0, 0.75)))
stopifnot(all(merged$is_author_supported_choice[merged$proposal == "prop-2"] == c(1, 0, 1)))
stopifnot(all(is.na(merged$author_support_fraction[merged$proposal == "prop-3"])))
stopifnot(all(is.na(merged$is_author_supported_choice[merged$proposal == "prop-3"])))

df_choices_with_no_author <- rbind(
  df_choices,
  data.frame(
    proposal = c("prop-4", "prop-4"),
    choice_idx = c(1, 2),
    prop_chid = c("prop-41", "prop-42"),
    type = c("approval", "approval"),
    stringsAsFactors = FALSE
  )
)

favorite_with_no_author <- rbind(
  favorite,
  data.frame(
    proposal = "prop-4",
    author = "0xmissing",
    author_choice = NA,
    author_choice_weight = NA,
    author_vp = NA,
    author_pos = NA,
    author_rank = NA,
    vote_mode = "favorite_choice",
    stringsAsFactors = FALSE
  )
)

support_with_no_author <- rbind(
  support,
  data.frame(
    proposal = "prop-4",
    author = "0xmissing",
    author_choice = NA,
    author_support_fraction = NA,
    author_vp = NA,
    author_pos = NA,
    author_rank = NA,
    vote_mode = "support_fraction",
    stringsAsFactors = FALSE
  )
)

merged_with_no_author <- mergeAuthorVoteOutputs(
  df_choices_with_no_author,
  favorite_with_no_author,
  support_with_no_author
)

stopifnot(nrow(merged_with_no_author) == 9)
stopifnot(all(is.na(merged_with_no_author$author_choice[merged_with_no_author$proposal == "prop-4"])))
stopifnot(all(is.na(merged_with_no_author$author_pos[merged_with_no_author$proposal == "prop-4"])))
stopifnot(all(is.na(merged_with_no_author$is_author_choice[merged_with_no_author$proposal == "prop-4"])))
stopifnot(all(is.na(merged_with_no_author$author_support_fraction[merged_with_no_author$proposal == "prop-4"])))
stopifnot(all(is.na(merged_with_no_author$is_author_supported_choice[merged_with_no_author$proposal == "prop-4"])))

df_choices_with_malformed_author <- rbind(
  df_choices,
  data.frame(
    proposal = c("prop-5", "prop-5"),
    choice_idx = c(1, 2),
    prop_chid = c("prop-51", "prop-52"),
    type = c("approval", "approval"),
    stringsAsFactors = FALSE
  )
)

favorite_with_malformed_author <- rbind(
  favorite,
  data.frame(
    proposal = "prop-5",
    author = "0xmalformed",
    author_choice = 9,
    author_choice_weight = 1,
    author_vp = 3,
    author_pos = 1,
    author_rank = 1,
    vote_mode = "favorite_choice",
    stringsAsFactors = FALSE
  )
)

support_with_malformed_author <- rbind(
  support,
  data.frame(
    proposal = "prop-5",
    author = "0xmalformed",
    author_choice = NA,
    author_support_fraction = NA,
    author_vp = 3,
    author_pos = 1,
    author_rank = 1,
    vote_mode = "support_fraction",
    stringsAsFactors = FALSE
  )
)

merged_with_malformed_author <- mergeAuthorVoteOutputs(
  df_choices_with_malformed_author,
  favorite_with_malformed_author,
  support_with_malformed_author
)

stopifnot(nrow(merged_with_malformed_author) == 9)
stopifnot(all(is.na(merged_with_malformed_author$is_author_choice[
  merged_with_malformed_author$proposal == "prop-5"
])))
