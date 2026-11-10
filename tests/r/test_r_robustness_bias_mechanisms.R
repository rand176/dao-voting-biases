source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
Sys.setenv(ROBUSTNESS_BIAS_MECHANISMS_SKIP_MAIN = "1")
Sys.setenv(THREE_BIAS_FOREST_BOOTSTRAP_SKIP_MAIN = "1")
script_file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
test_dir <- if (length(script_file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1]]), winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
generator_candidates <- c(
  file.path(dirname(test_dir), "r", "robustness_bias_mechanisms.R"),
  file.path(PACKAGE_ROOT, "src/r/models/workflow.R"),
  file.path("..", "r", "robustness_bias_mechanisms.R")
)
generator_path <- generator_candidates[file.exists(generator_candidates)][[1]]
source(generator_path)
source(file.path(PACKAGE_ROOT,"src/r/inference/bootstrap.R"), chdir = TRUE)

test_default_input_path_uses_current_release_registry <- function() {
  temp_dir <- tempfile("robustness-release-registry-")
  release_relative_path <- file.path(
    "data", "2024", "derived", "active", "registry_current.csv"
  )
  release_file <- file.path(temp_dir, "analysis", release_relative_path)
  registry_file <- file.path(temp_dir, "analysis", "metadata", "analysis_data_releases.csv")
  old_input <- Sys.getenv("ROBUSTNESS_INPUT_FILE", unset = NA_character_)

  on.exit({
    if (is.na(old_input)) {
      Sys.unsetenv("ROBUSTNESS_INPUT_FILE")
    } else {
      Sys.setenv(ROBUSTNESS_INPUT_FILE = old_input)
    }
    unlink(temp_dir, recursive = TRUE)
  })

  dir.create(dirname(release_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  file.create(release_file)
  write.csv(
    data.frame(
      release_name = "current",
      relative_path = release_relative_path,
      is_frozen = FALSE,
      notes = "test current release",
      stringsAsFactors = FALSE
    ),
    registry_file,
    row.names = FALSE
  )
  Sys.unsetenv("ROBUSTNESS_INPUT_FILE")

  stopifnot(identical(
    default_input_path(temp_dir),
    normalizePath(release_file, winslash = "/", mustWork = TRUE)
  ))
}

test_default_input_path_uses_current_release_registry()

toy_choices <- data.frame(
  proposal = c("p1", "p1", "p2", "p2", "p3", "p3", "p4", "p4", "p5", "p5", "p6", "p6"),
  space = c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4", "s5", "s5", "s6", "s6"),
  type = c(
    "single-choice", "single-choice", "basic", "basic", "approval", "approval",
    "weighted", "weighted", "quadratic", "quadratic", "ranked-choice", "ranked-choice"
  ),
  choice_idx = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
  N_choices = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
  choice_share_2 = c(0.7, 0.3, 0.4, 0.6, 0.8, 0.2, 0.6, 0.4, 0.55, 0.45, 0.65, 0.35),
  choice_share_no_author_vp = c(0.55, 0.45, 0.4, 0.6, NA, NA, NA, NA, NA, NA, NA, NA),
  is_author_choice2 = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
  author_choice = c(1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1),
  tot_votes_2 = c(3, 3, 1, 1, 4, 4, 5, 5, 6, 6, 7, 7),
  stance = c(
    "APPROVE", "REJECT", "OTHER", "OTHER", "APPROVE", "OTHER",
    "OTHER", "OTHER", "OTHER", "OTHER", "APPROVE", "REJECT"
  ),
  is_infavor = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0),
  author_pos6 = c(1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1),
  created_relative = c(10, 10, 20, 20, 30, 30, 40, 40, 50, 50, 60, 60),
  choice_text_len_rel = c(0.1, -0.1, 0.2, -0.2, 0.3, -0.3, 0.4, -0.4, 0.5, -0.5, 0.6, -0.6),
  n_votes = c(2, 1, 0, 1, 3, 1, 3, 2, 3, 3, 5, 2),
  flagged = FALSE,
  stringsAsFactors = FALSE
)

toy_choices <- rbind(
  toy_choices,
  data.frame(
    proposal = "p7",
    space = "s7",
    type = "single-choice",
    choice_idx = 1,
    N_choices = 1,
    choice_share_2 = 1,
    choice_share_no_author_vp = 1,
    is_author_choice2 = TRUE,
    author_choice = 1,
    tot_votes_2 = 8,
    stance = "APPROVE",
    is_infavor = 1,
    author_pos6 = 1,
    created_relative = 70,
    choice_text_len_rel = 0,
    n_votes = 8,
    flagged = FALSE,
    stringsAsFactors = FALSE
  ),
  data.frame(
    proposal = c("p8", "p8"),
    space = c("s8", "s8"),
    type = c("single-choice", "single-choice"),
    choice_idx = c(1, 2),
    N_choices = c(2, 2),
    choice_share_2 = c(0.45, 0.55),
    choice_share_no_author_vp = c(NA, NA),
    is_author_choice2 = c(NA, NA),
    author_choice = c(NA, NA),
    tot_votes_2 = c(10, 10),
    stance = c("OTHER", "REJECT"),
    is_infavor = c(0, 0),
    author_pos6 = c(NA, NA),
    created_relative = c(80, 80),
    choice_text_len_rel = c(0.15, -0.15),
    n_votes = c(4, 6),
    flagged = FALSE,
    stringsAsFactors = FALSE
  )
)

test_paper_exclusions_precede_sample_flags <- function() {
  temp_dir <- tempfile("robustness-paper-exclusions-")
  input_file <- file.path(temp_dir, "choices.csv")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  fixture <- toy_choices[toy_choices$proposal %in% c("p1", "p2"), ]
  flagged <- fixture[fixture$proposal == "p1", ]
  flagged$proposal <- "flagged-proposal"
  flagged$flagged <- TRUE
  single_option <- fixture[fixture$proposal == "p2", ][1, ]
  single_option$proposal <- "single-option-proposal"
  single_option$N_choices <- 1
  single_option$choice_idx <- 1
  single_option$choice_share_2 <- 1
  single_option$n_votes <- single_option$tot_votes_2
  valid <- fixture[fixture$proposal == "p2", ]
  valid$proposal <- "valid-proposal"
  valid$flagged <- FALSE
  utils::write.csv(rbind(flagged, single_option, valid), input_file, row.names = FALSE)

  prepared <- prepare_model_dataframe(load_choice_data(input_file))
  stopifnot(identical(sort(unique(prepared$proposal)), "valid-proposal"))
  stopifnot(sum(prepared$main_regression_row) == 2)
}

test_paper_exclusions_precede_sample_flags()

model_df <- prepare_model_dataframe(toy_choices)
stopifnot(nrow(model_df) == nrow(toy_choices) - 1)
stopifnot(!"p7" %in% model_df$proposal)
stopifnot(sum(model_df$single_preference_row) == 6)
stopifnot(sum(model_df$full_sample_regression_row) == 10)
stopifnot(all(model_df$full_sample_regression_row[model_df$type == "ranked-choice"]))
stopifnot(!any(model_df$full_sample_regression_row[model_df$type %in% c("weighted", "quadratic")]))
stopifnot(sum(model_df$all_vote_types_regression_row) == 14)
stopifnot(all(model_df$all_vote_types_regression_row[model_df$type %in% c("weighted", "quadratic")]))
stopifnot(sum(model_df$main_regression_row) == 6)
stopifnot(sum(model_df$author_voted_regression_row) == 4)
stopifnot(sum(model_df$vote_count_outcome_row) == 6)
stopifnot(sum(model_df$participation_control_row) == 6)
stopifnot(sum(model_df$no_author_vp_row) == 4)
stopifnot(sum(model_df$low_participation_row) == 4)
stopifnot(all(model_df$main_regression_row[model_df$proposal == "p8"]))
stopifnot(all(model_df$author_voted[model_df$proposal == "p8"] == 0))
stopifnot(all(model_df$is_author_choice_expanded[model_df$proposal == "p8"] == 0))
stopifnot(all(model_df$author_pos6_expanded[model_df$proposal == "p8"] == 0))
stopifnot(identical(
  model_df$author_voted_regression_row,
  model_df$main_regression_row & model_df$author_voted == 1
))
stopifnot(sum(model_df$first_author_choice_expanded) == 3)
stopifnot(sum(model_df$nonfirst_author_choice_expanded) == 1)
stopifnot(sum(model_df$infavor_author_choice_expanded) == 3)
stopifnot(sum(model_df$noninfavor_author_choice_expanded) == 1)
stopifnot(all(
  model_df$infavor_author_choice_expanded +
    model_df$noninfavor_author_choice_expanded ==
    model_df$is_author_choice_expanded
))
stopifnot(all(model_df$vote_count_share >= 0 & model_df$vote_count_share <= 1, na.rm = TRUE))
vote_count_sums <- aggregate(
  vote_count_share ~ proposal,
  model_df[model_df$vote_count_outcome_row, ],
  sum
)
stopifnot(all(abs(vote_count_sums$vote_count_share - 1) < 1e-8))

make_fractional_fixture <- function(n_proposals = 36, n_choices = 4) {
  rows <- expand.grid(
    proposal_idx = seq_len(n_proposals),
    choice_idx = seq_len(n_choices)
  )
  rows <- rows[order(rows$proposal_idx, rows$choice_idx), ]
  rows$proposal <- paste0("fp", rows$proposal_idx)
  rows$space <- paste0("fs", ((rows$proposal_idx - 1) %% 6) + 1)
  vote_types <- c("single-choice", "basic", "ranked-choice", "approval", "weighted", "quadratic")
  rows$type <- vote_types[
    ((rows$proposal_idx + ((rows$proposal_idx - 1) %/% 6) - 1) %% length(vote_types)) + 1
  ]
  rows$N_choices <- n_choices
  rows$is_author_choice <- as.integer(
    rows$choice_idx == ((rows$proposal_idx - 1) %% n_choices) + 1
  )
  rows$author_voted <- 1L
  rows$is_author_choice_expanded <- rows$is_author_choice
  rows$is_infavor <- as.integer(rows$choice_idx %in% c(1, 3))
  rows$stance <- ifelse(rows$is_infavor == 1, "APPROVE", "REJECT")
  rows$is_first_choice <- as.integer(rows$choice_idx == 1)
  rows$first_author_choice_expanded <- as.integer(rows$is_author_choice_expanded == 1 & rows$is_first_choice == 1)
  rows$nonfirst_author_choice_expanded <- as.integer(rows$is_author_choice_expanded == 1 & rows$is_first_choice == 0)
  rows$infavor_author_choice_expanded <- as.integer(
    rows$is_author_choice_expanded == 1 & rows$is_infavor == 1
  )
  rows$noninfavor_author_choice_expanded <- as.integer(
    rows$is_author_choice_expanded == 1 & rows$is_infavor == 0
  )
  rows$first_author_choice <- rows$first_author_choice_expanded
  rows$nonfirst_author_choice <- rows$nonfirst_author_choice_expanded
  rows$created_rel <- rows$proposal_idx / n_proposals
  rows$author_pos6 <- as.integer(rows$proposal_idx %% 3 == 0)
  rows$author_pos6_expanded <- rows$author_pos6
  rows$choice_text_len_rel <- (rows$choice_idx - mean(seq_len(n_choices))) / n_choices
  rows$tot_votes_2 <- 20 + (rows$proposal_idx %% 9)
  vote_weight <- n_choices - rows$choice_idx + 1
  rows$n_votes <- vote_weight / ave(vote_weight, rows$proposal_idx, FUN = sum) * rows$tot_votes_2
  rows$vote_count_share <- rows$n_votes / rows$tot_votes_2
  rows$log_tot_votes_2 <- log1p(rows$tot_votes_2)
  space_effect <- (as.integer(factor(rows$space)) - 3.5) / 10
  linear_predictor <- -1 +
    1.0 * rows$is_author_choice_expanded +
    0.4 * rows$is_infavor -
    0.12 * rows$choice_idx +
    0.25 * rows$is_first_choice +
    space_effect
  rows$choice_share <- stats::plogis(linear_predictor)
  rows$choice_share_no_author_vp <- pmax(
    pmin(rows$choice_share - 0.06 * rows$is_author_choice, 0.99),
    0.01
  )
  rows$single_preference_row <- rows$type %in% c("single-choice", "basic")
  rows$full_sample_supported_row <- rows$type %in% c(
    "single-choice", "basic", "ranked-choice", "approval"
  )
  rows$full_sample_regression_row <- rows$full_sample_supported_row
  rows$all_vote_types_regression_row <- TRUE
  rows$main_regression_row <- rows$single_preference_row
  rows$author_voted_regression_row <- rows$main_regression_row & rows$author_voted == 1
  rows$vote_count_outcome_row <- rows$main_regression_row
  rows$participation_control_row <- rows$main_regression_row
  rows$no_author_vp_row <- rows$author_voted_regression_row
  rows$low_participation_row <- rows$main_regression_row
  rows
}

fractional_fixture_base <- make_fractional_fixture()
# Add two proposals in parent-space.eth so it appears in release and qualifies
# parent-child protocol (rank 8 in write_top_tvl_fixture) as in_analysis_release.
extra_rows <- fractional_fixture_base[fractional_fixture_base$proposal == "fp1", ]
extra_rows$proposal <- c("fp-extra1", "fp-extra1", "fp-extra1", "fp-extra1")
extra_rows$space <- "parent-space.eth"
fractional_fixture <- rbind(fractional_fixture_base, extra_rows)
stopifnot(all(c(
  "infavor_author_choice_expanded",
  "noninfavor_author_choice_expanded"
) %in% names(fractional_fixture)))
stopifnot(all(
  fractional_fixture$infavor_author_choice_expanded +
    fractional_fixture$noninfavor_author_choice_expanded ==
    fractional_fixture$is_author_choice_expanded
))
fixture_model <- fit_fractional_logit(
  fractional_fixture,
  "choice_share",
  "main_regression_row",
  "any",
  "space",
  "Fixture rank-quadratic model"
)
fixture_formula <- paste(deparse(stats::formula(fixture_model)), collapse = " ")
stopifnot(grepl("author_voted", fixture_formula, fixed = TRUE))
stopifnot(grepl("is_author_choice_expanded", fixture_formula, fixed = TRUE))
stopifnot(!grepl("n_votes", fixture_formula, fixed = TRUE))
glm_model <- stats::glm(
  choice_share ~ is_author_choice + is_infavor + choice_idx + factor(space),
  data = fractional_fixture,
  family = stats::quasibinomial("logit")
)
attr(glm_model, "robustness_data") <- fractional_fixture
fixture_newdata <- average_adjusted_prediction_newdata(glm_model, "is_author_choice")
wrapped_difference <- average_adjusted_prediction_difference(
  glm_model,
  "is_author_choice",
  newdata = fixture_newdata
)
direct_slopes <- as.data.frame(marginaleffects::avg_slopes(
  glm_model,
  variables = "is_author_choice",
  newdata = fixture_newdata,
  vcov = ~space
))
stopifnot(identical(
  names(wrapped_difference),
  c(
    "term",
    "average_adjusted_prediction_difference",
    "average_adjusted_prediction_difference_pp",
    "average_adjusted_prediction_difference_se_pp",
    "average_adjusted_prediction_difference_p"
  )
))
stopifnot(isTRUE(all.equal(
  wrapped_difference$average_adjusted_prediction_difference,
  direct_slopes$estimate[direct_slopes$term == "is_author_choice"],
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
  wrapped_difference$average_adjusted_prediction_difference_se_pp,
  100 * direct_slopes[direct_slopes$term == "is_author_choice", "std.error"],
  tolerance = 1e-10,
  check.attributes = FALSE
)))
fixest_difference <- average_adjusted_prediction_difference(
  fixture_model,
  "is_author_choice_expanded"
)
stopifnot(is.na(fixest_difference$average_adjusted_prediction_difference_se_pp))
glm_first_choice_model <- stats::glm(
  choice_share ~ is_author_choice + is_infavor + is_first_choice + factor(space),
  data = fractional_fixture,
  family = stats::quasibinomial("logit")
)
attr(glm_first_choice_model, "robustness_data") <- fractional_fixture
manual_first <- fractional_fixture
manual_nonfirst <- fractional_fixture
manual_first$is_first_choice <- 1L
manual_nonfirst$is_first_choice <- 0L
manual_first_nonfirst_pp <- 100 * mean(
  stats::predict(glm_first_choice_model, newdata = manual_first, type = "response") -
    stats::predict(glm_first_choice_model, newdata = manual_nonfirst, type = "response"),
  na.rm = TRUE
)
first_nonfirst_difference <- average_first_nonfirst_prediction_difference(glm_first_choice_model)
stopifnot(identical(first_nonfirst_difference$term, "first_vs_nonfirst_choice"))
stopifnot(isTRUE(all.equal(
  first_nonfirst_difference$average_adjusted_prediction_difference_pp,
  manual_first_nonfirst_pp,
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(is.na(first_nonfirst_difference$average_adjusted_prediction_difference_se_pp))
stopifnot(is.na(first_nonfirst_difference$average_adjusted_prediction_difference_p))
rank_model_first_nonfirst_difference <- average_first_nonfirst_prediction_difference(fixture_model)
stopifnot(identical(rank_model_first_nonfirst_difference$term, "first_vs_nonfirst_choice"))
stopifnot(is.na(rank_model_first_nonfirst_difference$average_adjusted_prediction_difference_pp))

write_gzip_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(path, open = "wt")
  on.exit(close(con), add = TRUE)
  utils::write.csv(df, con, row.names = FALSE)
}

write_top_tvl_fixture <- function(project_root) {
  provenance_dir <- file.path(project_root, "metadata", "analysis_data_provenance")
  snapshot_dir <- file.path(project_root, "data", "2024", "raw")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)

  protocol_file <- file.path(provenance_dir, "defillama_protocol_ethereum_tvl_top100.csv")
  overrides_file <- file.path(provenance_dir, "defillama_snapshot_overrides.csv")
  diagnostics_file <- file.path(provenance_dir, "defillama_top_tvl_snapshot_match_diagnostics.csv")
  snapshot_file <- file.path(snapshot_dir, "spaces_0.csv.gz")
  legacy_contributor_file <- file.path(project_root, "data", "contributors", "defiLlamaEthDeFi.csv")
  legacy_checked_file <- file.path(project_root, "data", "contributors", "Llama_snapshot_match_checked.csv")
  dir.create(dirname(legacy_contributor_file), recursive = TRUE, showWarnings = FALSE)
  writeLines("id,category,totalLiquidityUSD\nwrong.eth,Dexes,999999", legacy_contributor_file)
  writeLines("id,protocol_name_y\nwrong.eth,Wrong legacy protocol", legacy_checked_file)

  utils::write.csv(
    data.frame(
      protocol_rank = 1:8,
      protocol_slug = c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "fs4", "parent-child"),
      protocol_name = c(
        "Alpha Protocol", "Beta Protocol", "Gamma Protocol", "Delta Protocol",
        "Epsilon Protocol", "Zeta Protocol", "FS4 Protocol", "Parent Child Protocol"
      ),
      protocol_id = paste0("p", 1:8),
      category = c("Dexes", "Yield", "Lending", "Services", "Dexes", "Yield", "Dexes", "Lending"),
      parent_protocol = c("", "", "", "", "", "", "", "parent#par"),
      snapshot_governance_ids = c("fs1", "flagged.eth", "missing-space.eth", "wrong.eth", "", "reject.eth", "", ""),
      parent_snapshot_governance_ids = c("", "", "", "", "", "", "", "parent-space.eth"),
      mean_ethereum_tvl_usd = c(400, 350, 300, 250, 200, 150, 125, 100),
      median_ethereum_tvl_usd = c(400, 350, 300, 250, 200, 150, 125, 100),
      tvl_day_count = 10,
      expected_period_days = 10,
      tvl_coverage_ratio = 1,
      first_tvl_date = "2021-01-01",
      last_tvl_date = "2021-01-10",
      cache_file = paste0("data/contributors/defillama_protocol_history/", c("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "fs4", "parent-child"), ".json"),
      cache_checksum = paste0("sha", 1:8),
      included_in_top100 = TRUE,
      excluded_reason = "",
      tvl_eligible_for_ranking = TRUE,
      ranking_exclusion_reason = "",
      fetch_error = "",
      stringsAsFactors = FALSE
    ),
    protocol_file,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      protocol_slug = c("delta", "epsilon", "zeta"),
      snapshot_space = c("fs2", "fs3", "reject.eth"),
      override_action = c("replace", "accept", "reject"),
      match_source = "manual_review",
      rationale = c("reviewed replacement", "reviewed accepted DAO", "not the DAO governance space"),
      evidence_url_or_note = "fixture review note",
      reviewer = "test",
      review_date = "2026-05-24",
      eligible_for_subset = c(TRUE, TRUE, FALSE),
      replaces_snapshot_space = c("wrong.eth", "", ""),
      stringsAsFactors = FALSE
    ),
    overrides_file,
    row.names = FALSE
  )
  write_gzip_csv(
    data.frame(
      id = c("fs1", "flagged.eth", "missing-space.eth", "wrong.eth", "fs2", "fs3", "reject.eth", "fs4", "parent-space.eth"),
      deleted = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
      flagged = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
      stringsAsFactors = FALSE
    ),
    snapshot_file
  )

  list(
    protocol_file = protocol_file,
    overrides_file = overrides_file,
    diagnostics_file = diagnostics_file,
    snapshot_file = snapshot_file,
    legacy_contributor_file = legacy_contributor_file,
    legacy_checked_file = legacy_checked_file
  )
}

write_top_tvl_overflow_fixture <- function(project_root, n = 55) {
  provenance_dir <- file.path(project_root, "metadata", "analysis_data_provenance")
  snapshot_dir <- file.path(project_root, "data", "2024", "raw")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)

  ranks <- seq_len(n)
  protocol_file <- file.path(provenance_dir, "defillama_protocol_ethereum_tvl_top100.csv")
  overrides_file <- file.path(provenance_dir, "defillama_snapshot_overrides.csv")
  diagnostics_file <- file.path(provenance_dir, "defillama_top_tvl_snapshot_match_diagnostics.csv")
  snapshot_file <- file.path(snapshot_dir, "spaces_0.csv.gz")

  utils::write.csv(
    data.frame(
      protocol_rank = ranks,
      protocol_slug = paste0("overflow-protocol-", ranks),
      protocol_name = paste("Overflow Protocol", ranks),
      protocol_id = paste0("overflow-", ranks),
      category = "Dexes",
      parent_protocol = "",
      snapshot_governance_ids = paste0("overflow", ranks, ".eth"),
      parent_snapshot_governance_ids = "",
      mean_ethereum_tvl_usd = rev(ranks) * 1000,
      median_ethereum_tvl_usd = rev(ranks) * 1000,
      tvl_day_count = 10,
      expected_period_days = 10,
      tvl_coverage_ratio = 1,
      first_tvl_date = "2021-01-01",
      last_tvl_date = "2021-01-10",
      cache_file = paste0("data/contributors/defillama_protocol_history/overflow-", ranks, ".json"),
      cache_checksum = paste0("sha-overflow-", ranks),
      included_in_top100 = TRUE,
      excluded_reason = "",
      tvl_eligible_for_ranking = TRUE,
      ranking_exclusion_reason = "",
      fetch_error = "",
      stringsAsFactors = FALSE
    ),
    protocol_file,
    row.names = FALSE
  )
  utils::write.csv(empty_defillama_snapshot_overrides(), overrides_file, row.names = FALSE)
  write_gzip_csv(
    data.frame(
      id = paste0("overflow", ranks, ".eth"),
      deleted = 0,
      flagged = 0,
      stringsAsFactors = FALSE
    ),
    snapshot_file
  )

  list(
    protocol_file = protocol_file,
    overrides_file = overrides_file,
    diagnostics_file = diagnostics_file,
    snapshot_file = snapshot_file
  )
}

make_top_tvl_overflow_model_df <- function(n = 55) {
  ranks <- seq_len(n)
  raw <- data.frame(
    proposal = rep(paste0("overflow-proposal-", ranks), each = 2),
    space = rep(paste0("overflow", ranks, ".eth"), each = 2),
    type = "single-choice",
    choice_idx = rep(c(1, 2), n),
    N_choices = 2,
    choice_share_2 = rep(c(0.6, 0.4), n),
    choice_share_no_author_vp = NA_real_,
    is_author_choice2 = rep(c(TRUE, FALSE), n),
    author_choice = 1,
    tot_votes_2 = 10,
    stance = rep(c("APPROVE", "REJECT"), n),
    is_infavor = rep(c(1, 0), n),
    author_pos6 = 1,
    created_relative = 10,
    choice_text_len_rel = 0,
    n_votes = rep(c(6, 4), n),
    flagged = FALSE,
    stringsAsFactors = FALSE
  )
  prepare_model_dataframe(raw)
}

expect_error_matching <- function(expr, pattern) {
  message <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(err) conditionMessage(err)
  )
  stopifnot(!is.null(message))
  stopifnot(grepl(pattern, message, fixed = TRUE))
}

make_passing_top_tvl_public_table <- function(n = 25, extra_n = 0) {
  ranks <- seq_len(n + extra_n)
  data.frame(
    protocol_rank = ranks,
    protocol_slug = paste0("protocol-", ranks),
    candidate_snapshot_space = paste0("space", ranks, ".eth"),
    protocol_name = paste("Protocol", ranks),
    match_status = "accepted",
    match_source = "defillama_governance_id",
    override_action = "",
    raw_snapshot_flagged = FALSE,
    raw_snapshot_deleted = FALSE,
    snapshot_status = "Active",
    category = "Dexes",
    median_ethereum_tvl_usd = 1000000 - ranks,
    in_analysis_release = TRUE,
    top_tvl_sample_rank = ranks,
    used_in_top_tvl_top25_sample = ranks <= 25,
    used_in_top_tvl_top50_sample = ranks <= 50,
    used_in_top_tvl_top100_sample = ranks <= 100,
    used_in_top_tvl_robustness_subset = ranks <= n,
    proposals = ranks,
    choices = ranks * 2L,
    stringsAsFactors = FALSE
  )
}

test_default_top_tvl_paths_and_legacy_refusal <- function() {
  temp_dir <- tempfile("top-tvl-project-root-")
  files <- write_top_tvl_fixture(temp_dir)
  old_top_tvl_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_FILE", unset = NA_character_)
  on.exit({
    if (is.na(old_top_tvl_file)) {
      Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_FILE")
    } else {
      Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = old_top_tvl_file)
    }
    unlink(temp_dir, recursive = TRUE)
  })

  Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_FILE")
  stopifnot(identical(default_top_tvl_spaces_path(temp_dir), files$protocol_file))
  expect_error_matching(
    load_top_tvl_table(files$legacy_contributor_file, project_root = temp_dir),
    "Legacy DefiLlama Top-TVL source is not allowed"
  )
}

test_default_top_tvl_paths_and_legacy_refusal()

test_override_validation <- function() {
  temp_dir <- tempfile("top-tvl-overrides-")
  files <- write_top_tvl_fixture(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  overrides <- utils::read.csv(files$overrides_file, stringsAsFactors = FALSE)

  incomplete <- overrides
  incomplete$reviewer[[1]] <- ""
  utils::write.csv(incomplete, files$overrides_file, row.names = FALSE)
  expect_error_matching(
    load_defillama_snapshot_overrides(files$overrides_file),
    "required audit field"
  )

  invalid <- overrides
  invalid$override_action[[1]] <- "maybe"
  utils::write.csv(invalid, files$overrides_file, row.names = FALSE)
  expect_error_matching(
    load_defillama_snapshot_overrides(files$overrides_file),
    "Invalid override_action"
  )

  conflicting <- rbind(overrides, overrides[1, , drop = FALSE])
  conflicting$rationale[nrow(conflicting)] <- "conflicting duplicate"
  utils::write.csv(conflicting, files$overrides_file, row.names = FALSE)
  expect_error_matching(
    load_defillama_snapshot_overrides(files$overrides_file),
    "Conflicting override rows"
  )
}

test_override_validation()

old_top_spaces_n <- Sys.getenv("ROBUSTNESS_TOP_SPACES_N", unset = NA_character_)
old_top_tvl_spaces_n <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_N", unset = NA_character_)
old_top_tvl_table_n <- Sys.getenv("ROBUSTNESS_TOP_TVL_TABLE_N", unset = NA_character_)
old_top_tvl_spaces_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_FILE", unset = NA_character_)
old_top_tvl_overrides_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE", unset = NA_character_)
old_snapshot_spaces_file <- Sys.getenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE", unset = NA_character_)
old_top_tvl_diagnostics_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE", unset = NA_character_)
top_tvl_project_root <- tempfile("top-tvl-matching-")
top_tvl_files <- write_top_tvl_fixture(top_tvl_project_root)
Sys.setenv(ROBUSTNESS_TOP_SPACES_N = "1")
Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_N = "2")
Sys.setenv(ROBUSTNESS_TOP_TVL_TABLE_N = "7")
Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = top_tvl_files$protocol_file)
Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = top_tvl_files$overrides_file)
Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = top_tvl_files$snapshot_file)
Sys.setenv(ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE = top_tvl_files$diagnostics_file)
fixture_models <- fit_model_set(fractional_fixture)
if (is.na(old_top_spaces_n)) {
  Sys.unsetenv("ROBUSTNESS_TOP_SPACES_N")
} else {
  Sys.setenv(ROBUSTNESS_TOP_SPACES_N = old_top_spaces_n)
}
if (is.na(old_top_tvl_spaces_n)) {
  Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_N")
} else {
  Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_N = old_top_tvl_spaces_n)
}
if (is.na(old_top_tvl_table_n)) {
  Sys.unsetenv("ROBUSTNESS_TOP_TVL_TABLE_N")
} else {
  Sys.setenv(ROBUSTNESS_TOP_TVL_TABLE_N = old_top_tvl_table_n)
}
if (is.na(old_top_tvl_spaces_file)) {
  Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_FILE")
} else {
  Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = old_top_tvl_spaces_file)
}
if (is.na(old_top_tvl_overrides_file)) {
  Sys.unsetenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE")
} else {
  Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = old_top_tvl_overrides_file)
}
if (is.na(old_snapshot_spaces_file)) {
  Sys.unsetenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE")
} else {
  Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = old_snapshot_spaces_file)
}
if (is.na(old_top_tvl_diagnostics_file)) {
  Sys.unsetenv("ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE")
} else {
  Sys.setenv(ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE = old_top_tvl_diagnostics_file)
}
stopifnot(all(c(
  "all_vote_types_any",
  "author_voted_only_any",
  "vote_count_share_any",
  "participation_control_any",
  "top_tvl_dao_any",
  "first_choice_sensitivity_any",
  "first_choice_sensitivity_split"
) %in% names(fixture_models)))
stopifnot(identical(
  attr(fixture_models$all_vote_types_any, "robustness_subset_flag"),
  "all_vote_types_regression_row"
))
stopifnot(any(attr(fixture_models$all_vote_types_any, "robustness_data")$type %in% c(
  "weighted", "quadratic"
)))
stopifnot(identical(
  attr(fixture_models$top_tvl_dao_any, "robustness_subset_flag"),
  "top_tvl_dao_row"
))
stopifnot(identical(
  attr(fixture_models$author_voted_only_any, "robustness_subset_flag"),
  "author_voted_regression_row"
))
stopifnot(identical(
  attr(fixture_models$vote_count_share_any, "robustness_outcome"),
  "vote_count_share"
))
stopifnot(identical(
  attr(fixture_models$participation_control_any, "robustness_subset_flag"),
  "participation_control_row"
))
participation_formula <- paste(deparse(stats::formula(
  fixture_models$participation_control_any
)), collapse = " ")
stopifnot(grepl("log_tot_votes_2", participation_formula, fixed = TRUE))
vote_count_formula <- paste(deparse(stats::formula(
  fixture_models$vote_count_share_any
)), collapse = " ")
stopifnot(grepl("model_outcome", vote_count_formula, fixed = TRUE))
fitted_model_formulas <- lapply(
  fixture_models[vapply(fixture_models, inherits, logical(1), what = "fixest")],
  function(model) paste(deparse(stats::formula(model)), collapse = " ")
)
stopifnot(length(fitted_model_formulas) > 0)
for (formula_text in fitted_model_formulas) {
  stopifnot(grepl("type", formula_text, fixed = TRUE))
}
proposal_fe_formula <- paste(deparse(stats::formula(
  fixture_models$proposal_fe_any
)), collapse = " ")
stopifnot(grepl("is_author_choice_expanded", proposal_fe_formula, fixed = TRUE))
stance_terms <- c("infavor_author_choice_expanded", "noninfavor_author_choice_expanded")
stopifnot(all(stance_terms %in% names(stats::coef(fixture_models$matched_observed_stance_split))))
stopifnot(all(stance_terms %in% names(stats::coef(fixture_models$matched_no_author_stance_split))))
stopifnot(all(stance_terms %in% names(stats::coef(fixture_models$proposal_fe_stance_split))))
stopifnot(!("is_author_choice_expanded" %in% names(stats::coef(fixture_models$matched_observed_stance_split))))
stance_formula <- paste(deparse(stats::formula(
  fixture_models$matched_observed_stance_split
)), collapse = " ")
stopifnot(grepl("infavor_author_choice_expanded", stance_formula, fixed = TRUE))
stopifnot(grepl("noninfavor_author_choice_expanded", stance_formula, fixed = TRUE))
stopifnot(grepl("is_infavor", stance_formula, fixed = TRUE))
stopifnot(identical(
  attr(fixture_models$matched_observed_stance_split, "robustness_author_terms"),
  "stance_split"
))
for (omitted_term in c(
  "author_voted", "created_rel", "N_choices",
  "log_tot_votes_2", "n_votes"
)) {
  stopifnot(!grepl(omitted_term, proposal_fe_formula, fixed = TRUE))
}
stopifnot(all(attr(fixture_models$top_tvl_dao_any, "robustness_data")$space %in% c("fs1", "fs2")))
stopifnot(identical(fixture_models$top_tvl_spaces, c("fs1", "fs2")))
stopifnot(identical(names(fixture_models$top_tvl_sample_spaces), c("top25", "top50", "top100")))
stopifnot(all(fixture_models$top_tvl_sample_spaces$top25 %in% fixture_models$top_tvl_sample_spaces$top50))
stopifnot(all(fixture_models$top_tvl_sample_spaces$top50 %in% fixture_models$top_tvl_sample_spaces$top100))
fixture_top_tvl_data <- attr(fixture_models$top_tvl_dao_any, "robustness_data")
stopifnot("top_tvl_dao_top25_row" %in% names(fixture_top_tvl_data))
stopifnot("top_tvl_dao_top50_row" %in% names(fixture_top_tvl_data))
stopifnot("top_tvl_dao_top100_row" %in% names(fixture_top_tvl_data))
top_tvl_diagnostics <- utils::read.csv(top_tvl_files$diagnostics_file, stringsAsFactors = FALSE)
top_tvl_memberships_file <- file.path(
  dirname(top_tvl_files$diagnostics_file),
  "defillama_top_tvl_snapshot_sample_memberships.csv"
)
top_tvl_memberships <- utils::read.csv(top_tvl_memberships_file, stringsAsFactors = FALSE)
stopifnot(all(c(
  "protocol_rank",
  "protocol_slug",
  "candidate_snapshot_space",
  "match_source",
  "match_status",
  "override_action",
  "raw_snapshot_flagged",
  "raw_snapshot_deleted",
  "in_analysis_release",
  "used_in_top_tvl_robustness_subset"
) %in% names(top_tvl_diagnostics)))
stopifnot(identical(
  names(top_tvl_memberships),
  c(
    "sample",
    "top_tvl_sample_rank",
    "candidate_snapshot_space",
    "protocol_rank",
    "protocol_slug",
    "protocol_name",
    "category",
    "median_ethereum_tvl_usd",
    "proposals",
    "choices"
  )
))
top_tvl_membership_counts <- table(top_tvl_memberships$sample)
# 4 accepted spaces: fs1 (alpha), fs2 (delta-replace), fs3 (epsilon-accept), parent-space.eth (parent-child)
stopifnot(identical(as.integer(top_tvl_membership_counts[c("top25", "top50", "top100")]), c(4L, 4L, 4L)))
top25_members <- top_tvl_memberships$candidate_snapshot_space[top_tvl_memberships$sample == "top25"]
top50_members <- top_tvl_memberships$candidate_snapshot_space[top_tvl_memberships$sample == "top50"]
top100_members <- top_tvl_memberships$candidate_snapshot_space[top_tvl_memberships$sample == "top100"]
stopifnot(all(top25_members %in% top50_members))
stopifnot(all(top50_members %in% top100_members))
stopifnot(top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "delta" &
    top_tvl_diagnostics$candidate_snapshot_space == "wrong.eth",
  "match_status"
] == "replaced")
stopifnot(top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "delta" &
    top_tvl_diagnostics$candidate_snapshot_space == "fs2",
  "used_in_top_tvl_robustness_subset"
])
stopifnot(!top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "beta" &
    top_tvl_diagnostics$candidate_snapshot_space == "flagged.eth",
  "used_in_top_tvl_robustness_subset"
])
stopifnot(!top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "gamma" &
    top_tvl_diagnostics$candidate_snapshot_space == "missing-space.eth",
  "used_in_top_tvl_robustness_subset"
])
stopifnot(top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "fs4" &
    top_tvl_diagnostics$candidate_snapshot_space == "fs4",
  "fuzzy_diagnostic"
])
stopifnot(!top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "fs4" &
    top_tvl_diagnostics$candidate_snapshot_space == "fs4",
  "used_in_top_tvl_robustness_subset"
])

# parent-child (rank 8) inherits parent-space.eth via defillama_parent_governance_id
parent_child_row <- top_tvl_diagnostics[
  top_tvl_diagnostics$protocol_slug == "parent-child" &
    top_tvl_diagnostics$candidate_snapshot_space == "parent-space.eth",
]
stopifnot(nrow(parent_child_row) == 1)
stopifnot(identical(parent_child_row$match_source, "defillama_parent_governance_id"))
stopifnot(identical(parent_child_row$match_status, "accepted"))
stopifnot(isTRUE(parent_child_row$in_analysis_release))

# test: override-reject of a parent-derived match overwrites match_source to override_reject
test_parent_id_reject_override <- function() {
  temp_dir <- tempfile("top-tvl-parent-reject-")
  files <- write_top_tvl_fixture(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  overrides_with_reject <- utils::read.csv(files$overrides_file, stringsAsFactors = FALSE)
  overrides_with_reject <- rbind(
    overrides_with_reject,
    data.frame(
      protocol_slug = "parent-child",
      snapshot_space = "parent-space.eth",
      override_action = "reject",
      match_source = "manual_review",
      rationale = "wrong governance space for this child protocol",
      evidence_url_or_note = "test fixture note",
      reviewer = "test",
      review_date = "2026-06-11",
      eligible_for_subset = FALSE,
      replaces_snapshot_space = "",
      stringsAsFactors = FALSE
    )
  )
  utils::write.csv(overrides_with_reject, files$overrides_file, row.names = FALSE)

  snapshot_spaces <- load_snapshot_spaces(files$snapshot_file)
  protocols <- load_top_tvl_table(files$protocol_file, project_root = temp_dir)
  overrides <- load_defillama_snapshot_overrides(files$overrides_file)
  counts <- current_release_space_counts(fractional_fixture)
  result <- build_top_tvl_match_table(protocols, overrides, snapshot_spaces, counts, n = 2)

  rejected_row <- result[
    result$protocol_slug == "parent-child" &
      result$candidate_snapshot_space == "parent-space.eth",
  ]
  stopifnot(nrow(rejected_row) == 1)
  # match_source is overwritten to override_reject — the intended outcome, not an accident
  stopifnot(identical(rejected_row$match_status, "rejected"))
  stopifnot(identical(rejected_row$match_source, "override_reject"))
  stopifnot(!isTRUE(rejected_row$subset_candidate))
}

test_parent_id_reject_override()

# test: fuzzy candidate generation produces slug+suffix variants
test_fuzzy_variant_candidates <- function() {
  snap_spaces <- data.table::data.table(
    space = c(
      "curve-dex",         # exact slug
      "curve-dex.eth",     # slug + .eth
      "curve.eth",         # stripped -dex + .eth
      "curvedao.eth",      # slug stripped + dao.eth
      "curvegov.eth",      # slug stripped + gov.eth (won't match "curvedao.eth")
      "unrelated.eth"
    ),
    raw_snapshot_flagged = FALSE,
    raw_snapshot_deleted = FALSE
  )
  protocol_row <- list(
    protocol_slug = "curve-dex",
    protocol_name = "Curve DEX",
    parent_protocol = "parent#curve-finance"
  )
  candidates <- fuzzy_snapshot_candidates(protocol_row, snap_spaces)
  stopifnot("curve-dex" %in% candidates)
  stopifnot("curve-dex.eth" %in% candidates)
  stopifnot("curve.eth" %in% candidates)
  stopifnot(!"unrelated.eth" %in% candidates)
}

test_fuzzy_variant_candidates()

# test: same parent space attached to multiple child protocols dedupes in memberships
test_parent_space_deduplicates_in_memberships <- function() {
  temp_dir <- tempfile("top-tvl-dedup-")
  provenance_dir <- file.path(temp_dir, "metadata", "analysis_data_provenance")
  snapshot_dir <- file.path(temp_dir, "data", "2024", "raw")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  shared_space <- "shared-parent.eth"
  protocol_file <- file.path(provenance_dir, "defillama_protocol_ethereum_tvl_top100.csv")
  overrides_file <- file.path(provenance_dir, "defillama_snapshot_overrides.csv")
  snapshot_file <- file.path(snapshot_dir, "spaces_0.csv.gz")
  diagnostics_file <- file.path(provenance_dir, "defillama_top_tvl_snapshot_match_diagnostics.csv")

  utils::write.csv(
    data.frame(
      protocol_rank = 1:2,
      protocol_slug = c("child-a", "child-b"),
      protocol_name = c("Child A", "Child B"),
      protocol_id = c("ca", "cb"),
      category = "Dexes",
      parent_protocol = c("parent#shared", "parent#shared"),
      snapshot_governance_ids = c("", ""),
      parent_snapshot_governance_ids = c(shared_space, shared_space),
      mean_ethereum_tvl_usd = c(1000, 900),
      median_ethereum_tvl_usd = c(1000, 900),
      tvl_day_count = 10, expected_period_days = 10, tvl_coverage_ratio = 1,
      first_tvl_date = "2021-01-01", last_tvl_date = "2021-01-10",
      cache_file = c("ca.json", "cb.json"),
      cache_checksum = c("sha-ca", "sha-cb"),
      included_in_top100 = TRUE, excluded_reason = "",
      tvl_eligible_for_ranking = TRUE, ranking_exclusion_reason = "",
      fetch_error = "",
      stringsAsFactors = FALSE
    ),
    protocol_file, row.names = FALSE
  )
  utils::write.csv(empty_defillama_snapshot_overrides(), overrides_file, row.names = FALSE)
  write_gzip_csv(
    data.frame(id = shared_space, deleted = 0, flagged = 0, stringsAsFactors = FALSE),
    snapshot_file
  )

  model_df <- data.frame(
    proposal = c("p1", "p1"),
    space = c(shared_space, shared_space),
    stringsAsFactors = FALSE
  )
  protocols <- load_top_tvl_table(protocol_file, project_root = temp_dir)
  overrides <- load_defillama_snapshot_overrides(overrides_file)
  snapshot_spaces <- load_snapshot_spaces(snapshot_file)
  counts <- current_release_space_counts(model_df)
  result <- build_top_tvl_match_table(protocols, overrides, snapshot_spaces, counts, n = 25)

  # Both child protocols produce accepted rows for the same space
  accepted <- result[result$match_status == "accepted", ]
  stopifnot(sum(accepted$candidate_snapshot_space == shared_space) == 2)

  # The spaces *vector* from top_tvl_dao_spaces() dedupes — only one unique entry
  # (membership table has two rows, one per protocol, both eligible for top25)
  memberships <- top_tvl_sample_membership_table(result)
  stopifnot(sum(memberships$candidate_snapshot_space == shared_space & memberships$sample == "top25") == 2)
  # The unique eligible space vector has one entry
  eligible_spaces <- unique(result$candidate_snapshot_space[result$subset_candidate])
  stopifnot(identical(eligible_spaces, shared_space))
}

test_parent_space_deduplicates_in_memberships()

test_top_tvl_sample_spaces_ignore_display_truncation <- function() {
  temp_dir <- tempfile("top-tvl-overflow-")
  files <- write_top_tvl_overflow_fixture(temp_dir, n = 55)
  overflow_df <- make_top_tvl_overflow_model_df(n = 55)

  old_top_tvl_spaces_n <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_N", unset = NA_character_)
  old_top_tvl_table_n <- Sys.getenv("ROBUSTNESS_TOP_TVL_TABLE_N", unset = NA_character_)
  old_top_tvl_spaces_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_FILE", unset = NA_character_)
  old_top_tvl_overrides_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE", unset = NA_character_)
  old_snapshot_spaces_file <- Sys.getenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE", unset = NA_character_)

  on.exit({
    if (is.na(old_top_tvl_spaces_n)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_N") else Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_N = old_top_tvl_spaces_n)
    if (is.na(old_top_tvl_table_n)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_TABLE_N") else Sys.setenv(ROBUSTNESS_TOP_TVL_TABLE_N = old_top_tvl_table_n)
    if (is.na(old_top_tvl_spaces_file)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_FILE") else Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = old_top_tvl_spaces_file)
    if (is.na(old_top_tvl_overrides_file)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE") else Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = old_top_tvl_overrides_file)
    if (is.na(old_snapshot_spaces_file)) Sys.unsetenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE") else Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = old_snapshot_spaces_file)
    unlink(temp_dir, recursive = TRUE)
  })

  Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_N = "2")
  Sys.setenv(ROBUSTNESS_TOP_TVL_TABLE_N = "33")
  Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = files$protocol_file)
  Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = files$overrides_file)
  Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = files$snapshot_file)

  sets <- shared_specification_sets(overflow_df, diagnostics_path = files$diagnostics_file)
  diagnostics <- utils::read.csv(files$diagnostics_file, stringsAsFactors = FALSE)
  memberships <- utils::read.csv(
    file.path(dirname(files$diagnostics_file), "defillama_top_tvl_snapshot_sample_memberships.csv"),
    stringsAsFactors = FALSE
  )

  stopifnot(identical(length(sets$top_tvl_spaces), 2L))
  stopifnot(identical(names(sets$top_tvl_sample_spaces), c("top25", "top50", "top100")))
  stopifnot(identical(length(sets$top_tvl_sample_spaces$top25), 25L))
  stopifnot(identical(length(sets$top_tvl_sample_spaces$top50), 50L))
  stopifnot(identical(length(sets$top_tvl_sample_spaces$top100), 55L))
  stopifnot(max(diagnostics$protocol_rank, na.rm = TRUE) == 55)
  stopifnot(any(diagnostics$protocol_rank > 33 & diagnostics$used_in_top_tvl_top50_sample))
  stopifnot(any(diagnostics$protocol_rank > 33 & diagnostics$used_in_top_tvl_top100_sample))
  membership_counts <- table(memberships$sample)
  stopifnot(identical(as.integer(membership_counts[c("top25", "top50", "top100")]), c(25L, 50L, 55L)))
  top25_members <- memberships$candidate_snapshot_space[memberships$sample == "top25"]
  top50_members <- memberships$candidate_snapshot_space[memberships$sample == "top50"]
  top100_members <- memberships$candidate_snapshot_space[memberships$sample == "top100"]
  stopifnot(all(top25_members %in% top50_members))
  stopifnot(all(top50_members %in% top100_members))
}

test_top_tvl_sample_spaces_ignore_display_truncation()

test_default_top_tvl_alias_is_top25 <- function() {
  temp_dir <- tempfile("top-tvl-default-alias-")
  files <- write_top_tvl_overflow_fixture(temp_dir, n = 55)
  overflow_df <- make_top_tvl_overflow_model_df(n = 55)

  old_top_tvl_spaces_n <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_N", unset = NA_character_)
  old_top_tvl_spaces_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_FILE", unset = NA_character_)
  old_top_tvl_overrides_file <- Sys.getenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE", unset = NA_character_)
  old_snapshot_spaces_file <- Sys.getenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE", unset = NA_character_)

  on.exit({
    if (is.na(old_top_tvl_spaces_n)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_N") else Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_N = old_top_tvl_spaces_n)
    if (is.na(old_top_tvl_spaces_file)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_FILE") else Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = old_top_tvl_spaces_file)
    if (is.na(old_top_tvl_overrides_file)) Sys.unsetenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE") else Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = old_top_tvl_overrides_file)
    if (is.na(old_snapshot_spaces_file)) Sys.unsetenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE") else Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = old_snapshot_spaces_file)
    unlink(temp_dir, recursive = TRUE)
  })

  Sys.unsetenv("ROBUSTNESS_TOP_TVL_SPACES_N")
  Sys.setenv(ROBUSTNESS_TOP_TVL_SPACES_FILE = files$protocol_file)
  Sys.setenv(ROBUSTNESS_TOP_TVL_OVERRIDES_FILE = files$overrides_file)
  Sys.setenv(ROBUSTNESS_SNAPSHOT_SPACES_FILE = files$snapshot_file)

  sets <- shared_specification_sets(overflow_df, diagnostics_path = NA_character_)
  flagged <- apply_shared_specification_sets(
    overflow_df,
    excluded_spaces = sets$excluded_spaces,
    top_tvl_spaces = sets$top_tvl_spaces,
    top_tvl_sample_spaces = sets$top_tvl_sample_spaces
  )

  stopifnot(identical(sets$top_tvl_spaces, sets$top_tvl_sample_spaces$top25))
  stopifnot(identical(flagged$top_tvl_dao_row, flagged$top_tvl_dao_top25_row))
}

test_default_top_tvl_alias_is_top25()

public_top_tvl_models <- list(top_tvl_table = make_passing_top_tvl_public_table(extra_n = 3))
formatted_top_tvl <- format_top_tvl_dao_table(public_top_tvl_models)
stopifnot(identical(
  names(formatted_top_tvl),
  c(
    "Rank",
    "DefiLlama protocol",
    "Snapshot space",
    "Category",
    "TVL",
    "Proposals",
    "Choices"
  )
))
stopifnot(nrow(formatted_top_tvl) == 25)
stopifnot(!"Match" %in% names(formatted_top_tvl))
stopifnot(!"Snapshot status" %in% names(formatted_top_tvl))
stopifnot(!"M/S" %in% names(formatted_top_tvl))
stopifnot(!"In analysis release" %in% names(formatted_top_tvl))
stopifnot(!"Used in subset" %in% names(formatted_top_tvl))
stopifnot(identical(formatted_top_tvl$Rank, 1:25))
stopifnot(!any(formatted_top_tvl$`Snapshot space` == "space26.eth"))
stopifnot(identical(formatted_top_tvl$`DefiLlama protocol`[[1]], "Protocol 1"))
stopifnot(identical(formatted_top_tvl$Proposals[[25]], "25"))

top100_tvl_memberships <- make_passing_top_tvl_public_table(n = 100)
top100_tvl_memberships$sample <- "top100"
formatted_top100_tvl <- format_top_tvl_sample_dao_table(top100_tvl_memberships, rank_min = 1L, rank_max = 25L)
stopifnot(identical(
  names(formatted_top100_tvl),
  c(
    "Rank",
    "DefiLlama rank",
    "DefiLlama protocol",
    "Snapshot space",
    "Category",
    "TVL",
    "Proposals",
    "Choices"
  )
))
stopifnot(nrow(formatted_top100_tvl) == 25)
stopifnot(identical(formatted_top100_tvl$Rank, 1:25))
stopifnot(identical(formatted_top100_tvl$`DefiLlama rank`, 1:25))
formatted_top100_tvl_continuation <- format_top_tvl_sample_dao_table(top100_tvl_memberships, rank_min = 26L)
stopifnot(nrow(formatted_top100_tvl_continuation) == 75)
stopifnot(identical(formatted_top100_tvl_continuation$Rank, 26:100))
stopifnot(identical(formatted_top100_tvl_continuation$`DefiLlama rank`, 26:100))
stopifnot(identical(formatted_top100_tvl_continuation$Choices[[75]], "200"))
formatted_top100_tvl_latex <- format_top_tvl_sample_dao_latex_table(
  top100_tvl_memberships,
  rank_min = 26L
)
stopifnot(identical(formatted_top100_tvl_latex$`DefiLlama rank`[[75]], "100"))

gapped_top_tvl_models <- list(top_tvl_table = make_passing_top_tvl_public_table(n = 27))
gapped_top_tvl_models$top_tvl_table$protocol_rank <- gapped_top_tvl_models$top_tvl_table$protocol_rank + 10L
gapped_top_tvl_models$top_tvl_table <- gapped_top_tvl_models$top_tvl_table[
  gapped_top_tvl_models$top_tvl_table$top_tvl_sample_rank <= 25,
]
gapped_top_tvl_models$top_tvl_table <- rbind(
  gapped_top_tvl_models$top_tvl_table,
  gapped_top_tvl_models$top_tvl_table[gapped_top_tvl_models$top_tvl_table$top_tvl_sample_rank == 5, ]
)
gapped_formatted_top_tvl <- format_top_tvl_dao_table(gapped_top_tvl_models)
stopifnot(nrow(gapped_formatted_top_tvl) == 26)
stopifnot(identical(sort(unique(gapped_formatted_top_tvl$Rank)), 1:25))
stopifnot(all(diff(gapped_formatted_top_tvl$Rank) >= 0))
stopifnot(sum(gapped_formatted_top_tvl$Rank == 5L) == 2L)

category_top_tvl_models <- list(top_tvl_table = make_passing_top_tvl_public_table(extra_n = 3))
category_top_tvl_models$top_tvl_table$category[1:5] <- c(
  "Liquidity Staking",
  "Yield Aggregator",
  "Leveraged Farming",
  "Uncollateralized Lending",
  "Algo-Stables"
)
category_top_tvl_models$top_tvl_table$category[[6]] <- "DEX Aggregators"
formatted_top_tvl_latex <- format_top_tvl_dao_latex_table(category_top_tvl_models)
stopifnot(identical(formatted_top_tvl_latex$Category[1], "\\makecell[l]{Liquidity\\\\Staking}"))
stopifnot(identical(formatted_top_tvl_latex$Category[2], "\\makecell[l]{Yield\\\\Aggregator}"))
stopifnot(identical(formatted_top_tvl_latex$Category[4], "\\makecell[l]{Uncoll.\\\\Lending}"))
stopifnot(identical(formatted_top_tvl_latex$Category[5], "\\makecell[l]{Algo\\\\Stables}"))
stopifnot(identical(formatted_top_tvl_latex$Category[6], "\\makecell[l]{DEX\\\\Aggregators}"))

release_failure_top_tvl <- make_passing_top_tvl_public_table()
release_failure_top_tvl$in_analysis_release[[12]] <- FALSE
release_failure_top_tvl$used_in_top_tvl_robustness_subset[[12]] <- FALSE
expect_error_matching(
  format_top_tvl_dao_latex_table(list(top_tvl_table = release_failure_top_tvl)),
  "rank=12 protocol=Protocol 12 space=space12.eth failed=in_analysis_release"
)
status_failure_top_tvl <- make_passing_top_tvl_public_table()
status_failure_top_tvl$match_status[[3]] <- "rejected"
status_failure_top_tvl$used_in_top_tvl_robustness_subset[[3]] <- FALSE
status_failure_top_tvl$raw_snapshot_flagged[[4]] <- TRUE
status_failure_top_tvl$snapshot_status[[4]] <- "Flagged"
status_failure_top_tvl$used_in_top_tvl_robustness_subset[[4]] <- FALSE
expect_error_matching(
  format_top_tvl_dao_latex_table(list(top_tvl_table = status_failure_top_tvl)),
  "rank=3 protocol=Protocol 3 space=space3.eth failed=match_status_accepted"
)
expect_error_matching(
  format_top_tvl_dao_latex_table(list(top_tvl_table = status_failure_top_tvl)),
  "rank=4 protocol=Protocol 4 space=space4.eth failed=snapshot_status_active"
)
unlink(top_tvl_project_root, recursive = TRUE)
stopifnot(identical(
  attr(fixture_models$first_choice_sensitivity_any, "robustness_position_spec"),
  "first_choice"
))
first_choice_formula <- paste(deparse(stats::formula(
  fixture_models$first_choice_sensitivity_any
)), collapse = " ")
stopifnot(grepl("is_first_choice", first_choice_formula, fixed = TRUE))
stopifnot(!grepl("I(choice_idx^2)", first_choice_formula, fixed = TRUE))
first_choice_shared_models <- fit_first_choice_shared_bias_summary_model_set(
  fractional_fixture,
  excluded_spaces = fixture_models$excluded_spaces,
  top_tvl_spaces = fixture_models$top_tvl_spaces,
  top_tvl_sample_spaces = fixture_models$top_tvl_sample_spaces
)
shared_specs <- shared_bias_summary_specifications()
vote_count_specification <- "Vote-count share, expanded single-preference sample"
expected_shared_specifications <- c(
  "Single preference",
  vote_count_specification,
  "Author-voted only",
  "Compatible full sample",
  "FULL sample",
  "Participation control",
  "Top 25 Ethereum-TVL DAO subset",
  "Top 50 Ethereum-TVL DAO subset",
  "Top 100 Ethereum-TVL DAO subset",
  "Proposal FE",
  "Low-participation exclusion",
  "Top-space exclusion"
)
stopifnot(nrow(shared_specs) == 12)
stopifnot(identical(shared_specs$specification, expected_shared_specifications))
stopifnot(identical(shared_specs$specification_order, seq_len(12)))
stopifnot(identical(
  shared_specs$model_name,
  c(
    "single_preference_any",
    "vote_count_share_any",
    "author_voted_only_any",
    "full_sample_any",
    "all_vote_types_any",
    "participation_control_any",
    "top_tvl_dao_any",
    "top_tvl_dao_top50_any",
    "top_tvl_dao_top100_any",
    "proposal_fe_any",
    "low_participation_any",
    "top_space_excluded_any"
  )
))
stopifnot(identical(names(first_choice_shared_models), shared_specs$model_name))

# Shared specs now carry the Top 25/50/100 Ethereum-TVL DAO subsets.
stopifnot(all(c(
  "Top 25 Ethereum-TVL DAO subset",
  "Top 50 Ethereum-TVL DAO subset",
  "Top 100 Ethereum-TVL DAO subset"
) %in% shared_specs$specification))
stopifnot(all(c(
  "top_tvl_dao_any",
  "top_tvl_dao_top50_any",
  "top_tvl_dao_top100_any"
) %in% shared_specs$model_name))

# Threading guard for the ROOT bug: the shared-summary fitters must receive
# top_tvl_sample_spaces so the Top 50/Top 100 row flags are NOT silently
# collapsed onto the Top 25 alias (top_tvl_dao_row). In the fixture the Top
# 50/100 curated samples are strictly larger than the alias subset, so a
# threading regression (defaulting to the alias) would make these models share
# the alias model's nobs. Assert they remain strictly larger for BOTH the
# first-choice and rank-quadratic shared fitters.
stopifnot(stats::nobs(first_choice_shared_models$top_tvl_dao_top50_any) >
  stats::nobs(first_choice_shared_models$top_tvl_dao_any))
stopifnot(stats::nobs(first_choice_shared_models$top_tvl_dao_top100_any) >
  stats::nobs(first_choice_shared_models$top_tvl_dao_any))
rank_quadratic_shared_models <- fit_shared_bias_summary_model_set(
  fractional_fixture,
  excluded_spaces = fixture_models$excluded_spaces,
  top_tvl_spaces = fixture_models$top_tvl_spaces,
  top_tvl_sample_spaces = fixture_models$top_tvl_sample_spaces
)
stopifnot(identical(names(rank_quadratic_shared_models), shared_specs$model_name))
stopifnot(stats::nobs(rank_quadratic_shared_models$top_tvl_dao_top50_any) >
  stats::nobs(rank_quadratic_shared_models$top_tvl_dao_any))
stopifnot(stats::nobs(rank_quadratic_shared_models$top_tvl_dao_top100_any) >
  stats::nobs(rank_quadratic_shared_models$top_tvl_dao_any))

for (shared_model in first_choice_shared_models) {
  shared_formula <- paste(deparse(stats::formula(shared_model)), collapse = " ")
  shared_terms <- attr(stats::terms(stats::formula(shared_model)), "term.labels")
  stopifnot(identical(attr(shared_model, "robustness_position_spec"), "first_choice"))
  stopifnot(grepl("is_first_choice", shared_formula, fixed = TRUE))
  stopifnot(!("choice_idx" %in% shared_terms))
  stopifnot(!("I(choice_idx^2)" %in% shared_terms))
  stopifnot(!grepl("I(choice_idx^2)", shared_formula, fixed = TRUE))
}

fixture_ladders <- fit_coefficient_ladders(fractional_fixture)
stopifnot(identical(names(fixture_ladders), c(
  "single_preference_rank_quadratic",
  "first_choice_position"
)))
stopifnot(length(fixture_ladders$single_preference_rank_quadratic) == 6)
stopifnot(length(fixture_ladders$first_choice_position) == 6)
stopifnot(all(vapply(
  fixture_ladders$single_preference_rank_quadratic,
  function(model) identical(attr(model, "robustness_subset_flag"), "main_regression_row"),
  logical(1)
)))
rank_formula <- paste(deparse(stats::formula(
  fixture_ladders$single_preference_rank_quadratic[[1]]
)), collapse = " ")
first_choice_ladder_formula <- paste(deparse(stats::formula(
  fixture_ladders$first_choice_position[[1]]
)), collapse = " ")
interaction_formula <- paste(deparse(stats::formula(
  fixture_ladders$single_preference_rank_quadratic[[6]]
)), collapse = " ")
stopifnot(grepl("choice_idx", rank_formula, fixed = TRUE))
stopifnot(grepl("I(choice_idx^2)", rank_formula, fixed = TRUE))
stopifnot(grepl("author_voted", rank_formula, fixed = TRUE))
stopifnot(grepl("is_author_choice_expanded", rank_formula, fixed = TRUE))
stopifnot(!grepl("n_votes", rank_formula, fixed = TRUE))
stopifnot(!grepl("is_first_choice", rank_formula, fixed = TRUE))
stopifnot(grepl("is_first_choice", first_choice_ladder_formula, fixed = TRUE))
stopifnot(!grepl("I(choice_idx^2)", first_choice_ladder_formula, fixed = TRUE))
stopifnot(grepl(
  "is_author_choice_expanded * author_pos6_expanded",
  interaction_formula,
  fixed = TRUE
))

fixture_effect_tables <- build_effect_tables(fixture_models)
planned_author_choice_model_label <- function(model, term_label) {
  model <- unname(model)
  term_label <- unname(term_label)
  if (identical(model, "Observed share, first/non-first split")) {
    return(if (identical(term_label, "Author choice, first position")) {
      "Observed share, first/non-first split: first position"
    } else {
      "Observed share, first/non-first split: non-first position"
    })
  }
  if (identical(model, "No-author-vp share, first/non-first split")) {
    return(if (identical(term_label, "Author choice, first position")) {
      "No-author-vp share, first/non-first split: first position"
    } else {
      "No-author-vp share, first/non-first split: non-first position"
    })
  }
  if (identical(model, "Observed share, proposal fixed effects split")) {
    return(if (identical(term_label, "Author choice, first position")) {
      "Observed share, proposal fixed effects split: first position"
    } else {
      "Observed share, proposal fixed effects split: non-first position"
    })
  }
  fixed_map <- c(
    "Observed share, proposal fixed effects" = "Proposal FE",
    "Observed share, author-voted-only sample" = "Author-voted only",
    "Observed share, participation-control sensitivity" = "Participation control",
    "Observed share, compatible full sample" = "Compatible full sample",
    "Observed share, FULL all-voting-system sample" = "FULL sample",
    "Observed share, expanded single-preference sample" = "Single preference",
    "Observed share, excluding tot_votes_2 <= 1" = "Low-participation exclusion",
    "Observed share, excluding top 5 spaces" = "Top-space exclusion",
    "Observed share, Top 25 Ethereum-TVL DAO subset" = "Top 25 Ethereum-TVL DAO subset",
    "Observed share, Top 50 Ethereum-TVL DAO subset" = "Top 50 Ethereum-TVL DAO subset",
    "Observed share, Top 100 Ethereum-TVL DAO subset" = "Top 100 Ethereum-TVL DAO subset"
  )
  if (model %in% names(fixed_map)) {
    unname(fixed_map[[model]])
  } else if (startsWith(model, "Observed share, excluding top ")) {
    "Top-space exclusion"
  } else {
    model
  }
}
effect_row_keys <- function(df, relabel = FALSE) {
  keyed <- df
  if (isTRUE(relabel)) {
    keyed$model <- mapply(
      planned_author_choice_model_label,
      keyed$model,
      keyed$term_label,
      USE.NAMES = FALSE
    )
  }
  key_cols <- c(
    "model",
    "outcome",
    "fixed_effect",
    "model_nobs",
    "term_label",
    "estimate",
    "std_error",
    "p_value",
    "average_adjusted_prediction_difference_pp"
  )
  apply(keyed[, key_cols, drop = FALSE], 1, function(row) {
    row[is.na(row)] <- "<NA>"
    paste(row, collapse = "\r")
  })
}
author_choice_newdata <- author_choice_effect_newdata(
  fixture_models$single_preference_any,
  "is_author_choice_expanded"
)
stopifnot(nrow(author_choice_newdata) > 0)
stopifnot(all(author_choice_newdata$author_voted == 1))
infavor_author_choice_newdata <- author_choice_effect_newdata(
  fixture_models$matched_observed_stance_split,
  "infavor_author_choice_expanded",
  "noninfavor_author_choice_expanded"
)
stopifnot(nrow(infavor_author_choice_newdata) > 0)
stopifnot(all(infavor_author_choice_newdata$is_infavor == 1))
stopifnot(all(infavor_author_choice_newdata$noninfavor_author_choice_expanded == 0))
noninfavor_author_choice_newdata <- author_choice_effect_newdata(
  fixture_models$matched_observed_stance_split,
  "noninfavor_author_choice_expanded",
  "infavor_author_choice_expanded"
)
stopifnot(nrow(noninfavor_author_choice_newdata) > 0)
stopifnot(all(noninfavor_author_choice_newdata$is_infavor == 0))
stopifnot(all(noninfavor_author_choice_newdata$infavor_author_choice_expanded == 0))
stopifnot(nrow(fixture_effect_tables$three_bias_summary) == 3)
stopifnot(nrow(fixture_effect_tables$author_choice_shared_specs) == 12)
stopifnot(identical(
  fixture_effect_tables$author_choice_shared_specs$model,
  names(shared_bias_summary_models(fixture_models))
))
stopifnot(nrow(fixture_effect_tables$author_choice_diagnostics) == 15)
stopifnot(!any(grepl(
  "Vote-count",
  fixture_effect_tables$author_choice_diagnostics$model,
  fixed = TRUE
)))
stopifnot(any(
  fixture_effect_tables$author_choice_diagnostics$model ==
    "Observed share, proposal fixed effects"
))
stopifnot(all(c(
  "Author choice, in favor",
  "Author choice, not in favor"
) %in% fixture_effect_tables$author_choice_diagnostics$term_label))
stopifnot(all(c(
  "Specification",
  "Author choice",
  "First vs second choice",
  "Approve stance"
) %in% names(format_three_bias_summary_table(fixture_effect_tables$three_bias_summary))))
formatted_shared_specs <- format_effect_table(fixture_effect_tables$author_choice_shared_specs)
formatted_three_bias <- format_three_bias_summary_table(fixture_effect_tables$three_bias_summary)
variation_key <- format_robustness_variation_key_table(
  models = fixture_models,
  effect_tables = fixture_effect_tables
)
diagnostics_matrix <- format_author_choice_diagnostics_matrix(
  fixture_effect_tables$author_choice_diagnostics
)
stopifnot(identical(names(diagnostics_matrix), c(
  "Author's VP", "N", "Pooled", "First pos.", "Non-first", "Approve", "Non-Approve"
)))
stopifnot(nrow(diagnostics_matrix) == 2)
stopifnot(is.null(attr(diagnostics_matrix, "midrule_before_rows", exact = TRUE)))
stopifnot(!any(names(diagnostics_matrix) %in% c("FE", "Contrast", "Coef. (SE)", "p", "AME (pp)")))
stopifnot(identical(diagnostics_matrix[["Author's VP"]], c("Included", "Removed")))
stopifnot(identical(
  names(variation_key),
  c("#", "Short name", "Dimension", "Definition", "N")
))
expected_variation_key_order <- c(
  "Main",
  "Vote-count",
  "Auth-voted", "Low-part.", "Vote-control",
  "TVL-25", "TVL-50", "TVL-100",
  "Proposal FE",
  "No top-5",
  "Expanded", "Full"
)
stopifnot(identical(variation_key[["#"]], seq_len(nrow(variation_key))))
stopifnot(identical(variation_key[["Short name"]], expected_variation_key_order))
stopifnot(identical(attr(variation_key, "midrule_before_rows"), c(2L, 3L, 6L, 9L, 10L, 11L)))
stopifnot(identical(
  variation_key[["Dimension"]],
  c(
    "Baseline",
    "Outcome",
    rep("Participation", 3),
    rep("TVL", 3),
    "Fixed effects",
    "Space influence",
    rep("Proposal type", 2)
  )
))
stopifnot(all(c(
  "Main", "Auth-voted", "Expanded", "Full", "Vote-control",
  "TVL-25", "TVL-50", "TVL-100", "Proposal FE", "Low-part.",
  "No top-5", "Vote-count"
) %in% variation_key[["Short name"]]))
stopifnot(!anyDuplicated(variation_key[["Short name"]]))
stopifnot(!any(c("Matched obs.", "No-author VP") %in% variation_key[["Short name"]]))
stopifnot(variation_key[variation_key[["Short name"]] == "Main", "N"] ==
  format_int(stats::nobs(fixture_models$single_preference_any)))
stopifnot(variation_key[variation_key[["Short name"]] == "Vote-count", "N"] ==
  format_int(stats::nobs(fixture_models$vote_count_share_any)))

formatted_shared_specs_short <- format_effect_table(
  fixture_effect_tables$author_choice_shared_specs,
  use_short_names = TRUE,
  include_n = FALSE
)
stopifnot(!("N" %in% names(formatted_shared_specs_short)))
stopifnot(all(c("Main", "Vote-count", "Auth-voted", "TVL-25", "TVL-50", "TVL-100") %in%
  formatted_shared_specs_short$Specification))
stopifnot(!any(formatted_shared_specs_short$Specification == "Matched obs."))
stopifnot(!any(formatted_shared_specs_short$Specification == "No-author VP"))
stopifnot(!any(grepl(
  "Ethereum-TVL DAO subset",
  formatted_shared_specs_short$Specification,
  fixed = TRUE
)))

formatted_three_bias_short <- format_three_bias_summary_table(
  fixture_effect_tables$three_bias_summary,
  use_short_names = TRUE
)
stopifnot(all(c("Main", "Vote-count", "Auth-voted", "TVL-25", "TVL-50", "TVL-100") %in%
  formatted_three_bias_short$Specification))

formatted_first_choice_short <- format_three_bias_summary_table(
  fixture_effect_tables$first_choice_three_bias_summary,
  use_short_names = TRUE
)
stopifnot(all(c("Main", "Vote-count", "Auth-voted", "TVL-25", "TVL-50", "TVL-100") %in%
  formatted_first_choice_short$Specification))
stopifnot(identical(formatted_shared_specs$Specification, formatted_three_bias$Specification))
stopifnot(identical(formatted_shared_specs[["AME (pp)"]], formatted_three_bias[["Author choice"]]))
stopifnot(all(c(
  "Single preference",
  "Compatible full sample",
  "FULL sample",
  "Top 25 Ethereum-TVL DAO subset",
  "Top 50 Ethereum-TVL DAO subset",
  "Top 100 Ethereum-TVL DAO subset"
) %in% format_three_bias_summary_table(fixture_effect_tables$three_bias_summary)$Specification))
stopifnot(nrow(fixture_effect_tables$author_voted_only) >= 1)
stopifnot(nrow(fixture_effect_tables$vote_count_share) >= 1)
stopifnot(nrow(fixture_effect_tables$participation_control) >= 1)
stopifnot(nrow(fixture_effect_tables$first_choice_three_bias_summary) == 3)
stopifnot(identical(
  fixture_effect_tables$first_choice_three_bias_summary[["Bias contrast"]],
  c("Author choice", "First vs non-first choice", "Approve stance")
))
first_choice_summary_value_cols <- setdiff(
  names(fixture_effect_tables$first_choice_three_bias_summary),
  c("Bias contrast", "Interpretation")
)
stopifnot(identical(first_choice_summary_value_cols, shared_specs$specification))
formatted_first_choice_three_bias <- format_three_bias_summary_table(
  fixture_effect_tables$first_choice_three_bias_summary
)
stopifnot(identical(
  names(formatted_first_choice_three_bias),
  c("Specification", "Author choice", "First vs non-first choice", "Approve stance")
))
stopifnot(nrow(formatted_first_choice_three_bias) == 12)

three_bias_long_grid <- build_three_bias_summary_long_grid(
  fixture_effect_tables$three_bias_summary,
  fixture_models
)
stopifnot(nrow(three_bias_long_grid) == 36)
stopifnot(identical(
  names(three_bias_long_grid),
  c(
    "specification",
    "specification_order",
    "bias_contrast",
    "contrast_order",
    "fixed_effect",
    "model_nobs",
    "ame_pp"
  )
))
stopifnot(identical(
  unique(three_bias_long_grid$specification),
  names(shared_bias_summary_models(fixture_models))
))
stopifnot(identical(
  unique(three_bias_long_grid$bias_contrast),
  c("Author choice", "First vs second choice", "Approve stance")
))
stopifnot(!anyDuplicated(three_bias_long_grid[c("specification", "bias_contrast")]))
stopifnot(all(is.finite(three_bias_long_grid$ame_pp)))
stopifnot(all(is.finite(three_bias_long_grid$model_nobs)))

make_three_bias_forest_inference_fixture <- function(
    three_bias_summary = fixture_effect_tables$three_bias_summary,
    models = fixture_models,
    bootstrap_replicates = 199L,
    bootstrap_seed = 20260518L,
    glm_iter = 100L) {
  point_grid <- build_three_bias_summary_long_grid(three_bias_summary, models)
  fwer_alpha <- 0.05
  family_size <- nrow(point_grid)
  fwer_z <- stats::qnorm(1 - fwer_alpha / (2 * family_size))
  bootstrap_se <- seq(0.25, length.out = nrow(point_grid), by = 0.01)
  data.frame(
    point_grid,
    bootstrap_se_pp = bootstrap_se,
    fwer_ci_low_pp = point_grid$ame_pp - fwer_z * bootstrap_se,
    fwer_ci_high_pp = point_grid$ame_pp + fwer_z * bootstrap_se,
    bootstrap_replicates = bootstrap_replicates,
    cluster_unit = "space",
    bootstrap_seed = bootstrap_seed,
    glm_iter = glm_iter,
    ci_method = "bootstrap_normal_bonferroni",
    fwer_family_size = family_size,
    fwer_alpha = fwer_alpha,
    fwer_z = fwer_z,
    analysis_data_release = "current",
    input_path = "/tmp/fixture-current-release.csv",
    successful_replicates = bootstrap_replicates,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

three_bias_forest_fixture <- make_three_bias_forest_inference_fixture()
validated_three_bias_forest <- validate_three_bias_forest_inference_source(
  three_bias_forest_fixture,
  fixture_effect_tables$three_bias_summary,
  fixture_models
)
stopifnot(nrow(validated_three_bias_forest) == 36)
stopifnot(all(validated_three_bias_forest$ci_method == "bootstrap_normal_bonferroni"))
stopifnot(all(validated_three_bias_forest$fwer_family_size == 36))
stopifnot(all(validated_three_bias_forest$cluster_unit == "space"))
stopifnot(all(is.finite(validated_three_bias_forest$bootstrap_se_pp)))
stopifnot(all(validated_three_bias_forest$fwer_ci_low_pp <= validated_three_bias_forest$ame_pp))
stopifnot(all(validated_three_bias_forest$fwer_ci_high_pp >= validated_three_bias_forest$ame_pp))
stopifnot(isTRUE(all.equal(
  validated_three_bias_forest$fwer_ci_low_pp,
  validated_three_bias_forest$ame_pp -
    validated_three_bias_forest$fwer_z * validated_three_bias_forest$bootstrap_se_pp,
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
  validated_three_bias_forest$fwer_ci_high_pp,
  validated_three_bias_forest$ame_pp +
    validated_three_bias_forest$fwer_z * validated_three_bias_forest$bootstrap_se_pp,
  tolerance = 1e-10,
  check.attributes = FALSE
)))

formatted_three_bias_ci <- format_three_bias_summary_ci_table(
  fixture_effect_tables$three_bias_summary,
  inference_source = three_bias_forest_fixture,
  models = fixture_models,
  use_short_names = TRUE
)
stopifnot(identical(
  names(formatted_three_bias_ci),
  c("Specification", "Author choice", "First vs second choice", "Approve stance")
))
stopifnot(nrow(formatted_three_bias_ci) == 12)
stopifnot(any(grepl("Main", formatted_three_bias_ci$Specification, fixed = TRUE)))
stopifnot(any(grepl("\\makecell[r]", formatted_three_bias_ci[["Author choice"]], fixed = TRUE)))
stopifnot(any(grepl("\\\\{\\scriptsize [", formatted_three_bias_ci[["Author choice"]], fixed = TRUE)))
stopifnot(any(grepl(
  format_num(three_bias_forest_fixture$fwer_ci_low_pp[[1]], 2),
  formatted_three_bias_ci[["Author choice"]],
  fixed = TRUE
)))
stopifnot(any(grepl(
  format_num(three_bias_forest_fixture$fwer_ci_high_pp[[1]], 2),
  formatted_three_bias_ci[["Author choice"]],
  fixed = TRUE
)))

make_fake_incremental_forest_point_grid <- function() {
  contrasts <- three_bias_contrast_specifications()
  rows <- lapply(seq_along(expected_shared_specifications), function(spec_idx) {
    spec <- expected_shared_specifications[[spec_idx]]
    data.frame(
      specification = spec,
      specification_order = spec_idx,
      bias_contrast = contrasts$bias_contrast,
      contrast_order = contrasts$contrast_order,
      fixed_effect = if (identical(spec, "Proposal FE")) "proposal" else "space",
      model_nobs = 1000L + spec_idx * 10L + contrasts$contrast_order,
      ame_pp = seq(10, by = 0.5, length.out = nrow(contrasts)) + spec_idx,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

make_fake_incremental_old_source <- function(point_grid) {
  old_grid <- point_grid[point_grid$specification != vote_count_specification, , drop = FALSE]
  old_specifications <- unique(old_grid$specification)
  old_grid$specification_order <- match(old_grid$specification, old_specifications)
  old_grid$model_nobs <- old_grid$model_nobs - 100L
  fwer_alpha <- 0.05
  fwer_family_size <- nrow(old_grid)
  fwer_z <- stats::qnorm(1 - fwer_alpha / (2 * fwer_family_size))
  bootstrap_se <- seq(0.25, length.out = nrow(old_grid), by = 0.01)
  data.frame(
    old_grid,
    bootstrap_se_pp = bootstrap_se,
    fwer_ci_low_pp = old_grid$ame_pp - fwer_z * bootstrap_se,
    fwer_ci_high_pp = old_grid$ame_pp + fwer_z * bootstrap_se,
    bootstrap_replicates = 4L,
    cluster_unit = "space",
    bootstrap_seed = 20260518L,
    glm_iter = 100L,
    ci_method = "bootstrap_normal_bonferroni",
    fwer_family_size = fwer_family_size,
    fwer_alpha = fwer_alpha,
    fwer_z = fwer_z,
    analysis_data_release = "current",
    input_path = "/tmp/fixture-current-release.csv",
    successful_replicates = 4L,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_incremental_vote_count_forest_merge <- function() {
  point_grid <- make_fake_incremental_forest_point_grid()
  old_source <- make_fake_incremental_old_source(point_grid)
  vote_count_rows <- point_grid[point_grid$specification == vote_count_specification, , drop = FALSE]
  vote_count_draws <- matrix(
    c(
      vote_count_rows$ame_pp - 0.3,
      vote_count_rows$ame_pp - 0.1,
      vote_count_rows$ame_pp + 0.1,
      vote_count_rows$ame_pp + 0.3
    ),
    nrow = 4L,
    byrow = TRUE
  )
  colnames(vote_count_draws) <- three_bias_grid_key(vote_count_rows)

  out <- build_incremental_vote_count_forest_source(
    old_source,
    point_grid,
    vote_count_bootstrap_draws = vote_count_draws
  )
  stopifnot(nrow(out) == 36)
  stopifnot(all(out$fwer_family_size == 36))
  stopifnot(all(out$successful_replicates == 4L))
  stopifnot(all(out$fwer_z == stats::qnorm(1 - 0.05 / (2 * 36))))
  stopifnot(identical(
    out$specification,
    point_grid$specification[order(point_grid$specification_order, point_grid$contrast_order)]
  ))

  old_key <- three_bias_grid_key(old_source)
  out_old <- out[match(old_key, three_bias_grid_key(out)), ]
  current_old <- point_grid[match(old_key, three_bias_grid_key(point_grid)), ]
  stopifnot(isTRUE(all.equal(
    out_old$bootstrap_se_pp,
    old_source$bootstrap_se_pp,
    tolerance = 1e-12,
    check.attributes = FALSE
  )))
  stopifnot(identical(as.integer(out_old$specification_order), as.integer(current_old$specification_order)))
  stopifnot(identical(as.integer(out_old$model_nobs), as.integer(current_old$model_nobs)))
  stopifnot(isTRUE(all.equal(out_old$ame_pp, current_old$ame_pp, tolerance = 1e-12, check.attributes = FALSE)))

  out_vote <- out[out$specification == vote_count_specification, , drop = FALSE]
  stopifnot(nrow(out_vote) == 3)
  stopifnot(identical(out_vote$bias_contrast, vote_count_rows$bias_contrast))
  stopifnot(all(is.finite(out_vote$bootstrap_se_pp)))
  stopifnot(isTRUE(all.equal(
    out$fwer_ci_low_pp,
    out$ame_pp - out$fwer_z * out$bootstrap_se_pp,
    tolerance = 1e-10,
    check.attributes = FALSE
  )))
  stopifnot(isTRUE(all.equal(
    out$fwer_ci_high_pp,
    out$ame_pp + out$fwer_z * out$bootstrap_se_pp,
    tolerance = 1e-10,
    check.attributes = FALSE
  )))

  duplicated_source <- rbind(old_source, old_source[1, ])
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      duplicated_source,
      point_grid,
      vote_count_bootstrap_draws = vote_count_draws
    ),
    "duplicated specification/contrast"
  )

  wrong_family <- old_source
  wrong_family$fwer_family_size[[1]] <- 34L
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      wrong_family,
      point_grid,
      vote_count_bootstrap_draws = vote_count_draws
    ),
    "fwer_family_size"
  )

  missing_old <- old_source[-1, ]
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      missing_old,
      point_grid,
      vote_count_bootstrap_draws = vote_count_draws
    ),
    "33 rows"
  )

  stale_old <- old_source
  stale_old$ame_pp[[1]] <- stale_old$ame_pp[[1]] + 1
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      stale_old,
      point_grid,
      vote_count_bootstrap_draws = vote_count_draws
    ),
    "old AME"
  )

  inconsistent_metadata <- old_source
  inconsistent_metadata$bootstrap_seed[[2]] <- 42L
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      inconsistent_metadata,
      point_grid,
      vote_count_bootstrap_draws = vote_count_draws
    ),
    "bootstrap_seed"
  )

  bad_draws <- vote_count_draws
  bad_draws[[1]] <- NA_real_
  expect_error_matching(
    build_incremental_vote_count_forest_source(
      old_source,
      point_grid,
      vote_count_bootstrap_draws = bad_draws
    ),
    "non-finite"
  )
}

test_incremental_vote_count_forest_merge()

test_three_bias_bootstrap_draws_can_run_in_parallel <- function() {
  point_grid <- data.frame(
    specification = "Spec",
    bias_contrast = "Author choice",
    ame_pp = 0,
    stringsAsFactors = FALSE
  )
  model_dt <- data.table::data.table(
    space = c("alpha", "beta"),
    value = c(1, 2)
  )
  space_rows <- split(seq_len(nrow(model_dt)), model_dt$space)
  sampled_space_sets <- list(
    c("beta"),
    c("alpha", "beta"),
    c("alpha")
  )
  fake_fit <- function(model_df, frozen_sets, glm_iter) {
    data.frame(
      specification = "Spec",
      bias_contrast = "Author choice",
      ame_pp = sum(model_df$value),
      stringsAsFactors = FALSE
    )
  }

  draws <- run_three_bias_bootstrap_replicates(
    sampled_space_sets,
    point_grid,
    model_dt,
    space_rows,
    frozen_sets = list(),
    glm_iter = 1L,
    cores = min(2L, parallel::detectCores(logical = TRUE)),
    fit_grid_fn = fake_fit
  )

  stopifnot(identical(as.numeric(draws[, 1]), c(2, 3, 1)))
}

test_three_bias_bootstrap_draws_can_run_in_parallel()

three_bias_forest_plot_grid <- build_three_bias_forest_plot_grid(
  three_bias_forest_fixture,
  fixture_effect_tables$three_bias_summary,
  fixture_models
)
author_plot_rows <- three_bias_forest_plot_grid[
  three_bias_forest_plot_grid$bias_contrast == "Author choice",
]
expected_author_plot_order <- three_bias_forest_fixture[
  three_bias_forest_fixture$bias_contrast == "Author choice",
]
expected_author_plot_order <- expected_author_plot_order[
  order(-expected_author_plot_order$ame_pp, expected_author_plot_order$specification_order),
]
stopifnot(identical(author_plot_rows$specification, expected_author_plot_order$specification))

stale_three_bias_forest <- three_bias_forest_fixture
stale_three_bias_forest$ame_pp[[1]] <- stale_three_bias_forest$ame_pp[[1]] + 1
stale_error <- tryCatch(
  {
    validate_three_bias_forest_inference_source(
      stale_three_bias_forest,
      fixture_effect_tables$three_bias_summary,
      fixture_models
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(stale_error))
stopifnot(grepl("does not match the current three-bias AME", stale_error, fixed = TRUE))

bad_ci_three_bias_forest <- three_bias_forest_fixture
bad_ci_three_bias_forest$fwer_ci_high_pp[[1]] <- bad_ci_three_bias_forest$ame_pp[[1]] - 0.1
bad_ci_error <- tryCatch(
  {
    validate_three_bias_forest_inference_source(
      bad_ci_three_bias_forest,
      fixture_effect_tables$three_bias_summary,
      fixture_models
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(bad_ci_error))
stopifnot(grepl("does not contain ame_pp", bad_ci_error, fixed = TRUE))

missing_col_three_bias_forest <- three_bias_forest_fixture
missing_col_three_bias_forest$bootstrap_seed <- NULL
missing_col_error <- tryCatch(
  {
    validate_three_bias_forest_inference_source(
      missing_col_three_bias_forest,
      fixture_effect_tables$three_bias_summary,
      fixture_models
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(missing_col_error))
stopifnot(grepl("missing column(s): bootstrap_seed", missing_col_error, fixed = TRUE))

forest_plot_dir <- tempfile("three-bias-forest-plot-")
dir.create(forest_plot_dir)
on.exit(unlink(forest_plot_dir, recursive = TRUE), add = TRUE)
forest_plot_paths <- write_three_bias_forest_plot(
  three_bias_forest_fixture,
  three_bias_summary = fixture_effect_tables$three_bias_summary,
  models = fixture_models,
  figure_dir = forest_plot_dir
)
stopifnot(file.exists(forest_plot_paths[["svg"]]))
stopifnot(file.exists(forest_plot_paths[["pdf"]]))
stopifnot(file.info(forest_plot_paths[["svg"]])$size > 0)
stopifnot(file.info(forest_plot_paths[["pdf"]])$size > 0)
forest_top100_paths <- write_three_bias_forest_top100_plot(
  three_bias_forest_fixture,
  three_bias_summary = fixture_effect_tables$three_bias_summary,
  models = fixture_models,
  figure_dir = forest_plot_dir
)
stopifnot(file.exists(forest_top100_paths[["svg"]]))
stopifnot(file.exists(forest_top100_paths[["pdf"]]))
stopifnot(file.info(forest_top100_paths[["svg"]])$size > 0)
stopifnot(file.info(forest_top100_paths[["pdf"]])$size > 0)
forest_svg <- paste(readLines(forest_plot_paths[["svg"]], warn = FALSE), collapse = "\n")
stopifnot(grepl("Average marginal effect (percentage points)", forest_svg, fixed = TRUE))
expected_forest_labels <- c(
  "Main", "Vote-count", "Auth-voted", "Expanded", "Full",
  "Vote-control", "TVL-25", "TVL-50", "TVL-100", "Proposal FE",
  "Low-part.", "No top-5"
)
stopifnot(all(vapply(
  expected_forest_labels,
  function(label) grepl(label, forest_svg, fixed = TRUE),
  logical(1)
)))
stopifnot(!grepl("Single preference", forest_svg, fixed = TRUE))
stopifnot(!grepl("Vote-count share, expanded single-preference sample", forest_svg, fixed = TRUE))
stopifnot(!grepl("Author-voted only", forest_svg, fixed = TRUE))
stopifnot(!grepl("Compat full", forest_svg, fixed = TRUE))
stopifnot(!grepl("All systems", forest_svg, fixed = TRUE))
stopifnot(!grepl("Compatible full sample", forest_svg, fixed = TRUE))
stopifnot(!grepl("Full sample", forest_svg, fixed = TRUE))
stopifnot(!grepl("FULL sample", forest_svg, fixed = TRUE))
stopifnot(!grepl("Top 25 Ethereum-TVL DAO subset", forest_svg, fixed = TRUE))
stopifnot(!grepl("Top 50 Ethereum-TVL DAO subset", forest_svg, fixed = TRUE))
stopifnot(!grepl("Top 100 Ethereum-TVL DAO subset", forest_svg, fixed = TRUE))
stopifnot(!grepl("Low-participation exclusion", forest_svg, fixed = TRUE))
stopifnot(!grepl("Top-space exclusion", forest_svg, fixed = TRUE))
stopifnot(!grepl("Intervals are Bonferroni FWER-adjusted", forest_svg, fixed = TRUE))
forest_top100_svg <- paste(readLines(forest_top100_paths[["svg"]], warn = FALSE), collapse = "\n")
stopifnot(grepl("Average marginal effect (percentage points)", forest_top100_svg, fixed = TRUE))
stopifnot(grepl("Expanded", forest_top100_svg, fixed = TRUE))
stopifnot(grepl("Full", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("Compat full", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("All systems", forest_top100_svg, fixed = TRUE))
stopifnot(grepl("TVL-100", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("TVL-25", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("TVL-50", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("Top 25 Ethereum-TVL DAO subset", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("Top 50 Ethereum-TVL DAO subset", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("Top 100 Ethereum-TVL DAO subset", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("Intervals are Bonferroni FWER-adjusted", forest_top100_svg, fixed = TRUE))
stopifnot(!grepl("TVL-25 and TVL-50 rows omitted", forest_top100_svg, fixed = TRUE))
forest_include <- three_bias_forest_latex_include()
stopifnot(grepl("figures/2026/robustness/figure_three_bias_forest.pdf", forest_include, fixed = TRUE))
stopifnot(grepl("Bonferroni FWER-adjusted across the 36 displayed AMEs", forest_include, fixed = TRUE))
stopifnot(grepl("\\label{fig:three-bias-forest}", forest_include, fixed = TRUE))
forest_top100_include <- three_bias_forest_top100_latex_include()
stopifnot(grepl("figures/2026/robustness/figure_three_bias_forest_top100.pdf", forest_top100_include, fixed = TRUE))
stopifnot(grepl("Bonferroni FWER-adjusted across the 36 shared AMEs", forest_top100_include, fixed = TRUE))
stopifnot(grepl("TVL-25 and TVL-50 rows omitted", forest_top100_include, fixed = TRUE))
stopifnot(grepl("\\label{fig:three-bias-forest}", forest_top100_include, fixed = TRUE))
write_primary_inference_fixture <- function(path, ames = NULL, p_values = c(0.010, 0.040, 0.200)) {
  single_preference <- fixture_effect_tables$three_bias_summary[
    ,
    c("Bias contrast", "Single preference"),
    drop = FALSE
  ]
  if (is.null(ames)) {
    ames <- single_preference[["Single preference"]]
  }
  utils::write.csv(
    data.frame(
      contrast = c("Author choice", "First vs second", "Approve stance"),
      three_bias_contrast = single_preference[["Bias contrast"]],
      ame_pp = ames,
      ordinary_ci_low_pp = c(64.18, 3.46, 4.99),
      ordinary_ci_high_pp = c(68.83, 5.69, 8.34),
      bootstrap_p = p_values,
      bonferroni_ci_low_pp = c(63.67, 3.10, 4.67),
      bonferroni_ci_high_pp = c(69.41, 5.78, 8.73),
      bootstrap_replicates = 199,
      cluster_unit = "space",
      bootstrap_seed = 20260518,
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
}

stance_audit <- build_stance_audit(model_df, documented_stance_proposals = "p1")
stopifnot(stance_audit[stance_audit$proposal == "p1", "stance_state"] == "both")
stopifnot(stance_audit[stance_audit$proposal == "p2", "stance_state"] == "neither")
stopifnot(stance_audit[stance_audit$proposal == "p3", "undocumented_one_sided"] == TRUE)

explicit_stance_exception <- data.frame(
  proposal = "p3",
  allowed_state = "approve_only",
  stringsAsFactors = FALSE
)
documented_stance_audit <- build_stance_audit(model_df, explicit_stance_exception)
documented_p3 <- documented_stance_audit[documented_stance_audit$proposal == "p3", ]
stopifnot(isTRUE(documented_p3$is_one_sided_approve_reject))
stopifnot(isTRUE(documented_p3$is_one_sided_documented_exception))
stopifnot(identical(documented_p3$one_sided_action, "kept_documented"))
stopifnot(!isTRUE(documented_p3$undocumented_one_sided))

normalized_df <- normalize_undocumented_one_sided_stance(
  model_df,
  documented_stance_proposals = "p1"
)
normalized_audit <- build_stance_audit(normalized_df, documented_stance_proposals = "p1")
normalized_summary <- build_stance_summary(
  normalized_audit,
  normalized_df,
  pre_normalization_stance_audit = stance_audit
)
before_approve_only <- normalized_summary[
  normalized_summary$when == "Before recoding" &
    normalized_summary$proposal_pattern == "Approve, no Reject",
]
after_neither <- normalized_summary[
  normalized_summary$when == "After recoding" &
    normalized_summary$proposal_pattern == "No Approve or Reject labels",
]
stopifnot(normalized_audit[normalized_audit$proposal == "p3", "stance_state"] == "neither")
stopifnot(normalized_df[normalized_df$proposal == "p3", "stance"] == "OTHER")
stopifnot(normalized_df[normalized_df$proposal == "p3", "is_infavor"] == 0)
stopifnot(before_approve_only$proposals == 1)
stopifnot(before_approve_only$rows == 2)
stopifnot(before_approve_only$spaces == 1)
stopifnot(after_neither$proposals == 5)
stopifnot(after_neither$rows == 10)
stopifnot(after_neither$spaces == 5)


attrition <- build_attrition_table(model_df, stance_audit)
stopifnot(attrition[attrition$sample == "Active choice rows", "rows"] == 14)
stopifnot(attrition[attrition$sample == "All-voting-system FULL regression sample", "rows"] == 14)
stopifnot(attrition[attrition$sample == "Compatible full-sample regression sample", "rows"] == 10)
stopifnot(attrition[attrition$sample == "Main expanded single-preference regression sample", "rows"] == 6)
stopifnot(attrition[attrition$sample == "Author-voted-only regression sample", "rows"] == 4)
stopifnot(attrition[attrition$sample == "Vote-count-share outcome sample", "rows"] == 6)
stopifnot(attrition[attrition$sample == "Participation-control sensitivity sample", "rows"] == 6)
stopifnot(attrition[attrition$sample == "No-author-vp sensitivity sample", "rows"] == 4)
full_sample_note <- attrition[attrition$sample == "Compatible full-sample regression sample", "note"]
full_regression_note <- attrition[attrition$sample == "All-voting-system FULL regression sample", "note"]
main_sample_note <- attrition[attrition$sample == "Main expanded single-preference regression sample", "note"]
stopifnot(grepl("ranked-choice", full_sample_note))
stopifnot(grepl("weighted and quadratic rows are excluded", full_sample_note))
stopifnot(grepl("includes weighted and quadratic", full_regression_note))
stopifnot(grepl("author_voted", main_sample_note, fixed = TRUE))

fake_model <- function(rows, removed_rows = integer()) {
  model <- list(obs_selection = if (length(removed_rows) > 0) list(-removed_rows) else list())
  attr(model, "robustness_data") <- rows
  model
}

estimated_attrition <- build_attrition_table(
  model_df,
  stance_audit,
  models = list(
    full_sample_any = fake_model(model_df[model_df$full_sample_regression_row %in% TRUE, ], 1L),
    single_preference_any = fake_model(model_df[model_df$main_regression_row %in% TRUE, ], 1L),
    matched_observed_any = fake_model(model_df[model_df$no_author_vp_row %in% TRUE, ]),
    low_participation_any = fake_model(model_df[model_df$low_participation_row %in% TRUE, ])
  )
)
stopifnot(estimated_attrition[estimated_attrition$sample == "Compatible full-sample regression sample", "rows"] == 9)
stopifnot(estimated_attrition[estimated_attrition$sample == "Main expanded single-preference regression sample", "rows"] == 5)
stopifnot(grepl("fixed-effect estimator exclusions", estimated_attrition[estimated_attrition$sample == "Main expanded single-preference regression sample", "note"]))

tex <- latex_table(
  data.frame(sample = "Active choice rows", rows = 6, stringsAsFactors = FALSE),
  caption = "Toy table",
  label = "tab:toy"
)
stopifnot(any(grepl("\\\\begin\\{tabular\\}", tex)))
stopifnot(any(grepl("\\\\caption\\{Toy table\\}", tex)))

wide_tex <- latex_table(
  data.frame(
    a = "row",
    b = "row",
    c = "row",
    d = "row",
    e = "row",
    f = "row",
    g = "row",
    stringsAsFactors = FALSE
  ),
  caption = "Average marginal effect test",
  label = "tab:wide"
)
stopifnot(any(grepl("\\\\begin\\{tabularx\\}", wide_tex)))
stopifnot(any(grepl("\\\\toprule", wide_tex)))
stopifnot(!any(grepl("ATE", wide_tex, fixed = TRUE)))

formatted_first_choice_grid <- format_three_bias_summary_table(
  fixture_effect_tables$first_choice_three_bias_summary
)
stopifnot(identical(
  names(formatted_first_choice_grid),
  c("Specification", "Author choice", "First vs non-first choice", "Approve stance")
))
formatted_first_choice_tex <- latex_table(
  formatted_first_choice_grid,
  caption = "First-choice three-bias summary test",
  label = "tab:first-choice-three-bias-test",
  align = three_bias_summary_align(),
  header_override = first_choice_three_bias_summary_header()
)
stopifnot(any(grepl("\\makecell[r]{First vs\\\\non-first}", formatted_first_choice_tex, fixed = TRUE)))
stopifnot(!any(grepl("Coef. (SE)", formatted_first_choice_tex, fixed = TRUE)))
stopifnot(!any(grepl("ATE", formatted_first_choice_tex, fixed = TRUE)))

formatted_three_bias <- format_three_bias_summary_table(fixture_effect_tables$three_bias_summary)
stopifnot(identical(
  names(formatted_three_bias),
  c("Specification", "Author choice", "First vs second choice", "Approve stance")
))
stopifnot(nrow(formatted_three_bias) == 12)
stopifnot("Vote-count share, expanded single-preference sample" %in% formatted_three_bias$Specification)
stopifnot("Author-voted only" %in% formatted_three_bias$Specification)
stopifnot("Compatible full sample" %in% formatted_three_bias$Specification)
stopifnot("FULL sample" %in% formatted_three_bias$Specification)
stopifnot("Participation control" %in% formatted_three_bias$Specification)
stopifnot("Top 25 Ethereum-TVL DAO subset" %in% formatted_three_bias$Specification)
stopifnot("Top 50 Ethereum-TVL DAO subset" %in% formatted_three_bias$Specification)
stopifnot("Top 100 Ethereum-TVL DAO subset" %in% formatted_three_bias$Specification)
three_bias_tex <- latex_table(
  formatted_three_bias,
  caption = "Three-bias summary test",
  label = "tab:three-bias-test",
  align = three_bias_summary_align(),
  header_override = three_bias_summary_header()
)
stopifnot(any(grepl("\\makecell[r]{Author\\\\choice}", three_bias_tex, fixed = TRUE)))
stopifnot(any(grepl("\\makecell[r]{Approve\\\\stance}", three_bias_tex, fixed = TRUE)))
stopifnot(any(grepl("Single preference", three_bias_tex, fixed = TRUE)))
stopifnot(any(grepl("\\begin{tabularx}{\\linewidth}", three_bias_tex, fixed = TRUE)))
stopifnot(any(grepl("\\makecell[r]{First vs\\\\second}", three_bias_tex, fixed = TRUE)))
stopifnot(!any(grepl("ATE", three_bias_tex, fixed = TRUE)))

primary_inference_source <- tempfile("primary-ame-bootstrap-", fileext = ".csv")
write_primary_inference_fixture(primary_inference_source)
derived_primary_inference <- primary_ame_inference_table(
  fixture_effect_tables$three_bias_summary,
  inference_path = primary_inference_source
)
formatted_primary_inference <- format_primary_ame_inference_table(derived_primary_inference)
stopifnot(identical(
  names(formatted_primary_inference),
  c(
    "Contrast",
    "AME (pp)",
    "Ordinary 95% bootstrap CI",
    "Bootstrap p",
    "Holm/Bonferroni p",
    "BH/FDR q",
    "Bonferroni 95% CI"
  )
))
stopifnot(nrow(formatted_primary_inference) == 3)
stopifnot(identical(
  formatted_primary_inference[formatted_primary_inference$Contrast == "Author choice", "AME (pp)"],
  format_num(
    fixture_effect_tables$three_bias_summary[
      fixture_effect_tables$three_bias_summary[["Bias contrast"]] == "Author choice",
      "Single preference"
    ],
    2
  )
))
primary_inference_tex <- latex_table(
  formatted_primary_inference,
  caption = "Primary AME inference test",
  label = "tab:primary-ame-inference-test",
  align = primary_ame_inference_align(),
  header_override = primary_ame_inference_header()
)
stopifnot(any(grepl("\\makecell[l]{Ordinary 95\\%\\\\bootstrap CI}", primary_inference_tex, fixed = TRUE)))
stopifnot(any(grepl("\\makecell[r]{Holm/\\\\Bonferroni p}", primary_inference_tex, fixed = TRUE)))
stopifnot(any(grepl("\\begin{tabularx}{\\linewidth}", primary_inference_tex, fixed = TRUE)))
stopifnot(any(grepl("[4.99, 8.34]", primary_inference_tex, fixed = TRUE)))
stopifnot(!any(grepl("ATE", primary_inference_tex, fixed = TRUE)))
stopifnot(isTRUE(all.equal(
  derived_primary_inference[["AME (pp)"]],
  fixture_effect_tables$three_bias_summary[["Single preference"]],
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
  derived_primary_inference[["Holm/Bonferroni p"]],
  stats::p.adjust(derived_primary_inference[["Bootstrap p"]], method = "holm"),
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
  derived_primary_inference[["BH/FDR q"]],
  stats::p.adjust(derived_primary_inference[["Bootstrap p"]], method = "BH"),
  tolerance = 1e-10,
  check.attributes = FALSE
)))
stopifnot(identical(attr(derived_primary_inference, "bootstrap_seed", exact = TRUE), 20260518L))

stale_primary_inference_source <- tempfile("stale-primary-ame-bootstrap-", fileext = ".csv")
write_primary_inference_fixture(
  stale_primary_inference_source,
  ames = fixture_effect_tables$three_bias_summary[["Single preference"]] + c(0.25, 0, 0)
)
stale_inference_error <- tryCatch(
  {
    primary_ame_inference_table(
      fixture_effect_tables$three_bias_summary,
      inference_path = stale_primary_inference_source
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(stale_inference_error))
stopifnot(grepl("does not match the current expanded single-preference AME", stale_inference_error, fixed = TRUE))

subset_primary_inference_source <- tempfile("subset-primary-ame-bootstrap-", fileext = ".csv")
write_primary_inference_fixture(subset_primary_inference_source)
subset_primary_inference <- utils::read.csv(subset_primary_inference_source, stringsAsFactors = FALSE)
utils::write.csv(
  subset_primary_inference[subset_primary_inference$three_bias_contrast != "Approve stance", ],
  subset_primary_inference_source,
  row.names = FALSE
)
subset_inference_error <- tryCatch(
  {
    primary_ame_inference_table(
      fixture_effect_tables$three_bias_summary,
      inference_path = subset_primary_inference_source
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(subset_inference_error))
stopifnot(grepl("exactly the focal three contrasts", subset_inference_error, fixed = TRUE))

wrong_cluster_inference_source <- tempfile("wrong-cluster-primary-ame-bootstrap-", fileext = ".csv")
write_primary_inference_fixture(wrong_cluster_inference_source)
wrong_cluster_inference <- utils::read.csv(wrong_cluster_inference_source, stringsAsFactors = FALSE)
wrong_cluster_inference$cluster_unit <- "proposal"
utils::write.csv(wrong_cluster_inference, wrong_cluster_inference_source, row.names = FALSE)
wrong_cluster_error <- tryCatch(
  {
    primary_ame_inference_table(
      fixture_effect_tables$three_bias_summary,
      inference_path = wrong_cluster_inference_source
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(wrong_cluster_error))
stopifnot(grepl("cluster_unit = space", wrong_cluster_error, fixed = TRUE))

missing_seed_inference_source <- tempfile("missing-seed-primary-ame-bootstrap-", fileext = ".csv")
write_primary_inference_fixture(missing_seed_inference_source)
missing_seed_inference <- utils::read.csv(missing_seed_inference_source, stringsAsFactors = FALSE)
missing_seed_inference$bootstrap_seed <- NULL
utils::write.csv(missing_seed_inference, missing_seed_inference_source, row.names = FALSE)
missing_seed_error <- tryCatch(
  {
    primary_ame_inference_table(
      fixture_effect_tables$three_bias_summary,
      inference_path = missing_seed_inference_source
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(!is.null(missing_seed_error))
stopifnot(grepl("missing column(s): bootstrap_seed", missing_seed_error, fixed = TRUE))

ladder_dir <- tempfile("robustness-ladder-")
dir.create(ladder_dir)
on.exit(unlink(ladder_dir, recursive = TRUE), add = TRUE)
ladder_path <- file.path(ladder_dir, "table_ladder.tex")
write_modelsummary_regression_table(
  fixture_ladders$single_preference_rank_quadratic,
  ladder_path,
  "Coefficient ladder test",
  "tab:coefficient-ladder-test",
  notes = "Width-safe regression table test."
)
ladder_tex <- readLines(ladder_path, warn = FALSE)
stopifnot(any(grepl("\\\\begin\\{tabularx\\}\\{\\\\linewidth\\}", ladder_tex)))
stopifnot(any(grepl("\\\\RaggedRight\\\\arraybackslash", ladder_tex)))
stopifnot(any(grepl("Author choice x first-six visibility", ladder_tex, fixed = TRUE)))
stopifnot(!any(grepl("\\\\num\\{", ladder_tex)))
stopifnot(!any(grepl("ATE", ladder_tex, fixed = TRUE)))

synthetic_ladder_df <- local({
  set.seed(20260525)
  n <- 300
  df <- data.frame(
    type = sample(c("single-choice", "approval"), n, replace = TRUE),
    choice_idx = sample(1:5, n, replace = TRUE),
    author_voted = stats::rbinom(n, 1, 0.55),
    is_author_choice_expanded = stats::rbinom(n, 1, 0.35),
    is_infavor = stats::rbinom(n, 1, 0.45),
    created_rel = stats::runif(n, -1, 1),
    space = sample(paste0("s", seq_len(20)), n, replace = TRUE)
  )
  eta <- -0.1 +
    0.2 * df$author_voted +
    0.5 * df$is_author_choice_expanded +
    0.4 * df$is_infavor -
    0.1 * df$choice_idx +
    0.05 * df$created_rel
  df$choice_share <- stats::plogis(eta)
  df
})
synthetic_ladder <- fixest::feglm(
  choice_share ~ type + choice_idx + I(choice_idx^2) + author_voted +
    is_author_choice_expanded + is_infavor + created_rel,
  data = synthetic_ladder_df,
  family = quasibinomial("logit")
)
synthetic_ladder_path <- file.path(ladder_dir, "synthetic_ladder.tex")
write_modelsummary_regression_table(
  list(synthetic_ladder),
  synthetic_ladder_path,
  "Synthetic coefficient ladder order test",
  "tab:synthetic-coefficient-ladder-order-test",
  compact_model_names = TRUE,
  section_break_after_terms = "Choice rank squared",
  section_break_before_terms = "N"
)
synthetic_ladder_tex <- readLines(synthetic_ladder_path, warn = FALSE)
synthetic_midrule_idx <- grep("^\\\\midrule$", synthetic_ladder_tex)
synthetic_hline_idx <- grep("^\\\\hline$", synthetic_ladder_tex)
stopifnot(length(synthetic_midrule_idx) == 1)
stopifnot(length(synthetic_hline_idx) >= 1)
synthetic_focal_block <- synthetic_ladder_tex[
  (synthetic_midrule_idx + 1L):(synthetic_hline_idx[[1]] - 1L)
]
synthetic_focal_terms <- synthetic_focal_block[!startsWith(synthetic_focal_block, "& ")]
synthetic_focal_terms <- sub(" &.*$", "", synthetic_focal_terms)
stopifnot(identical(
  synthetic_focal_terms,
  c("Author choice", "Approving choice", "Choice rank", "Choice rank squared")
))
synthetic_author_voted_idx <- grep("^Author voted &", synthetic_ladder_tex)
stopifnot(length(synthetic_author_voted_idx) == 1)
stopifnot(synthetic_author_voted_idx[[1]] == synthetic_hline_idx[[1]] + 1L)

escape_test_dir <- tempfile("robustness-latex-escape-")
dir.create(escape_test_dir)
on.exit(unlink(escape_test_dir, recursive = TRUE), add = TRUE)
escape_table_path <- file.path(escape_test_dir, "table_escape.tex")
write_latex_table(
  data.frame(
    Specification = "Observed share, excluding tot_votes_2 <= 1",
    p = "<0.001",
    check.names = FALSE,
    stringsAsFactors = FALSE
  ),
  path = escape_table_path,
  caption = "Escape regression test",
  label = "tab:escape-regression"
)
escape_tex <- readLines(escape_table_path, warn = FALSE)
stopifnot(any(grepl("\\\\textless\\{\\}0\\.001", escape_tex)))
stopifnot(!any(grepl("\\\\\\\\textless\\{\\}", escape_tex)))

space_exclusion <- format_space_exclusion_table(list(excluded_spaces = c("s1", "s2")))
stopifnot(identical(names(space_exclusion), c("Rank", "Space")))
stopifnot(identical(space_exclusion$Rank, 1:2))
stopifnot(identical(space_exclusion$Space, c("s1", "s2")))

cat("robustness_bias_mechanisms helper tests passed\n")
