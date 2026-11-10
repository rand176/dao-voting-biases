################################################################################
## Model fitting and average marginal effects
################################################################################

fit_fractional_logit <- function(model_df, outcome, subset_flag,
                                 author_terms = c("any", "split", "stance_split"),
                                 fixed_effect = c("space", "proposal"),
                                 label,
                                 position_spec = c("rank_quadratic", "first_choice"),
                                 include_participation_control = FALSE,
                                 glm_iter = as.integer(Sys.getenv("ROBUSTNESS_GLM_ITER", unset = "100"))) {
  require_packages(c("data.table", "fixest"))
  author_terms <- match.arg(author_terms)
  fixed_effect <- match.arg(fixed_effect)
  position_spec <- match.arg(position_spec)
  if (is.na(glm_iter) || glm_iter <= 0) {
    stop("glm_iter must be a positive integer for model: ", label, call. = FALSE)
  }

  dt <- data.table::copy(data.table::as.data.table(model_df)[get(subset_flag) %in% TRUE])
  if (nrow(dt) == 0) stop("No rows available for model: ", label, call. = FALSE)
  dt[, model_outcome := as.numeric(get(outcome))]
  dt <- dt[!is.na(model_outcome)]

  author_rhs <- if (author_terms == "any") {
    "is_author_choice_expanded"
  } else if (author_terms == "split") {
    "first_author_choice_expanded + nonfirst_author_choice_expanded"
  } else {
    "infavor_author_choice_expanded + noninfavor_author_choice_expanded"
  }
  position_rhs <- if (position_spec == "rank_quadratic") {
    c("choice_idx", "I(choice_idx^2)")
  } else {
    "is_first_choice"
  }

  if (fixed_effect == "space") {
    rhs_terms <- c(
      "author_voted",
      author_rhs,
      "is_infavor",
      position_rhs,
      "created_rel",
      "N_choices",
      "author_pos6_expanded",
      "choice_text_len_rel",
      "type"
    )
    if (isTRUE(include_participation_control)) {
      rhs_terms <- c(rhs_terms, "log_tot_votes_2")
    }
  } else {
    rhs_terms <- c(
      author_rhs,
      "is_infavor",
      position_rhs,
      "choice_text_len_rel",
      "type"
    )
  }
  rhs <- paste(rhs_terms, collapse = " + ")

  formula <- stats::as.formula(paste0("model_outcome ~ ", rhs, " | ", fixed_effect))
  model <- fixest::feglm(
    formula,
    data = dt,
    family = stats::quasibinomial("logit"),
    glm.iter = glm_iter
  )
  attr(model, "robustness_data") <- dt
  attr(model, "robustness_label") <- label
  attr(model, "robustness_outcome") <- outcome
  attr(model, "robustness_subset_flag") <- subset_flag
  attr(model, "robustness_author_terms") <- author_terms
  attr(model, "robustness_fixed_effect") <- fixed_effect
  attr(model, "robustness_position_spec") <- position_spec
  attr(model, "robustness_include_participation_control") <- isTRUE(include_participation_control)
  model
}

top_spaces <- function(model_df, n = 5) {
  require_packages("data.table")
  dt <- data.table::as.data.table(model_df)
  counts <- dt[main_regression_row %in% TRUE, .N, by = space][order(-N, space)]
  counts$space[seq_len(min(n, nrow(counts)))]
}


shared_specification_sets <- function(model_df,
                                      diagnostics_path = default_top_tvl_match_diagnostics_path(),
                                      sample_memberships_path = NULL) {
  top_n <- as.integer(Sys.getenv("ROBUSTNESS_TOP_SPACES_N", unset = "5"))
  top_tvl_n <- as.integer(Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_N", unset = "25"))
  top_tvl_table_n <- as.integer(Sys.getenv("ROBUSTNESS_TOP_TVL_TABLE_N", unset = "33"))
  # Resolve the curated memberships CSV independently of diagnostics writing.
  # Callers such as the bootstrap pass diagnostics_path = NA to suppress
  # diagnostics output, but the source-of-truth memberships CSV must still be
  # read; fall back to the canonical diagnostics location to locate it.
  if (is.null(sample_memberships_path)) {
    memberships_diagnostics_path <- diagnostics_path
    if (length(memberships_diagnostics_path) == 0 ||
        is.na(memberships_diagnostics_path) ||
        !nzchar(memberships_diagnostics_path)) {
      memberships_diagnostics_path <- default_top_tvl_match_diagnostics_path()
    }
    sample_memberships_path <- default_top_tvl_sample_memberships_path(
      diagnostics_path = memberships_diagnostics_path
    )
  }
  top_tvl <- top_tvl_dao_spaces(
    model_df,
    top_tvl_n,
    display_n = top_tvl_table_n,
    diagnostics_path = diagnostics_path,
    sample_memberships_path = sample_memberships_path
  )
  list(
    excluded_spaces = top_spaces(model_df, top_n),
    top_tvl_spaces = top_tvl$spaces,
    top_tvl_sample_spaces = top_tvl$sample_spaces,
    top_tvl_table = top_tvl$table
  )
}

apply_shared_specification_sets <- function(model_df, excluded_spaces, top_tvl_spaces,
                                            top_tvl_sample_spaces = NULL) {
  require_packages("data.table")
  dt <- data.table::copy(data.table::as.data.table(model_df))
  normalized_space <- normalize_top_tvl_space(dt$space)
  normalized_top_tvl_spaces <- normalize_top_tvl_space(top_tvl_spaces)
  if (is.null(top_tvl_sample_spaces)) {
    top_tvl_sample_spaces <- list(
      top25 = top_tvl_spaces,
      top50 = top_tvl_spaces,
      top100 = top_tvl_spaces
    )
  }

  sample_top25 <- normalize_top_tvl_space(top_tvl_sample_spaces$top25)
  sample_top50 <- normalize_top_tvl_space(top_tvl_sample_spaces$top50)
  sample_top100 <- normalize_top_tvl_space(top_tvl_sample_spaces$top100)

  dt[, top_space_excluded_row := main_regression_row & !(space %in% excluded_spaces)]
  dt[, top_tvl_dao_top25_row := main_regression_row & normalized_space %in% sample_top25]
  dt[, top_tvl_dao_top50_row := main_regression_row & normalized_space %in% sample_top50]
  dt[, top_tvl_dao_top100_row := main_regression_row & normalized_space %in% sample_top100]
  dt[, top_tvl_dao_row := main_regression_row & normalized_space %in% normalized_top_tvl_spaces]
  dt[]
}

fit_shared_bias_summary_model_set <- function(model_df, excluded_spaces = NULL,
                                              top_tvl_spaces = NULL,
                                              top_tvl_sample_spaces = NULL,
                                              glm_iter = as.integer(Sys.getenv("ROBUSTNESS_GLM_ITER", unset = "100"))) {
  if (is.null(excluded_spaces) || is.null(top_tvl_spaces) || is.null(top_tvl_sample_spaces)) {
    sets <- shared_specification_sets(model_df)
    if (is.null(excluded_spaces)) excluded_spaces <- sets$excluded_spaces
    if (is.null(top_tvl_spaces)) top_tvl_spaces <- sets$top_tvl_spaces
    if (is.null(top_tvl_sample_spaces)) top_tvl_sample_spaces <- sets$top_tvl_sample_spaces
  }
  model_df <- apply_shared_specification_sets(
    model_df, excluded_spaces, top_tvl_spaces,
    top_tvl_sample_spaces = top_tvl_sample_spaces
  )
  models <- list(
    full_sample_any = fit_fractional_logit(
      model_df, "choice_share", "full_sample_regression_row", "any", "space",
      "Observed share, compatible full sample",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    all_vote_types_any = fit_fractional_logit(
      model_df, "choice_share", "all_vote_types_regression_row", "any", "space",
      "Observed share, FULL all-voting-system sample",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    single_preference_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "space",
      "Observed share, expanded single-preference sample",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    vote_count_share_any = fit_fractional_logit(
      model_df, "vote_count_share", "vote_count_outcome_row", "any", "space",
      "Vote-count share, expanded single-preference sample",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    author_voted_only_any = fit_fractional_logit(
      model_df, "choice_share", "author_voted_regression_row", "any", "space",
      "Observed share, author-voted-only sample",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    participation_control_any = fit_fractional_logit(
      model_df, "choice_share", "participation_control_row", "any", "space",
      "Observed share, participation-control sensitivity",
      position_spec = "rank_quadratic",
      include_participation_control = TRUE,
      glm_iter = glm_iter
    ),
    top_tvl_dao_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_row", "any", "space",
      "Observed share, Top 25 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    top_tvl_dao_top50_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top50_row", "any", "space",
      "Observed share, Top 50 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    top_tvl_dao_top100_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top100_row", "any", "space",
      "Observed share, Top 100 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    proposal_fe_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "proposal",
      "Observed share, proposal fixed effects",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    low_participation_any = fit_fractional_logit(
      model_df, "choice_share", "low_participation_row", "any", "space",
      "Observed share, excluding tot_votes_2 <= 1",
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    ),
    top_space_excluded_any = fit_fractional_logit(
      model_df, "choice_share", "top_space_excluded_row", "any", "space",
      paste0("Observed share, excluding top ", length(excluded_spaces), " spaces"),
      position_spec = "rank_quadratic",
      glm_iter = glm_iter
    )
  )
  models[shared_bias_summary_specifications()$model_name]
}

fit_first_choice_shared_bias_summary_model_set <- function(model_df, excluded_spaces = NULL,
                                                           top_tvl_spaces = NULL,
                                                           top_tvl_sample_spaces = NULL,
                                                           glm_iter = as.integer(Sys.getenv("ROBUSTNESS_GLM_ITER", unset = "100"))) {
  if (is.null(excluded_spaces) || is.null(top_tvl_spaces) || is.null(top_tvl_sample_spaces)) {
    sets <- shared_specification_sets(model_df)
    if (is.null(excluded_spaces)) excluded_spaces <- sets$excluded_spaces
    if (is.null(top_tvl_spaces)) top_tvl_spaces <- sets$top_tvl_spaces
    if (is.null(top_tvl_sample_spaces)) top_tvl_sample_spaces <- sets$top_tvl_sample_spaces
  }
  model_df <- apply_shared_specification_sets(
    model_df, excluded_spaces, top_tvl_spaces,
    top_tvl_sample_spaces = top_tvl_sample_spaces
  )
  models <- list(
    full_sample_any = fit_fractional_logit(
      model_df, "choice_share", "full_sample_regression_row", "any", "space",
      "Observed share, compatible full sample",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    all_vote_types_any = fit_fractional_logit(
      model_df, "choice_share", "all_vote_types_regression_row", "any", "space",
      "Observed share, FULL all-voting-system sample",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    single_preference_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "space",
      "Observed share, expanded single-preference sample",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    vote_count_share_any = fit_fractional_logit(
      model_df, "vote_count_share", "vote_count_outcome_row", "any", "space",
      "Vote-count share, expanded single-preference sample",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    author_voted_only_any = fit_fractional_logit(
      model_df, "choice_share", "author_voted_regression_row", "any", "space",
      "Observed share, author-voted-only sample",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    participation_control_any = fit_fractional_logit(
      model_df, "choice_share", "participation_control_row", "any", "space",
      "Observed share, participation-control sensitivity",
      position_spec = "first_choice",
      include_participation_control = TRUE,
      glm_iter = glm_iter
    ),
    top_tvl_dao_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_row", "any", "space",
      "Observed share, Top 25 Ethereum-TVL DAO subset",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    top_tvl_dao_top50_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top50_row", "any", "space",
      "Observed share, Top 50 Ethereum-TVL DAO subset",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    top_tvl_dao_top100_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top100_row", "any", "space",
      "Observed share, Top 100 Ethereum-TVL DAO subset",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    proposal_fe_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "proposal",
      "Observed share, proposal fixed effects",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    low_participation_any = fit_fractional_logit(
      model_df, "choice_share", "low_participation_row", "any", "space",
      "Observed share, excluding tot_votes_2 <= 1",
      position_spec = "first_choice",
      glm_iter = glm_iter
    ),
    top_space_excluded_any = fit_fractional_logit(
      model_df, "choice_share", "top_space_excluded_row", "any", "space",
      paste0("Observed share, excluding top ", length(excluded_spaces), " spaces"),
      position_spec = "first_choice",
      glm_iter = glm_iter
    )
  )
  models[shared_bias_summary_specifications()$model_name]
}

fit_model_set <- function(model_df) {
  sets <- shared_specification_sets(model_df)
  excluded_spaces <- sets$excluded_spaces
  model_df <- apply_shared_specification_sets(
    model_df,
    excluded_spaces = sets$excluded_spaces,
    top_tvl_spaces = sets$top_tvl_spaces,
    top_tvl_sample_spaces = sets$top_tvl_sample_spaces
  )

  list(
    full_sample_any = fit_fractional_logit(
      model_df, "choice_share", "full_sample_regression_row", "any", "space",
      "Observed share, compatible full sample",
      position_spec = "rank_quadratic"
    ),
    all_vote_types_any = fit_fractional_logit(
      model_df, "choice_share", "all_vote_types_regression_row", "any", "space",
      "Observed share, FULL all-voting-system sample",
      position_spec = "rank_quadratic"
    ),
    single_preference_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "space",
      "Observed share, expanded single-preference sample",
      position_spec = "rank_quadratic"
    ),
    author_voted_only_any = fit_fractional_logit(
      model_df, "choice_share", "author_voted_regression_row", "any", "space",
      "Observed share, author-voted-only sample",
      position_spec = "rank_quadratic"
    ),
    vote_count_share_any = fit_fractional_logit(
      model_df, "vote_count_share", "vote_count_outcome_row", "any", "space",
      "Vote-count share, expanded single-preference sample",
      position_spec = "rank_quadratic"
    ),
    participation_control_any = fit_fractional_logit(
      model_df, "choice_share", "participation_control_row", "any", "space",
      "Observed share, participation-control sensitivity",
      position_spec = "rank_quadratic",
      include_participation_control = TRUE
    ),
    matched_observed_any = fit_fractional_logit(
      model_df, "choice_share", "no_author_vp_row", "any", "space",
      "Observed share, matched no-author-vp sample",
      position_spec = "rank_quadratic"
    ),
    matched_no_author_any = fit_fractional_logit(
      model_df, "choice_share_no_author_vp", "no_author_vp_row", "any", "space",
      "No-author-vp share, matched sample",
      position_spec = "rank_quadratic"
    ),
    matched_observed_split = fit_fractional_logit(
      model_df, "choice_share", "no_author_vp_row", "split", "space",
      "Observed share, first/non-first split",
      position_spec = "rank_quadratic"
    ),
    matched_no_author_split = fit_fractional_logit(
      model_df, "choice_share_no_author_vp", "no_author_vp_row", "split", "space",
      "No-author-vp share, first/non-first split",
      position_spec = "rank_quadratic"
    ),
    matched_observed_stance_split = fit_fractional_logit(
      model_df, "choice_share", "no_author_vp_row", "stance_split", "space",
      "Observed share, approve/non-approve split",
      position_spec = "rank_quadratic"
    ),
    matched_no_author_stance_split = fit_fractional_logit(
      model_df, "choice_share_no_author_vp", "no_author_vp_row", "stance_split", "space",
      "No-author-vp share, approve/non-approve split",
      position_spec = "rank_quadratic"
    ),
    proposal_fe_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "proposal",
      "Observed share, proposal fixed effects",
      position_spec = "rank_quadratic"
    ),
    proposal_fe_split = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "split", "proposal",
      "Observed share, proposal fixed effects split",
      position_spec = "rank_quadratic"
    ),
    proposal_fe_stance_split = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "stance_split", "proposal",
      "Observed share, proposal fixed effects approve/non-approve split",
      position_spec = "rank_quadratic"
    ),
    low_participation_any = fit_fractional_logit(
      model_df, "choice_share", "low_participation_row", "any", "space",
      "Observed share, excluding tot_votes_2 <= 1",
      position_spec = "rank_quadratic"
    ),
    top_space_excluded_any = fit_fractional_logit(
      model_df, "choice_share", "top_space_excluded_row", "any", "space",
      paste0("Observed share, excluding top ", length(excluded_spaces), " spaces"),
      position_spec = "rank_quadratic"
    ),
    top_tvl_dao_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_row", "any", "space",
      "Observed share, Top 25 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic"
    ),
    top_tvl_dao_top50_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top50_row", "any", "space",
      "Observed share, Top 50 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic"
    ),
    top_tvl_dao_top100_any = fit_fractional_logit(
      model_df, "choice_share", "top_tvl_dao_top100_row", "any", "space",
      "Observed share, Top 100 Ethereum-TVL DAO subset",
      position_spec = "rank_quadratic"
    ),
    first_choice_sensitivity_any = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "any", "space",
      "Voting-power share outcome; binary first-choice position control",
      position_spec = "first_choice"
    ),
    first_choice_sensitivity_split = fit_fractional_logit(
      model_df, "choice_share", "main_regression_row", "split", "space",
      "Voting-power share outcome; binary first-choice position control, author-choice split",
      position_spec = "first_choice"
    ),
    first_choice_shared_bias_summary = fit_first_choice_shared_bias_summary_model_set(
      model_df,
      excluded_spaces = sets$excluded_spaces,
      top_tvl_spaces = sets$top_tvl_spaces,
      top_tvl_sample_spaces = sets$top_tvl_sample_spaces
    ),
    excluded_spaces = excluded_spaces,
    top_tvl_spaces = sets$top_tvl_spaces,
    top_tvl_sample_spaces = sets$top_tvl_sample_spaces,
    top_tvl_table = sets$top_tvl_table
  )
}

fit_coefficient_ladder_model <- function(model_df, outcome, subset_flag,
                                         position_spec = c("rank_quadratic", "first_choice"),
                                         rhs_terms, label, ladder_name, model_name) {
  require_packages(c("data.table", "fixest"))
  position_spec <- match.arg(position_spec)

  dt <- data.table::copy(data.table::as.data.table(model_df)[get(subset_flag) %in% TRUE])
  if (nrow(dt) == 0) stop("No rows available for coefficient ladder model: ", label, call. = FALSE)
  dt[, model_outcome := as.numeric(get(outcome))]
  dt <- dt[!is.na(model_outcome)]
  if (nrow(dt) == 0) {
    stop("No non-missing outcome rows available for coefficient ladder model: ", label, call. = FALSE)
  }

  formula <- stats::as.formula(paste0(
    "model_outcome ~ ",
    paste(rhs_terms, collapse = " + "),
    " | space"
  ))
  model <- fixest::feglm(
    formula,
    data = dt,
    family = stats::quasibinomial("logit")
  )
  attr(model, "robustness_data") <- dt
  attr(model, "robustness_label") <- label
  attr(model, "robustness_outcome") <- outcome
  attr(model, "robustness_subset_flag") <- subset_flag
  attr(model, "robustness_fixed_effect") <- "space"
  attr(model, "robustness_position_spec") <- position_spec
  attr(model, "robustness_ladder") <- ladder_name
  attr(model, "robustness_ladder_model") <- model_name
  model
}

coefficient_ladder_rhs_sets <- function(position_spec = c("rank_quadratic", "first_choice")) {
  position_spec <- match.arg(position_spec)
  position_rhs <- if (position_spec == "rank_quadratic") {
    c("choice_idx", "I(choice_idx^2)")
  } else {
    "is_first_choice"
  }

  list(
    `1 Base` = c("type", position_rhs, "author_voted", "is_author_choice_expanded", "is_infavor"),
    `2 + date` = c(
      "type", position_rhs, "author_voted", "is_author_choice_expanded",
      "is_infavor", "created_rel"
    ),
    `3 + choices` = c(
      "type", position_rhs, "author_voted", "is_author_choice_expanded",
      "is_infavor", "created_rel", "N_choices"
    ),
    `4 + visibility` = c(
      "type", position_rhs, "author_voted", "is_author_choice_expanded",
      "is_infavor", "created_rel", "N_choices", "author_pos6_expanded"
    ),
    `5 + text` = c(
      "type", position_rhs, "author_voted", "is_author_choice_expanded",
      "is_infavor", "created_rel", "N_choices", "author_pos6_expanded",
      "choice_text_len_rel"
    ),
    `6 interaction` = c(
      "type", position_rhs, "author_voted", "is_infavor",
      "is_author_choice_expanded * author_pos6_expanded",
      "created_rel", "N_choices", "choice_text_len_rel"
    )
  )
}

fit_coefficient_ladder <- function(model_df, outcome, subset_flag,
                                   position_spec = c("rank_quadratic", "first_choice"),
                                   ladder_name, label_prefix) {
  position_spec <- match.arg(position_spec)
  rhs_sets <- coefficient_ladder_rhs_sets(position_spec)
  models <- lapply(seq_along(rhs_sets), function(i) {
    model_name <- names(rhs_sets)[[i]]
    fit_coefficient_ladder_model(
      model_df = model_df,
      outcome = outcome,
      subset_flag = subset_flag,
      position_spec = position_spec,
      rhs_terms = rhs_sets[[i]],
      label = paste0(label_prefix, ", ", model_name),
      ladder_name = ladder_name,
      model_name = model_name
    )
  })
  names(models) <- names(rhs_sets)
  models
}

fit_coefficient_ladders <- function(model_df) {
  list(
    single_preference_rank_quadratic = fit_coefficient_ladder(
      model_df,
      outcome = "choice_share",
      subset_flag = "main_regression_row",
      position_spec = "rank_quadratic",
      ladder_name = "single_preference_rank_quadratic",
      label_prefix = "Single-preference rank/quadratic coefficient ladder"
    ),
    first_choice_position = fit_coefficient_ladder(
      model_df,
      outcome = "choice_share",
      subset_flag = "main_regression_row",
      position_spec = "first_choice",
      ladder_name = "first_choice_position",
      label_prefix = "Single-preference first-choice coefficient ladder"
    )
  )
}
