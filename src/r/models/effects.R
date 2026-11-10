coef_table_for_terms <- function(model, terms) {
  summary_obj <- summary(model, cluster = ~space)
  coef_table <- summary_obj$coeftable
  if (is.null(coef_table)) coef_table <- fixest::coeftable(summary_obj)

  rows <- lapply(terms, function(term) {
    if (!(term %in% rownames(coef_table))) {
      return(data.frame(
        term = term,
        estimate = NA_real_,
        std_error = NA_real_,
        p_value = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    term_row <- coef_table[term, ]
    data.frame(
      term = term,
      estimate = unname(term_row[["Estimate"]]),
      std_error = unname(term_row[["Std. Error"]]),
      p_value = unname(term_row[[ncol(coef_table)]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

empty_average_adjusted_prediction_difference <- function(term) {
  data.frame(
    term = term,
    average_adjusted_prediction_difference = NA_real_,
    average_adjusted_prediction_difference_pp = NA_real_,
    average_adjusted_prediction_difference_se_pp = NA_real_,
    average_adjusted_prediction_difference_p = NA_real_,
    stringsAsFactors = FALSE
  )
}

average_adjusted_prediction_newdata <- function(model, term, companion_zero_terms = character()) {
  dt <- data.frame(attr(model, "robustness_data"))
  first_author_terms <- c("first_author_choice", "first_author_choice_expanded")
  nonfirst_author_terms <- c("nonfirst_author_choice", "nonfirst_author_choice_expanded")
  infavor_author_terms <- c("infavor_author_choice_expanded")
  noninfavor_author_terms <- c("noninfavor_author_choice_expanded")
  if (term %in% first_author_terms && "is_first_choice" %in% names(dt)) {
    dt <- dt[dt$is_first_choice == 1, , drop = FALSE]
  } else if (term %in% nonfirst_author_terms && "is_first_choice" %in% names(dt)) {
    dt <- dt[dt$is_first_choice == 0, , drop = FALSE]
  } else if (term %in% first_author_terms && "choice_idx" %in% names(dt)) {
    dt <- dt[dt$choice_idx == 1, , drop = FALSE]
  } else if (term %in% nonfirst_author_terms && "choice_idx" %in% names(dt)) {
    dt <- dt[dt$choice_idx != 1, , drop = FALSE]
  } else if (term %in% infavor_author_terms && "is_infavor" %in% names(dt)) {
    dt <- dt[dt$is_infavor == 1, , drop = FALSE]
  } else if (term %in% noninfavor_author_terms && "is_infavor" %in% names(dt)) {
    dt <- dt[dt$is_infavor == 0, , drop = FALSE]
  }

  for (zero_term in companion_zero_terms) {
    if (zero_term %in% names(dt)) {
      dt[[zero_term]] <- 0L
    }
  }
  dt
}

author_choice_terms <- function() {
  c(
    "is_author_choice",
    "is_author_choice_expanded",
    "first_author_choice",
    "first_author_choice_expanded",
    "nonfirst_author_choice",
    "nonfirst_author_choice_expanded",
    "infavor_author_choice_expanded",
    "noninfavor_author_choice_expanded"
  )
}

author_choice_effect_newdata <- function(model, term, companion_zero_terms = character()) {
  dt <- average_adjusted_prediction_newdata(model, term, companion_zero_terms)
  if (term %in% author_choice_terms() && "author_voted" %in% names(dt)) {
    dt <- dt[dt$author_voted == 1, , drop = FALSE]
  }
  dt
}

average_adjusted_prediction_difference <- function(model, term,
                                                   companion_zero_terms = character(),
                                                   newdata = NULL) {
  require_packages(c("marginaleffects"))
  model_terms <- names(stats::coef(model))
  if (!(term %in% model_terms)) {
    return(empty_average_adjusted_prediction_difference(term))
  }

  if (is.null(newdata)) {
    newdata <- average_adjusted_prediction_newdata(model, term, companion_zero_terms)
  } else {
    newdata <- data.frame(newdata)
  }
  if (nrow(newdata) == 0) {
    return(empty_average_adjusted_prediction_difference(term))
  }

  slopes <- tryCatch(
    as.data.frame(marginaleffects::avg_slopes(
      model,
      variables = term,
      newdata = newdata,
      vcov = ~space
    )),
    error = function(err) {
      if (!grepl("fixed-effects parameters", conditionMessage(err), fixed = TRUE)) {
        stop(err)
      }
      out <- as.data.frame(marginaleffects::avg_slopes(
        model,
        variables = term,
        newdata = newdata,
        vcov = FALSE
      ))
      out[["std.error"]] <- NA_real_
      out[["p.value"]] <- NA_real_
      out
    }
  )
  slopes <- slopes[slopes$term == term, , drop = FALSE]
  if (nrow(slopes) == 0) {
    return(empty_average_adjusted_prediction_difference(term))
  }

  estimate <- slopes$estimate[[1]]
  std_error <- slopes[["std.error"]][[1]]
  p_value <- if ("p.value" %in% names(slopes)) slopes[["p.value"]][[1]] else NA_real_
  data.frame(
    term = term,
    average_adjusted_prediction_difference = estimate,
    average_adjusted_prediction_difference_pp = 100 * estimate,
    average_adjusted_prediction_difference_se_pp = 100 * std_error,
    average_adjusted_prediction_difference_p = p_value,
    stringsAsFactors = FALSE
  )
}

average_first_second_prediction_difference <- function(model) {
  dt <- data.frame(attr(model, "robustness_data"))
  if (!all(c("choice_idx", "N_choices") %in% names(dt))) {
    return(empty_average_adjusted_prediction_difference("first_vs_second_choice"))
  }
  model_terms <- names(stats::coef(model))
  if (!("choice_idx" %in% model_terms)) {
    return(empty_average_adjusted_prediction_difference("first_vs_second_choice"))
  }

  dt <- dt[!is.na(dt$N_choices) & dt$N_choices >= 2, , drop = FALSE]
  if (nrow(dt) == 0) {
    return(empty_average_adjusted_prediction_difference("first_vs_second_choice"))
  }

  first_position <- dt
  second_position <- dt
  first_position$choice_idx <- 1
  second_position$choice_idx <- 2
  prediction_difference <- stats::predict(model, newdata = first_position, type = "response") -
    stats::predict(model, newdata = second_position, type = "response")
  prediction_difference <- prediction_difference[!is.na(prediction_difference)]
  if (length(prediction_difference) == 0) {
    return(empty_average_adjusted_prediction_difference("first_vs_second_choice"))
  }

  estimate <- mean(prediction_difference)
  data.frame(
    term = "first_vs_second_choice",
    average_adjusted_prediction_difference = estimate,
    average_adjusted_prediction_difference_pp = 100 * estimate,
    average_adjusted_prediction_difference_se_pp = NA_real_,
    average_adjusted_prediction_difference_p = NA_real_,
    stringsAsFactors = FALSE
  )
}

average_first_nonfirst_prediction_difference <- function(model) {
  dt <- data.frame(attr(model, "robustness_data"))
  if (!("is_first_choice" %in% names(dt))) {
    return(empty_average_adjusted_prediction_difference("first_vs_nonfirst_choice"))
  }
  model_terms <- names(stats::coef(model))
  if (!("is_first_choice" %in% model_terms)) {
    return(empty_average_adjusted_prediction_difference("first_vs_nonfirst_choice"))
  }

  first_position <- dt
  nonfirst_position <- dt
  first_position$is_first_choice <- 1L
  nonfirst_position$is_first_choice <- 0L
  prediction_difference <- stats::predict(model, newdata = first_position, type = "response") -
    stats::predict(model, newdata = nonfirst_position, type = "response")
  prediction_difference <- prediction_difference[!is.na(prediction_difference)]
  if (length(prediction_difference) == 0) {
    return(empty_average_adjusted_prediction_difference("first_vs_nonfirst_choice"))
  }

  estimate <- mean(prediction_difference)
  data.frame(
    term = "first_vs_nonfirst_choice",
    average_adjusted_prediction_difference = estimate,
    average_adjusted_prediction_difference_pp = 100 * estimate,
    average_adjusted_prediction_difference_se_pp = NA_real_,
    average_adjusted_prediction_difference_p = NA_real_,
    stringsAsFactors = FALSE
  )
}

model_effect_rows <- function(model, terms, term_labels, sample_rows = NULL) {
  coefs <- coef_table_for_terms(model, terms)
  companions <- list(
    is_author_choice = character(),
    is_author_choice_expanded = character(),
    first_author_choice = "nonfirst_author_choice",
    nonfirst_author_choice = "first_author_choice",
    first_author_choice_expanded = "nonfirst_author_choice_expanded",
    nonfirst_author_choice_expanded = "first_author_choice_expanded",
    infavor_author_choice_expanded = "noninfavor_author_choice_expanded",
    noninfavor_author_choice_expanded = "infavor_author_choice_expanded"
  )

  effects <- do.call(rbind, lapply(terms, function(term) {
    companion_terms <- companions[[term]]
    if (is.null(companion_terms)) companion_terms <- character()
    newdata <- if (term %in% author_choice_terms()) {
      author_choice_effect_newdata(model, term, companion_terms)
    } else {
      NULL
    }
    average_adjusted_prediction_difference(
      model,
      term,
      companion_terms,
      newdata = newdata
    )
  }))

  out <- merge(coefs, effects, by = "term", all.x = TRUE, sort = FALSE)
  out$model <- attr(model, "robustness_label")
  out$outcome <- attr(model, "robustness_outcome")
  out$fixed_effect <- attr(model, "robustness_fixed_effect")
  out$sample_rows <- if (is.null(sample_rows)) nrow(attr(model, "robustness_data")) else sample_rows
  out$model_nobs <- stats::nobs(model)
  out$term_label <- unname(term_labels[out$term])
  out[, c(
    "model", "outcome", "fixed_effect", "sample_rows", "model_nobs",
    "term_label", "estimate", "std_error", "p_value",
    "average_adjusted_prediction_difference",
    "average_adjusted_prediction_difference_pp",
    "average_adjusted_prediction_difference_se_pp",
    "average_adjusted_prediction_difference_p"
  )]
}

shared_bias_summary_specifications <- function() {
  data.frame(
    specification = c(
      "Single preference",
      "Vote-count share, expanded single-preference sample",
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
    ),
    specification_order = seq_len(12),
    model_name = c(
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
    ),
    fixed_effect = c(
      "space",
      "space",
      "space",
      "space",
      "space",
      "space",
      "space",
      "space",
      "space",
      "proposal",
      "space",
      "space"
    ),
    stringsAsFactors = FALSE
  )
}

shared_bias_summary_models <- function(models) {
  specs <- shared_bias_summary_specifications()
  missing_models <- specs$model_name[!vapply(specs$model_name, function(model_name) {
    !is.null(models[[model_name]])
  }, logical(1))]
  if (length(missing_models) > 0) {
    stop(
      "Shared bias summary model set missing model(s): ",
      paste(missing_models, collapse = ", "),
      call. = FALSE
    )
  }
  out <- lapply(specs$model_name, function(model_name) models[[model_name]])
  names(out) <- specs$specification
  out
}

first_choice_shared_bias_summary_models <- function(models) {
  specs <- shared_bias_summary_specifications()
  first_choice_models <- models$first_choice_shared_bias_summary
  if (is.null(first_choice_models)) {
    stop("Missing first-choice shared bias summary model set.", call. = FALSE)
  }
  missing_models <- specs$model_name[!vapply(specs$model_name, function(model_name) {
    !is.null(first_choice_models[[model_name]])
  }, logical(1))]
  if (length(missing_models) > 0) {
    stop(
      "First-choice shared bias summary model set missing model(s): ",
      paste(missing_models, collapse = ", "),
      call. = FALSE
    )
  }
  out <- lapply(specs$model_name, function(model_name) first_choice_models[[model_name]])
  names(out) <- specs$specification
  out
}

three_bias_contrast_specifications <- function() {
  data.frame(
    bias_contrast = c("Author choice", "First vs second choice", "Approve stance"),
    contrast_order = seq_len(3),
    interpretation = c(
      "Author-selected vs not author-selected among author-voted proposals",
      "Choice rank 1 vs choice rank 2",
      "Approve vs non-Approve/Other"
    ),
    type = c("term", "first_second_position", "term"),
    term = c("is_author_choice_expanded", NA_character_, "is_infavor"),
    stringsAsFactors = FALSE
  )
}

first_choice_three_bias_contrast_specifications <- function() {
  data.frame(
    bias_contrast = c("Author choice", "First vs non-first choice", "Approve stance"),
    contrast_order = seq_len(3),
    interpretation = c(
      "Author-selected vs not author-selected among author-voted proposals",
      "First choice vs non-first choice (binary position control)",
      "Approve vs non-Approve/Other"
    ),
    type = c("term", "first_nonfirst_position", "term"),
    term = c("is_author_choice_expanded", NA_character_, "is_infavor"),
    stringsAsFactors = FALSE
  )
}

three_bias_contrast_pp <- function(model, contrast) {
  if (identical(contrast$type, "first_second_position")) {
    return(average_first_second_prediction_difference(model)$average_adjusted_prediction_difference_pp[[1]])
  }
  newdata <- if (contrast$term %in% author_choice_terms()) {
    author_choice_effect_newdata(model, contrast$term)
  } else {
    NULL
  }
  average_adjusted_prediction_difference(
    model,
    contrast$term,
    newdata = newdata
  )$average_adjusted_prediction_difference_pp[[1]]
}

first_choice_three_bias_contrast_pp <- function(model, contrast) {
  if (identical(contrast$type, "first_nonfirst_position")) {
    return(average_first_nonfirst_prediction_difference(model)$average_adjusted_prediction_difference_pp[[1]])
  }
  newdata <- if (contrast$term %in% author_choice_terms()) {
    author_choice_effect_newdata(model, contrast$term)
  } else {
    NULL
  }
  average_adjusted_prediction_difference(
    model,
    contrast$term,
    newdata = newdata
  )$average_adjusted_prediction_difference_pp[[1]]
}

build_three_bias_summary <- function(models) {
  contrasts <- three_bias_contrast_specifications()
  summary_models <- shared_bias_summary_models(models)
  out <- data.frame(
    `Bias contrast` = contrasts$bias_contrast,
    Interpretation = contrasts$interpretation,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (model_name in names(summary_models)) {
    out[[model_name]] <- vapply(
      seq_len(nrow(contrasts)),
      function(contrast_idx) {
        three_bias_contrast_pp(summary_models[[model_name]], as.list(contrasts[contrast_idx, ]))
      },
      numeric(1)
    )
  }
  out
}

build_first_choice_three_bias_summary <- function(models) {
  contrasts <- first_choice_three_bias_contrast_specifications()
  summary_models <- first_choice_shared_bias_summary_models(models)
  out <- data.frame(
    `Bias contrast` = contrasts$bias_contrast,
    Interpretation = contrasts$interpretation,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (model_name in names(summary_models)) {
    out[[model_name]] <- vapply(
      seq_len(nrow(contrasts)),
      function(contrast_idx) {
        first_choice_three_bias_contrast_pp(summary_models[[model_name]], as.list(contrasts[contrast_idx, ]))
      },
      numeric(1)
    )
  }
  out
}

build_three_bias_summary_long_grid <- function(three_bias_summary, models = NULL) {
  specs <- shared_bias_summary_specifications()
  contrasts <- three_bias_contrast_specifications()
  required <- c("Bias contrast", specs$specification)
  missing <- setdiff(required, names(three_bias_summary))
  if (length(missing) > 0) {
    stop(
      "Three-bias summary missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  summary_contrasts <- as.character(three_bias_summary[["Bias contrast"]])
  if (!identical(summary_contrasts, contrasts$bias_contrast)) {
    stop(
      "Three-bias summary must contain the focal contrasts in order: ",
      paste(contrasts$bias_contrast, collapse = ", "),
      call. = FALSE
    )
  }

  model_lookup <- NULL
  if (!is.null(models)) {
    model_lookup <- shared_bias_summary_models(models)
  }

  rows <- lapply(seq_len(nrow(specs)), function(spec_idx) {
    spec <- specs[spec_idx, , drop = FALSE]
    model <- if (is.null(model_lookup)) NULL else model_lookup[[spec$specification]]
    fixed_effect <- if (is.null(model)) {
      spec$fixed_effect
    } else {
      model_fixed_effect <- attr(model, "robustness_fixed_effect", exact = TRUE)
      if (is.null(model_fixed_effect) || length(model_fixed_effect) == 0) {
        spec$fixed_effect
      } else {
        as.character(model_fixed_effect[[1]])
      }
    }
    model_nobs <- if (is.null(model)) NA_integer_ else as.integer(stats::nobs(model))
    data.frame(
      specification = spec$specification,
      specification_order = spec$specification_order,
      bias_contrast = contrasts$bias_contrast,
      contrast_order = contrasts$contrast_order,
      fixed_effect = fixed_effect,
      model_nobs = model_nobs,
      ame_pp = as.numeric(three_bias_summary[[spec$specification]]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (anyDuplicated(out[c("specification", "bias_contrast")])) {
    stop("Three-bias long grid has duplicated specification/contrast rows.", call. = FALSE)
  }
  if (any(!is.finite(out$ame_pp))) {
    stop("Three-bias long grid has non-finite AME values.", call. = FALSE)
  }
  out
}

relabel_effect_rows <- function(rows, labels) {
  if (length(labels) != nrow(rows)) {
    stop(
      "Effect-row label count does not match row count: ",
      length(labels), " label(s) for ", nrow(rows), " row(s).",
      call. = FALSE
    )
  }
  rows$model <- labels
  rows
}

build_author_choice_shared_specs <- function(models, labels) {
  summary_models <- shared_bias_summary_models(models)
  rows <- lapply(seq_along(summary_models), function(idx) {
    relabel_effect_rows(
      model_effect_rows(summary_models[[idx]], "is_author_choice_expanded", labels),
      names(summary_models)[[idx]]
    )
  })
  do.call(rbind, rows)
}

build_author_choice_diagnostics <- function(models, labels) {
  diagnostic_specs <- list(
    list(
      model = models$matched_observed_any,
      terms = "is_author_choice_expanded",
      row_labels = "Observed share, matched no-author-vp sample"
    ),
    list(
      model = models$matched_no_author_any,
      terms = "is_author_choice_expanded",
      row_labels = "No-author-vp share, matched sample"
    ),
    list(
      model = models$proposal_fe_any,
      terms = "is_author_choice_expanded",
      row_labels = "Observed share, proposal fixed effects"
    ),
    list(
      model = models$matched_observed_split,
      terms = c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      row_labels = c(
        "Observed share, first/non-first split: first position",
        "Observed share, first/non-first split: non-first position"
      )
    ),
    list(
      model = models$matched_no_author_split,
      terms = c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      row_labels = c(
        "No-author-vp share, first/non-first split: first position",
        "No-author-vp share, first/non-first split: non-first position"
      )
    ),
    list(
      model = models$proposal_fe_split,
      terms = c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      row_labels = c(
        "Observed share, proposal fixed effects split: first position",
        "Observed share, proposal fixed effects split: non-first position"
      )
    ),
    list(
      model = models$matched_observed_stance_split,
      terms = c("infavor_author_choice_expanded", "noninfavor_author_choice_expanded"),
      row_labels = c(
        "Observed share, approve/non-approve split: in favor",
        "Observed share, approve/non-approve split: not in favor"
      )
    ),
    list(
      model = models$matched_no_author_stance_split,
      terms = c("infavor_author_choice_expanded", "noninfavor_author_choice_expanded"),
      row_labels = c(
        "No-author-vp share, approve/non-approve split: in favor",
        "No-author-vp share, approve/non-approve split: not in favor"
      )
    ),
    list(
      model = models$proposal_fe_stance_split,
      terms = c("infavor_author_choice_expanded", "noninfavor_author_choice_expanded"),
      row_labels = c(
        "Observed share, proposal fixed effects approve/non-approve split: in favor",
        "Observed share, proposal fixed effects approve/non-approve split: not in favor"
      )
    )
  )
  rows <- lapply(diagnostic_specs, function(spec) {
    relabel_effect_rows(
      model_effect_rows(spec$model, spec$terms, labels),
      spec$row_labels
    )
  })
  do.call(rbind, rows)
}

build_effect_tables <- function(models) {
  labels <- c(
    is_author_choice_expanded = "Author choice",
    first_author_choice_expanded = "Author choice, first position",
    nonfirst_author_choice_expanded = "Author choice, non-first position",
    infavor_author_choice_expanded = "Author choice, in favor",
    noninfavor_author_choice_expanded = "Author choice, not in favor"
  )

  no_author <- rbind(
    model_effect_rows(models$matched_observed_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$matched_no_author_any, "is_author_choice_expanded", labels),
    model_effect_rows(
      models$matched_observed_split,
      c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      labels
    ),
    model_effect_rows(
      models$matched_no_author_split,
      c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      labels
    )
  )

  proposal_fe <- rbind(
    model_effect_rows(models$proposal_fe_any, "is_author_choice_expanded", labels),
    model_effect_rows(
      models$proposal_fe_split,
      c("first_author_choice_expanded", "nonfirst_author_choice_expanded"),
      labels
    )
  )

  author_voted_only <- model_effect_rows(
    models$author_voted_only_any,
    "is_author_choice_expanded",
    labels
  )

  vote_count_share <- model_effect_rows(
    models$vote_count_share_any,
    "is_author_choice_expanded",
    labels
  )

  participation_control <- model_effect_rows(
    models$participation_control_any,
    "is_author_choice_expanded",
    labels
  )

  robustness <- rbind(
    model_effect_rows(models$full_sample_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$all_vote_types_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$single_preference_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$low_participation_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$top_space_excluded_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$top_tvl_dao_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$top_tvl_dao_top50_any, "is_author_choice_expanded", labels),
    model_effect_rows(models$top_tvl_dao_top100_any, "is_author_choice_expanded", labels)
  )

  marginal_effects <- rbind(
    no_author,
    proposal_fe,
    author_voted_only,
    vote_count_share,
    participation_control,
    robustness
  )

  author_choice_shared_specs <- build_author_choice_shared_specs(models, labels)
  author_choice_diagnostics <- build_author_choice_diagnostics(models, labels)

  list(
    no_author = no_author,
    proposal_fe = proposal_fe,
    author_voted_only = author_voted_only,
    vote_count_share = vote_count_share,
    participation_control = participation_control,
    robustness = robustness,
    three_bias_summary = build_three_bias_summary(models),
    first_choice_three_bias_summary = build_first_choice_three_bias_summary(models),
    author_choice_shared_specs = author_choice_shared_specs,
    author_choice_diagnostics = author_choice_diagnostics,
    marginal_effects = marginal_effects
  )
}
