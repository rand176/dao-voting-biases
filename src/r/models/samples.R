################################################################################
## Stance validity and attrition
################################################################################

normalize_documented_stance_exceptions <- function(documented_stance_proposals = character()) {
  allowed_states <- c("approve_only", "reject_only")
  if (is.data.frame(documented_stance_proposals)) {
    required <- c("proposal", "allowed_state")
    missing <- setdiff(required, names(documented_stance_proposals))
    if (length(missing) > 0) {
      stop("Documented stance exception data missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    docs <- documented_stance_proposals[, required, drop = FALSE]
    docs$proposal <- trimws(tolower(as.character(docs$proposal)))
    docs$allowed_state <- trimws(tolower(as.character(docs$allowed_state)))
  } else {
    proposals <- unique(trimws(tolower(as.character(documented_stance_proposals))))
    proposals <- proposals[!is.na(proposals) & nzchar(proposals)]
    docs <- expand.grid(
      proposal = proposals,
      allowed_state = allowed_states,
      stringsAsFactors = FALSE
    )
  }

  unknown_states <- sort(unique(docs$allowed_state[
    !(docs$allowed_state %in% allowed_states) &
      !is.na(docs$allowed_state) &
      nzchar(docs$allowed_state)
  ]))
  if (length(unknown_states) > 0) {
    stop("Unknown documented stance exception state(s): ", paste(unknown_states, collapse = ", "), call. = FALSE)
  }

  unique(docs[!is.na(docs$proposal) & nzchar(docs$proposal), , drop = FALSE])
}

load_documented_stance_proposals <- function(path = default_stance_overrides_path()) {
  empty <- data.frame(proposal = character(), allowed_state = character(), stringsAsFactors = FALSE)
  if (length(path) == 0 || is.na(path) || !file.exists(path)) return(empty)
  stage <- Sys.getenv("ROBUSTNESS_STANCE_STAGE", unset = "2_stance_v5")
  overrides <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("stage", "proposal", "allowed_state")
  missing <- setdiff(required, names(overrides))
  if (length(missing) > 0) {
    stop("Stance one-sided exception file missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  normalize_documented_stance_exceptions(overrides[overrides$stage == stage, , drop = FALSE])
}

build_stance_audit <- function(model_df, documented_stance_proposals = character()) {
  require_packages("data.table")
  dt <- data.table::as.data.table(model_df)
  docs <- normalize_documented_stance_exceptions(documented_stance_proposals)
  normalized_props <- attr(model_df, "normalized_undocumented_one_sided_proposals")
  if (is.null(normalized_props)) normalized_props <- character()
  normalized_props <- unique(trimws(tolower(as.character(normalized_props))))
  audit <- dt[, .(
    rows = .N,
    spaces = data.table::uniqueN(space),
    has_approve = any(stance == "APPROVE", na.rm = TRUE),
    has_reject = any(stance == "REJECT", na.rm = TRUE),
    has_abstain = any(stance == "ABSTAIN", na.rm = TRUE),
    has_stance = any(stance %in% c("APPROVE", "REJECT", "ABSTAIN", "OTHER"), na.rm = TRUE)
  ), by = proposal]

  audit[, stance_state := data.table::fifelse(
    has_approve & has_reject, "both",
    data.table::fifelse(
      has_approve & !has_reject, "approve_only",
      data.table::fifelse(has_reject & !has_approve, "reject_only", "neither")
    )
  )]
  audit[, is_one_sided_approve_reject := stance_state %in% c("approve_only", "reject_only") |
          proposal %in% normalized_props]
  audit[, is_one_sided_documented_exception := mapply(
    function(proposal_id, state) {
      any(docs$proposal == proposal_id & docs$allowed_state == state, na.rm = TRUE)
    },
    proposal,
    stance_state
  )]
  audit[, documented_stance_override := is_one_sided_documented_exception]
  audit[, one_sided_action := data.table::fifelse(
    proposal %in% normalized_props,
    "normalized_to_other",
    data.table::fifelse(
      stance_state %in% c("approve_only", "reject_only") & is_one_sided_documented_exception,
      "kept_documented",
      data.table::fifelse(
        stance_state %in% c("approve_only", "reject_only"),
        "normalized_to_other",
        "not_applicable"
      )
    )
  )]
  audit[, undocumented_one_sided := stance_state %in% c("approve_only", "reject_only") &
          !is_one_sided_documented_exception]
  audit[]
}

normalize_undocumented_one_sided_stance <- function(model_df, documented_stance_proposals = character()) {
  require_packages("data.table")
  dt <- data.table::copy(data.table::as.data.table(model_df))
  audit <- build_stance_audit(dt, documented_stance_proposals)
  normalize_props <- audit$proposal[audit$undocumented_one_sided]
  affected <- dt$proposal %in% normalize_props

  if (any(affected, na.rm = TRUE)) {
    dt[affected, stance := "OTHER"]
    if ("is_infavor" %in% names(dt)) dt[affected, is_infavor := 0L]
    if ("is_against" %in% names(dt)) dt[affected, is_against := 0L]
    if ("is_abstain" %in% names(dt)) dt[affected, is_abstain := 0L]
    if ("is_other" %in% names(dt)) dt[affected, is_other := 1L]
  }

  data.table::setattr(dt, "normalized_undocumented_one_sided_proposals", normalize_props)
  data.table::setattr(dt, "normalized_undocumented_one_sided_rows", sum(affected, na.rm = TRUE))
  dt[]
}

estimation_data_for_model <- function(model) {
  require_packages("data.table")
  dt <- attr(model, "robustness_data")
  if (is.null(dt)) return(NULL)

  dt <- data.table::copy(data.table::as.data.table(dt))
  selections <- model$obs_selection
  if (!is.null(selections)) {
    for (selection in selections) {
      if (length(selection) == 0) next
      dt <- dt[selection]
    }
  }

  expected_nobs <- tryCatch(as.integer(stats::nobs(model)), error = function(e) NA_integer_)
  if (!is.na(expected_nobs) && nrow(dt) != expected_nobs) {
    warning(
      "estimation_data_for_model: reconstructed row count (", nrow(dt), ") ",
      "does not match model nobs (", expected_nobs, "). ",
      "obs_selection may not index into robustness_data correctly.",
      call. = FALSE
    )
  }

  dt[]
}

build_attrition_table <- function(model_df, stance_audit, stance_mode = "block",
                                  models = NULL) {
  require_packages("data.table")
  dt <- data.table::as.data.table(model_df)
  rows_for <- function(flag) {
    if (!flag %in% names(dt)) return(dt[FALSE])
    dt[get(flag) %in% TRUE]
  }
  model_rows_for <- function(model_name, fallback) {
    if (is.null(models) || is.null(models[[model_name]])) return(fallback)
    model_rows <- estimation_data_for_model(models[[model_name]])
    if (is.null(model_rows)) fallback else model_rows
  }
  has_model_rows <- function(model_name) {
    !is.null(models) &&
      !is.null(models[[model_name]]) &&
      !is.null(attr(models[[model_name]], "robustness_data"))
  }
  estimator_note <- function(note, model_name) {
    if (has_model_rows(model_name)) {
      paste0(note, " Retained rows are after fixed-effect estimator exclusions.")
    } else {
      note
    }
  }

  active <- dt
  single <- rows_for("single_preference_row")
  full_sample <- rows_for("full_sample_regression_row")
  all_vote_types <- rows_for("all_vote_types_regression_row")
  main <- rows_for("main_regression_row")
  author_voted_only <- rows_for("author_voted_regression_row")
  vote_count_outcome <- rows_for("vote_count_outcome_row")
  participation_control <- rows_for("participation_control_row")
  top_tvl <- rows_for("top_tvl_dao_row")
  no_author <- rows_for("no_author_vp_row")
  low_participation <- rows_for("low_participation_row")
  full_sample_model <- model_rows_for("full_sample_any", full_sample)
  all_vote_types_model <- model_rows_for("all_vote_types_any", all_vote_types)
  main_model <- model_rows_for("single_preference_any", main)
  author_voted_model <- model_rows_for("author_voted_only_any", author_voted_only)
  vote_count_model <- model_rows_for("vote_count_share_any", vote_count_outcome)
  participation_model <- model_rows_for("participation_control_any", participation_control)
  top_tvl_model <- model_rows_for("top_tvl_dao_any", top_tvl)
  no_author_model <- model_rows_for("matched_observed_any", no_author)
  low_participation_model <- model_rows_for("low_participation_any", low_participation)

  undocumented_props <- stance_audit$proposal[stance_audit$undocumented_one_sided]
  stance_valid <- main_model[!(proposal %in% undocumented_props) & !is.na(stance)]
  stance_blocked <- identical(stance_mode, "block") && length(undocumented_props) > 0
  undocumented_main_rows <- sum(main_model$proposal %in% undocumented_props, na.rm = TRUE)
  stance_note <- if (stance_blocked) {
    paste0(
      "Blocked by default; ",
      undocumented_main_rows,
      " main-sample rows in undocumented one-sided stance proposals."
    )
  } else if (length(undocumented_props) == 0) {
    "Same estimated rows as the expanded main single-preference model; no undocumented one-sided stance proposals remain."
  } else {
    "Undocumented one-sided stance proposals excluded."
  }

  table <- data.frame(
    sample = c(
      "Active choice rows",
      "Single-preference rows",
      "Compatible full-sample regression sample",
      "All-voting-system FULL regression sample",
      "Main expanded single-preference regression sample",
      "Author-voted-only regression sample",
      "Vote-count-share outcome sample",
      "Participation-control sensitivity sample",
      "Top 25 Ethereum-TVL DAO subset",
      "No-author-vp sensitivity sample",
      "Low-participation-excluded sample",
      "Stance-valid sensitivity sample"
    ),
    rows = c(
      nrow(active),
      nrow(single),
      nrow(full_sample_model),
      nrow(all_vote_types_model),
      nrow(main_model),
      nrow(author_voted_model),
      nrow(vote_count_model),
      nrow(participation_model),
      nrow(top_tvl_model),
      nrow(no_author_model),
      nrow(low_participation_model),
      if (stance_blocked) NA_integer_ else nrow(stance_valid)
    ),
    proposals = c(
      data.table::uniqueN(active$proposal),
      data.table::uniqueN(single$proposal),
      data.table::uniqueN(full_sample_model$proposal),
      data.table::uniqueN(all_vote_types_model$proposal),
      data.table::uniqueN(main_model$proposal),
      data.table::uniqueN(author_voted_model$proposal),
      data.table::uniqueN(vote_count_model$proposal),
      data.table::uniqueN(participation_model$proposal),
      data.table::uniqueN(top_tvl_model$proposal),
      data.table::uniqueN(no_author_model$proposal),
      data.table::uniqueN(low_participation_model$proposal),
      if (stance_blocked) NA_integer_ else data.table::uniqueN(stance_valid$proposal)
    ),
    spaces = c(
      data.table::uniqueN(active$space),
      data.table::uniqueN(single$space),
      data.table::uniqueN(full_sample_model$space),
      data.table::uniqueN(all_vote_types_model$space),
      data.table::uniqueN(main_model$space),
      data.table::uniqueN(author_voted_model$space),
      data.table::uniqueN(vote_count_model$space),
      data.table::uniqueN(participation_model$space),
      data.table::uniqueN(top_tvl_model$space),
      data.table::uniqueN(no_author_model$space),
      data.table::uniqueN(low_participation_model$space),
      if (stance_blocked) NA_integer_ else data.table::uniqueN(stance_valid$space)
    ),
    note = c(
      "Loaded active choice-level data.",
      "Voting type is single-choice or basic.",
      estimator_note(
        "Rows from single-choice, basic, ranked-choice, and approval voting with non-missing observed share and controls; weighted and quadratic rows are excluded.",
        "full_sample_any"
      ),
      estimator_note(
        "Rows from all voting systems with non-missing observed share and controls; includes weighted and quadratic rows as supplemental robustness evidence.",
        "all_vote_types_any"
      ),
      estimator_note(
        "Primary model sample; missing author reconstruction fields enter as author_voted = 0 with expanded author-choice indicators set to 0.",
        "single_preference_any"
      ),
      estimator_note(
        "Conditional robustness sample restricted to rows where the proposal author voted.",
        "author_voted_only_any"
      ),
      estimator_note(
        "Primary expanded sample rows with valid vote_count_share; proposal-level vote-count shares sum to one.",
        "vote_count_share_any"
      ),
      estimator_note(
        "Primary expanded sample rows with valid log_tot_votes_2 for the participation-control sensitivity.",
        "participation_control_any"
      ),
      estimator_note(
        "Primary expanded single-preference sample restricted to Top 25 matched Snapshot spaces by median Ethereum TVL.",
        "top_tvl_dao_any"
      ),
      estimator_note("Author-voted sample with no-author-vp share available.", "matched_observed_any"),
      estimator_note("Primary sample with tot_votes_2 > 1.", "low_participation_any"),
      stance_note
    ),
    stringsAsFactors = FALSE
  )
  table
}

build_stance_summary <- function(stance_audit, model_df,
                                 pre_normalization_stance_audit = NULL) {
  require_packages("data.table")
  audit <- data.table::as.data.table(stance_audit)
  model_dt <- data.table::as.data.table(model_df)
  proposal_spaces <- unique(model_dt[, .(proposal, space)])

  upstream_audit <- if (is.null(pre_normalization_stance_audit)) {
    audit
  } else {
    data.table::as.data.table(pre_normalization_stance_audit)
  }

  complete_counts <- function(summary_dt, state_col, states) {
    template <- data.table::data.table(
      state_value = states,
      state_order = seq_along(states)
    )
    summary_dt <- data.table::as.data.table(summary_dt)
    data.table::setnames(summary_dt, state_col, "state_value")
    out <- merge(template, summary_dt, by = "state_value", all.x = TRUE, all.y = FALSE)
    for (col in c("proposals", "rows", "spaces")) {
      out[is.na(get(col)), (col) := 0L]
    }
    out[order(state_order)][, state_order := NULL]
  }

  pre_base <- if (all(c("original_stance_state", "action", "rows") %in% names(upstream_audit))) {
    unique(upstream_audit[
      action == "normalized_to_other" &
        original_stance_state %in% c("approve_only", "reject_only"),
      .(
        proposal,
        pre_state = original_stance_state,
        pre_rows = as.integer(rows)
      )
    ])
  } else {
    unique(upstream_audit[
      undocumented_one_sided %in% TRUE &
        stance_state %in% c("approve_only", "reject_only"),
      .(
        proposal,
        pre_state = stance_state,
        pre_rows = as.integer(rows)
      )
    ])
  }

  pre_summary <- merge(pre_base, proposal_spaces, by = "proposal", all.x = TRUE, all.y = FALSE)
  pre_summary <- pre_summary[, .(
    proposals = data.table::uniqueN(proposal),
    rows = sum(pre_rows, na.rm = TRUE),
    spaces = data.table::uniqueN(space[!is.na(space)])
  ), by = pre_state]
  pre_summary <- complete_counts(pre_summary, "pre_state", c("reject_only", "approve_only"))

  post_summary <- merge(
    model_dt[, .(proposal, space)],
    audit[, .(proposal, stance_state)],
    by = "proposal",
    all.x = FALSE,
    all.y = FALSE
  )
  post_summary <- post_summary[
    stance_state %in% c("neither", "both"),
    .(
      proposals = data.table::uniqueN(proposal),
      rows = .N,
      spaces = data.table::uniqueN(space)
    ),
    by = stance_state
  ]
  post_summary <- complete_counts(post_summary, "stance_state", c("neither", "both"))

  data.frame(
    when = c("Before recoding", "Before recoding", "After recoding", "After recoding"),
    proposal_pattern = c(
      "Reject, no Approve",
      "Approve, no Reject",
      "No Approve or Reject labels",
      "Approve and Reject both present"
    ),
    proposals = c(pre_summary$proposals, post_summary$proposals),
    rows = c(pre_summary$rows, post_summary$rows),
    spaces = c(pre_summary$spaces, post_summary$spaces),
    stringsAsFactors = FALSE
  )
}
