loadProposalFakeFlags <- function(file_name = "proposal_fake_heuristics.csv.gz") {
  path <- requireExistingInputPath(file.path(PROPOSAL_FLAGS_DIR, file_name),file_name)
  if (!file.exists(path)) {
    stop(paste("Missing proposal fake heuristics file:", path))
  }

  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  df$id <- trimws(tolower(df$id))
  df
}

getFakeProposalIds <- function(strict_only = TRUE, categories = NULL, file_name = "proposal_fake_heuristics.csv.gz") {
  df <- loadProposalFakeFlags(file_name)
  df$id <- trimws(tolower(df$id))

  if (strict_only) {
    df <- df[df$flag_fake_strict == 1, ]
  } else {
    df <- df[df$flag_fake_broad == 1 | df$flag_fake_strict == 1, ]
  }

  if (!is.null(categories)) {
    df <- df[df$flag_category %in% categories, ]
  }

  unique(df$id)
}

applyFakeProposalExclusions <- function(df, proposal_col = "proposal", strict_only = TRUE, categories = NULL, file_name = "proposal_fake_heuristics.csv.gz") {
  if (!(proposal_col %in% colnames(df))) {
    stop(paste("Missing proposal column", proposal_col, "in dataframe passed to applyFakeProposalExclusions"))
  }

  proposal_ids <- getFakeProposalIds(strict_only = strict_only, categories = categories, file_name = file_name)
  proposal_values <- trimws(tolower(as.character(df[[proposal_col]])))
  df[!(proposal_values %in% proposal_ids), ]
}

loadProposalDuplicateBallotFlags <- function(file_name = "proposal_duplicate_ballot_flags.csv.gz") {
  path <- requireExistingInputPath(file.path(PROPOSAL_FLAGS_DIR, file_name),file_name)
  if (!file.exists(path)) {
    stop(paste("Missing proposal duplicate-ballot flags file:", path))
  }

  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  df$id <- trimws(tolower(df$id))
  df
}

loadProposalVoteIssueFlags <- function(file_name = "proposal_vote_issue_flags.csv.gz") {
  path <- requireExistingInputPath(file.path(PROPOSAL_FLAGS_DIR, file_name),file_name)
  if (!file.exists(path)) {
    stop(paste("Missing proposal vote-issue flags file:", path))
  }

  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = c(id = "character"))
  df$id <- trimws(tolower(df$id))
  df
}

getDuplicateBallotProposalIds <- function(file_name = "proposal_duplicate_ballot_flags.csv.gz") {
  df <- loadProposalDuplicateBallotFlags(file_name)
  df$id <- trimws(tolower(df$id))
  df <- df[df$flag_duplicate_ballots == 1, ]
  unique(df$id)
}

getProposalVoteIssueIds <- function(file_name = "proposal_vote_issue_flags.csv.gz") {
  df <- loadProposalVoteIssueFlags(file_name)
  df$id <- trimws(tolower(df$id))
  df <- df[df$flag_any_vote_issues == 1, ]
  unique(df$id)
}

applyDuplicateBallotExclusions <- function(df, proposal_col = "proposal", file_name = "proposal_duplicate_ballot_flags.csv.gz") {
  if (!(proposal_col %in% colnames(df))) {
    stop(paste("Missing proposal column", proposal_col, "in dataframe passed to applyDuplicateBallotExclusions"))
  }

  proposal_ids <- getDuplicateBallotProposalIds(file_name = file_name)
  proposal_values <- trimws(tolower(as.character(df[[proposal_col]])))
  df[!(proposal_values %in% proposal_ids), ]
}

applyProposalVoteIssueExclusions <- function(df, proposal_col = "proposal", file_name = "proposal_vote_issue_flags.csv.gz") {
  if (!(proposal_col %in% colnames(df))) {
    stop(paste("Missing proposal column", proposal_col, "in dataframe passed to applyProposalVoteIssueExclusions"))
  }

  proposal_ids <- getProposalVoteIssueIds(file_name = file_name)
  proposal_values <- trimws(tolower(as.character(df[[proposal_col]])))
  df[!(proposal_values %in% proposal_ids), ]
}

getProposalExclusionIds <- function(stage, include_duplicate_ballots = FALSE, file_name = "proposal_exclusions.csv", duplicate_file_name = "proposal_duplicate_ballot_flags.csv.gz", include_vote_issues = FALSE, vote_issue_file_name = "proposal_vote_issue_flags.csv.gz") {
  exceptions <- loadExceptionTable(file_name, c("stage", "proposal", "reason"))
  exceptions <- exceptions[exceptions$stage == stage, ]
  proposal_ids <- trimws(tolower(exceptions$proposal))

  if (include_duplicate_ballots) {
    proposal_ids <- c(proposal_ids, getDuplicateBallotProposalIds(file_name = duplicate_file_name))
  }
  if (include_vote_issues) {
    proposal_ids <- c(proposal_ids, getProposalVoteIssueIds(file_name = vote_issue_file_name))
  }

  unique(proposal_ids[nzchar(proposal_ids)])
}

loadExceptionTable <- function(file_name, expected_cols = NULL) {
  path <- file.path(EXCEPTIONS_DIR, file_name)
  if (!file.exists(path)) {
    if (is.null(expected_cols)) {
      return(data.frame())
    }
    out <- as.data.frame(setNames(replicate(length(expected_cols), character(0), simplify = FALSE), expected_cols))
    return(out)
  }

  df <- read.csv(path, stringsAsFactors = FALSE)
  if (!is.null(expected_cols)) {
    missing_cols <- setdiff(expected_cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(paste("Missing exception columns in", path, ":", paste(missing_cols, collapse = ", ")))
    }
  }
  df
}

applyProposalExclusions <- function(df, stage, proposal_col = "proposal", file_name = "proposal_exclusions.csv", include_duplicate_ballots = FALSE, duplicate_file_name = "proposal_duplicate_ballot_flags.csv.gz", include_vote_issues = FALSE, vote_issue_file_name = "proposal_vote_issue_flags.csv.gz") {
  proposal_ids <- getProposalExclusionIds(
    stage,
    include_duplicate_ballots = include_duplicate_ballots,
    file_name = file_name,
    duplicate_file_name = duplicate_file_name,
    include_vote_issues = include_vote_issues,
    vote_issue_file_name = vote_issue_file_name
  )
  if (length(proposal_ids) == 0) {
    return(df)
  }

  if (!(proposal_col %in% colnames(df))) {
    stop(paste("Missing proposal column", proposal_col, "in dataframe passed to applyProposalExclusions"))
  }

  proposal_values <- trimws(tolower(as.character(df[[proposal_col]])))
  df[!(proposal_values %in% proposal_ids), ]
}

applyChoiceSequenceOverrides <- function(df, stage, value_col = "stance2", file_name = "stance_choice_overrides.csv", warning_detail_limit = 10) {
  exceptions <- loadExceptionTable(file_name, c("stage", "proposal", "stance_sequence", "source_line", "note"))
  exceptions <- exceptions[exceptions$stage == stage, ]
  if (nrow(exceptions) == 0) {
    return(df)
  }

  df$proposal <- trimws(tolower(df$proposal))
  missing_proposals <- character()
  length_mismatches <- data.frame(
    proposal = character(),
    row_count = integer(),
    override_length = integer(),
    stringsAsFactors = FALSE
  )

  for (idx in seq_len(nrow(exceptions))) {
    proposal_id <- trimws(tolower(exceptions$proposal[idx]))
    override_values <- strsplit(exceptions$stance_sequence[idx], "|", fixed = TRUE)[[1]]
    proposal_rows <- which(df$proposal == proposal_id)

    if (length(proposal_rows) == 0) {
      missing_proposals <- c(missing_proposals, proposal_id)
      next
    }

    ordered_rows <- proposal_rows[order(df$choice_idx[proposal_rows], na.last = TRUE)]
    if (length(ordered_rows) != length(override_values)) {
      length_mismatches <- rbind(
        length_mismatches,
        data.frame(
          proposal = proposal_id,
          row_count = length(ordered_rows),
          override_length = length(override_values),
          stringsAsFactors = FALSE
        )
      )
      next
    }

    df[ordered_rows, value_col] <- override_values
  }

  if (length(missing_proposals) > 0) {
    shown <- head(missing_proposals, warning_detail_limit)
    warning(
      paste(
        "Skipping",
        length(missing_proposals),
        "documented stance overrides with no matching rows in current data.",
        "First proposals:",
        paste(shown, collapse = ", ")
      )
    )
  }

  if (nrow(length_mismatches) > 0) {
    shown <- head(length_mismatches, warning_detail_limit)
    shown_text <- paste0(
      shown$proposal,
      " (rows=",
      shown$row_count,
      ", overrides=",
      shown$override_length,
      ")"
    )
    warning(
      paste(
        "Skipping",
        nrow(length_mismatches),
        "documented stance overrides with row-count mismatches.",
        "First mismatches:",
        paste(shown_text, collapse = "; ")
      )
    )
  }

  df
}

applyProposalValueOverrides <- function(df, stage, file_name = "proposal_value_overrides.csv") {
  exceptions <- loadExceptionTable(file_name, c("stage", "proposal", "column_name", "value", "source_line", "note"))
  exceptions <- exceptions[exceptions$stage == stage, ]
  if (nrow(exceptions) == 0) {
    return(df)
  }

  df$proposal <- trimws(tolower(df$proposal))
  for (idx in seq_len(nrow(exceptions))) {
    proposal_id <- trimws(tolower(exceptions$proposal[idx]))
    column_name <- exceptions$column_name[idx]
    if (!(column_name %in% colnames(df))) {
      warning(paste("Skipping documented value override for missing column", column_name))
      next
    }
    typed_value <- type.convert(as.character(exceptions$value[idx]), as.is = TRUE)
    df[df$proposal == proposal_id, column_name] <- typed_value
  }

  df
}

applySpaceExclusions <- function(df, stage, space_col = "space", file_name = "space_exclusions.csv") {
  exceptions <- loadExceptionTable(file_name, c("stage", "space", "reason"))
  exceptions <- exceptions[exceptions$stage == stage, ]
  if (nrow(exceptions) == 0) {
    return(df)
  }

  if (!(space_col %in% colnames(df))) {
    stop(paste("Missing space column", space_col, "in dataframe passed to applySpaceExclusions"))
  }

  excluded_spaces <- unique(trimws(tolower(as.character(exceptions$space))))
  space_values <- trimws(tolower(as.character(df[[space_col]])))
  df[!(space_values %in% excluded_spaces), ]
}

applySpaceValueOverrides <- function(df, stage, space_col = "space", file_name = "space_value_overrides.csv") {
  exceptions <- loadExceptionTable(file_name, c("stage", "space", "column_name", "value", "source_line", "note"))
  exceptions <- exceptions[exceptions$stage == stage, ]
  if (nrow(exceptions) == 0) {
    return(df)
  }

  if (!(space_col %in% colnames(df))) {
    stop(paste("Missing space column", space_col, "in dataframe passed to applySpaceValueOverrides"))
  }

  out <- df
  space_values <- trimws(tolower(as.character(out[[space_col]])))

  for (idx in seq_len(nrow(exceptions))) {
    space_name <- trimws(tolower(exceptions$space[idx]))
    column_name <- exceptions$column_name[idx]
    if (!(column_name %in% colnames(out))) {
      warning(paste("Skipping documented space-level value override for missing column", column_name))
      next
    }

    typed_value <- type.convert(as.character(exceptions$value[idx]), as.is = TRUE)
    out[space_values == space_name, column_name] <- typed_value
  }

  out
}

normalizeStage2StanceLabels <- function(df, value_col = "stance2") {
  if (!(value_col %in% colnames(df))) {
    stop(paste("Missing stance column", value_col, "in dataframe passed to normalizeStage2StanceLabels"))
  }

  out <- df
  normalized_values <- trimws(as.character(out[[value_col]]))
  normalized_values[toupper(normalized_values) == "A"] <- "APPROVE"
  normalized_values[toupper(normalized_values) == "ABSTAIN"] <- "ABSTAIN"
  normalized_values[toupper(normalized_values) == "APPROVE"] <- "APPROVE"
  normalized_values[toupper(normalized_values) == "REJECT"] <- "REJECT"
  normalized_values[toupper(normalized_values) == "OTHER"] <- "OTHER"
  out[[value_col]] <- normalized_values
  out
}

loadStage2OneSidedStanceExceptions <- function(stage, file_name = "stance_one_sided_exceptions.csv") {
  exceptions <- loadExceptionTable(
    file_name,
    c("stage", "proposal", "allowed_state", "source_line", "note")
  )
  exceptions <- exceptions[exceptions$stage == stage, ]
  if (nrow(exceptions) == 0) {
    return(exceptions)
  }

  exceptions$proposal <- trimws(tolower(as.character(exceptions$proposal)))
  exceptions$allowed_state <- trimws(tolower(as.character(exceptions$allowed_state)))
  allowed_states <- c("approve_only", "reject_only")
  unknown_states <- sort(unique(exceptions$allowed_state[
    !(exceptions$allowed_state %in% allowed_states) &
      !is.na(exceptions$allowed_state) &
      nzchar(exceptions$allowed_state)
  ]))
  if (length(unknown_states) > 0) {
    stop(paste(
      "Unknown one-sided stance exception states:",
      paste(unknown_states, collapse = ", ")
    ))
  }

  exceptions
}

getDocumentedStage2StanceProposalIds <- function(stage, value_col = "stance2") {
  exceptions <- loadStage2OneSidedStanceExceptions(stage)
  valid_proposals <- !is.na(exceptions$proposal) & nzchar(exceptions$proposal)
  unique(exceptions$proposal[valid_proposals])
}

getDocumentedStage2StanceMetadata <- function(stage, value_col = "stance2") {
  sequence_overrides <- loadExceptionTable(
    "stance_choice_overrides.csv",
    c("stage", "proposal", "stance_sequence", "source_line", "note")
  )
  sequence_overrides <- sequence_overrides[sequence_overrides$stage == stage, ]

  proposal_overrides <- loadExceptionTable(
    "proposal_value_overrides.csv",
    c("stage", "proposal", "column_name", "value", "source_line", "note")
  )
  proposal_overrides <- proposal_overrides[
    proposal_overrides$stage == stage & proposal_overrides$column_name == value_col,
  ]

  space_overrides <- loadExceptionTable(
    "space_value_overrides.csv",
    c("stage", "space", "column_name", "value", "source_line", "note")
  )
  space_overrides <- space_overrides[
    space_overrides$stage == stage & space_overrides$column_name == value_col,
  ]
  one_sided_exceptions <- loadStage2OneSidedStanceExceptions(stage)

  list(
    choice_sequence_proposals = unique(trimws(tolower(sequence_overrides$proposal))),
    proposal_value_proposals = unique(trimws(tolower(proposal_overrides$proposal))),
    space_value_spaces = unique(trimws(tolower(space_overrides$space))),
    one_sided_exception_proposals = unique(one_sided_exceptions$proposal),
    one_sided_exceptions = one_sided_exceptions
  )
}

buildStanceProposalStateAudit <- function(df, stage, checkpoint, value_col = "stance2", proposal_col = "proposal", space_col = "space") {
  required_cols <- c(proposal_col, value_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in buildStanceProposalStateAudit:", paste(missing_cols, collapse = ", ")))
  }

  proposal_values <- trimws(tolower(as.character(df[[proposal_col]])))
  stance_values <- trimws(as.character(df[[value_col]]))
  by_proposal <- split(stance_values, proposal_values)
  proposal_ids <- names(by_proposal)
  metadata <- getDocumentedStage2StanceMetadata(stage, value_col = value_col)

  space_by_proposal <- NULL
  if (space_col %in% colnames(df)) {
    space_values <- trimws(tolower(as.character(df[[space_col]])))
    space_by_proposal <- split(space_values, proposal_values)
  }

  title_by_proposal <- NULL
  if ("title" %in% colnames(df)) {
    title_by_proposal <- split(as.character(df$title), proposal_values)
  }

  first_value <- function(values) {
    values <- values[!is.na(values) & nzchar(values)]
    if (length(values) == 0) return("")
    values[1]
  }

  state_for_values <- function(values) {
    has_approve <- any(values == "APPROVE", na.rm = TRUE)
    has_reject <- any(values == "REJECT", na.rm = TRUE)
    if (has_approve && has_reject) return("both")
    if (has_approve && !has_reject) return("approve_only")
    if (has_reject && !has_approve) return("reject_only")
    "neither"
  }

  stance_states <- vapply(by_proposal, state_for_values, character(1))
  normalized_proposals <- attr(df, "normalized_one_sided_stage2_proposals")
  if (is.null(normalized_proposals)) {
    normalized_proposals <- character()
  }
  normalized_proposals <- unique(trimws(tolower(as.character(normalized_proposals))))
  normalized_states <- attr(df, "normalized_one_sided_stage2_states")
  if (is.null(normalized_states)) {
    normalized_states <- character()
  }
  normalized_state_matches <- match(proposal_ids, names(normalized_states))
  original_stance_states <- stance_states
  matched_normalized_states <- !is.na(normalized_state_matches)
  original_stance_states[matched_normalized_states] <- normalized_states[
    normalized_state_matches[matched_normalized_states]
  ]

  is_current_one_sided <- stance_states %in% c("approve_only", "reject_only")
  is_normalized_one_sided <- proposal_ids %in% normalized_proposals
  is_one_sided_approve_reject <- is_current_one_sided | is_normalized_one_sided
  one_sided_exceptions <- metadata$one_sided_exceptions
  is_one_sided_documented_exception <- mapply(
    function(proposal_id, stance_state) {
      any(
        one_sided_exceptions$proposal == proposal_id &
          one_sided_exceptions$allowed_state == stance_state,
        na.rm = TRUE
      )
    },
    proposal_ids,
    original_stance_states
  )
  is_one_sided_documented_exception <- is_one_sided_documented_exception & is_current_one_sided
  one_sided_action <- ifelse(
    is_normalized_one_sided,
    "normalized_to_other",
    ifelse(
      is_current_one_sided & is_one_sided_documented_exception,
      "kept_documented",
      ifelse(is_current_one_sided, "normalized_to_other", "not_applicable")
    )
  )

  metadata_sources <- vapply(proposal_ids, function(proposal_id) {
    sources <- character()
    if (proposal_id %in% metadata$choice_sequence_proposals) {
      sources <- c(sources, "stance_choice_overrides")
    }
    if (proposal_id %in% metadata$proposal_value_proposals) {
      sources <- c(sources, "proposal_value_overrides")
    }
    if (!is.null(space_by_proposal)) {
      proposal_spaces <- unique(space_by_proposal[[proposal_id]])
      if (any(proposal_spaces %in% metadata$space_value_spaces)) {
        sources <- c(sources, "space_value_overrides")
      }
    }
    if (proposal_id %in% metadata$one_sided_exception_proposals) {
      sources <- c(sources, "stance_one_sided_exceptions")
    }
    paste(unique(sources), collapse = "|")
  }, character(1))

  out <- data.frame(
    stage = stage,
    checkpoint = checkpoint,
    proposal = proposal_ids,
    row_count = as.integer(vapply(by_proposal, length, integer(1))),
    stance_state = stance_states,
    has_approve = vapply(by_proposal, function(values) any(values == "APPROVE", na.rm = TRUE), logical(1)),
    has_reject = vapply(by_proposal, function(values) any(values == "REJECT", na.rm = TRUE), logical(1)),
    has_abstain = vapply(by_proposal, function(values) any(values == "ABSTAIN", na.rm = TRUE), logical(1)),
    is_one_sided_approve_reject = is_one_sided_approve_reject,
    is_one_sided_documented_exception = is_one_sided_documented_exception,
    one_sided_action = one_sided_action,
    has_existing_stance_metadata = nzchar(metadata_sources),
    existing_stance_metadata_sources = metadata_sources,
    stringsAsFactors = FALSE
  )

  if (!is.null(space_by_proposal)) {
    out$space <- vapply(proposal_ids, function(proposal_id) first_value(unique(space_by_proposal[[proposal_id]])), character(1))
  }
  if (!is.null(title_by_proposal)) {
    out$title <- vapply(proposal_ids, function(proposal_id) first_value(title_by_proposal[[proposal_id]]), character(1))
  }

  out
}

buildStanceChoiceMatchAudit <- function(df, match_df, stage, checkpoint, value_col = "stance2", proposal_col = "proposal", space_col = "space", choice_idx_col = "choice_idx", choice_text_col = "choice_text") {
  if (nrow(df) != nrow(match_df)) {
    stop("Choice match audit row count must match the source dataframe row count.")
  }
  if (!(proposal_col %in% colnames(df))) {
    stop(paste("Missing proposal column", proposal_col, "in buildStanceChoiceMatchAudit"))
  }
  if (!(value_col %in% colnames(df))) {
    stop(paste("Missing stance column", value_col, "in buildStanceChoiceMatchAudit"))
  }

  column_or_default <- function(column_name, default = "") {
    if (column_name %in% colnames(df)) {
      return(as.character(df[[column_name]]))
    }
    rep(default, nrow(df))
  }

  required_match_cols <- c(
    "resolved_stance",
    "match_source",
    "matched_pattern",
    "canonical_pattern",
    "all_match_sources",
    "has_conflicting_matches"
  )
  missing_match_cols <- setdiff(required_match_cols, colnames(match_df))
  if (length(missing_match_cols) > 0) {
    stop(paste("Missing raw match audit columns:", paste(missing_match_cols, collapse = ", ")))
  }

  raw_match_df <- match_df[, required_match_cols, drop = FALSE]
  colnames(raw_match_df) <- paste0("raw_", colnames(raw_match_df))
  current_stance <- trimws(as.character(df[[value_col]]))

  out <- data.frame(
    stage = stage,
    checkpoint = checkpoint,
    proposal = trimws(tolower(as.character(df[[proposal_col]]))),
    space = column_or_default(space_col),
    choice_idx = column_or_default(choice_idx_col),
    choice_text = column_or_default(choice_text_col),
    current_stance = current_stance,
    resolved_stance = current_stance,
    stringsAsFactors = FALSE
  )

  cbind(out, raw_match_df)
}

normalizeOneSidedStage2Stances <- function(df, stage, value_col = "stance2", proposal_col = "proposal") {
  required_cols <- c(proposal_col, value_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in normalizeOneSidedStage2Stances:", paste(missing_cols, collapse = ", ")))
  }

  out <- df
  out[[proposal_col]] <- trimws(tolower(as.character(out[[proposal_col]])))
  documented_exceptions <- loadStage2OneSidedStanceExceptions(stage)

  by_proposal <- split(trimws(as.character(out[[value_col]])), out[[proposal_col]])
  state_for_values <- function(values) {
    has_approve <- any(values == "APPROVE", na.rm = TRUE)
    has_reject <- any(values == "REJECT", na.rm = TRUE)
    if (has_approve && has_reject) return("both")
    if (has_approve && !has_reject) return("approve_only")
    if (has_reject && !has_approve) return("reject_only")
    "neither"
  }
  stance_states <- vapply(by_proposal, state_for_values, character(1))
  one_sided_proposals <- names(stance_states[stance_states %in% c("approve_only", "reject_only")])
  one_sided_states <- stance_states[one_sided_proposals]
  if (length(one_sided_proposals) == 0) {
    attr(out, "normalized_one_sided_stage2_proposals") <- character()
    attr(out, "normalized_one_sided_stage2_states") <- character()
    return(out)
  }

  documented_one_sided <- mapply(
    function(proposal_id, stance_state) {
      any(
        documented_exceptions$proposal == proposal_id &
          documented_exceptions$allowed_state == stance_state,
        na.rm = TRUE
      )
    },
    one_sided_proposals,
    one_sided_states
  )
  undocumented_one_sided <- one_sided_proposals[!documented_one_sided]
  undocumented_one_sided_states <- one_sided_states[!documented_one_sided]
  names(undocumented_one_sided_states) <- undocumented_one_sided

  attr(out, "normalized_one_sided_stage2_proposals") <- undocumented_one_sided
  attr(out, "normalized_one_sided_stage2_states") <- undocumented_one_sided_states
  if (length(undocumented_one_sided) == 0) {
    return(out)
  }

  out[out[[proposal_col]] %in% undocumented_one_sided, value_col] <- "OTHER"
  out
}

applyStage2StanceOverrides <- function(df, stage, value_col = "stance2", choice_col = "choice_text") {
  out <- applySpaceValueOverrides(df, stage = stage)
  out <- applyProposalValueOverrides(out, stage = stage)
  out <- applyChoiceSequenceOverrides(out, stage = stage, value_col = value_col)
  out <- normalizeStage2StanceLabels(out, value_col = value_col)
  validateStage2Choices(out, value_col = value_col, choice_col = choice_col)
  out <- normalizeOneSidedStage2Stances(out, stage = stage, value_col = value_col)
  validateStage2Choices(out, value_col = value_col, choice_col = choice_col)
  out
}
