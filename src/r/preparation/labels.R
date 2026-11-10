stage5EmptyToNa <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) == "N/A"] <- NA_character_
  x
}

stage5NormalizeProposal <- function(x) {
  trimws(tolower(as.character(x)))
}

stage5MakePropChid <- function(proposal, choice_idx) {
  paste0(stage5NormalizeProposal(proposal), as.integer(choice_idx))
}

stage5RequireColumns <- function(df, cols, df_name) {
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in", df_name, ":", paste(missing_cols, collapse = ", ")))
  }
  invisible(df)
}

stage5EnsurePropChid <- function(df, df_name = "dataframe") {
  stage5RequireColumns(df, c("proposal", "choice_idx"), df_name)
  df$proposal <- stage5NormalizeProposal(df$proposal)
  df$choice_idx <- as.integer(df$choice_idx)
  if (!"prop_chid" %in% colnames(df)) {
    df$prop_chid <- stage5MakePropChid(df$proposal, df$choice_idx)
  } else {
    missing_prop_chid <- is.na(df$prop_chid) | trimws(as.character(df$prop_chid)) == ""
    df$prop_chid[missing_prop_chid] <- stage5MakePropChid(
      df$proposal[missing_prop_chid],
      df$choice_idx[missing_prop_chid]
    )
  }
  df
}

stage5CanonicalCoverageFlagColumns <- function() {
  c(
    "proposal_in_gpt4o_review_universe",
    "proposal_in_gpt4o_analysis_scope"
  )
}

stage5TruthyFlag <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x %in% c("1", "TRUE", "T", "YES", "Y")
}

stage5LogicalFlag <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  if (is.numeric(x) || is.integer(x)) {
    return(!is.na(x) & x != 0)
  }
  stage5TruthyFlag(x)
}

stage5AssertUniqueKey <- function(df, key_cols, df_name = "dataframe") {
  if (exists("assert_unique_key", mode = "function")) {
    return(assert_unique_key(df, key_cols, df_name))
  }
  stage5RequireColumns(df, key_cols, df_name)
  if (any(duplicated(df[, key_cols, drop = FALSE]))) {
    stop(paste("Duplicate key rows found in", df_name, "for key", paste(key_cols, collapse = ", ")))
  }
  invisible(df)
}

stage5LeftJoinChoiceLabels <- function(left_df, label_df, source_name, by = "prop_chid") {
  overlap_cols <- intersect(setdiff(colnames(left_df), by), setdiff(colnames(label_df), by))
  if (length(overlap_cols) > 0) {
    stop(paste(
      "Stage-5 label join would create overlapping non-key columns for",
      source_name,
      ":",
      paste(overlap_cols, collapse = ", ")
    ))
  }
  stage5AssertUniqueKey(label_df, by, paste(source_name, "labels"))
  before_n <- nrow(left_df)
  if (exists("audit_left_join_df", mode = "function")) {
    joined_df <- audit_left_join_df(left_df, label_df, by, "stage5_choices", paste(source_name, "labels"))
  } else {
    joined_df <- merge(left_df, label_df, by = by, all.x = TRUE, sort = FALSE)
  }
  if (nrow(joined_df) != before_n) {
    stop(paste("Stage-5 label join changed row count for", source_name, ":", before_n, "->", nrow(joined_df)))
  }
  joined_df
}

stage5ParseStanceList <- function(x) {
  x <- stage5EmptyToNa(x)
  if (length(x) == 0 || is.na(x[[1]])) {
    return(character())
  }
  x <- gsub("^\\s*\\[|\\]\\s*$", "", x[[1]])
  x <- trimws(x)
  if (!nzchar(x)) {
    return(character())
  }
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  parts <- gsub("^['\"]|['\"]$", "", parts)
  stage5EmptyToNa(parts)
}

stage5Llama33ConflictColumns <- function(df, cols) {
  conflicts <- cols[vapply(cols, function(col) {
    values <- as.character(stage5EmptyToNa(df[[col]]))
    values[is.na(values)] <- "<NA>"
    length(unique(values)) > 1L
  }, logical(1))]
  paste(conflicts, collapse = ";")
}

stage5BuildLlama33ConflictAudit <- function(df, key_cols, conflict_cols) {
  duplicate_rows <- duplicated(df[, key_cols, drop = FALSE]) |
    duplicated(df[, key_cols, drop = FALSE], fromLast = TRUE)
  if (!any(duplicate_rows)) {
    return(df[FALSE, , drop = FALSE])
  }

  duplicate_df <- df[duplicate_rows, , drop = FALSE]
  duplicate_keys <- unique(duplicate_df[, key_cols, drop = FALSE])
  audit_rows <- list()
  audit_i <- 1L

  for (i in seq_len(nrow(duplicate_keys))) {
    key <- duplicate_keys[i, , drop = FALSE]
    key_match <- rep(TRUE, nrow(duplicate_df))
    for (key_col in key_cols) {
      key_match <- key_match & duplicate_df[[key_col]] == key[[key_col]]
    }
    group_df <- duplicate_df[key_match, , drop = FALSE]
    conflict_columns <- stage5Llama33ConflictColumns(group_df, conflict_cols)
    if (!nzchar(conflict_columns)) {
      next
    }
    group_df$conflict_columns_llama33 <- conflict_columns
    group_df$duplicate_key_rows_llama33 <- nrow(group_df)
    audit_rows[[audit_i]] <- group_df
    audit_i <- audit_i + 1L
  }

  if (length(audit_rows) == 0) {
    return(df[FALSE, , drop = FALSE])
  }
  do.call(rbind, audit_rows)
}

readStage5Llama33ChoiceLabels <- function(paths, conflict_audit_path = NULL) {
  if (length(paths) == 0) {
    stop("No Llama 3.3 label files configured.")
  }
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    stop(paste("Missing Llama 3.3 label files:", paste(missing_paths, collapse = ", ")))
  }

  rows <- list()
  row_i <- 1L
  for (source_rank in seq_along(paths)) {
    path <- paths[[source_rank]]
    raw <- read.csv(
      path,
      header = FALSE,
      col.names = c("effect", "stances", "suggested_stance", "proposal", "raw_row_idx"),
      stringsAsFactors = FALSE,
      colClasses = "character"
    )
    raw$proposal <- stage5NormalizeProposal(raw$proposal)
    raw$effect <- stage5EmptyToNa(raw$effect)
    raw$suggested_stance <- stage5EmptyToNa(raw$suggested_stance)

    for (i in seq_len(nrow(raw))) {
      stances <- stage5ParseStanceList(raw$stances[[i]])
      if (length(stances) == 0) {
        next
      }
      choice_idx <- seq_along(stances)
      rows[[row_i]] <- data.frame(
        proposal = raw$proposal[[i]],
        choice_idx = choice_idx,
        prop_chid = stage5MakePropChid(raw$proposal[[i]], choice_idx),
        effect_llama33 = raw$effect[[i]],
        suggested_stance_llama33 = raw$suggested_stance[[i]],
        stance_llama33 = stances,
        source_file_llama33 = basename(path),
        source_rank_llama33 = source_rank,
        source_row_llama33 = i,
        stringsAsFactors = FALSE
      )
      row_i <- row_i + 1L
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      proposal = character(),
      choice_idx = integer(),
      prop_chid = character(),
      effect_llama33 = character(),
      suggested_stance_llama33 = character(),
      stance_llama33 = character(),
      source_file_llama33 = character(),
      llama33_label_conflict = logical(),
      conflict_columns_llama33 = character(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  out$llama33_label_conflict <- FALSE
  out$conflict_columns_llama33 <- NA_character_
  key_cols <- c("proposal", "choice_idx")
  conflict_cols <- c("effect_llama33", "suggested_stance_llama33", "stance_llama33")
  conflict_audit <- stage5BuildLlama33ConflictAudit(out, key_cols, conflict_cols)
  if (nrow(conflict_audit) > 0) {
    conflict_key_count <- nrow(unique(conflict_audit[, key_cols, drop = FALSE]))
    if (is.null(conflict_audit_path) || length(conflict_audit_path) == 0 || !nzchar(conflict_audit_path[[1]])) {
      stop(paste(
        "Conflicting duplicate Llama 3.3 labels found for",
        conflict_key_count,
        "proposal-choice keys. Supply conflict_audit_path to write an audit CSV and keep latest configured source_rank rows."
      ))
    }
    audit_dir <- dirname(conflict_audit_path)
    if (nzchar(audit_dir) && audit_dir != ".") {
      dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
    }
    write.csv(conflict_audit, conflict_audit_path, row.names = FALSE)
    conflict_keys <- unique(conflict_audit[, c(key_cols, "conflict_columns_llama33"), drop = FALSE])
    for (i in seq_len(nrow(conflict_keys))) {
      key_match <- out$proposal == conflict_keys$proposal[[i]] &
        out$choice_idx == conflict_keys$choice_idx[[i]]
      out$llama33_label_conflict[key_match] <- TRUE
      out$conflict_columns_llama33[key_match] <- conflict_keys$conflict_columns_llama33[[i]]
    }
    warning(paste(
      "Conflicting duplicate Llama 3.3 labels found for",
      conflict_key_count,
      "proposal-choice keys; wrote audit to",
      conflict_audit_path,
      "and kept latest configured source_rank rows."
    ), call. = FALSE)
  }

  duplicate_extra_count <- nrow(out) - nrow(unique(out[, key_cols, drop = FALSE]))
  if (duplicate_extra_count > 0) {
    message(paste(
      "Duplicate Llama 3.3 proposal-choice keys detected:",
      duplicate_extra_count,
      "extra duplicate rows; keeping latest configured source_rank rows."
    ))
  }

  out <- out[order(out$proposal, out$choice_idx, out$source_rank_llama33, out$source_row_llama33), ]
  out <- out[!duplicated(out[, c("proposal", "choice_idx"), drop = FALSE], fromLast = TRUE), ]
  out[c("source_rank_llama33", "source_row_llama33")] <- NULL
  rownames(out) <- NULL
  stage5AssertUniqueKey(out, "prop_chid", "df_llm_llama33")
  out
}

stage5HumanChoiceColumns <- function() {
  c(
    "proposal",
    "choice_idx",
    "prop_chid",
    "stance_hl",
    "suggested_stance_hl",
    "issue_hl",
    "n_evaluations",
    "n_non_low_effort_evaluations",
    "n_low_effort_evaluations",
    "n_approve",
    "n_reject",
    "n_abstain",
    "n_other",
    "n_effective_approve",
    "n_effective_reject",
    "n_effective_abstain",
    "n_effective_other",
    "low_effort_only",
    "label_conflict",
    "raw_label_conflict",
    "has_evaluator_label_conflict"
  )
}

stage5HumanChoiceJoinColumns <- function() {
  setdiff(stage5HumanChoiceColumns(), c("proposal", "choice_idx"))
}

stage5EmptyHumanChoiceLabels <- function() {
  cols <- stage5HumanChoiceColumns()
  values <- lapply(cols, function(col) character())
  names(values) <- cols
  df <- as.data.frame(values, stringsAsFactors = FALSE)

  integer_cols <- c(
    "choice_idx",
    "n_evaluations",
    "n_non_low_effort_evaluations",
    "n_low_effort_evaluations",
    "n_approve",
    "n_reject",
    "n_abstain",
    "n_other",
    "n_effective_approve",
    "n_effective_reject",
    "n_effective_abstain",
    "n_effective_other"
  )
  logical_cols <- c(
    "low_effort_only",
    "label_conflict",
    "raw_label_conflict",
    "has_evaluator_label_conflict"
  )
  for (col in integer_cols) {
    df[[col]] <- integer()
  }
  for (col in logical_cols) {
    df[[col]] <- logical()
  }
  df <- df[, cols, drop = FALSE]
  rownames(df) <- NULL
  df
}

readStage5HumanChoiceLabels <- function(path) {
  if (!file.exists(path)) {
    warning(paste(
      "Required human-label choice output is missing:",
      path,
      "Continuing with empty human-label frame; human columns will be NA after the Stage 5 join.",
      "Generate the artifact with: python run.py prepare --stage human-labels --data-root INPUT"
    ), call. = FALSE)
    return(stage5EmptyHumanChoiceLabels())
  }
  df <- read.csv(path, stringsAsFactors = FALSE, colClasses = c(proposal = "character"))
  stage5RequireColumns(df, stage5HumanChoiceColumns(), "human_label_choice_labels")
  df <- stage5EnsurePropChid(df, "human_label_choice_labels")
  df <- df[, stage5HumanChoiceColumns(), drop = FALSE]
  stage5AssertUniqueKey(df, "prop_chid", "human_label_choice_labels")
  df
}

stage5DefaultCoverageSources <- function() {
  list(
    oai = c("stance_oai", "suggested_stance_oai", "effect_oai"),
    llama = c("stance_llama", "suggested_stance_llama", "effect_llama"),
    llama33 = c("stance_llama33", "suggested_stance_llama33", "effect_llama33"),
    chatgpt = c("stance_chatgpt", "suggested_stance_chatgpt", "effect_chatgpt"),
    human = c("stance_hl", "suggested_stance_hl")
  )
}

stage5PlotModelLabels <- function(x, default = "Heuristic") {
  label_map <- c(
    stance_oai = "OAI",
    stance_llama = "Llama",
    stance_chatgpt = "ChatGPT",
    stance_llama33 = "Llama 3.3",
    stance_hl = "Human labels"
  )
  labels <- unname(label_map[as.character(x)])
  labels[is.na(labels) | !nzchar(labels)] <- default
  labels
}

stage5HasLabelValue <- function(df) {
  if (ncol(df) == 0) {
    return(rep(FALSE, nrow(df)))
  }
  values <- as.data.frame(lapply(df, stage5EmptyToNa), stringsAsFactors = FALSE)
  rowSums(!is.na(values)) > 0
}

stage5ProposalMembersForFlag <- function(df, flag_col, df_name = "stage5 choices") {
  stage5RequireColumns(df, c("proposal", flag_col), df_name)
  proposals <- stage5NormalizeProposal(df$proposal)
  unique(proposals[stage5TruthyFlag(df[[flag_col]])])
}

stage5CanonicalCoverageFlagsFromSource <- function(df_stage5_source) {
  stage5RequireColumns(df_stage5_source, "proposal", "stage5 enriched source")
  review_flag <- "proposal_in_gpt4o_review_universe"
  analysis_flag <- "proposal_in_gpt4o_analysis_scope"
  legacy_analysis_flag <- "valid_GPT4o_v1"

  if (review_flag %in% colnames(df_stage5_source)) {
    review_universe <- stage5ProposalMembersForFlag(df_stage5_source, review_flag, "stage5 enriched source")
  } else {
    review_universe <- unique(stage5NormalizeProposal(df_stage5_source$proposal))
  }

  if (analysis_flag %in% colnames(df_stage5_source)) {
    analysis_scope <- stage5ProposalMembersForFlag(df_stage5_source, analysis_flag, "stage5 enriched source")
  } else if (legacy_analysis_flag %in% colnames(df_stage5_source)) {
    analysis_scope <- stage5ProposalMembersForFlag(df_stage5_source, legacy_analysis_flag, "stage5 enriched source")
  } else {
    analysis_scope <- character()
  }

  list(
    proposal_in_gpt4o_review_universe = review_universe,
    proposal_in_gpt4o_analysis_scope = analysis_scope
  )
}

stage5NormalizeProposalCoverageFlags <- function(proposal_coverage_flags = NULL) {
  flag_names <- stage5CanonicalCoverageFlagColumns()
  if (!is.null(proposal_coverage_flags) && length(proposal_coverage_flags) > 0) {
    flag_names <- unique(c(flag_names, names(proposal_coverage_flags)))
  }

  out <- setNames(vector("list", length(flag_names)), flag_names)
  for (flag_name in flag_names) {
    values <- character()
    if (!is.null(proposal_coverage_flags) && flag_name %in% names(proposal_coverage_flags)) {
      values <- proposal_coverage_flags[[flag_name]]
    }
    out[[flag_name]] <- unique(stage5NormalizeProposal(values))
  }

  review_flag <- "proposal_in_gpt4o_review_universe"
  analysis_flag <- "proposal_in_gpt4o_analysis_scope"
  if (review_flag %in% names(out) && analysis_flag %in% names(out)) {
    missing_review_membership <- setdiff(out[[analysis_flag]], out[[review_flag]])
    if (length(missing_review_membership) > 0) {
      stop(paste(
        "proposal_in_gpt4o_analysis_scope must be a subset of proposal_in_gpt4o_review_universe;",
        "found",
        length(missing_review_membership),
        "proposal(s) outside the review universe."
      ))
    }
  }

  out
}

stage5AddProposalCoverageFlags <- function(df_choices, proposal_coverage_flags) {
  stage5RequireColumns(df_choices, "proposal", "stage5 choices")
  normalized_flags <- stage5NormalizeProposalCoverageFlags(proposal_coverage_flags)
  proposals <- stage5NormalizeProposal(df_choices$proposal)

  for (flag_name in names(normalized_flags)) {
    df_choices[[flag_name]] <- as.integer(proposals %in% normalized_flags[[flag_name]])
  }

  df_choices
}

stage5AssertCanonicalRowRetention <- function(
  df_stage4,
  df_stage5,
  stage4_name = "stage 4 choice table",
  stage5_name = "canonical stage-5 export"
) {
  df_stage4 <- stage5EnsurePropChid(df_stage4, stage4_name)
  df_stage5 <- stage5EnsurePropChid(df_stage5, stage5_name)
  stage5AssertUniqueKey(df_stage4, "prop_chid", stage4_name)
  stage5AssertUniqueKey(df_stage5, "prop_chid", stage5_name)

  if (nrow(df_stage5) != nrow(df_stage4)) {
    stop(paste(
      stage5_name,
      "must preserve every row from",
      stage4_name,
      ":",
      nrow(df_stage4),
      "->",
      nrow(df_stage5)
    ))
  }

  if (!identical(df_stage5$prop_chid, df_stage4$prop_chid)) {
    missing_keys <- setdiff(df_stage4$prop_chid, df_stage5$prop_chid)
    extra_keys <- setdiff(df_stage5$prop_chid, df_stage4$prop_chid)
    stop(paste(
      stage5_name,
      "must preserve the exact proposal-choice key set from",
      stage4_name,
      "(missing:",
      length(missing_keys),
      "extra:",
      length(extra_keys),
      ")."
    ))
  }

  invisible(df_stage5)
}

buildCanonicalStage5ChoiceTable <- function(
  df_stage4,
  df_stage5_source,
  proposal_coverage_flags = NULL
) {
  df_stage4 <- stage5EnsurePropChid(df_stage4, "stage 4 choice table")
  df_stage5_source <- stage5EnsurePropChid(df_stage5_source, "stage5 enriched source")
  stage5AssertUniqueKey(df_stage4, "prop_chid", "stage 4 choice table")
  stage5AssertUniqueKey(df_stage5_source, "prop_chid", "stage5 enriched source")

  if (is.null(proposal_coverage_flags)) {
    proposal_coverage_flags <- stage5CanonicalCoverageFlagsFromSource(df_stage5_source)
  }
  proposal_coverage_flags <- stage5NormalizeProposalCoverageFlags(proposal_coverage_flags)

  excluded_source_columns <- c("valid_GPT4o_v1", names(proposal_coverage_flags))
  stage5_delta_cols <- setdiff(
    colnames(df_stage5_source),
    c(colnames(df_stage4), excluded_source_columns)
  )

  out <- df_stage4
  if (length(stage5_delta_cols) > 0) {
    stage5_delta <- df_stage5_source[, c("prop_chid", stage5_delta_cols), drop = FALSE]
    out <- stage5LeftJoinChoiceLabels(out, stage5_delta, "canonical stage-5 enrichment")
  }

  out <- stage5AddProposalCoverageFlags(out, proposal_coverage_flags)
  stage5AssertCanonicalRowRetention(
    df_stage4,
    out,
    stage4_name = "stage 4 choice table",
    stage5_name = "canonical stage-5 export"
  )
  out
}

buildStage5LabelCoverageAudit <- function(df_choices, source_columns = stage5DefaultCoverageSources()) {
  stage5RequireColumns(df_choices, c("proposal"), "stage5 choices")
  proposals <- stage5NormalizeProposal(df_choices$proposal)
  proposal_levels <- unique(proposals)
  total_by_proposal <- table(factor(proposals, levels = proposal_levels))

  rows <- lapply(names(source_columns), function(source_name) {
    cols <- source_columns[[source_name]]
    stage5RequireColumns(df_choices, cols, paste("stage5 choices for", source_name))
    labeled <- stage5HasLabelValue(df_choices[, cols, drop = FALSE])
    labeled_by_proposal <- tapply(labeled, factor(proposals, levels = proposal_levels), sum)
    labeled_by_proposal[is.na(labeled_by_proposal)] <- 0
    reviewed_col <- paste0(source_name, "_reviewed")
    not_reviewed_col <- paste0(source_name, "_not_reviewed")
    no_suggestion_col <- paste0(source_name, "_no_suggestion")
    missing_label_col <- paste0(source_name, "_missing_label")
    conflict_col <- paste0(source_name, "_label_conflict")
    basic_exclusion_col <- paste0(source_name, "_basic_proposal_exclusion")

    reviewed <- if (reviewed_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[reviewed_col]])
    } else {
      labeled
    }
    not_reviewed <- if (not_reviewed_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[not_reviewed_col]])
    } else {
      !reviewed
    }
    no_suggestion <- if (no_suggestion_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[no_suggestion_col]])
    } else {
      rep(FALSE, nrow(df_choices))
    }
    missing_label <- if (missing_label_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[missing_label_col]])
    } else {
      rep(FALSE, nrow(df_choices))
    }
    conflict <- if (conflict_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[conflict_col]])
    } else {
      rep(FALSE, nrow(df_choices))
    }
    basic_exclusion <- if (basic_exclusion_col %in% colnames(df_choices)) {
      stage5LogicalFlag(df_choices[[basic_exclusion_col]])
    } else {
      rep(FALSE, nrow(df_choices))
    }

    data.frame(
      label_source = source_name,
      label_columns = paste(cols, collapse = ";"),
      total_rows = nrow(df_choices),
      labeled_rows = sum(labeled),
      reviewed_rows = sum(reviewed),
      not_reviewed_rows = sum(not_reviewed),
      no_suggestion_rows = sum(no_suggestion),
      missing_label_rows = sum(missing_label),
      conflict_rows = sum(conflict),
      basic_exclusion_rows = sum(basic_exclusion),
      total_proposals = length(proposal_levels),
      labeled_proposals = sum(labeled_by_proposal > 0),
      complete_proposals = sum(labeled_by_proposal == total_by_proposal),
      partial_proposals = sum(labeled_by_proposal > 0 & labeled_by_proposal < total_by_proposal),
      missing_proposals = sum(labeled_by_proposal == 0),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
