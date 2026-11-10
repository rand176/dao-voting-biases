stage1RequireColumns <- function(df, cols, df_name = "dataframe") {
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in", df_name, ":", paste(missing_cols, collapse = ", ")))
  }
}

stage1SafeDivide <- function(numerator, denominator) {
  numerator <- suppressWarnings(as.numeric(numerator))
  denominator <- suppressWarnings(as.numeric(denominator))
  out <- numerator / denominator
  out[is.na(denominator) | denominator == 0 | !is.finite(out)] <- NA_real_
  out
}

addStage1ChoiceShare <- function(df_choices) {
  stage1RequireColumns(df_choices, c("choice_score", "scores_total"), "df_choices")
  df_choices$choice_share <- stage1SafeDivide(df_choices$choice_score, df_choices$scores_total)
  df_choices
}

addStage1ChoiceCounts <- function(df_choices) {
  stage1RequireColumns(df_choices, "proposal", "df_choices")
  df_choices$N_choices <- as.integer(ave(seq_len(nrow(df_choices)), df_choices$proposal, FUN = length))
  df_choices
}

dropSingleOptionProposals <- function(df_choices, proposal_col = "proposal", n_choices_col = "N_choices") {
  stage1RequireColumns(df_choices, proposal_col, "df_choices")

  proposal_values <- trimws(tolower(as.character(df_choices[[proposal_col]])))
  current_counts <- as.integer(ave(seq_len(nrow(df_choices)), proposal_values, FUN = length))
  keep <- !is.na(proposal_values) & nzchar(proposal_values) & current_counts > 1

  if (n_choices_col %in% colnames(df_choices)) {
    reported_counts <- suppressWarnings(as.numeric(as.character(df_choices[[n_choices_col]])))
    keep <- keep & (is.na(reported_counts) | reported_counts > 1)
  }

  df_choices[keep, , drop = FALSE]
}

addStage1RandomAndExcessShare <- function(df_choices) {
  stage1RequireColumns(df_choices, c("N_choices", "choice_share"), "df_choices")
  df_choices$rnd_share <- ifelse(df_choices$N_choices > 0, 1 / df_choices$N_choices, NA_real_)
  df_choices$excess_share <- df_choices$choice_share - df_choices$rnd_share
  df_choices
}

addStage1AuthorShare <- function(df_choices) {
  stage1RequireColumns(df_choices, c("author_vp", "scores_total"), "df_choices")
  df_choices$author_share <- stage1SafeDivide(df_choices$author_vp, df_choices$scores_total)
  df_choices
}

stage1ProposalChoiceKey <- function(proposal, choice_idx) {
  proposal <- trimws(tolower(as.character(proposal)))
  choice_idx <- suppressWarnings(as.numeric(choice_idx))
  valid <- !is.na(proposal) & nzchar(proposal) & !is.na(choice_idx)
  key <- rep(NA_character_, length(proposal))
  key[valid] <- paste(proposal[valid], choice_idx[valid], sep = "::")
  key
}

stage1AuthorChoiceFlag <- function(df_choices, author_choices) {
  stage1RequireColumns(df_choices, c("proposal", "choice_idx"), "df_choices")
  stage1RequireColumns(author_choices, c("proposal", "author_choice"), "author_choices")

  choice_proposals <- trimws(tolower(as.character(df_choices$proposal)))
  author_proposals <- trimws(tolower(as.character(author_choices$proposal)))
  author_choice <- suppressWarnings(as.numeric(author_choices$author_choice))

  flag <- rep(NA_integer_, nrow(df_choices))

  choice_keys <- stage1ProposalChoiceKey(df_choices$proposal, df_choices$choice_idx)
  valid_author_choice <- !is.na(author_proposals) & nzchar(author_proposals) & !is.na(author_choice)
  author_keys <- unique(stage1ProposalChoiceKey(
    author_choices$proposal[valid_author_choice],
    author_choice[valid_author_choice]
  ))

  matched_author_choice <- !is.na(choice_keys) & choice_keys %in% author_keys
  proposals_with_matched_author_choice <- unique(choice_proposals[matched_author_choice])
  flag[choice_proposals %in% proposals_with_matched_author_choice] <- 0L
  flag[matched_author_choice] <- 1L

  flag
}
