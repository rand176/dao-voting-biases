capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

assert_unique_key <- function(df, key_cols, df_name = "dataframe") {
  if (length(key_cols) == 0) {
    stop("assert_unique_key requires at least one key column")
  }
  missing_cols <- setdiff(key_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing key columns in", df_name, ":", paste(missing_cols, collapse = ", ")))
  }
  key_df <- df[, key_cols, drop = FALSE]
  duplicated_rows <- duplicated(key_df)
  if (any(duplicated_rows)) {
    stop(
      paste(
        "Duplicate key rows found in",
        df_name,
        "for key",
        paste(key_cols, collapse = ", ")
      )
    )
  }
  invisible(df)
}

audit_left_join_df <- function(left_df, right_df, by, left_name = "left_df", right_name = "right_df") {
  assert_unique_key(right_df, by, right_name)
  before_n <- nrow(left_df)
  joined_df <- left_join(left_df, right_df, by = by)
  after_n <- nrow(joined_df)
  if (before_n != after_n) {
    stop(
      paste(
        "Left join changed row count:",
        left_name,
        before_n,
        "->",
        after_n,
        "when joining",
        right_name
      )
    )
  }
  message(
    paste(
      "Left join OK:",
      left_name,
      before_n,
      "rows;",
      right_name,
      nrow(right_df),
      "rows"
    )
  )
  joined_df
}

process_df_choices <- function(df_choices) {
  df_choices$proposal <- trimws(tolower(df_choices$proposal))

  if ("type" %in% colnames(df_choices)) {
    df_choices$Type <- capitalize_first(df_choices$type)
    df_choices$Type <- factor(
      df_choices$Type,
      levels = c("Single-choice", "Basic", "Approval", "Quadratic", "Ranked-choice", "Weighted")
    )
    df_choices$type <- factor(
      df_choices$type,
      levels = c("single-choice", "basic", "quadratic", "weighted", "ranked-choice", "approval")
    )
    df_choices$is_single_preference <- ifelse(df_choices$type %in% c("single-choice", "basic"), 1, 0)
  }

  if ("choice_idx" %in% colnames(df_choices)) {
    df_choices$is_first_choice <- ifelse(df_choices$choice_idx == 1, 1, 0)
    df_choices$is_first_choice_str <- ifelse(df_choices$is_first_choice == 1, "First choice", "Others")
    df_choices$is_first_choice_str <- factor(df_choices$is_first_choice_str, levels = c("First choice", "Others"))
    df_choices$is_first_choice_str2 <- ifelse(df_choices$is_first_choice == 1, "First", "Others")
    df_choices$is_first_choice_str2 <- factor(df_choices$is_first_choice_str2, levels = c("First", "Others"))
  }

  if ("created_relative" %in% colnames(df_choices)) {
    df_choices$created_rel <- df_choices$created_relative
  }

  if ("choice_share" %in% colnames(df_choices)) {
    df_choices$choice_share <- NULL
  }
  if ("stance" %in% colnames(df_choices)) {
    df_choices$all_other <- ave(
      as.character(df_choices$stance),
      df_choices$proposal,
      FUN = function(x) as.integer(all(x == "OTHER"))
    )
    df_choices$stance <- factor(df_choices$stance, levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER"))
  }

  if ("N_choices" %in% colnames(df_choices)) {
    df_choices$N_choices10 <- ifelse(df_choices$N_choices <= 10, df_choices$N_choices, "10+")
    df_choices$N_choices10 <- ordered(df_choices$N_choices10, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))
    df_choices$N_choices5 <- ifelse(df_choices$N_choices <= 5, df_choices$N_choices, "5+")
    df_choices$N_choices5 <- ordered(df_choices$N_choices5, levels = c("1", "2", "3", "4", "5", "5+"))
  }

  if ("choice_idx" %in% colnames(df_choices)) {
    df_choices$choice_idx5 <- ifelse(df_choices$choice_idx <= 5, df_choices$choice_idx, "5+")
    df_choices$choice_idx5 <- ordered(df_choices$choice_idx5, levels = c("1", "2", "3", "4", "5", "5+"))
  }

  if ("author_choice_stance" %in% colnames(df_choices)) {
    df_choices$author_choice_stance <- ifelse(df_choices$author_choice_stance == "", NA, df_choices$author_choice_stance)
  }

  if ("is_infavor" %in% colnames(df_choices)) {
    df_choices$is_infavor_str <- ifelse(df_choices$is_infavor == 1, "Approve", "Others")
    df_choices$is_infavor_str <- factor(df_choices$is_infavor_str, levels = c("Approve", "Others"))
  }

  if ("author_choice_stance_compact" %in% colnames(df_choices)) {
    df_choices$author_choice_stance_compact <- factor(
      df_choices$author_choice_stance_compact,
      levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER", "MIXED")
    )
  }

  if ("is_author_choice2" %in% colnames(df_choices)) {
    df_choices$is_author_choice_str2 <- ifelse(
      is.na(df_choices$is_author_choice2),
      "No Auth.\nVote",
      ifelse(df_choices$is_author_choice2, "Author", "Others")
    )
    df_choices$is_author_choice_str2 <- factor(
      df_choices$is_author_choice_str2,
      levels = c("Author", "Others", "No Auth.\nVote")
    )
  }

  if (all(c("is_author_choice2", "choice_won") %in% colnames(df_choices))) {
    df_choices$won_author_choice <- ifelse(df_choices$is_author_choice2 == TRUE & df_choices$choice_won == 1, 1, 0)
  }

  if (all(c("is_infavor", "choice_won") %in% colnames(df_choices))) {
    df_choices$won_approve <- ifelse(df_choices$is_infavor == 1 & df_choices$choice_won == 1, 1, 0)
  }

  df_choices
}

validateStage2Choices <- function(df, value_col = "stance2", choice_col = "choice_text") {
  if (!(value_col %in% colnames(df))) {
    stop(paste("Missing stance column", value_col, "in dataframe passed to validateStage2Choices"))
  }
  if (!(choice_col %in% colnames(df))) {
    stop(paste("Missing choice column", choice_col, "in dataframe passed to validateStage2Choices"))
  }

  choice_values <- trimws(as.character(df[[choice_col]]))
  missing_choice_mask <- is.na(df[[choice_col]]) | is.na(choice_values) | choice_values == ""
  if (any(missing_choice_mask)) {
    stop("Stage 2 has missing choice_text after exclusions; document an exclusion or fill the value upstream.")
  }

  stance_values <- trimws(as.character(df[[value_col]]))
  allowed_stances <- c("APPROVE", "REJECT", "ABSTAIN", "OTHER")
  unknown_labels <- sort(unique(stance_values[!(stance_values %in% allowed_stances) & !is.na(stance_values) & stance_values != ""]))
  if (length(unknown_labels) > 0) {
    stop(paste("Unknown stance labels in stage 2:", paste(unknown_labels, collapse = ", ")))
  }

  invisible(df)
}

classifyChoiceIdxValues <- function(choice_idx) {
  raw_values <- as.character(choice_idx)
  trimmed_values <- trimws(raw_values)
  is_missing <- is.na(choice_idx) | is.na(trimmed_values) | trimmed_values == ""
  categories <- rep("positive_integer", length(trimmed_values))

  categories[is_missing] <- "na"
  categories[!is_missing & grepl("^0x", trimmed_values, ignore.case = TRUE)] <- "hex"
  categories[!is_missing & grepl("\"", trimmed_values, fixed = TRUE)] <- "quoted"
  categories[!is_missing & trimmed_values == "{"] <- "brace"
  categories[!is_missing & trimmed_values == "0"] <- "zero"
  categories[!is_missing & trimmed_values == "-1"] <- "minus_one"

  unresolved <- !is_missing & categories == "positive_integer"
  unresolved_idx <- which(unresolved)
  unresolved_is_positive_integer <- grepl("^[1-9][0-9]*$", trimmed_values[unresolved_idx])
  categories[unresolved_idx[!unresolved_is_positive_integer]] <- "other_text"
  categories
}

cleanStage3VoteCounts <- function(df, stage = "3_merging_N_votes_v2", include_duplicate_ballots = TRUE, include_vote_issues = FALSE, proposal_col = "proposal", choice_col = "choice_idx", count_col = "n_votes", total_col = "tot_votes", overrides_file = "proposal_value_overrides.csv") {
  required_cols <- c(proposal_col, choice_col, count_col, total_col)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required stage-3 vote-count columns:", paste(missing_cols, collapse = ", ")))
  }

  out <- df
  out[[proposal_col]] <- trimws(tolower(as.character(out[[proposal_col]])))
  out[[choice_col]] <- trimws(as.character(out[[choice_col]]))
  out[[count_col]] <- type.convert(as.character(out[[count_col]]), as.is = TRUE)
  out[[total_col]] <- type.convert(as.character(out[[total_col]]), as.is = TRUE)

  audit <- data.frame(
    phase = character(0),
    metric = character(0),
    value = numeric(0),
    detail = character(0),
    stringsAsFactors = FALSE
  )

  append_audit <- function(phase, metric, value, detail = "") {
    audit <<- rbind(
      audit,
      data.frame(
        phase = phase,
        metric = metric,
        value = as.numeric(value),
        detail = as.character(detail),
        stringsAsFactors = FALSE
      )
    )
  }

  append_audit("load_vote_counts", "rows_before", nrow(out))
  append_audit("load_vote_counts", "proposals_before", length(unique(out[[proposal_col]])))

  proposals_before <- unique(out[[proposal_col]])
  out <- applyProposalExclusions(
    out,
    stage = stage,
    proposal_col = proposal_col,
    include_duplicate_ballots = include_duplicate_ballots,
    include_vote_issues = include_vote_issues
  )
  proposals_after <- unique(out[[proposal_col]])
  append_audit("apply_stage_exclusions", "rows_after", nrow(out))
  append_audit("apply_stage_exclusions", "proposals_removed", length(setdiff(proposals_before, proposals_after)))

  if (nrow(out) == 0) {
    return(list(data = out, audit = audit))
  }

  out$choice_idx_issue <- classifyChoiceIdxValues(out[[choice_col]])
  invalid_rows <- out[out$choice_idx_issue != "positive_integer", , drop = FALSE]
  if (nrow(invalid_rows) > 0) {
    invalid_summary <- invalid_rows %>%
      group_by(choice_idx_issue) %>%
      summarise(
        rows_removed = n(),
        invalid_vote_mass = sum(.data[[count_col]], na.rm = TRUE),
        .groups = "drop"
      )

    for (idx in seq_len(nrow(invalid_summary))) {
      append_audit("remove_invalid_choice_idx", "rows_removed", invalid_summary$rows_removed[idx], invalid_summary$choice_idx_issue[idx])
      append_audit("remove_invalid_choice_idx", "invalid_vote_mass", invalid_summary$invalid_vote_mass[idx], invalid_summary$choice_idx_issue[idx])
    }

    invalid_vote_totals <- invalid_rows %>%
      group_by(.data[[proposal_col]]) %>%
      summarise(invalid_vote_mass = sum(.data[[count_col]], na.rm = TRUE), .groups = "drop")

    out <- out[out$choice_idx_issue == "positive_integer", , drop = FALSE]
    out <- audit_left_join_df(out, invalid_vote_totals, proposal_col, "stage3_vote_counts", "invalid_vote_totals")
    out$invalid_vote_mass <- ifelse(is.na(out$invalid_vote_mass), 0, out$invalid_vote_mass)
    out[[total_col]] <- out[[total_col]] - out$invalid_vote_mass
    out$invalid_vote_mass <- NULL
  }

  out <- applyProposalValueOverrides(out, stage = stage, file_name = overrides_file)
  append_audit("apply_value_overrides", "rows_after", nrow(out))

  out[[choice_col]] <- as.integer(out[[choice_col]])
  if (any(is.na(out[[choice_col]]) | out[[choice_col]] <= 0)) {
    stop("Invalid choice_idx values remain after stage-3 vote-count cleanup")
  }

  out$choice_idx_issue <- NULL
  list(data = out, audit = audit)
}

zeroFillMissingFavoriteCounts <- function(df_votes, df_choices, proposal_col = "proposal", choice_col = "choice_idx", total_col = "tot_votes", count_col = "n_votes") {
  vote_counts <- df_votes
  vote_counts[[proposal_col]] <- trimws(tolower(as.character(vote_counts[[proposal_col]])))
  vote_counts[[choice_col]] <- as.integer(vote_counts[[choice_col]])
  if (!("prop_chid" %in% colnames(vote_counts))) {
    vote_counts$prop_chid <- paste0(vote_counts[[proposal_col]], vote_counts[[choice_col]])
  }

  choices <- df_choices
  choices[[proposal_col]] <- trimws(tolower(as.character(choices[[proposal_col]])))
  choices[[choice_col]] <- as.integer(choices[[choice_col]])
  if (!("prop_chid" %in% colnames(choices))) {
    choices$prop_chid <- paste0(choices[[proposal_col]], choices[[choice_col]])
  }

  missing_choices <- choices[!(choices$prop_chid %in% vote_counts$prop_chid), c(proposal_col, choice_col, "prop_chid"), drop = FALSE]
  if (nrow(missing_choices) == 0) {
    return(vote_counts)
  }

  proposal_totals <- vote_counts[, c(proposal_col, total_col), drop = FALSE]
  proposal_totals <- proposal_totals[!duplicated(proposal_totals[[proposal_col]]), , drop = FALSE]
  assert_unique_key(proposal_totals, proposal_col, "proposal_totals")
  missing_choices <- audit_left_join_df(missing_choices, proposal_totals, proposal_col, "missing_choices", "proposal_totals")
  if (any(is.na(missing_choices[[total_col]]))) {
    stop("Missing tot_votes while zero-filling favorite counts")
  }
  missing_choices[[count_col]] <- 0

  aligned_vote_counts <- vote_counts
  aligned_vote_counts <- aligned_vote_counts[, colnames(aligned_vote_counts), drop = FALSE]
  missing_choices <- missing_choices[, colnames(aligned_vote_counts), drop = FALSE]
  rbind(aligned_vote_counts, missing_choices)
}

augmentChoiceDf <- function(df) {

  ## Quorum stats
  df = merge(df, df_quorum_stats, by="proposal")

  ## Add number of choices
  mysm_prop <- summarySE(df, "choice_share", c("proposal"), na.rm=TRUE)
  mysm_prop <- mysm_prop[, c("proposal", "N")]
  colnames(mysm_prop)[2] <- "N_choices"
  df <- merge(df, mysm_prop, by="proposal")
  nrow(df)

  ## Choice len stats

  mysm_choice_len <- summarySE(df, "choice_text_len", "proposal")
  colnames(df_quorum_stats)[1] <- "proposal"

  mysm_choice_len <- mysm_choice_len[,c("proposal", "choice_text_len", "sd")]
  colnames(mysm_choice_len) <- c("proposal", "choice_text_len_avg", "choice_text_len_sd")

  df = merge(df, mysm_choice_len, by="proposal")
  df$choice_text_len_rel = df$choice_text_len / df$choice_text_len_avg


  ## Add spaces settings

  df = merge(df, df_spaces_settings, by="space")

  ## Add spaces delegates

  df = merge(df, df_delegates, by="proposal")


  ## Support / Not Support

  nos <- c("No",
           "Against",
           "Reject",
           "否",
           "不是",
           "反对",
           "不会",
           "反対",
           "不能",
           "不同意",
           "棄権",
           "没有",
           "不可以",
           "Disagree",
           "NAY",
           "Deny",
           "Decline",
           "Disapprove",
           "Nope",
           "não",
           "n",
           "Oppose",
           "I do not support")
  nos <- tolower(nos)

  nos_begins <- c("no,", "no ", "si ",
                  "si,", "não", "nope,", "nope ",
                  "nay,", "nay ")

  nos_contains <- c(
    "Against",
    "Reject",
    "否",
    "不是",
    "反对",
    "不会",
    "反対",
    "不能",
    "不同意",
    "棄権",
    "没有",
    "不可以",
    "Disagree",
    "NAY",
    "Deny",
    "Decline",
    "Disapprove",
    "Oppose",
    "I do not support")

  yess <- c("Yes",
            "For",
            "Approve",
            "y",
            "Agree",
            "YAE",
            "是",
            "同意",
            "賛成",
            "支持",
            "Support",
            "Accept",
            "能",
            "Pass",
            "In favor",
            "s",
            "ok",
            "Si",
            "Oui",
            "I support")
  yess <- tolower(yess)

  yess_begins <- c("yes,", "yes ", "ok,", "ok ", "yae, ", "yae ",
                   "oui,", "oui ", "si, ", "si ")

  yess_contains <- c(
    "Approve",
    "Agree",
    "YAE",
    "是",
    "同意",
    "賛成",
    "支持",
    "Support",
    "Accept",
    "能",
    "In favor",
    "I support")


  abstains <- c("abstain")
  abstains <- tolower(abstains)

  infavor_cb <- function(row) {
    c <- tolower(row['choice_text'])
    # print(c)
    NO  <- "NO"
    YES <- "YES"
    ABSTAIN <- "ABSTAIN"
    OTHER   <- "OTHER"

    if (c %in% nos) {
      return(NO)
    }
    if (c %in% yess) {
      return(YES)
    }
    if (c %in% abstains) {
      return(ABSTAIN)
    }

    if (any(str_starts(c, nos_begins))) {
      return(NO)
    }

    if (any(str_starts(c, yess_begins))) {
      return(YES)
    }

    if (any(str_detect(c, nos_contains))) {
      return(NO)
    }

    if (any(str_detect(c, yess_contains))) {
      return(YES)
    }

    if (any(str_detect(c, abstains))) {
      return(ABSTAIN)
    }

    return(OTHER)


  }

  df$infavor <- apply(df, 1, infavor_cb)


  ### Prop counter and dates

  df = merge(df, df_dates, by="proposal")

  ### Vote of author

  df$author_choice = NA
  df$author_vp = NA

  df = merge(df, df_author_choice, by="proposal")

  return(df)

}
