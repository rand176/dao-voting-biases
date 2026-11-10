buildNoAuthorVpRelease <- function(df_choices) {
  required_cols <- c(
    "proposal",
    "choice_idx",
    "type",
    "is_author_choice",
    "author_vp",
    "choice_score",
    "quorum"
  )
  missing_cols <- setdiff(required_cols, colnames(df_choices))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required no-author-vp columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  df <- df_choices
  multi_choice_types <- !df$type %in% c("basic", "single-choice")
  author_choice_rows <- df[
    multi_choice_types & !is.na(df$is_author_choice) & df$is_author_choice == 1,
    c("proposal", "is_author_choice"),
    drop = FALSE
  ]

  author_choice_counts <- if (nrow(author_choice_rows) > 0) {
    aggregate(is_author_choice ~ proposal, data = author_choice_rows, FUN = sum)
  } else {
    data.frame(
      proposal = character(),
      is_author_choice = integer(),
      stringsAsFactors = FALSE
    )
  }
  names(author_choice_counts)[names(author_choice_counts) == "is_author_choice"] <- "author_n_choices"

  df$author_n_choices <- author_choice_counts$author_n_choices[match(df$proposal, author_choice_counts$proposal)]
  df$author_n_choices[is.na(df$author_n_choices)] <- 1

  author_vp_contribution <- rep(0, nrow(df))
  support_fraction_available <- rep(FALSE, nrow(df))
  if ("author_support_fraction" %in% colnames(df)) {
    author_support_fraction <- suppressWarnings(as.numeric(df$author_support_fraction))
    support_fraction_available <- ave(
      !is.na(author_support_fraction),
      df$proposal,
      FUN = function(x) rep(any(x), length(x))
    )
    author_support_fraction[support_fraction_available & is.na(author_support_fraction)] <- 0
    supported_rows <- support_fraction_available & !is.na(author_support_fraction) &
      author_support_fraction > 0
    author_vp_contribution[supported_rows] <- df$author_vp[supported_rows] *
      author_support_fraction[supported_rows]
  }

  is_author_choice <- !is.na(df$is_author_choice) & df$is_author_choice == 1
  fallback_rows <- !support_fraction_available & is_author_choice
  vp_share <- df$author_vp / df$author_n_choices
  author_vp_contribution[fallback_rows] <- vp_share[fallback_rows]

  df$choice_score_no_author_vp <- pmax(df$choice_score - author_vp_contribution, 0)

  df$choice_share_no_author_vp <- ave(
    df$choice_score_no_author_vp,
    df$proposal,
    FUN = function(x) {
      total <- sum(x, na.rm = TRUE)
      if (total == 0) {
        rep(NA_real_, length(x))
      } else {
        x / total
      }
    }
  )

  max_choice_share_no_author_vp <- ave(
    df$choice_share_no_author_vp,
    df$proposal,
    FUN = function(x) {
      if (all(is.na(x))) {
        rep(NA_real_, length(x))
      } else {
        rep(max(x, na.rm = TRUE), length(x))
      }
    }
  )

  is_max <- !is.na(df$choice_share_no_author_vp) &
    !is.na(max_choice_share_no_author_vp) &
    abs(df$choice_share_no_author_vp - max_choice_share_no_author_vp) <= sqrt(.Machine$double.eps)

  df$first_choice_won_no_author_vp <- as.integer(
    (df$choice_idx == 1) &
      is_max &
      (df$choice_score_no_author_vp > df$quorum)
  )
  df$choice_won_no_author_vp <- as.integer(
    is_max &
      (df$choice_score_no_author_vp > df$quorum)
  )

  df$author_n_choices <- NULL
  rownames(df) <- NULL
  df
}
