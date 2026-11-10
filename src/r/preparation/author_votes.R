normalizeAuthorVoteTable <- function(df, value_col = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  df$proposal <- trimws(tolower(as.character(df$proposal)))
  df$author <- trimws(tolower(as.character(df$author)))
  df$author_choice <- suppressWarnings(as.numeric(df$author_choice))
  df$author_vp <- suppressWarnings(as.numeric(df$author_vp))
  df$author_pos <- suppressWarnings(as.numeric(df$author_pos))
  df$author_rank <- suppressWarnings(as.numeric(df$author_rank))
  if (!is.null(value_col) && value_col %in% colnames(df)) {
    df[[value_col]] <- suppressWarnings(as.numeric(df[[value_col]]))
  }

  df
}

formatAuthorVoteExamples <- function(values, limit = 5) {
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return("")
  }
  paste(utils::head(values, limit), collapse = ", ")
}

proposalChoiceKeys <- function(df, proposal_col = "proposal", choice_col = "choice_idx") {
  if (is.null(df) || nrow(df) == 0) {
    return(character())
  }

  proposals <- trimws(tolower(as.character(df[[proposal_col]])))
  choices <- suppressWarnings(as.numeric(df[[choice_col]]))
  valid <- !is.na(proposals) & nzchar(proposals) & !is.na(choices)
  if (!any(valid)) {
    return(character())
  }

  unique(paste(proposals[valid], choices[valid], sep = "::"))
}

assertAuthorVoteProposalOverlap <- function(df_choices, source_df, label) {
  if (is.null(source_df) || nrow(source_df) == 0 || is.null(df_choices) || nrow(df_choices) == 0) {
    return(invisible(NULL))
  }

  choice_props <- unique(trimws(tolower(as.character(df_choices$proposal))))
  source_props <- unique(trimws(tolower(as.character(source_df$proposal))))
  choice_props <- choice_props[!is.na(choice_props) & nzchar(choice_props)]
  source_props <- source_props[!is.na(source_props) & nzchar(source_props)]

  if (length(intersect(choice_props, source_props)) == 0) {
    stop(
      paste(
        "No overlapping proposals between df_choices and",
        paste0("author ", label, " outputs."),
        "This would make all merged author-vote columns NA.",
        "Check proposal normalization and upstream file selection."
      )
    )
  }

  invisible(NULL)
}

assertAuthorVoteProposalCoverage <- function(df_choices, expected_proposals, coverage_cols, label) {
  if (is.null(df_choices) || nrow(df_choices) == 0) {
    return(invisible(NULL))
  }

  expected_proposals <- unique(trimws(tolower(as.character(expected_proposals))))
  expected_proposals <- expected_proposals[!is.na(expected_proposals) & nzchar(expected_proposals)]
  expected_proposals <- intersect(
    expected_proposals,
    unique(trimws(tolower(as.character(df_choices$proposal))))
  )
  if (length(expected_proposals) == 0) {
    return(invisible(NULL))
  }

  coverage_cols <- intersect(coverage_cols, colnames(df_choices))
  if (length(coverage_cols) == 0) {
    stop(paste("Missing coverage columns for", label, "author-vote validation."))
  }

  coverage_mask <- rowSums(!is.na(df_choices[, coverage_cols, drop = FALSE])) > 0
  matched_proposals <- unique(trimws(tolower(as.character(df_choices$proposal[coverage_mask]))))
  matched_proposals <- matched_proposals[!is.na(matched_proposals) & nzchar(matched_proposals)]

  missing_proposals <- setdiff(expected_proposals, matched_proposals)
  if (length(missing_proposals) > 0) {
    stop(
      paste(
        "Author-vote merge lost expected",
        label,
        "coverage for",
        length(missing_proposals),
        "proposal(s).",
        "Example proposal IDs:",
        formatAuthorVoteExamples(missing_proposals)
      )
    )
  }

  invisible(NULL)
}

assertAuthorVoteChoiceCoverage <- function(expected_lookup, observed_df, label) {
  expected_keys <- proposalChoiceKeys(expected_lookup)
  if (length(expected_keys) == 0) {
    return(invisible(NULL))
  }

  observed_keys <- proposalChoiceKeys(observed_df)
  missing_keys <- setdiff(expected_keys, observed_keys)
  if (length(missing_keys) > 0) {
    stop(
      paste(
        "Author-vote merge lost expected",
        label,
        "proposal-choice matches.",
        "Example keys:",
        formatAuthorVoteExamples(missing_keys)
      )
    )
  }

  invisible(NULL)
}

authorVoteJobCommands <- function() {
  paste(
    "Run from the public package:",
    "python run.py prepare --stage author-votes --data-root INPUT --workers 8",
    sep = "\n"
  )
}

stopAuthorVoteContract <- function(...) {
  stop(paste(..., sep = "\n"))
}

readAuthorVoteJsonFile <- function(path, missing_message) {
  if (!file.exists(path)) {
    stopAuthorVoteContract(
      missing_message,
      "The v3 R pipeline now requires a fully reduced, canonical `author_votes` run.",
      authorVoteJobCommands()
    )
  }

  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

loadAuthorVoteContract <- function() {
  run_summary_path <- cleanAuthorVoteOutputsPath("run_summary.json")
  run_meta_path <- cleanAuthorVoteOutputsPath("manifests", "author_votes_v1_run.json")

  run_summary <- readAuthorVoteJsonFile(
    run_summary_path,
    "Missing canonical author-vote run summary."
  )
  run_meta <- readAuthorVoteJsonFile(
    run_meta_path,
    "Missing current author-vote run metadata."
  )

  if (!isTRUE(run_summary$is_complete)) {
    stopAuthorVoteContract(
      "Canonical author-vote outputs are incomplete.",
      "run_summary.json reports `is_complete = FALSE` or is missing that field.",
      authorVoteJobCommands()
    )
  }

  summary_run_id <- if (!is.null(run_summary$run_id)) as.character(run_summary$run_id) else ""
  meta_run_id <- if (!is.null(run_meta$run_id)) as.character(run_meta$run_id) else ""

  if (!nzchar(summary_run_id) || !nzchar(meta_run_id) || !identical(summary_run_id, meta_run_id)) {
    stopAuthorVoteContract(
      "Canonical author-vote outputs are stale.",
      "run_summary.json does not match the current `author_votes` run metadata.",
      authorVoteJobCommands()
    )
  }

  list(
    run_summary = run_summary,
    run_meta = run_meta
  )
}

loadAuthorVoteOutputs <- function() {
  favorite_path <- cleanAuthorVoteOutputsPath("prop_author_favorite_choice.csv")
  support_path <- cleanAuthorVoteOutputsPath("prop_author_support_fraction.csv")

  if (!file.exists(favorite_path) || !file.exists(support_path)) {
    stop(
      paste(
        "Missing canonical clean author-vote outputs.",
        "The v3 R pipeline now requires the reduced outputs from the `author_votes` job.",
        "Expected files:",
        favorite_path,
        "and",
        support_path,
        authorVoteJobCommands(),
        sep = "\n"
      )
    )
  }

  loadAuthorVoteContract()

  hex_cols <- c(proposal = "character", author = "character")
  favorite <- read.csv(favorite_path, stringsAsFactors = FALSE, colClasses = hex_cols)
  support <- read.csv(support_path, stringsAsFactors = FALSE, colClasses = hex_cols)

  list(
    favorite = normalizeAuthorVoteTable(favorite, "author_choice_weight"),
    support = normalizeAuthorVoteTable(support, "author_support_fraction")
  )
}

buildAuthorFavoriteMetadata <- function(favorite) {
  if (is.null(favorite) || nrow(favorite) == 0) {
    return(data.frame(
      proposal = character(),
      author = character(),
      author_choice = numeric(),
      author_vp = numeric(),
      author_pos = numeric(),
      author_rank = numeric(),
      author_multi = integer(),
      stringsAsFactors = FALSE
    ))
  }

  fav_key <- paste(favorite$proposal, favorite$author)
  favorite$author_multi <- ifelse(
    duplicated(fav_key) | duplicated(fav_key, fromLast = TRUE),
    1L,
    0L
  )

  favorite[!duplicated(fav_key), c(
    "proposal",
    "author",
    "author_choice",
    "author_vp",
    "author_pos",
    "author_rank",
    "author_multi"
  )]
}

buildAuthorFavoriteLookup <- function(favorite) {
  if (is.null(favorite) || nrow(favorite) == 0) {
    return(data.frame(
      proposal = character(),
      choice_idx = numeric(),
      is_author_choice = integer(),
      stringsAsFactors = FALSE
    ))
  }

  lookup <- favorite[!is.na(favorite$author_choice), c("proposal", "author_choice")]
  if (nrow(lookup) == 0) {
    return(data.frame(
      proposal = character(),
      choice_idx = numeric(),
      is_author_choice = integer(),
      stringsAsFactors = FALSE
    ))
  }

  colnames(lookup)[colnames(lookup) == "author_choice"] <- "choice_idx"
  lookup$is_author_choice <- 1L
  unique(lookup)
}

buildAuthorSupportStatus <- function(support) {
  if (is.null(support) || nrow(support) == 0) {
    return(data.frame(
      proposal = character(),
      has_valid_author_support = integer(),
      stringsAsFactors = FALSE
    ))
  }

  status <- aggregate(
    valid_support_row ~ proposal,
    data = transform(support, valid_support_row = !is.na(author_choice)),
    FUN = any
  )
  colnames(status)[colnames(status) == "valid_support_row"] <- "has_valid_author_support"
  status$has_valid_author_support <- as.integer(status$has_valid_author_support)
  status
}

buildAuthorSupportLookup <- function(support) {
  if (is.null(support) || nrow(support) == 0) {
    return(data.frame(
      proposal = character(),
      choice_idx = numeric(),
      author_support_fraction = numeric(),
      is_author_supported_choice = integer(),
      stringsAsFactors = FALSE
    ))
  }

  lookup <- support[!is.na(support$author_choice), c("proposal", "author_choice", "author_support_fraction")]
  if (nrow(lookup) == 0) {
    return(data.frame(
      proposal = character(),
      choice_idx = numeric(),
      author_support_fraction = numeric(),
      is_author_supported_choice = integer(),
      stringsAsFactors = FALSE
    ))
  }

  colnames(lookup)[colnames(lookup) == "author_choice"] <- "choice_idx"
  lookup <- aggregate(author_support_fraction ~ proposal + choice_idx, data = lookup, FUN = sum)
  lookup$is_author_supported_choice <- ifelse(lookup$author_support_fraction > 0, 1L, 0L)
  lookup
}

mergeAuthorVoteOutputs <- function(df_choices, favorite, support) {
  favorite <- normalizeAuthorVoteTable(favorite, "author_choice_weight")
  support <- normalizeAuthorVoteTable(support, "author_support_fraction")

  assertAuthorVoteProposalOverlap(df_choices, favorite, "favorite")
  assertAuthorVoteProposalOverlap(df_choices, support, "support")

  favorite_meta <- buildAuthorFavoriteMetadata(favorite)
  favorite_lookup <- buildAuthorFavoriteLookup(favorite)
  support_status <- buildAuthorSupportStatus(support)
  support_lookup <- buildAuthorSupportLookup(support)

  df_choices$proposal <- trimws(tolower(as.character(df_choices$proposal)))
  if ("choice_idx" %in% colnames(df_choices)) {
    df_choices$choice_idx <- suppressWarnings(as.numeric(df_choices$choice_idx))
  }

  drop_cols <- intersect(
    c(
      "author",
      "author_vp",
      "author_choice",
      "author_pos",
      "author_rank",
      "author_multi",
      "is_author_choice",
      "author_support_fraction",
      "is_author_supported_choice"
    ),
    colnames(df_choices)
  )
  if (length(drop_cols) > 0) {
    df_choices[drop_cols] <- NULL
  }

  n_before <- nrow(df_choices)

  df_choices <- merge(df_choices, favorite_meta, by = "proposal", all.x = TRUE)
  stopifnot(nrow(df_choices) == n_before)

  df_choices <- merge(df_choices, favorite_lookup, by = c("proposal", "choice_idx"), all.x = TRUE)
  stopifnot(nrow(df_choices) == n_before)

  favorite_lookup_observed <- favorite_lookup[
    proposalChoiceKeys(favorite_lookup) %in% proposalChoiceKeys(df_choices),
    ,
    drop = FALSE
  ]
  proposals_with_matched_author_choice <- unique(df_choices$proposal[
    !is.na(df_choices$is_author_choice) & df_choices$is_author_choice == 1L
  ])

  df_choices <- merge(df_choices, support_status, by = "proposal", all.x = TRUE)
  stopifnot(nrow(df_choices) == n_before)

  df_choices <- merge(df_choices, support_lookup, by = c("proposal", "choice_idx"), all.x = TRUE)
  stopifnot(nrow(df_choices) == n_before)

  fill_author_zero <- df_choices$proposal %in% proposals_with_matched_author_choice &
    is.na(df_choices$is_author_choice)
  df_choices$is_author_choice[fill_author_zero] <- 0L

  fill_zero <- !is.na(df_choices$has_valid_author_support) &
    df_choices$has_valid_author_support == 1L &
    is.na(df_choices$author_support_fraction)
  df_choices$author_support_fraction[fill_zero] <- 0
  df_choices$is_author_supported_choice[fill_zero] <- 0L

  favorite_signal_mask <- rowSums(!is.na(favorite_meta[, c(
    "author_pos",
    "author_choice",
    "author_vp",
    "author_rank"
  ), drop = FALSE])) > 0
  expected_favorite_proposals <- unique(favorite_meta$proposal[favorite_signal_mask])
  expected_support_proposals <- unique(
    support_status$proposal[!is.na(support_status$has_valid_author_support) &
      support_status$has_valid_author_support == 1L]
  )
  expected_support_pairs <- support_lookup[
    !is.na(support_lookup$author_support_fraction) &
      support_lookup$author_support_fraction > 0,
    c("proposal", "choice_idx"),
    drop = FALSE
  ]

  assertAuthorVoteProposalCoverage(
    df_choices,
    expected_favorite_proposals,
    c("author_pos", "author_choice", "author_vp", "author_rank"),
    "favorite"
  )
  assertAuthorVoteChoiceCoverage(
    favorite_lookup_observed,
    df_choices[
      !is.na(df_choices$is_author_choice) & df_choices$is_author_choice == 1L,
      c("proposal", "choice_idx"),
      drop = FALSE
    ],
    "favorite"
  )
  assertAuthorVoteProposalCoverage(
    df_choices,
    expected_support_proposals,
    c("author_support_fraction", "is_author_supported_choice"),
    "support"
  )
  assertAuthorVoteChoiceCoverage(
    expected_support_pairs,
    df_choices[
      !is.na(df_choices$is_author_supported_choice) &
        df_choices$is_author_supported_choice == 1L &
        !is.na(df_choices$author_support_fraction) &
        df_choices$author_support_fraction > 0,
      c("proposal", "choice_idx"),
      drop = FALSE
    ],
    "support"
  )

  df_choices$has_valid_author_support <- NULL

  df_choices
}
