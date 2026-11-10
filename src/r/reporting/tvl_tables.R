top_tvl_public_n <- function() {
  25L
}

top_tvl_bool <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  out <- as_binary01(x) == 1L
  out[is.na(out)] <- FALSE
  out
}

top_tvl_row_label <- function(row, failed, rank_override = NA_integer_, rank_col = "protocol_rank") {
  rank <- rank_override
  protocol <- ""
  space <- ""
  if (!is.null(row) && nrow(row) > 0) {
    if (rank_col %in% names(row)) {
      rank <- row[[rank_col]][[1]]
    } else {
      rank <- row$protocol_rank[[1]]
    }
    protocol <- trimws(as.character(row$protocol_name[[1]]))
    if (!nzchar(protocol) && "protocol_slug" %in% names(row)) {
      protocol <- trimws(as.character(row$protocol_slug[[1]]))
    }
    space <- trimws(as.character(row$candidate_snapshot_space[[1]]))
  }
  if (is.na(rank)) rank <- ""
  if (is.na(protocol)) protocol <- ""
  if (is.na(space)) space <- ""
  paste0("rank=", rank, " protocol=", protocol, " space=", space, " failed=", failed)
}

validate_top_tvl_public_table <- function(table, public_n = top_tvl_public_n()) {
  required <- c(
    "top_tvl_sample_rank", "protocol_rank", "protocol_name", "candidate_snapshot_space",
    "match_status", "snapshot_status", "raw_snapshot_flagged",
    "raw_snapshot_deleted", "in_analysis_release",
    "used_in_top_tvl_robustness_subset"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0) {
    stop(
      "Top-TVL audit table missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- as.data.frame(table, stringsAsFactors = FALSE)
  dt$top_tvl_sample_rank <- as.integer(dt$top_tvl_sample_rank)
  dt$protocol_rank <- as.integer(dt$protocol_rank)
  dt$protocol_name <- trimws(as.character(dt$protocol_name))
  dt$candidate_snapshot_space <- normalize_top_tvl_space(dt$candidate_snapshot_space)
  dt$match_status <- trimws(tolower(as.character(dt$match_status)))
  dt$snapshot_status <- trimws(as.character(dt$snapshot_status))
  dt$raw_snapshot_flagged <- top_tvl_bool(dt$raw_snapshot_flagged)
  dt$raw_snapshot_deleted <- top_tvl_bool(dt$raw_snapshot_deleted)
  dt$in_analysis_release <- top_tvl_bool(dt$in_analysis_release)
  dt$used_in_top_tvl_robustness_subset <- top_tvl_bool(dt$used_in_top_tvl_robustness_subset)
  dt$accepted_match <- dt$match_status == "accepted"
  dt$active_snapshot <- dt$snapshot_status == "Active" &
    !dt$raw_snapshot_flagged &
    !dt$raw_snapshot_deleted

  failures <- character()
  required_ranks <- seq_len(public_n)
  for (rank in required_ranks) {
    rows <- dt[dt$top_tvl_sample_rank == rank, , drop = FALSE]
    if (nrow(rows) == 0) {
      failures <- c(failures, top_tvl_row_label(NULL, "sample_rank_present", rank_override = rank))
      next
    }

    accepted <- rows[rows$accepted_match, , drop = FALSE]
    if (nrow(accepted) == 0) {
      failures <- c(failures, top_tvl_row_label(rows[1, , drop = FALSE], "match_status_accepted", rank_col = "top_tvl_sample_rank"))
      next
    }

    active <- accepted[accepted$active_snapshot, , drop = FALSE]
    if (nrow(active) == 0) {
      failures <- c(failures, top_tvl_row_label(accepted[1, , drop = FALSE], "snapshot_status_active", rank_col = "top_tvl_sample_rank"))
      next
    }

    in_release <- active[active$in_analysis_release, , drop = FALSE]
    if (nrow(in_release) == 0) {
      failures <- c(failures, top_tvl_row_label(active[1, , drop = FALSE], "in_analysis_release", rank_col = "top_tvl_sample_rank"))
      next
    }

    used <- in_release[in_release$used_in_top_tvl_robustness_subset, , drop = FALSE]
    if (nrow(used) == 0) {
      failures <- c(failures, top_tvl_row_label(in_release[1, , drop = FALSE], "used_in_top_tvl_robustness_subset", rank_col = "top_tvl_sample_rank"))
    }
  }

  if (length(failures) > 0) {
    stop(
      "Top-TVL public table audit failed for sample ranks 1:",
      public_n,
      ": ",
      paste(failures, collapse = "; "),
      call. = FALSE
    )
  }
  invisible(dt)
}

top_tvl_public_rows <- function(table, public_n = top_tvl_public_n()) {
  dt <- validate_top_tvl_public_table(table, public_n = public_n)
  public <- dt[
    dt$top_tvl_sample_rank %in% seq_len(public_n) &
      dt$used_in_top_tvl_robustness_subset,
    ,
    drop = FALSE
  ]
  public[order(public$top_tvl_sample_rank, public$protocol_rank, public$candidate_snapshot_space), , drop = FALSE]
}

top_tvl_has_displayed_manual_overrides <- function(table) {
  public <- top_tvl_public_rows(table)
  override_action <- if ("override_action" %in% names(public)) {
    trimws(as.character(public$override_action))
  } else {
    rep("", nrow(public))
  }
  match_source <- if ("match_source" %in% names(public)) {
    trimws(as.character(public$match_source))
  } else {
    rep("", nrow(public))
  }
  any(nzchar(override_action) | grepl("^override_", match_source))
}

top_tvl_dao_notes <- function(models) {
  notes <- c(
    "DefiLlama protocol ranks and TVL are read from metadata/analysis_data_provenance/defillama_protocol_ethereum_tvl_top100.csv under the selected input root.",
    "The public table shows Top-TVL sample ranks 1-25 after the generator verifies accepted Snapshot matches, active, unflagged, undeleted Snapshot status, current-release presence, and Top-TVL subset inclusion; multiple DefiLlama protocol rows can share a Snapshot space and sample rank.",
    "The omitted audit fields (match status, Snapshot status, current-release presence, and subset inclusion) are checked by the generator and stored in metadata/analysis_data_provenance/defillama_top_tvl_snapshot_match_diagnostics.csv under the selected output root."
  )
  if (top_tvl_has_displayed_manual_overrides(models$top_tvl_table)) {
    notes <- c(
      notes,
      "Displayed Top-TVL sample rows with manual Snapshot overrides passed the generator's hard audit checks."
    )
  }
  notes
}

format_top_tvl_dao_table <- function(models) {
  table <- models$top_tvl_table
  table <- top_tvl_public_rows(table)
  data.frame(
    Rank = as.integer(table$top_tvl_sample_rank),
    `DefiLlama protocol` = table$protocol_name,
    `Snapshot space` = table$candidate_snapshot_space,
    Category = table$category,
    TVL = format_currency(table$median_ethereum_tvl_usd),
    Proposals = format_int(table$proposals),
    Choices = format_int(table$choices),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_top_tvl_category <- function(category) {
  category <- as.character(category)
  category[is.na(category)] <- ""
  vapply(category, function(value) {
    value <- trimws(value)
    if (!nzchar(value)) {
      return("")
    }
    value <- switch(
      value,
      "Liquidity Staking" = "Liquidity;Staking",
      "Yield Aggregator" = "Yield;Aggregator",
      "Leveraged Farming" = "Leveraged;Farming",
      "Uncollateralized Lending" = "Uncoll.;Lending",
      "Algo-Stables" = "Algo;Stables",
      value
    )
    value <- gsub("[- ]+", ";", value)
    if (grepl(";", value, fixed = TRUE)) {
      latex_makecell_left(value, split_semicolon = TRUE)
    } else {
      latex_escape(value)
    }
  }, character(1), USE.NAMES = FALSE)
}


format_top_tvl_dao_latex_table <- function(models) {
  table <- format_top_tvl_dao_table(models)
  data.frame(
    Rank = as.character(table$Rank),
    `DefiLlama protocol` = latex_makecell_left(table$`DefiLlama protocol`, split_semicolon = TRUE),
    `Snapshot space` = latex_url_same_style(table$`Snapshot space`),
    Category = format_top_tvl_category(table$Category),
    TVL = latex_escape(table$TVL),
    Proposals = latex_escape(table$Proposals),
    Choices = latex_escape(table$Choices),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

top_tvl_sample_dao_rows <- function(table, sample_name = NULL,
                                    rank_min = NULL, rank_max = NULL) {
  required <- c(
    "top_tvl_sample_rank", "protocol_rank", "protocol_name",
    "candidate_snapshot_space", "category", "median_ethereum_tvl_usd",
    "proposals", "choices"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0) {
    stop(
      "Top-TVL sample table missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  dt <- as.data.frame(table, stringsAsFactors = FALSE)
  if (!is.null(sample_name) && "sample" %in% names(dt)) {
    dt <- dt[trimws(as.character(dt$sample)) == sample_name, , drop = FALSE]
  }
  if (nrow(dt) == 0) {
    label <- if (is.null(sample_name)) "" else paste0(" for sample ", sample_name)
    stop("Top-TVL sample table has no row(s)", label, ".", call. = FALSE)
  }

  dt$top_tvl_sample_rank <- suppressWarnings(as.integer(dt$top_tvl_sample_rank))
  dt$protocol_rank <- suppressWarnings(as.integer(dt$protocol_rank))
  dt$protocol_name <- trimws(as.character(dt$protocol_name))
  dt$candidate_snapshot_space <- normalize_top_tvl_space(dt$candidate_snapshot_space)
  dt$category <- trimws(as.character(dt$category))
  dt$median_ethereum_tvl_usd <- suppressWarnings(as.numeric(dt$median_ethereum_tvl_usd))
  dt$proposals <- suppressWarnings(as.integer(dt$proposals))
  dt$choices <- suppressWarnings(as.integer(dt$choices))

  missing_key <- is.na(dt$top_tvl_sample_rank) |
    is.na(dt$protocol_rank) |
    !nzchar(dt$protocol_name) |
    !nzchar(dt$candidate_snapshot_space)
  if (any(missing_key)) {
    stop("Top-TVL sample table has row(s) with missing rank, protocol, or Snapshot-space fields.", call. = FALSE)
  }

  if (!is.null(rank_min)) {
    dt <- dt[dt$top_tvl_sample_rank >= as.integer(rank_min), , drop = FALSE]
  }
  if (!is.null(rank_max)) {
    dt <- dt[dt$top_tvl_sample_rank <= as.integer(rank_max), , drop = FALSE]
  }
  if (nrow(dt) == 0) {
    label <- if (is.null(sample_name)) "" else paste0(" for sample ", sample_name)
    range_label <- if (is.null(rank_min) && is.null(rank_max)) {
      ""
    } else {
      paste0(" in rank range ", ifelse(is.null(rank_min), "-Inf", rank_min), "-", ifelse(is.null(rank_max), "Inf", rank_max))
    }
    stop("Top-TVL sample table has no row(s)", label, range_label, ".", call. = FALSE)
  }

  dt[order(dt$top_tvl_sample_rank, dt$protocol_rank, dt$candidate_snapshot_space), , drop = FALSE]
}

read_top_tvl_sample_dao_rows <- function(
    sample_name = "top100",
    path = default_top_tvl_sample_memberships_path(),
    rank_min = NULL,
    rank_max = NULL) {
  require_packages("data.table")
  if (length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("Top-TVL sample memberships file is missing: ", path, call. = FALSE)
  }
  table <- data.table::fread(path, showProgress = FALSE)
  top_tvl_sample_dao_rows(table, sample_name = sample_name, rank_min = rank_min, rank_max = rank_max)
}

format_top_tvl_sample_dao_table <- function(table, sample_name = NULL,
                                            rank_min = NULL, rank_max = NULL) {
  table <- top_tvl_sample_dao_rows(table, sample_name = sample_name, rank_min = rank_min, rank_max = rank_max)
  data.frame(
    Rank = as.integer(table$top_tvl_sample_rank),
    `DefiLlama rank` = as.integer(table$protocol_rank),
    `DefiLlama protocol` = table$protocol_name,
    `Snapshot space` = table$candidate_snapshot_space,
    Category = table$category,
    TVL = format_currency(table$median_ethereum_tvl_usd),
    Proposals = format_int(table$proposals),
    Choices = format_int(table$choices),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_top_tvl_sample_dao_latex_table <- function(table, sample_name = NULL,
                                                  rank_min = NULL,
                                                  rank_max = NULL) {
  table <- format_top_tvl_sample_dao_table(table, sample_name = sample_name, rank_min = rank_min, rank_max = rank_max)
  data.frame(
    Rank = as.character(table$Rank),
    `DefiLlama rank` = latex_escape(table$`DefiLlama rank`),
    `DefiLlama protocol` = latex_makecell_left(table$`DefiLlama protocol`, split_semicolon = TRUE),
    `Snapshot space` = latex_url_same_style(table$`Snapshot space`),
    Category = format_top_tvl_category(table$Category),
    TVL = latex_escape(table$TVL),
    Proposals = latex_escape(table$Proposals),
    Choices = latex_escape(table$Choices),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

top_tvl_dao_header <- function() {
  paste(
    "\\makecell[r]{\\#}",
    "\\makecell[l]{DefiLlama\\\\protocol}",
    "\\makecell[l]{Snapshot\\\\space}",
    "Category",
    "\\makecell[r]{TVL}",
    "\\makecell[r]{Prop.}",
    "\\makecell[r]{Choices}",
    sep = " & "
  )
}

top_tvl_sample_dao_header <- function() {
  paste(
    "\\makecell[r]{\\#}",
    "\\makecell[r]{DefiLlama\\\\rank}",
    "\\makecell[l]{DefiLlama\\\\protocol}",
    "\\makecell[l]{Snapshot\\\\space}",
    "Category",
    "\\makecell[r]{TVL}",
    "\\makecell[r]{Prop.}",
    "\\makecell[r]{Choices}",
    sep = " & "
  )
}

top_tvl_sample_rank_range_label <- function(rank_min = NULL, rank_max = NULL) {
  if (is.null(rank_min) && is.null(rank_max)) return("all Top-TVL sample ranks")
  if (is.null(rank_min)) return(paste0("Top-TVL sample ranks up to ", as.integer(rank_max)))
  if (is.null(rank_max)) return(paste0("Top-TVL sample ranks ", as.integer(rank_min), " and higher"))
  paste0("Top-TVL sample ranks ", as.integer(rank_min), "-", as.integer(rank_max))
}

top_tvl_sample_dao_notes <- function(sample_name = "top100",
                                     rank_min = NULL,
                                     rank_max = NULL,
                                     continuation = FALSE) {
  c(
    paste0(
      "This part reports ",
      top_tvl_sample_rank_range_label(rank_min, rank_max),
      "."
    ),
    "Not all protocols found in DefiLLama can be matched in our Snapshot database; multiple DefiLlama protocols can be matched to the same Snapshot space."
  )
}
