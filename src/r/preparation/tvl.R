normalize_top_tvl_space <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- sub("^snapshot:", "", x)
  x[is.na(x)] <- ""
  x
}

assert_not_legacy_top_tvl_source <- function(path) {
  legacy_files <- c("defiLlamaEthDeFi.csv", "Llama_snapshot_match_checked.csv")
  if (!is.na(path) && basename(path) %in% legacy_files) {
    stop(
      "Legacy DefiLlama Top-TVL source is not allowed: ",
      path,
      ". Use tracked analysis_data_provenance protocol and override artifacts.",
      call. = FALSE
    )
  }
}

parse_snapshot_governance_ids_r <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(character())
  }
  parts <- unlist(strsplit(as.character(x), "[;,]", perl = TRUE), use.names = FALSE)
  parts <- normalize_top_tvl_space(parts)
  unique(parts[!is.na(parts) & nzchar(parts)])
}

load_top_tvl_table <- function(path = default_top_tvl_spaces_path(),
                               protocol_match_path = default_top_tvl_protocol_match_path(),
                               project_root = find_project_root()) {
  assert_not_legacy_top_tvl_source(path)
  assert_not_legacy_top_tvl_source(protocol_match_path)
  if (is.na(path) || !file.exists(path)) {
    stop(
      "Top-TVL protocol table is missing: ",
      path,
      ". Run python run.py fetch-tvl with cached/raw DefiLlama inputs ",
      "before fitting the Top-TVL robustness model.",
      call. = FALSE
    )
  }
  table <- data.table::fread(path, showProgress = FALSE)
  required <- c(
    "protocol_rank", "protocol_slug", "protocol_name", "category",
    "snapshot_governance_ids", "parent_snapshot_governance_ids", "median_ethereum_tvl_usd"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0) {
    stop(
      "Top-TVL protocol table missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  table[, protocol_rank := as.integer(protocol_rank)]
  table[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
  table[, protocol_name := trimws(as.character(protocol_name))]
  table[, category := trimws(as.character(category))]
  table[, median_ethereum_tvl_usd := as.numeric(median_ethereum_tvl_usd)]
  table <- table[!is.na(protocol_rank) & nzchar(protocol_slug) & !is.na(median_ethereum_tvl_usd)]
  data.table::setorder(table, protocol_rank, protocol_slug)
  table[]
}

load_snapshot_spaces <- function(path = default_snapshot_spaces_path()) {
  if (is.na(path) || !file.exists(path)) {
    stop("Raw Snapshot spaces file is missing: ", path, call. = FALSE)
  }
  spaces <- data.table::fread(path, showProgress = FALSE)
  space_col <- if ("id" %in% names(spaces)) "id" else if ("space" %in% names(spaces)) "space" else NA_character_
  if (is.na(space_col)) {
    stop("Raw Snapshot spaces file is missing id/space column: ", path, call. = FALSE)
  }
  if (!"flagged" %in% names(spaces)) spaces[, flagged := 0L]
  if (!"deleted" %in% names(spaces)) spaces[, deleted := 0L]
  spaces[, space := normalize_top_tvl_space(get(space_col))]
  spaces[, raw_snapshot_flagged := as_binary01(flagged) == 1L]
  spaces[, raw_snapshot_deleted := as_binary01(deleted) == 1L]
  unique(spaces[nzchar(space), .(space, raw_snapshot_flagged, raw_snapshot_deleted)])
}

empty_defillama_snapshot_overrides <- function() {
  data.frame(
    protocol_slug = character(),
    snapshot_space = character(),
    override_action = character(),
    match_source = character(),
    rationale = character(),
    evidence_url_or_note = character(),
    reviewer = character(),
    review_date = character(),
    eligible_for_subset = logical(),
    replaces_snapshot_space = character(),
    stringsAsFactors = FALSE
  )
}

snapshot_review_columns <- function() {
  c(
    "review_decision",
    "reviewed_snapshot_space",
    "release_proposals",
    "release_choices",
    "evidence_url_or_note",
    "reviewer",
    "review_date",
    "review_note"
  )
}

top100_review_columns <- snapshot_review_columns

snapshot_review_decisions <- function() {
  c(
    "already_matched",
    "pending_review",
    "accept_override",
    "accept_not_in_release",
    "no_snapshot_governance",
    "ambiguous_no_override",
    "reject_candidate"
  )
}

top100_review_decisions <- snapshot_review_decisions

collapse_nonempty_strings <- function(x) {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  paste(x, collapse = " | ")
}

empty_top100_review_seed <- function() {
  data.table::data.table(
    protocol_slug = character(),
    reviewed_snapshot_space = character(),
    evidence_url_or_note = character(),
    review_note = character()
  )
}

normalize_top100_review_seed <- function(review_seed) {
  require_packages("data.table")
  if (is.null(review_seed)) {
    return(empty_top100_review_seed())
  }
  seed <- data.table::copy(data.table::as.data.table(review_seed))
  if (nrow(seed) == 0) {
    return(empty_top100_review_seed())
  }
  if (!"protocol_slug" %in% names(seed)) {
    stop("Top 100 review seed missing column: protocol_slug", call. = FALSE)
  }
  optional <- c("reviewed_snapshot_space", "evidence_url_or_note", "review_note")
  for (field in optional) {
    if (!field %in% names(seed)) seed[, (field) := ""]
  }
  seed[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
  seed[, reviewed_snapshot_space := normalize_top_tvl_space(reviewed_snapshot_space)]
  seed[, evidence_url_or_note := trimws(as.character(evidence_url_or_note))]
  seed[, review_note := trimws(as.character(review_note))]
  seed <- seed[nzchar(protocol_slug)]
  if (nrow(seed) == 0) {
    return(empty_top100_review_seed())
  }

  seed[, .(
    reviewed_snapshot_space = collapse_nonempty_strings(reviewed_snapshot_space),
    evidence_url_or_note = collapse_nonempty_strings(evidence_url_or_note),
    review_note = collapse_nonempty_strings(review_note)
  ), by = protocol_slug]
}

top100_review_seed_from_draft <- function(path = default_top100_overrides_draft_path()) {
  require_packages("data.table")
  if (length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(empty_top100_review_seed())
  }
  draft <- data.table::fread(path, showProgress = FALSE)
  required <- c("protocol_slug", "snapshot_space")
  missing <- setdiff(required, names(draft))
  if (length(missing) > 0) {
    stop("Top 100 draft override seed missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!"evidence_url_or_note" %in% names(draft)) draft[, evidence_url_or_note := ""]
  if (!"rationale" %in% names(draft)) draft[, rationale := ""]
  if (!"override_action" %in% names(draft)) draft[, override_action := ""]
  normalize_top100_review_seed(draft[, .(
    protocol_slug,
    reviewed_snapshot_space = snapshot_space,
    evidence_url_or_note,
    review_note = paste(
      "Draft override",
      trimws(as.character(override_action)),
      "candidate:",
      trimws(as.character(rationale))
    )
  )])
}

extract_top100_review_candidates <- function(lines) {
  candidate_lines <- lines[grepl(
    "Fuzzy candidates|Draft override|Snapshot space|snapshot space|governance space|uses `|candidate",
    lines,
    ignore.case = TRUE
  )]
  if (length(candidate_lines) == 0) return(character())

  candidates <- unlist(lapply(candidate_lines, function(line) {
    accept_matches <- regmatches(
      line,
      gregexpr("accept[[:space:]]+([A-Za-z0-9._-]+)", line, perl = TRUE)
    )[[1]]
    accept_candidates <- sub("^accept[[:space:]]+", "", accept_matches, perl = TRUE)

    backtick_matches <- regmatches(
      line,
      gregexpr("`([A-Za-z0-9][A-Za-z0-9._-]*)`", line, perl = TRUE)
    )[[1]]
    backtick_candidates <- sub("^`([^`]+)`$", "\\1", backtick_matches, perl = TRUE)
    if (!grepl("Fuzzy candidates", line, ignore.case = TRUE)) {
      backtick_candidates <- backtick_candidates[grepl("\\.", backtick_candidates)]
    }
    c(accept_candidates, backtick_candidates)
  }), use.names = FALSE)

  candidates <- normalize_top_tvl_space(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  unique(candidates)
}

top100_review_seed_from_markdown <- function(path = default_top100_match_review_path()) {
  if (length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(empty_top100_review_seed())
  }
  lines <- readLines(path, warn = FALSE)
  all_starts <- grep("^##+ ", lines)
  starts <- all_starts[grepl("^### [^[:space:]]+ \\(rank [0-9]+\\)", lines[all_starts])]
  if (length(starts) == 0) {
    return(empty_top100_review_seed())
  }
  rows <- lapply(seq_along(starts), function(idx) {
    start <- starts[[idx]]
    later_starts <- all_starts[all_starts > start]
    end <- if (length(later_starts) > 0) later_starts[[1L]] - 1L else length(lines)
    section <- lines[start:end]
    slug <- sub("^### ([^[:space:]]+) .*", "\\1", section[[1]])
    candidates <- extract_top100_review_candidates(section)
    text <- paste(trimws(section[nzchar(trimws(section))]), collapse = " ")
    data.table::data.table(
      protocol_slug = slug,
      reviewed_snapshot_space = paste(candidates, collapse = " | "),
      evidence_url_or_note = "Seeded from defillama_snapshot_match_review.md",
      review_note = text
    )
  })
  normalize_top100_review_seed(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}

top100_review_seed_table <- function(
    draft_path = default_top100_overrides_draft_path(),
    review_report_path = default_top100_match_review_path()) {
  require_packages("data.table")
  normalize_top100_review_seed(data.table::rbindlist(
    list(
      top100_review_seed_from_markdown(review_report_path),
      top100_review_seed_from_draft(draft_path)
    ),
    use.names = TRUE,
    fill = TRUE
  ))
}

load_defillama_snapshot_overrides <- function(path = default_top_tvl_overrides_path()) {
  required <- names(empty_defillama_snapshot_overrides())
  if (is.na(path) || !file.exists(path)) {
    stop("DefiLlama Snapshot override artifact is missing: ", path, call. = FALSE)
  }
  overrides <- data.table::fread(path, showProgress = FALSE)
  missing <- setdiff(required, names(overrides))
  if (length(missing) > 0) {
    stop("DefiLlama Snapshot override file missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  overrides <- overrides[, ..required]
  if (nrow(overrides) == 0) {
    return(data.table::as.data.table(empty_defillama_snapshot_overrides()))
  }
  overrides[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
  overrides[, snapshot_space := normalize_top_tvl_space(snapshot_space)]
  overrides[, replaces_snapshot_space := normalize_top_tvl_space(replaces_snapshot_space)]
  overrides[, override_action := trimws(tolower(as.character(override_action)))]
  overrides[, eligible_for_subset := as_binary01(eligible_for_subset) == 1L]
  overrides[, rationale := as.character(rationale)]
  overrides[, evidence_url_or_note := as.character(evidence_url_or_note)]
  overrides[, reviewer := as.character(reviewer)]
  overrides[, review_date := as.character(review_date)]

  allowed_actions <- c("accept", "reject", "replace")
  invalid_actions <- sort(unique(overrides$override_action[!(overrides$override_action %in% allowed_actions)]))
  if (length(invalid_actions) > 0) {
    stop("Invalid override_action value(s): ", paste(invalid_actions, collapse = ", "), call. = FALSE)
  }

  audit_fields <- c("rationale", "evidence_url_or_note", "reviewer", "review_date")
  for (field in audit_fields) {
    bad <- is.na(overrides[[field]]) | !nzchar(trimws(as.character(overrides[[field]])))
    if (any(bad)) {
      stop("Override row has empty required audit field: ", field, call. = FALSE)
    }
  }
  if (any(overrides$override_action == "reject" & overrides$eligible_for_subset)) {
    stop("Reject overrides must set eligible_for_subset to false.", call. = FALSE)
  }
  missing_replace <- overrides$override_action == "replace" &
    (is.na(overrides$replaces_snapshot_space) | !nzchar(overrides$replaces_snapshot_space))
  if (any(missing_replace)) {
    stop("Each replace override must provide replaces_snapshot_space.", call. = FALSE)
  }
  duplicate_keys <- duplicated(overrides[, .(protocol_slug, snapshot_space)]) |
    duplicated(overrides[, .(protocol_slug, snapshot_space)], fromLast = TRUE)
  if (any(duplicate_keys)) {
    stop("Conflicting override rows for the same protocol_slug plus snapshot_space.", call. = FALSE)
  }
  overrides[]
}

fuzzy_snapshot_candidates <- function(protocol, snapshot_spaces) {
  slug <- normalize_top_tvl_space(protocol$protocol_slug)
  name_slug <- normalize_top_tvl_space(gsub("[^A-Za-z0-9_.-]+", "-", protocol$protocol_name))
  parent_raw <- if (!is.null(protocol$parent_protocol)) as.character(protocol$parent_protocol) else ""
  parent_slug <- normalize_top_tvl_space(sub("^parent#", "", parent_raw))

  bases <- unique(Filter(nzchar, c(slug, name_slug, parent_slug)))

  # Strip common version and category suffixes
  strip_patterns <- c("-v[0-9]+(\\.[0-9]+)?$", "-dex$", "-finance$", "-protocol$",
                      "-lending$", "-cdp$")
  stripped <- unique(unlist(lapply(bases, function(b) {
    s <- b
    for (p in strip_patterns) s <- sub(p, "", s, perl = TRUE)
    c(b, s)
  })))
  stripped <- unique(Filter(nzchar, stripped))

  # Append Snapshot-style suffixes
  suffixes <- c(".eth", "dao.eth", "-dao.eth", "gov.eth")
  candidates <- unique(c(
    stripped,
    unlist(lapply(stripped, function(b) paste0(b, suffixes)))
  ))

  candidates[candidates %in% snapshot_spaces$space]
}

current_release_space_counts <- function(model_df) {
  require_packages("data.table")
  dt <- data.table::copy(data.table::as.data.table(model_df))
  required <- c("space", "proposal")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(
      "Current release data missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  dt[, space := trimws(tolower(as.character(space)))]
  dt <- dt[!is.na(space) & nzchar(space)]
  dt[, .(
    proposals = data.table::uniqueN(proposal),
    choices = .N
  ), by = space]
}

top_tvl_sample_sizes <- function() {
  c(top25 = 25L, top50 = 50L, top100 = 100L)
}

top_tvl_sample_column <- function(sample_name) {
  paste0("used_in_top_tvl_", sample_name, "_sample")
}

annotate_top_tvl_sample_memberships <- function(table_dt, alias_n = 25L) {
  sample_sizes <- top_tvl_sample_sizes()
  table_dt[, top_tvl_sample_rank := NA_integer_]

  eligible_spaces <- unique(table_dt[subset_candidate == TRUE, candidate_snapshot_space])
  if (length(eligible_spaces) > 0) {
    rank_dt <- data.table::data.table(
      candidate_snapshot_space = eligible_spaces,
      top_tvl_sample_rank = seq_along(eligible_spaces)
    )
    table_dt <- merge(
      table_dt,
      rank_dt,
      by = "candidate_snapshot_space",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", "_ranked")
    )
    table_dt[, top_tvl_sample_rank := top_tvl_sample_rank_ranked]
    table_dt[, top_tvl_sample_rank_ranked := NULL]
  }

  for (sample_name in names(sample_sizes)) {
    sample_n <- sample_sizes[[sample_name]]
    column <- top_tvl_sample_column(sample_name)
    table_dt[, (column) := subset_candidate & !is.na(top_tvl_sample_rank) & top_tvl_sample_rank <= sample_n]
  }

  table_dt[, used_in_top_tvl_robustness_subset := subset_candidate &
    !is.na(top_tvl_sample_rank) &
    top_tvl_sample_rank <= alias_n]
  table_dt[]
}

build_top_tvl_match_table <- function(protocols, overrides, snapshot_spaces, counts,
                                      n = 25, display_n = 33) {
  require_packages("data.table")
  protocols <- data.table::copy(data.table::as.data.table(protocols))
  overrides <- data.table::copy(data.table::as.data.table(overrides))
  snapshot_spaces <- data.table::copy(data.table::as.data.table(snapshot_spaces))
  counts <- data.table::copy(data.table::as.data.table(counts))
  counts[, space := normalize_top_tvl_space(space)]

  rows <- list()
  add_row <- function(protocol, candidate, match_source, match_status,
                      override = NULL, fuzzy = FALSE, eligible_override = TRUE,
                      replaced_space = "") {
    if (length(candidate) == 0 || is.na(candidate) || !nzchar(candidate)) return(NULL)
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      protocol_rank = protocol$protocol_rank,
      protocol_slug = protocol$protocol_slug,
      protocol_name = protocol$protocol_name,
      category = protocol$category,
      median_ethereum_tvl_usd = protocol$median_ethereum_tvl_usd,
      candidate_snapshot_space = normalize_top_tvl_space(candidate),
      match_source = match_source,
      match_status = match_status,
      override_action = if (is.null(override)) "" else as.character(override$override_action),
      override_rationale = if (is.null(override)) "" else as.character(override$rationale),
      override_evidence_url_or_note = if (is.null(override)) "" else as.character(override$evidence_url_or_note),
      override_reviewer = if (is.null(override)) "" else as.character(override$reviewer),
      override_review_date = if (is.null(override)) "" else as.character(override$review_date),
      replaces_snapshot_space = as.character(replaced_space),
      override_eligible_for_subset = isTRUE(eligible_override),
      fuzzy_diagnostic = isTRUE(fuzzy)
    )
  }

  for (idx in seq_len(nrow(protocols))) {
    protocol <- protocols[idx]
    protocol_overrides <- overrides[protocol_slug == protocol$protocol_slug]
    direct_spaces <- parse_snapshot_governance_ids_r(protocol$snapshot_governance_ids)
    if (length(direct_spaces) > 0) {
      for (space in sort(direct_spaces)) {
        add_row(protocol, space, "defillama_governance_id", "accepted")
      }
    }

    parent_spaces <- parse_snapshot_governance_ids_r(protocol$parent_snapshot_governance_ids)
    parent_spaces_new <- setdiff(parent_spaces, direct_spaces)
    if (length(parent_spaces_new) > 0) {
      for (space in sort(parent_spaces_new)) {
        add_row(protocol, space, "defillama_parent_governance_id", "accepted")
      }
    }

    if (nrow(protocol_overrides) > 0) {
      rejects <- protocol_overrides[override_action == "reject"]
      if (nrow(rejects) > 0) {
        for (reject_idx in seq_len(nrow(rejects))) {
          rejected_space <- rejects$snapshot_space[[reject_idx]]
          existing <- vapply(rows, function(row) {
            row$protocol_slug == protocol$protocol_slug &&
              row$candidate_snapshot_space == rejected_space
          }, logical(1))
          if (any(existing)) {
            for (row_idx in which(existing)) {
              rows[[row_idx]][, `:=`(
                match_status = "rejected",
                match_source = "override_reject",
                override_action = "reject",
                override_rationale = rejects$rationale[[reject_idx]],
                override_evidence_url_or_note = rejects$evidence_url_or_note[[reject_idx]],
                override_reviewer = rejects$reviewer[[reject_idx]],
                override_review_date = rejects$review_date[[reject_idx]],
                override_eligible_for_subset = FALSE
              )]
            }
          } else {
            add_row(
              protocol,
              rejected_space,
              "override_reject",
              "rejected",
              override = rejects[reject_idx],
              eligible_override = FALSE
            )
          }
        }
      }

      replacements <- protocol_overrides[override_action == "replace"]
      if (nrow(replacements) > 0) {
        for (replace_idx in seq_len(nrow(replacements))) {
          old_space <- replacements$replaces_snapshot_space[[replace_idx]]
          new_space <- replacements$snapshot_space[[replace_idx]]
          existing_old <- vapply(rows, function(row) {
            row$protocol_slug == protocol$protocol_slug &&
              row$candidate_snapshot_space == old_space
          }, logical(1))
          if (any(existing_old)) {
            for (row_idx in which(existing_old)) {
              rows[[row_idx]][, `:=`(
                match_status = "replaced",
                match_source = "override_replace",
                override_action = "replace",
                override_rationale = replacements$rationale[[replace_idx]],
                override_evidence_url_or_note = replacements$evidence_url_or_note[[replace_idx]],
                override_reviewer = replacements$reviewer[[replace_idx]],
                override_review_date = replacements$review_date[[replace_idx]],
                replaces_snapshot_space = old_space,
                override_eligible_for_subset = FALSE
              )]
            }
          } else {
            add_row(
              protocol,
              old_space,
              "override_replace",
              "replaced",
              override = replacements[replace_idx],
              eligible_override = FALSE,
              replaced_space = old_space
            )
          }
          add_row(
            protocol,
            new_space,
            "override_replace",
            "accepted",
            override = replacements[replace_idx],
            eligible_override = isTRUE(replacements$eligible_for_subset[[replace_idx]]),
            replaced_space = old_space
          )
        }
      }

      accepts <- protocol_overrides[override_action == "accept"]
      if (nrow(accepts) > 0) {
        for (accept_idx in seq_len(nrow(accepts))) {
          accepted_space <- accepts$snapshot_space[[accept_idx]]
          existing <- vapply(rows, function(row) {
            row$protocol_slug == protocol$protocol_slug &&
              row$candidate_snapshot_space == accepted_space &&
              row$match_status != "replaced"
          }, logical(1))
          if (any(existing)) {
            for (row_idx in which(existing)) {
              rows[[row_idx]][, `:=`(
                match_source = "override_accept",
                override_action = "accept",
                override_rationale = accepts$rationale[[accept_idx]],
                override_evidence_url_or_note = accepts$evidence_url_or_note[[accept_idx]],
                override_reviewer = accepts$reviewer[[accept_idx]],
                override_review_date = accepts$review_date[[accept_idx]],
                override_eligible_for_subset = isTRUE(accepts$eligible_for_subset[[accept_idx]])
              )]
            }
          } else {
            add_row(
              protocol,
              accepted_space,
              "override_accept",
              "accepted",
              override = accepts[accept_idx],
              eligible_override = isTRUE(accepts$eligible_for_subset[[accept_idx]])
            )
          }
        }
      }
    }

    accepted_now <- vapply(rows, function(row) {
      row$protocol_slug == protocol$protocol_slug && row$match_status == "accepted"
    }, logical(1))
    if (!any(accepted_now)) {
      fuzzy_spaces <- fuzzy_snapshot_candidates(protocol, snapshot_spaces)
      if (length(fuzzy_spaces) > 0) {
        for (space in sort(fuzzy_spaces)) {
          add_row(protocol, space, "fuzzy_diagnostic", "fuzzy_diagnostic", fuzzy = TRUE, eligible_override = FALSE)
        }
      }
    }
  }

  if (length(rows) == 0) {
    return(data.table::data.table())
  }
  table_dt <- data.table::rbindlist(rows, fill = TRUE)
  table_dt <- merge(table_dt, snapshot_spaces, by.x = "candidate_snapshot_space", by.y = "space", all.x = TRUE, sort = FALSE)
  table_dt[is.na(raw_snapshot_flagged), raw_snapshot_flagged := FALSE]
  table_dt[is.na(raw_snapshot_deleted), raw_snapshot_deleted := FALSE]
  table_dt <- merge(table_dt, counts, by.x = "candidate_snapshot_space", by.y = "space", all.x = TRUE, sort = FALSE)
  table_dt[, in_analysis_release := !is.na(choices) & choices > 0]
  table_dt[is.na(proposals), proposals := NA_integer_]
  table_dt[is.na(choices), choices := NA_integer_]
  table_dt[, snapshot_status := data.table::fifelse(
    raw_snapshot_deleted, "Deleted",
    data.table::fifelse(raw_snapshot_flagged, "Flagged", "Active")
  )]
  table_dt[, subset_candidate := match_status == "accepted" &
    override_eligible_for_subset &
    !raw_snapshot_flagged &
    !raw_snapshot_deleted &
    in_analysis_release]
  data.table::setorder(table_dt, protocol_rank, candidate_snapshot_space, match_status)
  table_dt <- annotate_top_tvl_sample_memberships(table_dt, alias_n = n)
  table_dt[, subset_exclusion_reason := data.table::fifelse(
    used_in_top_tvl_robustness_subset, "",
    data.table::fifelse(match_status == "rejected", "rejected_override",
      data.table::fifelse(match_status == "replaced", "replaced_by_override",
        data.table::fifelse(fuzzy_diagnostic, "fuzzy_diagnostic_only",
          data.table::fifelse(raw_snapshot_deleted, "raw_snapshot_deleted",
            data.table::fifelse(raw_snapshot_flagged, "raw_snapshot_flagged",
              data.table::fifelse(!in_analysis_release, "missing_from_current_release",
                data.table::fifelse(match_status != "accepted", "not_accepted", "outside_top_tvl_subset")
              )
            )
          )
        )
      )
    )
  )]
  table_dt[]
}

write_top_tvl_match_diagnostics <- function(table, path = default_top_tvl_match_diagnostics_path()) {
  path <- generated_output_path(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(table, path)
  invisible(path)
}

snapshot_match_summary_table <- function(protocols, diagnostics, existing_summary = NULL,
                                         review_seed = NULL,
                                         expected_n = NULL,
                                         scope_label = "Snapshot review") {
  require_packages("data.table")
  protocols <- data.table::copy(data.table::as.data.table(protocols))
  diagnostics <- data.table::copy(data.table::as.data.table(diagnostics))

  required_protocol <- c("protocol_rank", "protocol_slug", "protocol_name")
  missing_protocol <- setdiff(required_protocol, names(protocols))
  if (length(missing_protocol) > 0) {
    stop(scope_label, " protocol table missing column(s): ", paste(missing_protocol, collapse = ", "), call. = FALSE)
  }
  protocols[, protocol_rank := as.integer(protocol_rank)]
  protocols[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
  protocols[, protocol_name := trimws(as.character(protocol_name))]
  protocols <- protocols[order(protocol_rank, protocol_slug)]
  if (!is.null(expected_n) && nrow(protocols) != expected_n) {
    stop(scope_label, " review summary must be generated from exactly ", expected_n, " protocol rows.", call. = FALSE)
  }

  if (nrow(diagnostics) == 0) {
    diagnostic_summary <- data.table::data.table(
      protocol_rank = integer(),
      protocol_slug = character(),
      matched_spaces = character(),
      match_source = character(),
      fuzzy_candidates = character(),
      override_accept_spaces = character(),
      override_accept_proposals = integer(),
      override_accept_choices = integer(),
      override_accept_evidence_url_or_note = character(),
      override_accept_rationale = character(),
      override_accept_reviewer = character(),
      override_accept_review_date = character()
    )
  } else {
    required_diagnostics <- c("protocol_rank", "protocol_slug", "candidate_snapshot_space", "match_source", "match_status")
    missing_diagnostics <- setdiff(required_diagnostics, names(diagnostics))
    if (length(missing_diagnostics) > 0) {
      stop("Top-TVL diagnostics missing column(s): ", paste(missing_diagnostics, collapse = ", "), call. = FALSE)
    }
    diagnostics[, protocol_rank := as.integer(protocol_rank)]
    diagnostics[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
    diagnostics[, candidate_snapshot_space := normalize_top_tvl_space(candidate_snapshot_space)]
    diagnostics[, match_source := trimws(as.character(match_source))]
    diagnostics[, match_status := trimws(as.character(match_status))]
    for (field in c("proposals", "choices")) {
      if (!field %in% names(diagnostics)) {
        diagnostics[, (field) := NA_integer_]
      }
      diagnostics[, (field) := suppressWarnings(as.integer(get(field)))]
    }
    for (field in c("override_rationale", "override_evidence_url_or_note", "override_reviewer", "override_review_date")) {
      if (!field %in% names(diagnostics)) {
        diagnostics[, (field) := ""]
      }
      diagnostics[, (field) := trimws(as.character(get(field)))]
    }
    sum_optional_int <- function(x) {
      x <- suppressWarnings(as.integer(x))
      if (length(x) == 0L || all(is.na(x))) {
        return(NA_integer_)
      }
      as.integer(sum(x, na.rm = TRUE))
    }
    diagnostic_summary <- diagnostics[, .(
      matched_spaces = paste(sort(unique(candidate_snapshot_space[match_status == "accepted"])), collapse = " | "),
      match_source = paste(sort(unique(match_source[match_status == "accepted"])), collapse = " | "),
      fuzzy_candidates = paste(sort(unique(candidate_snapshot_space[match_status == "fuzzy_diagnostic"])), collapse = " | "),
      override_accept_spaces = paste(
        sort(unique(candidate_snapshot_space[match_status == "accepted" & match_source == "override_accept"])),
        collapse = " | "
      ),
      override_accept_proposals = sum_optional_int(proposals[match_status == "accepted" & match_source == "override_accept"]),
      override_accept_choices = sum_optional_int(choices[match_status == "accepted" & match_source == "override_accept"]),
      override_accept_evidence_url_or_note = collapse_nonempty_strings(
        override_evidence_url_or_note[match_status == "accepted" & match_source == "override_accept"]
      ),
      override_accept_rationale = collapse_nonempty_strings(
        override_rationale[match_status == "accepted" & match_source == "override_accept"]
      ),
      override_accept_reviewer = collapse_nonempty_strings(
        override_reviewer[match_status == "accepted" & match_source == "override_accept"]
      ),
      override_accept_review_date = collapse_nonempty_strings(
        override_review_date[match_status == "accepted" & match_source == "override_accept"]
      )
    ), by = .(protocol_rank, protocol_slug)]
  }

  result <- merge(
    protocols[, .(protocol_rank, protocol_slug, protocol_name)],
    diagnostic_summary,
    by = c("protocol_rank", "protocol_slug"),
    all.x = TRUE,
    sort = FALSE
  )
  for (field in c(
    "matched_spaces", "match_source", "fuzzy_candidates", "override_accept_spaces",
    "override_accept_evidence_url_or_note", "override_accept_rationale",
    "override_accept_reviewer", "override_accept_review_date"
  )) {
    result[is.na(get(field)), (field) := ""]
  }
  for (field in c("override_accept_proposals", "override_accept_choices")) {
    result[is.na(get(field)), (field) := NA_integer_]
  }
  result[, status := data.table::fifelse(
    nzchar(matched_spaces),
    "matched",
    data.table::fifelse(nzchar(fuzzy_candidates), "fuzzy_only", "unmatched")
  )]
  result <- result[, .(
    protocol_rank,
    protocol_slug,
    protocol_name,
    status,
    matched_spaces,
    match_source,
    fuzzy_candidates,
    override_accept_spaces,
    override_accept_proposals,
    override_accept_choices,
    override_accept_evidence_url_or_note,
    override_accept_rationale,
    override_accept_reviewer,
    override_accept_review_date
  )]

  review_cols <- snapshot_review_columns()
  for (field in setdiff(review_cols, c("release_proposals", "release_choices"))) {
    result[, (field) := ""]
  }
  result[, release_proposals := NA_integer_]
  result[, release_choices := NA_integer_]
  result[status == "matched", review_decision := "already_matched"]
  result[status != "matched", review_decision := "pending_review"]
  result[status == "fuzzy_only" & nzchar(fuzzy_candidates), reviewed_snapshot_space := fuzzy_candidates]
  result[nzchar(override_accept_spaces), `:=`(
    review_decision = "accept_override",
    reviewed_snapshot_space = override_accept_spaces,
    release_proposals = override_accept_proposals,
    release_choices = override_accept_choices,
    evidence_url_or_note = override_accept_evidence_url_or_note,
    reviewer = override_accept_reviewer,
    review_date = override_accept_review_date,
    review_note = override_accept_rationale
  )]
  result[, c(
    "override_accept_spaces", "override_accept_proposals", "override_accept_choices",
    "override_accept_evidence_url_or_note", "override_accept_rationale",
    "override_accept_reviewer", "override_accept_review_date"
  ) := NULL]

  seed <- normalize_top100_review_seed(review_seed)
  if (nrow(seed) > 0) {
    result <- merge(
      result,
      seed,
      by = "protocol_slug",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", "_seed")
    )
    for (field in c("reviewed_snapshot_space", "evidence_url_or_note", "review_note")) {
      seed_field <- paste0(field, "_seed")
      if (!seed_field %in% names(result)) next
      use_seed <- result$review_decision == "pending_review" &
        (is.na(result[[field]]) | !nzchar(trimws(as.character(result[[field]])))) &
        !is.na(result[[seed_field]]) &
        nzchar(trimws(as.character(result[[seed_field]])))
      result[use_seed, (field) := get(seed_field)]
      result[, (seed_field) := NULL]
    }
  }
  missing_review_seed <- result$review_decision == "pending_review" &
    (is.na(result$reviewed_snapshot_space) | !nzchar(trimws(as.character(result$reviewed_snapshot_space)))) &
    (is.na(result$evidence_url_or_note) | !nzchar(trimws(as.character(result$evidence_url_or_note)))) &
    (is.na(result$review_note) | !nzchar(trimws(as.character(result$review_note))))
  result[missing_review_seed, review_note := "No seed candidate found; reviewer should verify manually before deciding."]

  if (!is.null(existing_summary)) {
    existing <- data.table::copy(data.table::as.data.table(existing_summary))
    existing[, protocol_rank := as.integer(protocol_rank)]
    existing[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
    if ("review_decision" %in% names(existing)) {
      existing[, review_decision := trimws(as.character(review_decision))]
      has_existing_decision <- !is.na(existing$review_decision) & nzchar(existing$review_decision)
      invalid_existing <- sort(unique(existing$review_decision[
        has_existing_decision &
          !(existing$review_decision %in% snapshot_review_decisions())
      ]))
      if (length(invalid_existing) > 0) {
        stop("Invalid ", scope_label, " review_decision value(s): ", paste(invalid_existing, collapse = ", "), call. = FALSE)
      }
    }
    available_review_cols <- intersect(review_cols, names(existing))
    if (length(available_review_cols) > 0) {
      existing_review <- existing[, c("protocol_rank", "protocol_slug", available_review_cols), with = FALSE]
      result <- merge(
        result,
        existing_review,
        by = c("protocol_rank", "protocol_slug"),
        all.x = TRUE,
        sort = FALSE,
        suffixes = c("", "_existing")
      )
      manual_decisions <- setdiff(snapshot_review_decisions(), c("already_matched", "pending_review"))
      if ("review_decision_existing" %in% names(result)) {
        live_override_accept <- grepl("override_accept", result$match_source, fixed = TRUE)
        preserve_existing_review <- result$review_decision_existing %in% manual_decisions &
          !live_override_accept
      } else {
        preserve_existing_review <- rep(FALSE, nrow(result))
      }
      for (field in available_review_cols) {
        existing_field <- paste0(field, "_existing")
        keep_existing <- preserve_existing_review &
          !is.na(result[[existing_field]]) &
          nzchar(trimws(as.character(result[[existing_field]])))
        if (field %in% c("release_proposals", "release_choices")) {
          keep_existing <- preserve_existing_review & !is.na(result[[existing_field]])
        }
        result[keep_existing, (field) := get(existing_field)]
        result[, (existing_field) := NULL]
      }
    }
  }

  invalid <- sort(unique(result$review_decision[!(result$review_decision %in% snapshot_review_decisions())]))
  if (length(invalid) > 0) {
    stop("Invalid ", scope_label, " review_decision value(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  }
  result[order(protocol_rank, protocol_slug)]
}

top100_match_summary_table <- function(protocols, diagnostics, existing_summary = NULL,
                                       review_seed = NULL) {
  snapshot_match_summary_table(
    protocols,
    diagnostics,
    existing_summary = existing_summary,
    review_seed = review_seed,
    expected_n = 100L,
    scope_label = "Top 100"
  )
}

write_top100_match_summary <- function(
    protocols,
    diagnostics,
    path = default_top100_match_summary_path(),
    existing_summary_path = path,
    review_seed = NULL) {
  existing_summary <- NULL
  if (!is.na(existing_summary_path) && nzchar(existing_summary_path) && file.exists(existing_summary_path)) {
    existing_summary <- data.table::fread(existing_summary_path, showProgress = FALSE)
  }
  summary_dt <- top100_match_summary_table(
    protocols,
    diagnostics,
    existing_summary = existing_summary,
    review_seed = review_seed
  )
  path <- generated_output_path(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(summary_dt, path)
  invisible(path)
}

top_tvl_sample_membership_table <- function(table) {
  require_packages("data.table")
  dt <- data.table::copy(data.table::as.data.table(table))
  sample_sizes <- top_tvl_sample_sizes()
  required <- c(
    "top_tvl_sample_rank", "candidate_snapshot_space", "protocol_rank",
    "protocol_slug", "protocol_name", "category", "median_ethereum_tvl_usd",
    "proposals", "choices",
    vapply(names(sample_sizes), top_tvl_sample_column, character(1))
  )
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop(
      "Top-TVL match table missing membership column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  rows <- lapply(names(sample_sizes), function(sample_name) {
    column <- top_tvl_sample_column(sample_name)
    sample_dt <- dt[get(column) == TRUE]
    if (nrow(sample_dt) == 0) {
      return(NULL)
    }
    sample_dt <- sample_dt[order(top_tvl_sample_rank, protocol_rank, candidate_snapshot_space)]
    sample_dt[, .(
      sample = sample_name,
      top_tvl_sample_rank,
      candidate_snapshot_space,
      protocol_rank,
      protocol_slug,
      protocol_name,
      category,
      median_ethereum_tvl_usd,
      proposals,
      choices
    )]
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(data.table::data.table(
      sample = character(),
      top_tvl_sample_rank = integer(),
      candidate_snapshot_space = character(),
      protocol_rank = integer(),
      protocol_slug = character(),
      protocol_name = character(),
      category = character(),
      median_ethereum_tvl_usd = numeric(),
      proposals = integer(),
      choices = integer()
    ))
  }
  data.table::rbindlist(rows, use.names = TRUE)
}

write_top_tvl_sample_memberships <- function(
    table,
    path = default_top_tvl_sample_memberships_path()) {
  membership_dt <- top_tvl_sample_membership_table(table)
  path <- generated_output_path(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(membership_dt, path)
  invisible(path)
}

# Read the curated Top-TVL sample memberships CSV (the source of truth produced
# by python run.py tvl-memberships) and return a named list of
# normalized, unique candidate spaces per sample (top25/top50/top100). Returns
# NULL if the file cannot be read or lacks the required columns, so callers can
# fall back to recomputing from the match table.
read_top_tvl_sample_memberships <- function(
    path = default_top_tvl_sample_memberships_path()) {
  require_packages("data.table")
  if (length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  membership_dt <- tryCatch(
    data.table::fread(path, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(membership_dt) ||
      !all(c("sample", "candidate_snapshot_space") %in% names(membership_dt))) {
    return(NULL)
  }
  sample_sizes <- top_tvl_sample_sizes()
  sample_spaces <- lapply(names(sample_sizes), function(sample_name) {
    spaces <- membership_dt[sample == sample_name, candidate_snapshot_space]
    spaces <- normalize_top_tvl_space(spaces)
    unique(spaces[!is.na(spaces) & nzchar(spaces)])
  })
  names(sample_spaces) <- names(sample_sizes)
  sample_spaces
}

write_top100_matching_review_artifacts <- function(
    model_df,
    path = default_top_tvl_spaces_path(),
    display_n = 33,
    overrides_path = default_top_tvl_overrides_path(),
    snapshot_spaces_path = default_snapshot_spaces_path(),
    diagnostics_path = default_top_tvl_match_diagnostics_path(),
    top100_summary_path = NULL,
    sample_memberships_path = NA_character_,
    draft_review_path = default_top100_overrides_draft_path(project_root),
    review_report_path = default_top100_match_review_path(project_root),
    project_root = find_project_root()) {
  require_packages("data.table")
  if (is.null(top100_summary_path)) {
    top100_summary_path <- default_top100_match_summary_path(project_root)
  }
  protocols <- load_top_tvl_table(path, project_root = project_root)
  overrides <- load_defillama_snapshot_overrides(overrides_path)
  snapshot_spaces <- load_snapshot_spaces(snapshot_spaces_path)
  counts <- current_release_space_counts(model_df)
  table_dt <- build_top_tvl_match_table(
    protocols,
    overrides,
    snapshot_spaces,
    counts,
    n = 25,
    display_n = display_n
  )

  if (!is.na(diagnostics_path) && nzchar(diagnostics_path)) {
    write_top_tvl_match_diagnostics(table_dt, diagnostics_path)
  }
  if (!is.na(top100_summary_path) && nzchar(top100_summary_path)) {
    review_seed <- top100_review_seed_table(
      draft_path = draft_review_path,
      review_report_path = review_report_path
    )
    write_top100_match_summary(
      protocols,
      table_dt,
      path = top100_summary_path,
      review_seed = review_seed
    )
  }
  if (!is.na(sample_memberships_path) && nzchar(sample_memberships_path)) {
    write_top_tvl_sample_memberships(table_dt, sample_memberships_path)
  }

  invisible(list(
    table = as.data.frame(table_dt),
    paths = list(
      diagnostics_path = diagnostics_path,
      top100_summary_path = top100_summary_path,
      sample_memberships_path = sample_memberships_path
    )
  ))
}

top_tvl_dao_spaces <- function(model_df, n = 25, path = default_top_tvl_spaces_path(),
                               display_n = 33,
                               overrides_path = default_top_tvl_overrides_path(),
                               snapshot_spaces_path = default_snapshot_spaces_path(),
                               diagnostics_path = default_top_tvl_match_diagnostics_path(),
                               sample_memberships_path = NULL,
                               project_root = find_project_root()) {
  require_packages("data.table")
  if (is.null(sample_memberships_path)) {
    sample_memberships_path <- default_top_tvl_sample_memberships_path(
      project_root,
      diagnostics_path = diagnostics_path
    )
  }
  table <- load_top_tvl_table(path, project_root = project_root)
  overrides <- load_defillama_snapshot_overrides(overrides_path)
  snapshot_spaces <- load_snapshot_spaces(snapshot_spaces_path)
  counts <- current_release_space_counts(model_df)
  table_dt <- build_top_tvl_match_table(
    table,
    overrides,
    snapshot_spaces,
    counts,
    n = n,
    display_n = display_n
  )
  if (!is.na(diagnostics_path) && nzchar(diagnostics_path)) {
    write_top_tvl_match_diagnostics(table_dt, diagnostics_path)
  }
  sample_sizes <- top_tvl_sample_sizes()

  # The curated memberships CSV is the source of truth: it carries the reviewer
  # matches (matched_spaces + reviewed_snapshot_space) that the on-the-fly match
  # table does not reproduce. Prefer it when present; only fall back to the
  # recomputed table flags if it is missing/unreadable, and warn loudly so a
  # stale or absent CSV cannot silently collapse the three subsets together.
  # Regeneration is the dedicated script's job, so we never write the CSV here.
  csv_sample_spaces <- read_top_tvl_sample_memberships(sample_memberships_path)
  if (!is.null(csv_sample_spaces)) {
    sample_spaces <- csv_sample_spaces
    # Source of truth present: the Top 25 alias (drives top_tvl_dao_row) is the
    # curated Top 25 sample.
    spaces <- sample_spaces[["top25"]]
  } else {
    if (!is.na(sample_memberships_path) && nzchar(sample_memberships_path)) {
      warning(
        "Top-TVL sample memberships CSV missing or unreadable at '",
        sample_memberships_path,
        "'; falling back to recomputed match-table flags (reviewer matches ",
        "will be ignored). Run python run.py tvl-memberships --data-root INPUT.",
        call. = FALSE
      )
    }
    sample_spaces <- lapply(names(sample_sizes), function(sample_name) {
      column <- top_tvl_sample_column(sample_name)
      unique(table_dt[get(column) == TRUE, candidate_snapshot_space])
    })
    names(sample_spaces) <- names(sample_sizes)
    # Fall back to the existing computation: the alias stays the rank-truncated
    # robustness subset (respects ROBUSTNESS_TOP_TVL_SPACES_N).
    spaces <- unique(table_dt[used_in_top_tvl_robustness_subset == TRUE, candidate_snapshot_space])
    # No curated CSV existed, so emit the recomputed memberships as a derived
    # diagnostic. This only writes when the source-of-truth CSV is absent; it
    # never overwrites a curated CSV (that path is handled above).
    if (!is.na(sample_memberships_path) && nzchar(sample_memberships_path)) {
      write_top_tvl_sample_memberships(table_dt, sample_memberships_path)
    }
  }

  table <- as.data.frame(table_dt)
  list(spaces = spaces, sample_spaces = sample_spaces, table = table)
}
