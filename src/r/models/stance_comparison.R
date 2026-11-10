################################################################################
## Stance heuristic comparison tables
################################################################################

source(file.path(PACKAGE_ROOT,"src/r/preparation/stance_patterns.R"),local=TRUE)

stance_levels <- c("APPROVE", "REJECT", "ABSTAIN", "OTHER")

analysis_root <- Sys.getenv("DAO_BIAS_DATA_ROOT",unset=PACKAGE_ROOT)
analysis_path <- function(...) {
  file.path(analysis_root, ...)
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

current_release_path_from_registry <- function() {
  registry_path <- analysis_path("metadata", "analysis_data_releases.csv")
  if (!file.exists(registry_path)) {
    stop("Release registry is missing: ", registry_path, call. = FALSE)
  }

  registry <- utils::read.csv(registry_path, stringsAsFactors = FALSE)
  if (!"release_name" %in% colnames(registry)) {
    stop("Release registry is missing release_name: ", registry_path, call. = FALSE)
  }

  current_rows <- registry[
    !is.na(registry$release_name) & registry$release_name == "current",
    ,
    drop = FALSE
  ]
  if (nrow(current_rows) != 1) {
    stop(
      "Release registry must contain exactly one current row: ",
      registry_path,
      call. = FALSE
    )
  }

  release_path <- if ("relative_path" %in% colnames(current_rows)) {
    current_rows$relative_path[[1]]
  } else if ("path" %in% colnames(current_rows)) {
    current_rows$path[[1]]
  } else {
    NA_character_
  }
  if (is.na(release_path) || !nzchar(release_path)) {
    stop("Current release registry row is missing relative_path/path: ", registry_path, call. = FALSE)
  }

  if (is_absolute_path(release_path)) {
    return(release_path)
  }
  analysis_path(release_path)
}

default_current_release_path <- function() {
  env_path <- Sys.getenv("STANCE_HEURISTIC_CURRENT_RELEASE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  current_release_path_from_registry()
}

default_final_candidate_path <- function() {
  Sys.getenv(
    "STANCE_HEURISTIC_FINAL_CANDIDATE",
    unset = analysis_path("data", "2024", "derived", "debug", "choices_25-nov-17_B_dup_excluded_m1_instrumented.csv")
  )
}

default_raw_choice_audit_path <- function() {
  Sys.getenv(
    "STANCE_HEURISTIC_RAW_CHOICE_AUDIT",
    unset = analysis_path("data", "2024", "derived", "debug", "stance_audit_raw_heuristic_choice_matches.csv")
  )
}

default_final_proposal_audit_path <- function() {
  Sys.getenv(
    "STANCE_HEURISTIC_FINAL_PROPOSAL_AUDIT",
    unset = analysis_path("data", "2024", "derived", "debug", "stance_audit_final_after_overrides_proposal_state.csv")
  )
}

default_old_raw_choice_audit_path <- function() {
  Sys.getenv("STANCE_HEURISTIC_OLD_RAW_CHOICE_AUDIT", unset = "")
}

default_output_dir <- function() {
  env_output <- Sys.getenv("STANCE_HEURISTIC_COMPARE_OUTPUT_DIR", unset = "")
  if (nzchar(env_output)) {
    return(env_output)
  }
  file.path(
    analysis_root,
    "data", "2024", "derived", "debug",
    paste0("stance_heuristic_comparison_", format(Sys.time(), "%Y%m%d-%H%M%S"))
  )
}

require_existing_file <- function(path, label) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop(paste("Missing", label, "at", path), call. = FALSE)
  }
  path
}

read_stance_csv <- function(path, label = basename(path)) {
  read.csv(
    require_existing_file(path, label),
    colClasses = "character",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

read_stance_csv_columns <- function(path, columns, label = basename(path)) {
  path <- require_existing_file(path, label)
  header <- names(read.csv(path, nrows = 0, check.names = FALSE))
  missing <- setdiff(columns, header)
  if (length(missing) > 0) {
    stop(
      paste(label, "missing column(s):", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  col_classes <- rep("NULL", length(header))
  names(col_classes) <- header
  col_classes[columns] <- "character"
  read.csv(
    path,
    colClasses = col_classes,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

as_clean_character <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x
}

normalize_stance <- function(x) {
  x <- toupper(as_clean_character(x))
  x[!nzchar(x)] <- "OTHER"
  x
}

require_columns <- function(df, cols, label) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(
      paste(label, "missing column(s):", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

logicalish <- function(x) {
  if (is.logical(x)) {
    return(x %in% TRUE)
  }
  tolower(as_clean_character(x)) %in% c("true", "t", "1", "yes")
}

drop_single_option_choice_rows <- function(df, proposal_col = "proposal", n_choices_col = "N_choices") {
  require_columns(df, proposal_col, "choice rows")
  proposals <- tolower(as_clean_character(df[[proposal_col]]))
  current_counts <- as.integer(ave(seq_len(nrow(df)), proposals, FUN = length))
  keep <- nzchar(proposals) & current_counts > 1

  if (n_choices_col %in% colnames(df)) {
    reported_counts <- suppressWarnings(as.numeric(as.character(df[[n_choices_col]])))
    keep <- keep & (is.na(reported_counts) | reported_counts > 1)
  }

  df[keep, , drop = FALSE]
}

stance_value_counts <- function(df, stance_col, dataset) {
  require_columns(df, stance_col, dataset)
  values <- normalize_stance(df[[stance_col]])
  counts <- as.integer(table(factor(values, levels = stance_levels)))
  data.frame(
    dataset = dataset,
    stance = stance_levels,
    rows = counts,
    stringsAsFactors = FALSE
  )
}

proposal_state_for_values <- function(values) {
  values <- normalize_stance(values)
  has_approve <- any(values == "APPROVE", na.rm = TRUE)
  has_reject <- any(values == "REJECT", na.rm = TRUE)
  if (has_approve && has_reject) return("both")
  if (has_approve && !has_reject) return("approve_only")
  if (!has_approve && has_reject) return("reject_only")
  "neither"
}

proposal_state_counts <- function(df, stance_col, dataset, proposal_col = "proposal") {
  require_columns(df, c(proposal_col, stance_col), dataset)
  proposals <- as_clean_character(df[[proposal_col]])
  states <- vapply(
    split(df[[stance_col]], proposals),
    proposal_state_for_values,
    character(1)
  )
  state_levels <- c("both", "approve_only", "reject_only", "neither")
  counts <- as.integer(table(factor(states, levels = state_levels)))
  data.frame(
    dataset = dataset,
    stance_state = state_levels,
    proposals = counts,
    stringsAsFactors = FALSE
  )
}

choice_key_frame <- function(df, key_cols) {
  require_columns(df, key_cols, "choice key")
  out <- as.data.frame(lapply(df[key_cols], as_clean_character), stringsAsFactors = FALSE)
  names(out) <- key_cols
  out
}

compound_choice_key <- function(key_df) {
  do.call(paste, c(key_df, sep = "\r"))
}

take_or_na <- function(values, idx) {
  out <- rep(NA_character_, length(idx))
  keep <- !is.na(idx)
  out[keep] <- values[idx[keep]]
  out
}

compare_choice_labels <- function(before_df, after_df, before_stance_col, after_stance_col, key_cols = c("proposal", "choice_idx")) {
  require_columns(before_df, c(key_cols, before_stance_col), "before label data")
  require_columns(after_df, c(key_cols, after_stance_col), "after label data")

  before_keys <- choice_key_frame(before_df, key_cols)
  after_keys <- choice_key_frame(after_df, key_cols)
  before_key <- compound_choice_key(before_keys)
  after_key <- compound_choice_key(after_keys)

  if (any(duplicated(before_key))) {
    stop("before label data has duplicate choice keys", call. = FALSE)
  }
  if (any(duplicated(after_key))) {
    stop("after label data has duplicate choice keys", call. = FALSE)
  }

  all_keys <- unique(c(before_key, after_key))
  before_idx <- match(all_keys, before_key)
  after_idx <- match(all_keys, after_key)

  joined <- data.frame(.row_id = seq_along(all_keys), stringsAsFactors = FALSE)
  for (key_col in key_cols) {
    value <- take_or_na(before_keys[[key_col]], before_idx)
    missing_before <- is.na(value)
    value[missing_before] <- take_or_na(after_keys[[key_col]], after_idx)[missing_before]
    joined[[key_col]] <- value
  }

  before_choice_text <- if ("choice_text" %in% names(before_df)) as_clean_character(before_df$choice_text) else rep("", nrow(before_df))
  after_choice_text <- if ("choice_text" %in% names(after_df)) as_clean_character(after_df$choice_text) else rep("", nrow(after_df))

  joined$before_stance <- take_or_na(normalize_stance(before_df[[before_stance_col]]), before_idx)
  joined$after_stance <- take_or_na(normalize_stance(after_df[[after_stance_col]]), after_idx)
  joined$before_choice_text <- take_or_na(before_choice_text, before_idx)
  joined$after_choice_text <- take_or_na(after_choice_text, after_idx)
  joined$row_status <- ifelse(
    is.na(joined$before_stance), "after_only",
    ifelse(is.na(joined$after_stance), "before_only", "both")
  )
  joined$choice_text <- ifelse(
    !is.na(joined$after_choice_text) & nzchar(joined$after_choice_text),
    joined$after_choice_text,
    joined$before_choice_text
  )
  joined$changed <- joined$row_status == "both" & joined$before_stance != joined$after_stance
  joined$.row_id <- NULL
  joined
}

row_status_counts <- function(joined) {
  statuses <- c("both", "before_only", "after_only")
  data.frame(
    row_status = statuses,
    rows = as.integer(table(factor(joined$row_status, levels = statuses))),
    stringsAsFactors = FALSE
  )
}

top_label_changes <- function(joined, from_stances, to_stances, n = 25) {
  filtered <- joined[
    joined$row_status == "both" &
      joined$before_stance %in% from_stances &
      joined$after_stance %in% to_stances,
    ,
    drop = FALSE
  ]
  if (nrow(filtered) == 0) {
    return(data.frame(
      before_stance = character(),
      after_stance = character(),
      choice_text = character(),
      rows = integer(),
      example_proposal = character(),
      stringsAsFactors = FALSE
    ))
  }

  filtered$rows <- 1L
  group_cols <- c("before_stance", "after_stance", "choice_text")
  counts <- aggregate(filtered$rows, filtered[group_cols], sum)
  names(counts)[names(counts) == "x"] <- "rows"
  examples <- aggregate(filtered$proposal, filtered[group_cols], function(x) x[1])
  names(examples)[names(examples) == "x"] <- "example_proposal"
  out <- merge(counts, examples, by = group_cols, sort = FALSE)
  out <- out[order(-out$rows, out$before_stance, out$after_stance, out$choice_text), ]
  rownames(out) <- NULL
  head(out, n)
}

low_confidence_rows <- function(raw_choice_audit) {
  required <- c(
    "raw_resolved_stance",
    "raw_match_source",
    "raw_matched_pattern",
    "raw_canonical_pattern",
    "raw_all_match_sources"
  )
  require_columns(raw_choice_audit, required, "raw choice audit")

  selected_low_confidence <- grepl("low_confidence", raw_choice_audit$raw_match_source, fixed = TRUE)
  matched_low_confidence <- grepl("low_confidence", raw_choice_audit$raw_all_match_sources, fixed = TRUE)
  low_conf <- raw_choice_audit[selected_low_confidence | matched_low_confidence, , drop = FALSE]
  if (nrow(low_conf) == 0) {
    low_conf$low_confidence_role <- character()
    low_conf$selected_label_depends_on_low_confidence <- logical()
    return(low_conf)
  }

  selected_subset <- selected_low_confidence[selected_low_confidence | matched_low_confidence]
  low_conf$low_confidence_role <- ifelse(
    selected_subset,
    "selected_low_confidence_source",
    "also_matched_low_confidence"
  )
  low_conf$selected_label_depends_on_low_confidence <- selected_subset &
    low_conf$raw_resolved_stance != "OTHER"
  low_conf
}

low_confidence_dependency_counts <- function(raw_choice_audit) {
  low_conf <- low_confidence_rows(raw_choice_audit)
  counts <- data.frame(
    low_confidence_count_type = c(
      "selected_label_depends_on_low_confidence",
      "selected_low_confidence_source",
      "also_matched_low_confidence",
      "any_low_confidence_match"
    ),
    selected_label_depends_on_low_confidence = c(TRUE, FALSE, FALSE, FALSE),
    rows = c(
      sum(low_conf$selected_label_depends_on_low_confidence),
      sum(low_conf$low_confidence_role == "selected_low_confidence_source"),
      sum(low_conf$low_confidence_role == "also_matched_low_confidence"),
      nrow(low_conf)
    ),
    stringsAsFactors = FALSE
  )
  counts$rows <- as.integer(counts$rows)
  counts
}

low_confidence_summary <- function(raw_choice_audit, n = 50) {
  low_conf <- low_confidence_rows(raw_choice_audit)
  if (nrow(low_conf) == 0) {
    return(data.frame(
      low_confidence_role = character(),
      selected_label_depends_on_low_confidence = logical(),
      raw_resolved_stance = character(),
      raw_match_source = character(),
      raw_matched_pattern = character(),
      raw_canonical_pattern = character(),
      rows = integer(),
      stringsAsFactors = FALSE
    ))
  }
  low_conf$rows <- 1L
  group_cols <- c(
    "low_confidence_role",
    "selected_label_depends_on_low_confidence",
    "raw_resolved_stance",
    "raw_match_source",
    "raw_matched_pattern",
    "raw_canonical_pattern"
  )
  out <- aggregate(low_conf$rows, low_conf[group_cols], sum)
  names(out)[names(out) == "x"] <- "rows"
  out <- out[order(out$low_confidence_role, -out$rows, out$raw_match_source, out$raw_matched_pattern), ]
  rownames(out) <- NULL
  head(out, n)
}

conflict_summary <- function(raw_choice_audit, n = 50) {
  required <- c(
    "raw_resolved_stance",
    "raw_match_source",
    "raw_matched_pattern",
    "raw_all_match_sources",
    "raw_has_conflicting_matches",
    "choice_text"
  )
  require_columns(raw_choice_audit, required, "raw choice audit")
  conflicts <- raw_choice_audit[
    logicalish(raw_choice_audit$raw_has_conflicting_matches),
    ,
    drop = FALSE
  ]
  if (nrow(conflicts) == 0) {
    return(data.frame(
      raw_resolved_stance = character(),
      raw_match_source = character(),
      raw_matched_pattern = character(),
      raw_all_match_sources = character(),
      choice_text = character(),
      rows = integer(),
      stringsAsFactors = FALSE
    ))
  }
  conflicts$rows <- 1L
  group_cols <- c(
    "raw_resolved_stance",
    "raw_match_source",
    "raw_matched_pattern",
    "raw_all_match_sources",
    "choice_text"
  )
  out <- aggregate(conflicts$rows, conflicts[group_cols], sum)
  names(out)[names(out) == "x"] <- "rows"
  out <- out[order(-out$rows, out$choice_text), ]
  rownames(out) <- NULL
  head(out, n)
}

known_example_summary <- function() {
  examples <- c(
    "s",
    " y ",
    "Adam S",
    "Jeep Wrangler Unlimited S",
    "No change",
    "do not support",
    "We do not support X",
    "support",
    "customer support",
    "支持",
    "不支持",
    "会",
    "不会",
    "可以",
    "不可以",
    "有",
    "这个提案有问题"
  )
  rows <- lapply(examples, function(choice_text) {
    match <- classifyStanceChoiceInstrumented(data.frame(
      choice_text = choice_text,
      body = "Governance proposal body",
      stringsAsFactors = FALSE
    ))
    data.frame(
      choice_text = choice_text,
      resolved_stance = match$resolved_stance,
      match_source = match$match_source,
      matched_pattern = match$matched_pattern,
      canonical_pattern = match$canonical_pattern,
      all_match_sources = match$all_match_sources,
      has_conflicting_matches = match$has_conflicting_matches,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

build_final_proposal_audit <- function(final_candidate, stage = "2_stance_v5") {
  buildStanceProposalStateAudit(
    final_candidate,
    stage = stage,
    checkpoint = "final_after_overrides",
    value_col = "stance2"
  )
}

load_or_build_final_proposal_audit <- function(path, final_candidate) {
  required <- c(
    "is_one_sided_approve_reject",
    "is_one_sided_documented_exception",
    "one_sided_action"
  )
  if (nzchar(path) && file.exists(path)) {
    audit <- read_stance_csv(path, "final proposal audit")
    if (all(required %in% names(audit))) {
      return(audit)
    }
  }
  build_final_proposal_audit(final_candidate)
}

undocumented_final_one_sided <- function(final_proposal_audit) {
  require_columns(
    final_proposal_audit,
    c("proposal", "stance_state", "is_one_sided_documented_exception"),
    "final proposal audit"
  )
  final_proposal_audit[
    final_proposal_audit$stance_state %in% c("approve_only", "reject_only") &
      !logicalish(final_proposal_audit$is_one_sided_documented_exception),
    ,
    drop = FALSE
  ]
}

assert_no_undocumented_one_sided <- function(final_proposal_audit) {
  bad <- undocumented_final_one_sided(final_proposal_audit)
  if (nrow(bad) > 0) {
    stop(
      paste("Final audit still has", nrow(bad), "undocumented one-sided proposal(s)."),
      call. = FALSE
    )
  }
  TRUE
}

raw_heuristic_comparison <- function(old_raw_choice_audit, new_raw_choice_audit) {
  compare_choice_labels(
    old_raw_choice_audit,
    new_raw_choice_audit,
    before_stance_col = "raw_resolved_stance",
    after_stance_col = "raw_resolved_stance"
  )
}

write_csv_output <- function(df, path) {
  if (file.exists(path)) {
    stop(paste("Refusing to overwrite existing comparison output:", path), call. = FALSE)
  }
  write.csv(df, path, row.names = FALSE)
  invisible(path)
}

write_stance_heuristic_comparison <- function(
  current_release_path = default_current_release_path(),
  final_candidate_path = default_final_candidate_path(),
  raw_choice_audit_path = default_raw_choice_audit_path(),
  final_proposal_audit_path = default_final_proposal_audit_path(),
  old_raw_choice_audit_path = default_old_raw_choice_audit_path(),
  output_dir = default_output_dir()
) {
  current_release <- read_stance_csv_columns(
    current_release_path,
    c("proposal", "choice_idx", "choice_text", "N_choices", "stance"),
    "frozen current release"
  )
  final_candidate <- read_stance_csv_columns(
    final_candidate_path,
    c("proposal", "choice_idx", "choice_text", "N_choices", "stance2"),
    "final candidate"
  )
  raw_choice_audit <- read_stance_csv_columns(
    raw_choice_audit_path,
    c(
      "proposal",
      "choice_idx",
      "choice_text",
      "current_stance",
      "raw_resolved_stance",
      "raw_match_source",
      "raw_matched_pattern",
      "raw_canonical_pattern",
      "raw_all_match_sources",
      "raw_has_conflicting_matches"
    ),
    "raw heuristic choice audit"
  )
  current_release <- drop_single_option_choice_rows(current_release)
  final_candidate <- drop_single_option_choice_rows(final_candidate)
  final_candidate_proposals <- unique(as_clean_character(final_candidate$proposal))
  raw_choice_audit <- raw_choice_audit[
    as_clean_character(raw_choice_audit$proposal) %in% final_candidate_proposals,
    ,
    drop = FALSE
  ]
  final_proposal_audit <- load_or_build_final_proposal_audit(final_proposal_audit_path, final_candidate)
  final_proposal_audit <- final_proposal_audit[
    as_clean_character(final_proposal_audit$proposal) %in% final_candidate_proposals,
    ,
    drop = FALSE
  ]

  assert_no_undocumented_one_sided(final_proposal_audit)

  joined <- compare_choice_labels(
    current_release,
    final_candidate,
    before_stance_col = "stance",
    after_stance_col = "stance2"
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  outputs <- list(
    stance_totals = write_csv_output(
      rbind(
        stance_value_counts(current_release, "stance", "frozen_current_release"),
        stance_value_counts(final_candidate, "stance2", "final_candidate"),
        stance_value_counts(raw_choice_audit, "raw_resolved_stance", "new_raw_heuristic")
      ),
      file.path(output_dir, "stance_totals.csv")
    ),
    proposal_state_totals = write_csv_output(
      rbind(
        proposal_state_counts(current_release, "stance", "frozen_current_release"),
        proposal_state_counts(final_candidate, "stance2", "final_candidate"),
        data.frame(
          dataset = "final_candidate_audit",
          stance_state = names(table(final_proposal_audit$stance_state)),
          proposals = as.integer(table(final_proposal_audit$stance_state)),
          stringsAsFactors = FALSE
        )
      ),
      file.path(output_dir, "proposal_state_totals.csv")
    ),
    row_status_counts = write_csv_output(
      row_status_counts(joined),
      file.path(output_dir, "row_status_counts.csv")
    ),
    top_approve_reject_to_other = write_csv_output(
      top_label_changes(joined, from_stances = c("APPROVE", "REJECT"), to_stances = "OTHER"),
      file.path(output_dir, "top_approve_reject_to_other.csv")
    ),
    top_other_to_approve_reject = write_csv_output(
      top_label_changes(joined, from_stances = "OTHER", to_stances = c("APPROVE", "REJECT")),
      file.path(output_dir, "top_other_to_approve_reject.csv")
    ),
    low_confidence_dependency_counts = write_csv_output(
      low_confidence_dependency_counts(raw_choice_audit),
      file.path(output_dir, "low_confidence_dependency_counts.csv")
    ),
    low_confidence_matches = write_csv_output(
      low_confidence_summary(raw_choice_audit),
      file.path(output_dir, "low_confidence_matches.csv")
    ),
    conflicting_matches = write_csv_output(
      conflict_summary(raw_choice_audit),
      file.path(output_dir, "conflicting_matches.csv")
    ),
    known_examples = write_csv_output(
      known_example_summary(),
      file.path(output_dir, "known_examples.csv")
    ),
    final_one_sided_gate = write_csv_output(
      data.frame(
        metric = "undocumented_one_sided_final_proposals",
        value = nrow(undocumented_final_one_sided(final_proposal_audit)),
        stringsAsFactors = FALSE
      ),
      file.path(output_dir, "final_one_sided_gate.csv")
    )
  )

  if (nzchar(old_raw_choice_audit_path)) {
    old_raw_choice_audit <- read_stance_csv_columns(
      old_raw_choice_audit_path,
      c("proposal", "choice_idx", "choice_text", "raw_resolved_stance"),
      "old raw heuristic choice audit"
    )
    old_raw_choice_audit <- old_raw_choice_audit[
      as_clean_character(old_raw_choice_audit$proposal) %in% final_candidate_proposals,
      ,
      drop = FALSE
    ]
    raw_joined <- raw_heuristic_comparison(old_raw_choice_audit, raw_choice_audit)
    outputs$raw_heuristic_row_status_counts <- write_csv_output(
      row_status_counts(raw_joined),
      file.path(output_dir, "raw_heuristic_row_status_counts.csv")
    )
    outputs$raw_heuristic_approve_reject_to_other <- write_csv_output(
      top_label_changes(raw_joined, from_stances = c("APPROVE", "REJECT"), to_stances = "OTHER"),
      file.path(output_dir, "raw_heuristic_approve_reject_to_other.csv")
    )
    outputs$raw_heuristic_other_to_approve_reject <- write_csv_output(
      top_label_changes(raw_joined, from_stances = "OTHER", to_stances = c("APPROVE", "REJECT")),
      file.path(output_dir, "raw_heuristic_other_to_approve_reject.csv")
    )
  } else {
    outputs$raw_heuristic_comparison_status <- write_csv_output(
      data.frame(
        metric = "old_raw_choice_audit_path",
        status = "not_provided",
        note = "Set STANCE_HEURISTIC_OLD_RAW_CHOICE_AUDIT to compare old raw heuristic vs new raw heuristic on the same input.",
        stringsAsFactors = FALSE
      ),
      file.path(output_dir, "raw_heuristic_comparison_status.csv")
    )
  }

  message("Wrote stance heuristic comparison outputs to: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
  invisible(list(output_dir = output_dir, outputs = outputs))
}
