if (!exists("buildCanonicalStage5ChoiceTable", mode = "function")) {
  source(file.path(PACKAGE_ROOT, "src/r/preparation/labels.R"),local=TRUE)
}

stage5Stage4ChoiceTablePath <- function() {
  debugDataPath("df_choices_25B_more_vars_stage4.csv")
}

stage5CanonicalChoiceTablePath <- function() {
  debugDataPath("df_choices_27_stage5_canonical.csv")
}

stage5CanonicalCoverageAuditPath <- function() {
  debugDataPath("df_choices_27_stage5_canonical_label_coverage.csv")
}

stage5DefaultLlama33LabelPaths <- function() {
  llmDataPath(
    "llama3.3",
    c(
      "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r1.csv",
      "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r2.csv",
      "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r3.csv"
    )
  )
}

stage5Llama33ConflictAuditPath <- function() {
  debugDataPath("llama33_choice_label_conflicts.csv")
}

stage5FilteredCandidateInputPath <- function() {
  resolvePreferredInputPath(
    debugDataPath("df_choices_27_llm_human_no_author_vp.csv"),
    debugDataPath("choices_25_v4_27_26_llm_stage5.csv")
  )
}

stage5CandidateInputPath <- function() {
  stage5FilteredCandidateInputPath()
}

stage5RawReleaseArtifactPath <- function() {
  debugStoragePath(
    "stage5_candidate_release",
    "df_choices_27_llm_human_no_author_vp_raw_release_artifact.csv"
  )
}

stage5CandidateReleasePath <- function() {
  activeDataPath("df_choices_27_candidate_stance_normalized_no_author_vp.csv")
}

stage5CandidateNormalizationAuditPath <- function() {
  file.path(
    ANALYSIS_PROVENANCE_DIR,
    "stage5_candidate_release_normalization_audit.csv"
  )
}

resolvePythonCommand <- function() {
  env_candidates <- c(
    Sys.getenv("PYTHON", unset = ""),
    Sys.getenv("PYTHON3", unset = "")
  )
  path_candidates <- unname(Sys.which(c("python3", "python")))
  candidates <- unique(c(
    env_candidates[nzchar(env_candidates)],
    path_candidates[nzchar(path_candidates)]
  ))
  if (length(candidates) == 0) {
    stop("Could not find a Python interpreter for stage-5 candidate release helpers")
  }
  candidates[[1]]
}

runPythonHelper <- function(args, python_bin = resolvePythonCommand()) {
  output <- system2(
    python_bin,
    args,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop(paste(
      c(paste("Python helper failed with status", status), output),
      collapse = "\n"
    ))
  }
  invisible(output)
}

parseCommandKeyValueOutput <- function(lines) {
  if (length(lines) == 0) {
    return(list())
  }

  parsed <- list()
  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      next
    }
    key <- trimws(parts[[1]])
    value <- trimws(parts[[2]])
    if (!nzchar(key)) {
      next
    }
    if (grepl("^[0-9]+$", value)) {
      parsed[[key]] <- as.integer(value)
    } else if (grepl("^[0-9]+\\.[0-9]+$", value)) {
      parsed[[key]] <- as.numeric(value)
    } else {
      parsed[[key]] <- value
    }
  }
  parsed
}

stage5ReleaseAliasPath <- function(release_name, analysis_root = MAIN_DIR) {
  registry_path <- file.path(analysis_root, "metadata", "analysis_data_releases.csv")
  registry_path <- requireExistingInputPath(registry_path, "analysis data release registry")
  releases <- read.csv(registry_path, stringsAsFactors = FALSE)
  stage5RequireColumns(
    releases,
    c("release_name", "relative_path"),
    "analysis data release registry"
  )
  release_row <- releases[releases$release_name == release_name, , drop = FALSE]
  if (nrow(release_row) != 1L) {
    stop(paste("Expected exactly one release alias for", release_name, "in", registry_path))
  }
  file.path(analysis_root, release_row$relative_path[[1]])
}

stage5Llama33Columns <- function() {
  c(
    "effect_llama33",
    "suggested_stance_llama33",
    "stance_llama33",
    "source_file_llama33",
    "llama33_label_conflict",
    "conflict_columns_llama33"
  )
}

stage5Llama33StatusColumns <- function() {
  c(
    "llama33_reviewed",
    "llama33_not_reviewed",
    "llama33_no_suggestion",
    "llama33_missing_label",
    "llama33_positive_suggested_stance",
    "llama33_basic_proposal_exclusion"
  )
}

stage5AddLlama33StatusColumns <- function(df) {
  for (col in stage5Llama33Columns()) {
    if (!col %in% colnames(df)) {
      if (col == "llama33_label_conflict") {
        df[[col]] <- FALSE
      } else {
        df[[col]] <- NA_character_
      }
    }
  }
  if (!"type" %in% colnames(df)) {
    df$type <- NA_character_
  }

  label_value <- stage5HasLabelValue(df[, c(
    "effect_llama33",
    "suggested_stance_llama33",
    "stance_llama33",
    "source_file_llama33"
  ), drop = FALSE])
  conflict <- stage5LogicalFlag(df$llama33_label_conflict)
  suggested <- stage5EmptyToNa(df$suggested_stance_llama33)
  stance <- stage5EmptyToNa(df$stance_llama33)
  reviewed <- label_value | conflict

  df$llama33_label_conflict <- conflict
  df$llama33_reviewed <- reviewed
  df$llama33_not_reviewed <- !reviewed
  df$llama33_no_suggestion <- reviewed & !conflict & is.na(suggested)
  df$llama33_missing_label <- reviewed & !conflict & (is.na(suggested) | is.na(stance))
  df$llama33_positive_suggested_stance <- reviewed & !conflict &
    suggested %in% c("APPROVE", "REJECT", "ABSTAIN", "OTHER")
  df$llama33_basic_proposal_exclusion <- !reviewed & trimws(tolower(as.character(df$type))) == "basic"
  df
}

stage5AllowedCandidateDeltaColumns <- function() {
  c(stage5Llama33Columns(), stage5Llama33StatusColumns())
}

stage5CandidateRequiredColumns <- function() {
  c(
    "proposal",
    "space",
    "type",
    "scores_total",
    "n_votes",
    "flagged",
    "stance",
    "stance_oai",
    "suggested_stance_oai",
    "effect_oai",
    "stance_llama",
    "suggested_stance_llama",
    "effect_llama",
    "stance_chatgpt",
    "suggested_stance_chatgpt",
    "effect_chatgpt",
    "stance_hl",
    "suggested_stance_hl",
    "n_evaluations",
    "n_non_low_effort_evaluations",
    "n_low_effort_evaluations",
    "low_effort_only",
    stage5AllowedCandidateDeltaColumns()
  )
}

assertStage5CandidateReleaseSchema <- function(
  df_candidate,
  candidate_output_path = "normalized candidate release"
) {
  missing_columns <- setdiff(stage5CandidateRequiredColumns(), colnames(df_candidate))
  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Normalized candidate release is missing required columns in",
        candidate_output_path,
        ":",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  invisible(df_candidate)
}

validateStage5CandidateAgainstCurrent <- function(
  candidate_path,
  current_path,
  key_cols = c("proposal", "choice_idx"),
  allowed_delta_columns = stage5AllowedCandidateDeltaColumns()
) {
  candidate_path <- requireExistingInputPath(candidate_path, "candidate release data")
  current_path <- requireExistingInputPath(current_path, "current release data")
  df_candidate <- read.csv(candidate_path, colClasses = c(proposal = "character"))
  df_current <- read.csv(current_path, colClasses = c(proposal = "character"))
  stage5RequireColumns(df_candidate, key_cols, "candidate release")
  stage5RequireColumns(df_current, key_cols, "current release")
  stage5AssertUniqueKey(df_candidate, key_cols, "candidate release")
  stage5AssertUniqueKey(df_current, key_cols, "current release")

  if (nrow(df_candidate) != nrow(df_current)) {
    stop(paste("Candidate release row count mismatch against current:", nrow(df_candidate), "!=", nrow(df_current)))
  }
  for (key_col in key_cols) {
    if (!identical(as.character(df_candidate[[key_col]]), as.character(df_current[[key_col]]))) {
      stop(paste("Candidate release key order or values differ from current for", key_col))
    }
  }

  dropped_columns <- setdiff(colnames(df_current), colnames(df_candidate))
  if (length(dropped_columns) > 0) {
    stop(paste("Candidate release dropped current columns:", paste(dropped_columns, collapse = ", ")))
  }
  unsupported_columns <- setdiff(colnames(df_candidate), c(colnames(df_current), allowed_delta_columns))
  if (length(unsupported_columns) > 0) {
    stop(paste("Candidate release added unsupported columns:", paste(unsupported_columns, collapse = ", ")))
  }

  compare_cols <- setdiff(intersect(colnames(df_current), colnames(df_candidate)), key_cols)
  changed_cols <- character()
  for (col in compare_cols) {
    current_values <- as.character(df_current[[col]])
    candidate_values <- as.character(df_candidate[[col]])
    same <- (is.na(current_values) & is.na(candidate_values)) |
      (!is.na(current_values) & !is.na(candidate_values) & current_values == candidate_values)
    if (!all(same)) {
      changed_cols <- c(changed_cols, col)
    }
  }
  if (length(changed_cols) > 0) {
    stop(paste("Candidate release changed existing analytical columns:", paste(changed_cols, collapse = ", ")))
  }

  list(
    row_count = nrow(df_candidate),
    key_count = nrow(unique(df_candidate[, key_cols, drop = FALSE])),
    compared_columns = length(compare_cols),
    allowed_delta_columns = intersect(allowed_delta_columns, colnames(df_candidate))
  )
}

overlayStage5CandidateWithCurrentColumns <- function(
  candidate_path,
  current_path,
  key_cols = c("proposal", "choice_idx"),
  allowed_delta_columns = stage5AllowedCandidateDeltaColumns()
) {
  candidate_path <- requireExistingInputPath(candidate_path, "candidate release data")
  current_path <- requireExistingInputPath(current_path, "current release data")
  df_candidate <- read.csv(candidate_path, colClasses = c(proposal = "character"))
  df_current <- read.csv(current_path, colClasses = c(proposal = "character"))
  stage5RequireColumns(df_candidate, key_cols, "candidate release")
  stage5RequireColumns(df_current, key_cols, "current release")
  stage5AssertUniqueKey(df_candidate, key_cols, "candidate release")
  stage5AssertUniqueKey(df_current, key_cols, "current release")
  if (nrow(df_candidate) != nrow(df_current)) {
    stop(paste("Candidate release row count mismatch against current:", nrow(df_candidate), "!=", nrow(df_current)))
  }
  for (key_col in key_cols) {
    if (!identical(as.character(df_candidate[[key_col]]), as.character(df_current[[key_col]]))) {
      stop(paste("Candidate release key order or values differ from current for", key_col))
    }
  }

  dropped_columns <- setdiff(colnames(df_current), colnames(df_candidate))
  if (length(dropped_columns) > 0) {
    stop(paste("Candidate release dropped current columns:", paste(dropped_columns, collapse = ", ")))
  }
  unsupported_columns <- setdiff(colnames(df_candidate), c(colnames(df_current), allowed_delta_columns))
  if (length(unsupported_columns) > 0) {
    stop(paste("Candidate release added unsupported columns:", paste(unsupported_columns, collapse = ", ")))
  }

  overlay_cols <- setdiff(intersect(colnames(df_current), colnames(df_candidate)), key_cols)
  for (col in overlay_cols) {
    df_candidate[[col]] <- df_current[[col]]
  }
  write.csv(df_candidate, candidate_path, row.names = FALSE)
  list(
    row_count = nrow(df_candidate),
    overlaid_columns = length(overlay_cols),
    allowed_delta_columns = intersect(allowed_delta_columns, colnames(df_candidate))
  )
}

buildStage5RawCandidateRelease <- function(df_choices_stage5) {
  buildNoAuthorVpRelease(df_choices_stage5)
}

persistStage5CanonicalChoiceTable <- function(
  df_stage5_canonical,
  output_path = stage5CanonicalChoiceTablePath()
) {
  temp_output_path <- tempfile(fileext = ".csv")
  on.exit({
    if (file.exists(temp_output_path)) {
      unlink(temp_output_path)
    }
  }, add = TRUE)

  write.csv(df_stage5_canonical, temp_output_path, row.names = FALSE)
  ensureCandidateFileCopy(
    temp_output_path,
    output_path,
    "canonical stage-5 choice table"
  )
}

persistStage5CanonicalCoverageAudit <- function(
  stage5_coverage_audit,
  output_path = stage5CanonicalCoverageAuditPath()
) {
  temp_output_path <- tempfile(fileext = ".csv")
  on.exit({
    if (file.exists(temp_output_path)) {
      unlink(temp_output_path)
    }
  }, add = TRUE)

  write.csv(stage5_coverage_audit, temp_output_path, row.names = FALSE)
  ensureCandidateFileCopy(
    temp_output_path,
    output_path,
    "canonical stage-5 coverage audit"
  )
}

assembleStage5CanonicalChoiceTable <- function(
  stage4_input = stage5Stage4ChoiceTablePath(),
  stage5_source_input = stage5FilteredCandidateInputPath(),
  output_path = stage5CanonicalChoiceTablePath(),
  coverage_audit_path = stage5CanonicalCoverageAuditPath(),
  llama33_label_paths = character(),
  llama33_conflict_audit_path = stage5Llama33ConflictAuditPath(),
  read_llama33_labels_fn = readStage5Llama33ChoiceLabels,
  persist_choice_table_fn = persistStage5CanonicalChoiceTable,
  persist_coverage_audit_fn = persistStage5CanonicalCoverageAudit
) {
  stage4_input <- requireExistingInputPath(stage4_input, "stage 4 choice table")
  stage5_source_input <- requireExistingInputPath(
    stage5_source_input,
    "stage 5 LLM/human-label source input"
  )

  df_stage4 <- read.csv(
    stage4_input,
    colClasses = c(proposal = "character")
  )
  df_stage5_source <- read.csv(
    stage5_source_input,
    colClasses = c(proposal = "character")
  )

  df_stage5_canonical <- buildCanonicalStage5ChoiceTable(
    df_stage4 = df_stage4,
    df_stage5_source = df_stage5_source
  )
  if (!is.null(llama33_label_paths) &&
      length(llama33_label_paths) > 0 &&
      !"source_file_llama33" %in% colnames(df_stage5_canonical)) {
    df_llama33 <- read_llama33_labels_fn(
      llama33_label_paths,
      conflict_audit_path = llama33_conflict_audit_path
    )
    df_llama33_m <- df_llama33[, c(
      stage5Llama33Columns(),
      "prop_chid"
    ), drop = FALSE]
    df_stage5_canonical <- stage5LeftJoinChoiceLabels(
      df_stage5_canonical,
      df_llama33_m,
      "Llama 3.3"
    )
  }
  df_stage5_canonical <- stage5AddLlama33StatusColumns(df_stage5_canonical)
  coverage_sources <- stage5DefaultCoverageSources()
  coverage_sources <- coverage_sources[vapply(
    coverage_sources,
    function(cols) all(cols %in% colnames(df_stage5_canonical)),
    logical(1)
  )]
  if (length(coverage_sources) == 0) {
    stage5_coverage_audit <- data.frame(
      label_source = character(),
      label_columns = character(),
      total_rows = integer(),
      labeled_rows = integer(),
      reviewed_rows = integer(),
      not_reviewed_rows = integer(),
      no_suggestion_rows = integer(),
      missing_label_rows = integer(),
      conflict_rows = integer(),
      basic_exclusion_rows = integer(),
      total_proposals = integer(),
      labeled_proposals = integer(),
      complete_proposals = integer(),
      partial_proposals = integer(),
      missing_proposals = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    stage5_coverage_audit <- buildStage5LabelCoverageAudit(
      df_stage5_canonical,
      source_columns = coverage_sources
    )
  }

  output_path <- persist_choice_table_fn(
    df_stage5_canonical,
    output_path = output_path
  )
  coverage_audit_path <- persist_coverage_audit_fn(
    stage5_coverage_audit,
    output_path = coverage_audit_path
  )

  list(
    source_input_path = stage5_source_input,
    output_path = output_path,
    coverage_audit_path = coverage_audit_path,
    rows = nrow(df_stage5_canonical),
    df_stage5_canonical = df_stage5_canonical,
    coverage_audit = stage5_coverage_audit
  )
}

persistStage5RawCandidateRelease <- function(
  df_raw_candidate,
  raw_output_path = stage5RawReleaseArtifactPath()
) {
  temp_raw_path <- tempfile(fileext = ".csv")
  on.exit({
    if (file.exists(temp_raw_path)) {
      unlink(temp_raw_path)
    }
  }, add = TRUE)

  write.csv(df_raw_candidate, temp_raw_path, row.names = FALSE)
  ensureCandidateFileCopy(
    temp_raw_path,
    raw_output_path,
    "stage-5 raw release artifact"
  )
}

normalizeStage5CandidateRelease <- function(
  input_path,
  output_path = stage5CandidateReleasePath(),
  audit_path = stage5CandidateNormalizationAuditPath(),
  stage = "2_stance_v5",
  python_bin = resolvePythonCommand()
) {
  input_path <- requireExistingInputPath(input_path, "raw stage-5 release artifact")
  assertManagedOutputPath(output_path, DERIVED_ACTIVE_DIR, "Release outputs")
  assertManagedOutputPath(audit_path, ANALYSIS_PROVENANCE_DIR, "Analysis provenance outputs")
  temp_output_path <- tempfile(fileext = ".csv")
  temp_audit_path <- tempfile(fileext = ".csv")
  on.exit({
    if (file.exists(temp_output_path)) {
      unlink(temp_output_path)
    }
    if (file.exists(temp_audit_path)) {
      unlink(temp_audit_path)
    }
  }, add = TRUE)

  output <- runPythonHelper(
    c(
      file.path(PACKAGE_ROOT,"src/python/dao_bias/preparation/normalize_stance.py"),
      "--input", input_path,
      "--output", temp_output_path,
      "--exceptions", file.path(MAIN_DIR, "metadata", "exceptions", "stance_one_sided_exceptions.csv"),
      "--audit-output", temp_audit_path,
      "--stage", stage
    ),
    python_bin = python_bin
  )
  ensureReleaseFileCopy(
    temp_output_path,
    output_path,
    "normalized stage-5 candidate release"
  )
  ensureManagedFileCopy(
    temp_audit_path,
    audit_path,
    ANALYSIS_PROVENANCE_DIR,
    "Analysis provenance outputs",
    "stage-5 normalization audit"
  )

  list(
    output_path = output_path,
    audit_path = audit_path,
    summary = parseCommandKeyValueOutput(output)
  )
}

refreshStage5CandidateReleaseStats <- function(
  analysis_root = MAIN_DIR,
  python_bin = resolvePythonCommand()
) {
  analysis_root <- normalizePath(analysis_root, winslash = "/", mustWork = TRUE)
  runPythonHelper(
    c(
      file.path(PACKAGE_ROOT,"src/python/dao_bias/reporting/counts.py"),
      "--release", "candidate",
      "--analysis-root", analysis_root,
      "--skip-macro-output"
    ),
    python_bin = python_bin
  )

  list(
    csv_path = file.path(
      analysis_root,
      "metadata",
      "analysis_data_provenance",
      "latex_counts_candidate.csv"
    ),
    json_path = file.path(
      analysis_root,
      "metadata",
      "analysis_data_provenance",
      "latex_counts_candidate.json"
    )
  )
}

defaultSaveStage5CandidateProvenance <- function(provenance) {
  saveAnalysisDataProvenance(
    notebook_name = "stage5_candidate_release",
    provenance = provenance
  )
}

buildStage5CandidateRelease <- function(
  stage5_candidate_input = stage5CandidateInputPath(),
  stage4_input = stage5Stage4ChoiceTablePath(),
  proposals_path = rawDataPath("proposals_0.csv.gz"),
  canonical_stage5_output_path = stage5CanonicalChoiceTablePath(),
  canonical_stage5_coverage_audit_path = stage5CanonicalCoverageAuditPath(),
  raw_output_path = stage5RawReleaseArtifactPath(),
  candidate_output_path = stage5CandidateReleasePath(),
  normalization_audit_path = stage5CandidateNormalizationAuditPath(),
  stage = "2_stance_v5",
  analysis_root = MAIN_DIR,
  python_bin = resolvePythonCommand(),
  llama33_label_paths = stage5DefaultLlama33LabelPaths(),
  llama33_conflict_audit_path = stage5Llama33ConflictAuditPath(),
  current_release_path = stage5ReleaseAliasPath("current", analysis_root = analysis_root),
  assemble_stage5_fn = assembleStage5CanonicalChoiceTable,
  persist_canonical_stage5_fn = persistStage5CanonicalChoiceTable,
  persist_canonical_coverage_audit_fn = persistStage5CanonicalCoverageAudit,
  persist_raw_release_fn = persistStage5RawCandidateRelease,
  normalize_release_fn = normalizeStage5CandidateRelease,
  validate_current_fn = validateStage5CandidateAgainstCurrent,
  save_provenance_fn = defaultSaveStage5CandidateProvenance,
  refresh_stats_fn = refreshStage5CandidateReleaseStats
) {
  stage5_candidate_input <- requireExistingInputPath(
    stage5_candidate_input,
    "stage 5 LLM/human-label candidate input"
  )
  stage4_input <- requireExistingInputPath(stage4_input, "stage 4 choice table")
  proposals_path <- requireExistingInputPath(proposals_path, "proposals_0.csv.gz")

  canonical_stage5_result <- assemble_stage5_fn(
    stage4_input = stage4_input,
    stage5_source_input = stage5_candidate_input,
    output_path = canonical_stage5_output_path,
    coverage_audit_path = canonical_stage5_coverage_audit_path,
    llama33_label_paths = llama33_label_paths,
    llama33_conflict_audit_path = llama33_conflict_audit_path,
    persist_choice_table_fn = persist_canonical_stage5_fn,
    persist_coverage_audit_fn = persist_canonical_coverage_audit_fn
  )
  canonical_stage5_output_path <- requireExistingInputPath(
    canonical_stage5_result$output_path,
    "canonical stage-5 choice table"
  )
  canonical_stage5_coverage_audit_path <- requireExistingInputPath(
    canonical_stage5_result$coverage_audit_path,
    "canonical stage-5 coverage audit"
  )

  df_choices_stage5 <- canonical_stage5_result$df_stage5_canonical
  df_raw_candidate <- buildStage5RawCandidateRelease(df_choices_stage5)

  raw_output_path <- persist_raw_release_fn(
    df_raw_candidate,
    raw_output_path = raw_output_path
  )
  raw_output_path <- requireExistingInputPath(
    raw_output_path,
    "stage-5 raw release artifact"
  )

  normalize_result <- normalize_release_fn(
    input_path = raw_output_path,
    output_path = candidate_output_path,
    audit_path = normalization_audit_path,
    stage = stage,
    python_bin = python_bin
  )
  candidate_output_path <- requireExistingInputPath(
    candidate_output_path,
    "normalized candidate release"
  )
  normalization_audit_path <- requireExistingInputPath(
    normalization_audit_path,
    "candidate normalization audit"
  )

  current_overlay <- NULL
  if (!is.null(current_release_path) && length(current_release_path) > 0 && nzchar(current_release_path[[1]])) {
    current_overlay <- overlayStage5CandidateWithCurrentColumns(
      candidate_path = candidate_output_path,
      current_path = current_release_path
    )
  }
  df_candidate <- read.csv(
    candidate_output_path,
    colClasses = c(proposal = "character")
  )
  assertStage5CandidateReleaseSchema(
    df_candidate,
    candidate_output_path = candidate_output_path
  )
  current_validation <- NULL
  if (!is.null(current_release_path) && length(current_release_path) > 0 && nzchar(current_release_path[[1]])) {
    current_validation <- validate_current_fn(
      candidate_path = candidate_output_path,
      current_path = current_release_path
    )
  }
  df_proposals <- read.csv(
    proposals_path,
    colClasses = c(id = "character")
  )
  df_proposals$id <- trimws(tolower(df_proposals$id))

  analysis_data_provenance <- buildAnalysisDataProvenance(
    release_name = "candidate",
    choices_path = candidate_output_path,
    proposals_path = proposals_path,
    df_choices = df_candidate,
    df_proposals = df_proposals
  )
  save_provenance_result <- save_provenance_fn(analysis_data_provenance)
  stats_result <- refresh_stats_fn(
    analysis_root = analysis_root,
    python_bin = python_bin
  )

  list(
    stage5_source_input_path = stage5_candidate_input,
    canonical_stage5_output_path = canonical_stage5_output_path,
    canonical_stage5_coverage_audit_path = canonical_stage5_coverage_audit_path,
    raw_output_path = raw_output_path,
    candidate_output_path = candidate_output_path,
    normalization_audit_path = normalization_audit_path,
    canonical_stage5_rows = nrow(df_choices_stage5),
    raw_rows = nrow(df_raw_candidate),
    candidate_rows = nrow(df_candidate),
    normalize_result = normalize_result,
    current_overlay = current_overlay,
    current_validation = current_validation,
    provenance = analysis_data_provenance,
    save_provenance_result = save_provenance_result,
    stats_result = stats_result
  )
}
