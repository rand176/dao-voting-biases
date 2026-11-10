################################################################################
## LOAD ANALYSIS DATA WITH PROVENANCE, RELEASE REGISTRY, AND DRIFT CHECKS
################################################################################

ANALYSIS_RELEASE_REGISTRY_PATH <- file.path(OUTPUT_ROOT,"metadata/analysis_data_releases.csv")
ANALYSIS_PROVENANCE_DIR <- file.path(OUTPUT_ROOT, "provenance")

source(file.path(PACKAGE_ROOT, "src/r/preparation/choice_helpers.R"),local=TRUE)

defaultAnalysisReleaseRegistry <- function() {
  data.frame(
    release_name = c("current", "candidate"),
    relative_path = c(
      file.path("data", "2024", "derived", "active", "df_choices_28_stance_normalized_no_author_vp_llama33.csv"),
      file.path("data", "2024", "derived", "active", "df_choices_27_candidate_stance_normalized_no_author_vp.csv")
    ),
    is_frozen = c(FALSE, FALSE),
    notes = c(
      "Canonical current analysis dataset alias promoted from the normalized Llama 3.3 stage-5 candidate release",
      "Normalized stage-5 candidate release alias; raw build artifact remains debug-only and promotion to current is separate"
    ),
    stringsAsFactors = FALSE
  )
}

ensureDir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

safeNormalizePath <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

safeFileHash <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(as.character(tools::md5sum(path)))
}

safeFileMtimeUtc <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  as.character(as.POSIXct(file.info(path)$mtime, tz = "UTC"))
}

safeFileSizeBytes <- function(path) {
  if (!file.exists(path)) {
    return(NA_real_)
  }
  as.numeric(file.info(path)$size)
}

safeGitCommit <- function(repo_dir = MAIN_DIR) {
  res <- tryCatch(
    system2("git", c("-C", repo_dir, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(res) == 0) {
    return(NA_character_)
  }
  trimws(res[[1]])
}

readAnalysisReleaseRegistry <- function(path = ANALYSIS_RELEASE_REGISTRY_PATH) {
  if (!file.exists(path)) {
    candidate <- file.path(INPUT_ROOT,"metadata/analysis_data_releases.csv")
    if (file.exists(candidate)) return(read.csv(candidate,stringsAsFactors=FALSE))
    return(defaultAnalysisReleaseRegistry())
  }
  read.csv(path, stringsAsFactors = FALSE)
}

writeAnalysisReleaseRegistry <- function(registry = defaultAnalysisReleaseRegistry(), path = ANALYSIS_RELEASE_REGISTRY_PATH) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  write.csv(registry, path, row.names = FALSE)
  invisible(path)
}

analysisReleaseRegistry <- defaultAnalysisReleaseRegistry()

releasePathFromRegistryRow <- function(registry_row) {
  if ("relative_path" %in% colnames(registry_row) && nzchar(registry_row$relative_path[[1]])) {
    prepared <- file.path(OUTPUT_ROOT,registry_row$relative_path[[1]])
    return(if (file.exists(prepared)) prepared else file.path(INPUT_ROOT,registry_row$relative_path[[1]]))
  }
  if ("path" %in% colnames(registry_row) && nzchar(registry_row$path[[1]])) {
    return(registry_row$path[[1]])
  }
  stop("Registry row is missing a relative_path/path value")
}

analysisDataReleasePath <- function(version = "current") {
  registry <- readAnalysisReleaseRegistry()
  if (!version %in% registry$release_name) {
    stop(
      paste(
        "Unknown analysis data version:",
        version,
        "Available versions:",
        paste(registry$release_name, collapse = ", ")
      )
    )
  }
  registry_row <- registry[registry$release_name == version, , drop = FALSE][1, , drop = FALSE]
  releasePathFromRegistryRow(registry_row)
}

sanitizeNotebookName <- function(notebook_name) {
  gsub("[^A-Za-z0-9_.-]", "_", notebook_name)
}

analysisProvenanceLatestCsvPath <- function(notebook_name) {
  safe_name <- sanitizeNotebookName(notebook_name)
  file.path(ANALYSIS_PROVENANCE_DIR, paste0(safe_name, "__latest.csv"))
}

analysisProvenanceLatestJsonPath <- function(notebook_name) {
  safe_name <- sanitizeNotebookName(notebook_name)
  file.path(ANALYSIS_PROVENANCE_DIR, paste0(safe_name, "__latest.json"))
}

analysisProvenanceTimestampedCsvPath <- function(notebook_name, loaded_at_utc) {
  safe_name <- sanitizeNotebookName(notebook_name)
  safe_ts <- gsub("[: ]", "_", loaded_at_utc)
  safe_ts <- gsub("[^A-Za-z0-9_.-]", "_", safe_ts)
  file.path(ANALYSIS_PROVENANCE_DIR, paste0(safe_name, "__", safe_ts, ".csv"))
}

analysisProvenanceTimestampedJsonPath <- function(notebook_name, loaded_at_utc) {
  safe_name <- sanitizeNotebookName(notebook_name)
  safe_ts <- gsub("[: ]", "_", loaded_at_utc)
  safe_ts <- gsub("[^A-Za-z0-9_.-]", "_", safe_ts)
  file.path(ANALYSIS_PROVENANCE_DIR, paste0(safe_name, "__", safe_ts, ".json"))
}

buildAnalysisDataProvenance <- function(
  release_name,
  choices_path,
  proposals_path,
  df_choices,
  df_proposals,
  loader_path = file.path(PACKAGE_ROOT, "src/r/data/load.R")
) {
  data.frame(
    release_name = release_name,
    choices_file = basename(choices_path),
    choices_path = safeNormalizePath(choices_path),
    choices_md5 = safeFileHash(choices_path),
    choices_mtime_utc = safeFileMtimeUtc(choices_path),
    choices_size_bytes = safeFileSizeBytes(choices_path),
    choices_nrow = nrow(df_choices),
    choices_ncol = ncol(df_choices),
    proposals_file = basename(proposals_path),
    proposals_path = safeNormalizePath(proposals_path),
    proposals_md5 = safeFileHash(proposals_path),
    proposals_mtime_utc = safeFileMtimeUtc(proposals_path),
    proposals_size_bytes = safeFileSizeBytes(proposals_path),
    proposals_nrow = nrow(df_proposals),
    proposals_ncol = ncol(df_proposals),
    loader_path = safeNormalizePath(loader_path),
    loader_md5 = safeFileHash(loader_path),
    loader_mtime_utc = safeFileMtimeUtc(loader_path),
    git_commit = safeGitCommit(),
    loaded_at_utc = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    stringsAsFactors = FALSE
  )
}

printAnalysisDataProvenance <- function(provenance = analysis_data_provenance) {
  if (!exists("analysis_data_provenance") && missing(provenance)) {
    stop("analysis_data_provenance has not been created yet")
  }
  cat("Analysis data provenance\n")
  cat("  release_name:", provenance$release_name[[1]], "\n")
  cat("  choices_file:", provenance$choices_file[[1]], "\n")
  cat("  choices_md5:", provenance$choices_md5[[1]], "\n")
  cat("  choices_mtime_utc:", provenance$choices_mtime_utc[[1]], "\n")
  cat("  choices_nrow:", provenance$choices_nrow[[1]], "\n")
  cat("  choices_ncol:", provenance$choices_ncol[[1]], "\n")
  cat("  proposals_file:", provenance$proposals_file[[1]], "\n")
  cat("  proposals_md5:", provenance$proposals_md5[[1]], "\n")
  cat("  loader_md5:", provenance$loader_md5[[1]], "\n")
  cat("  git_commit:", provenance$git_commit[[1]], "\n")
  cat("  loaded_at_utc:", provenance$loaded_at_utc[[1]], "\n")
}

saveAnalysisDataProvenance <- function(
  notebook_name = NULL,
  provenance = analysis_data_provenance,
  save_json = TRUE
) {
  if (is.null(notebook_name) || !nzchar(notebook_name)) {
    stop("saveAnalysisDataProvenance requires notebook_name")
  }

  ensureDir(ANALYSIS_PROVENANCE_DIR)

  latest_csv <- analysisProvenanceLatestCsvPath(notebook_name)
  latest_json <- analysisProvenanceLatestJsonPath(notebook_name)
  timestamped_csv <- analysisProvenanceTimestampedCsvPath(notebook_name, provenance$loaded_at_utc[[1]])
  timestamped_json <- analysisProvenanceTimestampedJsonPath(notebook_name, provenance$loaded_at_utc[[1]])

  write.csv(provenance, latest_csv, row.names = FALSE)
  write.csv(provenance, timestamped_csv, row.names = FALSE)

  if (isTRUE(save_json) && requireNamespace("jsonlite", quietly = TRUE)) {
    json_payload <- jsonlite::toJSON(
      as.list(provenance[1, , drop = FALSE]),
      pretty = TRUE,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    )
    writeLines(json_payload, latest_json)
    writeLines(json_payload, timestamped_json)
  } else if (isTRUE(save_json)) {
    warning("jsonlite not available; wrote CSV provenance only")
  }

  invisible(
    list(
      latest_csv = latest_csv,
      timestamped_csv = timestamped_csv,
      latest_json = latest_json,
      timestamped_json = timestamped_json
    )
  )
}

loadLastSavedAnalysisDataProvenance <- function(notebook_name) {
  latest_csv <- analysisProvenanceLatestCsvPath(notebook_name)
  if (!file.exists(latest_csv)) {
    return(NULL)
  }
  read.csv(latest_csv, stringsAsFactors = FALSE)
}

compareAnalysisDataProvenance <- function(
  current_provenance = analysis_data_provenance,
  previous_provenance,
  compare_fields = c(
    "release_name",
    "choices_path",
    "choices_md5",
    "choices_nrow",
    "choices_ncol",
    "proposals_md5",
    "loader_md5",
    "git_commit"
  )
) {
  if (is.null(previous_provenance) || nrow(previous_provenance) == 0) {
    return(data.frame(field = character(0), previous = character(0), current = character(0), stringsAsFactors = FALSE))
  }

  compare_fields <- intersect(compare_fields, intersect(colnames(current_provenance), colnames(previous_provenance)))
  changed <- lapply(compare_fields, function(field) {
    previous_value <- as.character(previous_provenance[[field]][[1]])
    current_value <- as.character(current_provenance[[field]][[1]])
    if (!identical(previous_value, current_value)) {
      data.frame(field = field, previous = previous_value, current = current_value, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  changed <- changed[!vapply(changed, is.null, logical(1))]
  if (length(changed) == 0) {
    return(data.frame(field = character(0), previous = character(0), current = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, changed)
}

analysisTruthyFlag <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x %in% c("1", "TRUE", "T", "YES", "Y")
}

snapshotFlaggedProposalIds <- function(df_proposals) {
  if (!all(c("id", "flagged") %in% colnames(df_proposals))) {
    return(character())
  }
  unique(trimws(tolower(df_proposals$id[analysisTruthyFlag(df_proposals$flagged)])))
}

excludeSnapshotFlaggedChoices <- function(df_choices, df_proposals) {
  flagged_ids <- snapshotFlaggedProposalIds(df_proposals)
  if (length(flagged_ids) == 0 || !"proposal" %in% colnames(df_choices)) {
    return(df_choices)
  }
  df_choices[!trimws(tolower(df_choices$proposal)) %in% flagged_ids, , drop = FALSE]
}

checkAnalysisDataChangedFromLastLoad <- function(
  notebook_name,
  current_provenance = analysis_data_provenance,
  warn_only = TRUE
) {
  previous_provenance <- loadLastSavedAnalysisDataProvenance(notebook_name)
  if (is.null(previous_provenance)) {
    message(paste("No prior saved provenance found for notebook:", notebook_name))
    return(invisible(data.frame(field = character(0), previous = character(0), current = character(0), stringsAsFactors = FALSE)))
  }

  changed_fields <- compareAnalysisDataProvenance(
    current_provenance = current_provenance,
    previous_provenance = previous_provenance
  )

  if (nrow(changed_fields) == 0) {
    message(paste("Analysis data unchanged since last saved load for notebook:", notebook_name))
    return(invisible(changed_fields))
  }

  message(paste("Analysis data changed since last saved load for notebook:", notebook_name))
  print(changed_fields)

  if (!isTRUE(warn_only)) {
    stop("Analysis data drift detected from last saved load")
  }

  warning("Analysis data drift detected from last saved load; user action required")
  invisible(changed_fields)
}

loadAnalysisData <- function(version = "current", file = NULL) {
  proposals_file <- "proposals_0.csv.gz"
  proposals_path <- requireExistingInputPath(rawDataPath(proposals_file), proposals_file)

  release_name <- version
  if (!is.null(file)) {
    choices_path <- requireExistingInputPath(file, basename(file))
    release_name <- paste0("custom:", basename(file))
  } else {
    choices_path <- requireExistingInputPath(
      analysisDataReleasePath(version),
      paste("analysis data release", version)
    )
  }

  df_proposals_local <- read.csv(proposals_path, colClasses = c(id = "character"))
  df_proposals_local$id <- trimws(tolower(df_proposals_local$id))

  df_choices_local <- read.csv(choices_path, colClasses = c(proposal = "character"))
  df_choices_local <- process_df_choices(df_choices_local)
  df_choices_local <- excludeSnapshotFlaggedChoices(df_choices_local, df_proposals_local)
  df_choices_local <- dropSingleOptionProposals(df_choices_local)

  df_choices_full_local <- df_choices_local
  df_choices_gt1vote_local <- df_choices_local[df_choices_local$tot_votes_2 > 1, ]
  df_choices_multichoice_local <- df_choices_local[df_choices_local$N_choices > 1, ]
  df_choices_multichoice_gt1vote_local <- df_choices_multichoice_local[df_choices_multichoice_local$tot_votes_2 > 1, ]

  analysis_data_provenance <<- buildAnalysisDataProvenance(
    release_name = release_name,
    choices_path = choices_path,
    proposals_path = proposals_path,
    df_choices = df_choices_local,
    df_proposals = df_proposals_local
  )

  df_proposals <<- df_proposals_local
  df_choices <<- df_choices_local
  df_choices_full <<- df_choices_full_local
  df_choices_gt1vote <<- df_choices_gt1vote_local
  df_choices_multichoice <<- df_choices_multichoice_local
  df_choices_multichoice_gt1vote <<- df_choices_multichoice_gt1vote_local

  invisible(
    list(
      df_proposals = df_proposals_local,
      df_choices = df_choices_local,
      df_choices_full = df_choices_full_local,
      df_choices_gt1vote = df_choices_gt1vote_local,
      df_choices_multichoice = df_choices_multichoice_local,
      df_choices_multichoice_gt1vote = df_choices_multichoice_gt1vote_local,
      analysis_data_provenance = analysis_data_provenance
    )
  )
}
