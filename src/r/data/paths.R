preparedDataPath <- function(...) {
  generated <- file.path(OUTPUT_DATA_DIR,...)
  if(file.exists(generated)) generated else file.path(DATA_DIR,...)
}

rawDataPath <- function(...) {
  file.path(RAW_DATA_DIR, ...)
}

activeDataPath <- function(...) {
  file.path(DERIVED_ACTIVE_DIR, ...)
}

cleanAuthorVoteOutputsPath <- function(...) {
  preparedDataPath("clean_author_vote_outputs", ...)
}

cleanVoteCumulativeOutputsPath <- function(...) {
  preparedDataPath("clean_vote_cumulative_outputs", ...)
}

archiveLargeDataPath <- function(...) {
  file.path(DERIVED_ARCHIVE_LARGE_DIR, ...)
}

legacyDataPath <- function(...) {
  file.path(DERIVED_LEGACY_DIR, ...)
}

debugDataPath <- function(...) {
  file.path(DERIVED_DEBUG_DIR, ...)
}

debugStoragePath <- function(...) {
  file.path(DERIVED_DEBUG_DIR, "storage", ...)
}

llmDataPath <- function(...) {
  file.path(LLM_DATA_DIR, ...)
}

requireExistingInputPath <- function(path, label=basename(path)) {
  if (!is.null(path) && nzchar(path) && file.exists(path)) return(path)
  # Only the explicitly selected input root is considered, never a private sibling.
  if (!is.null(path) && exists("OUTPUT_ROOT") && startsWith(path,paste0(OUTPUT_ROOT,"/"))) {
    candidate <- file.path(INPUT_ROOT,substring(path,nchar(OUTPUT_ROOT)+2L))
    if (file.exists(candidate)) return(candidate)
  }
  stop(paste("Required input is missing:",label,"at",path))
}

resolvePreferredInputPath <- function(preferred_path, fallback_path) {
  if (!is.null(preferred_path) && nzchar(preferred_path) && file.exists(preferred_path)) {
    return(preferred_path)
  }
  fallback_path
}

cleanVoteOutputPath <- function(file_name, legacy_subdir = "old_run_with_approval_discarded") {
  current_path <- preparedDataPath("clean_vote_outputs", file_name)
  legacy_path <- file.path(DATA_DIR, "clean_vote_outputs", legacy_subdir, file_name)
  resolvePreferredInputPath(current_path, legacy_path)
}

writeCandidateCsv <- function(df, path, row.names = FALSE) {
  assertManagedOutputPath(path, DERIVED_DEBUG_DIR, "Candidate outputs")
  if (file.exists(path)) {
    stop(paste("Candidate output already exists:", path))
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, path, row.names = row.names)
  message("Candidate CSV written to: ", normalizePath(path, winslash = "/", mustWork = FALSE))
  invisible(path)
}

canonicalizeManagedPath <- function(path) {
  if (is.null(path) || length(path) == 0 || !nzchar(path[[1]])) {
    stop("Managed output path must be a non-empty string")
  }

  current <- gsub("\\\\", "/", path.expand(path[[1]]))
  if (!startsWith(current, "/")) {
    current <- file.path(getwd(), current)
  }

  parts <- strsplit(current, "/", fixed = TRUE)[[1]]
  resolved <- "/"
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      resolved <- dirname(resolved)
      if (!startsWith(resolved, "/")) {
        resolved <- "/"
      }
      next
    }

    candidate <- file.path(resolved, part)
    if (file.exists(candidate)) {
      resolved <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    } else {
      resolved <- candidate
    }
  }

  normalizePath(resolved, winslash = "/", mustWork = FALSE)
}

pathIsWithinDirectory <- function(path, dir_path) {
  normalized_dir <- canonicalizeManagedPath(dir_path)
  normalized_path <- canonicalizeManagedPath(path)
  normalized_dir_prefix <- paste0(normalized_dir, "/")
  normalized_path == normalized_dir || startsWith(normalized_path, normalized_dir_prefix)
}

assertManagedOutputPath <- function(path, dir_path, label) {
  normalized_dir <- canonicalizeManagedPath(dir_path)
  if (!pathIsWithinDirectory(path, dir_path)) {
    stop(paste(label, "must be written under", normalized_dir))
  }
  invisible(path)
}

logicalFileMd5 <- function(path) {
  path <- requireExistingInputPath(path, basename(path))

  if (!grepl("\\.gz$", path, ignore.case = TRUE)) {
    return(unname(as.character(tools::md5sum(path))))
  }

  temp_path <- tempfile(fileext = ".contents")
  in_con <- gzfile(path, open = "rb")
  out_con <- file(temp_path, open = "wb")

  on.exit({
    try(close(in_con), silent = TRUE)
    try(close(out_con), silent = TRUE)
    if (file.exists(temp_path)) {
      unlink(temp_path)
    }
  }, add = TRUE)

  repeat {
    chunk <- readBin(in_con, what = "raw", n = 1024 * 1024)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, out_con)
  }

  close(in_con)
  close(out_con)

  unname(as.character(tools::md5sum(temp_path)))
}

ensureManagedFileCopy <- function(source_path, dest_path, dest_dir, output_label, source_label = basename(source_path)) {
  source_path <- requireExistingInputPath(source_path, source_label)
  assertManagedOutputPath(dest_path, dest_dir, output_label)

  if (file.exists(dest_path)) {
    source_hash <- logicalFileMd5(source_path)
    dest_hash <- logicalFileMd5(dest_path)
    if (!identical(source_hash, dest_hash)) {
      stop(
        paste(
          output_label,
          "already exists with different contents:",
          dest_path,
          "Delete or rename the stale output before rerunning."
        )
      )
    }
    return(dest_path)
  }

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(source_path, dest_path, overwrite = FALSE, copy.mode = TRUE)
  if (!isTRUE(copied)) {
    stop(paste("Failed to copy managed output file from", source_path, "to", dest_path))
  }
  invisible(dest_path)
}

ensureCandidateFileCopy <- function(source_path, dest_path, source_label = basename(source_path)) {
  ensureManagedFileCopy(
    source_path = source_path,
    dest_path = dest_path,
    dest_dir = DERIVED_DEBUG_DIR,
    output_label = "Candidate outputs",
    source_label = source_label
  )
}

ensureReleaseFileCopy <- function(source_path, dest_path, source_label = basename(source_path)) {
  ensureManagedFileCopy(
    source_path = source_path,
    dest_path = dest_path,
    dest_dir = DERIVED_ACTIVE_DIR,
    output_label = "Release outputs",
    source_label = source_label
  )
}
