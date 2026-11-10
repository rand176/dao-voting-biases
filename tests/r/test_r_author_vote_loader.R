source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
MAIN_DIR <- PACKAGE_ROOT
R_DIR <- file.path(PACKAGE_ROOT,"src/r")
DATA_DIR <- file.path(tempdir(), "dao-author-vote-loader")
RAW_DATA_DIR <- file.path(DATA_DIR, "raw")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
DERIVED_ACTIVE_DIR <- file.path(DERIVED_DIR, "active")
DERIVED_LEGACY_DIR <- file.path(DERIVED_DIR, "legacy")
DERIVED_DEBUG_DIR <- file.path(DERIVED_DIR, "debug")
PROPOSAL_FLAGS_DIR <- file.path(DATA_DIR, "clean_proposal_flags")
EXCEPTIONS_DIR <- file.path(DATA_DIR, "metadata", "exceptions")

dir.create(RAW_DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_ACTIVE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_LEGACY_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_DEBUG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROPOSAL_FLAGS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(EXCEPTIONS_DIR, recursive = TRUE, showWarnings = FALSE)
author_vote_root <- file.path(DATA_DIR, "clean_author_vote_outputs")
dir.create(author_vote_root, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(author_vote_root, "manifests"), recursive = TRUE, showWarnings = FALSE)

initialize_test_analysis()
source(file.path(PACKAGE_ROOT, "src/r/preparation/author_votes.R"))

author_vote_dir <- file.path(DATA_DIR, "clean_author_vote_outputs")
manifest_dir <- file.path(author_vote_dir, "manifests")
favorite_path <- file.path(author_vote_dir, "prop_author_favorite_choice.csv")
support_path <- file.path(author_vote_dir, "prop_author_support_fraction.csv")
run_summary_path <- file.path(author_vote_dir, "run_summary.json")
run_meta_path <- file.path(manifest_dir, "author_votes_v1_run.json")

write_author_outputs <- function() {
  write.csv(
    data.frame(
      proposal = "prop-1",
      author = "0xauthor",
      author_choice = "2",
      author_choice_weight = "1",
      author_vp = "5",
      author_pos = "1",
      author_rank = "1",
      vote_mode = "favorite_choice",
      stringsAsFactors = FALSE
    ),
    favorite_path,
    row.names = FALSE
  )

  write.csv(
    data.frame(
      proposal = "prop-1",
      author = "0xauthor",
      author_choice = "2",
      author_support_fraction = "1",
      author_vp = "5",
      author_pos = "1",
      author_rank = "1",
      vote_mode = "support_fraction",
      stringsAsFactors = FALSE
    ),
    support_path,
    row.names = FALSE
  )
}

write_json_file <- function(path, payload) {
  cat(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE), file = path)
}

capture_loader_error <- function() {
  tryCatch(
    {
      loadAuthorVoteOutputs()
      NULL
    },
    error = function(err) err$message
  )
}

write_author_outputs()
write_json_file(
  run_meta_path,
  list(
    job_name = "author_votes_v1",
    run_id = "run-1",
    tmp_root = file.path(author_vote_dir, "tmp", "run-1"),
    generated_at = "2026-04-16T10:00:00Z"
  )
)

missing_summary_error <- capture_loader_error()
stopifnot(is.character(missing_summary_error))
stopifnot(grepl("Missing canonical author-vote run summary", missing_summary_error, fixed = TRUE))

write_json_file(
  run_summary_path,
  list(
    job_name = "author_votes_v1",
    run_id = "run-1",
    is_complete = FALSE,
    manifest_total = 1,
    manifest_finished = 0,
    manifest_pending = 1,
    manifest_running = 0,
    manifest_failed = 0,
    tmp_root = file.path(author_vote_dir, "tmp", "run-1")
  )
)

incomplete_summary_error <- capture_loader_error()
stopifnot(is.character(incomplete_summary_error))
stopifnot(grepl("incomplete", incomplete_summary_error, fixed = TRUE))

write_json_file(
  run_summary_path,
  list(
    job_name = "author_votes_v1",
    run_id = "run-older",
    is_complete = TRUE,
    manifest_total = 1,
    manifest_finished = 1,
    manifest_pending = 0,
    manifest_running = 0,
    manifest_failed = 0,
    tmp_root = file.path(author_vote_dir, "tmp", "run-older")
  )
)

run_id_mismatch_error <- capture_loader_error()
stopifnot(is.character(run_id_mismatch_error))
stopifnot(grepl("stale", run_id_mismatch_error, fixed = TRUE))

write_json_file(
  run_summary_path,
  list(
    job_name = "author_votes_v1",
    run_id = "run-1",
    is_complete = TRUE,
    manifest_total = 1,
    manifest_finished = 1,
    manifest_pending = 0,
    manifest_running = 0,
    manifest_failed = 0,
    tmp_root = file.path(author_vote_dir, "tmp", "run-1")
  )
)

outputs <- loadAuthorVoteOutputs()
stopifnot(is.list(outputs))
stopifnot(identical(outputs$favorite$proposal[[1]], "prop-1"))
stopifnot(is.numeric(outputs$favorite$author_choice[[1]]))
stopifnot(outputs$favorite$author_choice[[1]] == 2)
stopifnot(outputs$support$author_support_fraction[[1]] == 1)
