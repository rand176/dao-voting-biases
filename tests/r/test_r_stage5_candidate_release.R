source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
MAIN_DIR <- PACKAGE_ROOT

LOAD_ANALYSIS_DATA_ON_SOURCE <- FALSE
initialize_test_analysis()
source(file.path(MAIN_DIR, "src/r/data/load.R"))
source(file.path(MAIN_DIR, "src/r/preparation/author_power.R"))
source(file.path(MAIN_DIR, "src/r/preparation/labels.R"))
source(file.path(MAIN_DIR, "src/r/preparation/release.R"))

# An explicit empty curated table represents this fixture's no-exception policy.
MAIN_DIR <- tempfile("stage5-inputs-")
EXCEPTIONS_DIR <- file.path(MAIN_DIR,"metadata/exceptions")
dir.create(EXCEPTIONS_DIR,recursive=TRUE)
write.csv(data.frame(stage=character(),proposal=character(),allowed_state=character()),
  file.path(EXCEPTIONS_DIR,"stance_one_sided_exceptions.csv"),row.names=FALSE)

candidate_input <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-a", "prop-b"),
  choice_idx = c(1, 2, 3, 1),
  prop_chid = c("prop-a1", "prop-a2", "prop-a3", "prop-b1"),
  type = c("weighted", "weighted", "weighted", "single-choice"),
  is_author_choice = c(1, 0, 0, 1),
  is_author_supported_choice = c(1, 1, 1, 1),
  author_support_fraction = c(0.5, 0.25, 0.25, 1),
  author_vp = c(4, 4, 4, 2),
  choice_score = c(12, 10, 8, 7),
  choice_share = c(NA_real_, NA_real_, NA_real_, NA_real_),
  quorum = c(5, 5, 5, 4),
  stance_oai = c("APPROVE", "REJECT", "ABSTAIN", "APPROVE"),
  suggested_stance_oai = c("APPROVE", "REJECT", "ABSTAIN", "APPROVE"),
  effect_oai = c("E1", "E1", "E1", "E2"),
  stance_hl = c("APPROVE", "REJECT", NA, "APPROVE"),
  suggested_stance_hl = c("APPROVE", "REJECT", NA, "APPROVE"),
  stringsAsFactors = FALSE
)

candidate_output <- buildNoAuthorVpRelease(candidate_input)

stopifnot(nrow(candidate_output) == nrow(candidate_input))
stopifnot(identical(candidate_output$prop_chid, candidate_input$prop_chid))
stopifnot(identical(candidate_output$stance_hl, candidate_input$stance_hl))
stopifnot(identical(candidate_output$suggested_stance_oai, candidate_input$suggested_stance_oai))
stopifnot(all(c("choice_score_no_author_vp", "choice_share_no_author_vp", "first_choice_won_no_author_vp", "choice_won_no_author_vp") %in% colnames(candidate_output)))
stopifnot(isTRUE(all.equal(as.numeric(candidate_output$choice_score_no_author_vp), c(10, 9, 7, 5), check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(candidate_output$choice_share_no_author_vp[1:3], c(10 / 26, 9 / 26, 7 / 26), tolerance = 1e-12)))
stopifnot(identical(candidate_output$choice_share_no_author_vp[4], 1))
stopifnot(identical(candidate_output$first_choice_won_no_author_vp, c(1L, 0L, 0L, 1L)))
stopifnot(identical(candidate_output$choice_won_no_author_vp, c(1L, 0L, 0L, 1L)))
stopifnot(!("author_n_choices" %in% colnames(candidate_output)))

raw_release_input <- tempfile(fileext = ".csv")
normalized_candidate_output <- tempfile(
  pattern = "stage5_candidate_release_",
  tmpdir = DERIVED_ACTIVE_DIR,
  fileext = ".csv"
)
normalization_audit_output <- tempfile(
  pattern = "stage5_candidate_release_audit_",
  tmpdir = ANALYSIS_PROVENANCE_DIR,
  fileext = ".csv"
)
on.exit({
  unlink(raw_release_input)
  unlink(normalized_candidate_output)
  unlink(normalization_audit_output)
}, add = TRUE)

write.csv(
  data.frame(
    proposal = "prop-undocumented-stage5",
    choice_idx = 1,
    stance = "APPROVE",
    is_infavor = 1,
    is_against = 0,
    is_abstain = 0,
    is_other = 0,
    author_choice_stance = "APPROVE",
    author_choice_stance_compact = "APPROVE",
    author_multi_approve = 1,
    stringsAsFactors = FALSE
  ),
  raw_release_input,
  row.names = FALSE
)

normalization_result <- normalizeStage5CandidateRelease(
  input_path = raw_release_input,
  output_path = normalized_candidate_output,
  audit_path = normalization_audit_output,
  stage = "2_stance_v5",
  python_bin = resolvePythonCommand()
)
normalized_rows <- read.csv(normalized_candidate_output, stringsAsFactors = FALSE)
normalization_audit <- read.csv(normalization_audit_output, stringsAsFactors = FALSE)

stopifnot(identical(normalized_rows$stance[[1]], "OTHER"))
stopifnot(identical(as.integer(normalized_rows$is_other[[1]]), 1L))
stopifnot(identical(normalization_audit$action[[1]], "normalized_to_other"))
stopifnot(identical(normalization_result$summary$normalized_proposals, 1L))

escaped_audit_output <- file.path(
  ANALYSIS_PROVENANCE_DIR,
  "allowed",
  "..",
  "..",
  "exceptions",
  "stage5_candidate_release_escape_audit.csv"
)
escaped_audit_real_path <- file.path(MAIN_DIR, "metadata", "exceptions", "stage5_candidate_release_escape_audit.csv")
if (file.exists(escaped_audit_real_path)) {
  unlink(escaped_audit_real_path)
}
escaped_audit_error <- tryCatch(
  {
    normalizeStage5CandidateRelease(
      input_path = raw_release_input,
      output_path = normalized_candidate_output,
      audit_path = escaped_audit_output,
      stage = "2_stance_v5",
      python_bin = resolvePythonCommand()
    )
    NULL
  },
  error = function(err) conditionMessage(err)
)
stopifnot(is.character(escaped_audit_error))
stopifnot(grepl("Analysis provenance outputs must be written under", escaped_audit_error, fixed = TRUE))
stopifnot(!file.exists(escaped_audit_real_path))

stage5_test_dir <- file.path(tempdir(), "stage5_candidate_release_orchestrator")
dir.create(stage5_test_dir, recursive = TRUE, showWarnings = FALSE)
stage5_candidate_input_path <- file.path(stage5_test_dir, "stage5_candidate_input.csv")
stage4_input_path <- file.path(stage5_test_dir, "stage4_input.csv")
stage5_proposals_path <- file.path(stage5_test_dir, "proposals.csv")
stage5_canonical_output_path <- file.path(stage5_test_dir, "stage5_canonical.csv")
stage5_coverage_audit_path <- file.path(stage5_test_dir, "stage5_coverage_audit.csv")
stage5_raw_output_path <- file.path(stage5_test_dir, "raw_release.csv")
stage5_candidate_output_path <- file.path(stage5_test_dir, "candidate_release.csv")
stage5_audit_path <- file.path(stage5_test_dir, "candidate_audit.csv")
on.exit(unlink(stage5_test_dir, recursive = TRUE), add = TRUE)

write.csv(
  data.frame(
    proposal = c("prop-x", "prop-y"),
    choice_idx = c(1, 1),
    type = c("single-choice", "single-choice"),
    is_author_choice = c(1, 1),
    author_vp = c(2, 1),
    choice_score = c(5, 3),
    quorum = c(1, 1),
    stringsAsFactors = FALSE
  ),
  stage5_candidate_input_path,
  row.names = FALSE
)
write.csv(
  data.frame(
    proposal = c("prop-x", "prop-y"),
    choice_idx = c(1, 1),
    prop_chid = c("prop-x1", "prop-y1"),
    type = c("single-choice", "single-choice"),
    is_author_choice = c(1, 1),
    author_vp = c(2, 1),
    choice_score = c(5, 3),
    quorum = c(1, 1),
    stringsAsFactors = FALSE
  ),
  stage4_input_path,
  row.names = FALSE
)
write.csv(
  data.frame(
    id = c("prop-x", "prop-y"),
    stringsAsFactors = FALSE
  ),
  stage5_proposals_path,
  row.names = FALSE
)

orchestrator_calls <- new.env(parent = emptyenv())
orchestrator_calls$persisted <- NULL
orchestrator_calls$normalized <- NULL
orchestrator_calls$provenance <- NULL
orchestrator_calls$stats <- NULL

build_result <- buildStage5CandidateRelease(
  stage5_candidate_input = stage5_candidate_input_path,
  stage4_input = stage4_input_path,
  proposals_path = stage5_proposals_path,
  canonical_stage5_output_path = stage5_canonical_output_path,
  canonical_stage5_coverage_audit_path = stage5_coverage_audit_path,
  raw_output_path = stage5_raw_output_path,
  candidate_output_path = stage5_candidate_output_path,
      normalization_audit_path = stage5_audit_path,
      analysis_root = stage5_test_dir,
      python_bin = "python-stub",
      llama33_label_paths = character(),
      current_release_path = NULL,
      persist_canonical_stage5_fn = function(df_stage5_canonical, output_path) {
        write.csv(df_stage5_canonical, output_path, row.names = FALSE)
        output_path
      },
  persist_canonical_coverage_audit_fn = function(stage5_coverage_audit, output_path) {
    write.csv(stage5_coverage_audit, output_path, row.names = FALSE)
    output_path
  },
  persist_raw_release_fn = function(df_raw_candidate, raw_output_path) {
    orchestrator_calls$persisted <- df_raw_candidate
    write.csv(df_raw_candidate, raw_output_path, row.names = FALSE)
    raw_output_path
  },
  normalize_release_fn = function(input_path, output_path, audit_path, stage, python_bin) {
    orchestrator_calls$normalized <- list(
      input_path = input_path,
      output_path = output_path,
      audit_path = audit_path,
      stage = stage,
      python_bin = python_bin
    )
    normalized_df <- data.frame(
      proposal = c("prop-x", "prop-y"),
      space = c("space-x", "space-y"),
      choice_idx = c(1, 1),
      type = c("single-choice", "single-choice"),
      scores_total = c(5, 3),
      n_votes = c(5, 3),
      flagged = c(0, 0),
      stance = c("OTHER", "OTHER"),
      stance_oai = c("OTHER", "OTHER"),
      suggested_stance_oai = c("OTHER", "OTHER"),
      effect_oai = c("E1", "E2"),
      stance_llama = c("OTHER", "OTHER"),
      suggested_stance_llama = c("OTHER", "OTHER"),
      effect_llama = c("E1", "E2"),
      stance_chatgpt = c("OTHER", "OTHER"),
      suggested_stance_chatgpt = c("OTHER", "OTHER"),
      effect_chatgpt = c("E1", "E2"),
      stance_hl = c(NA_character_, NA_character_),
      suggested_stance_hl = c(NA_character_, NA_character_),
      n_evaluations = c(0, 0),
      n_non_low_effort_evaluations = c(0, 0),
      n_low_effort_evaluations = c(0, 0),
      low_effort_only = c(0, 0),
      effect_llama33 = c(NA_character_, NA_character_),
      suggested_stance_llama33 = c(NA_character_, NA_character_),
      stance_llama33 = c(NA_character_, NA_character_),
      source_file_llama33 = c(NA_character_, NA_character_),
      llama33_label_conflict = c(FALSE, FALSE),
      conflict_columns_llama33 = c(NA_character_, NA_character_),
      llama33_reviewed = c(FALSE, FALSE),
      llama33_not_reviewed = c(TRUE, TRUE),
      llama33_no_suggestion = c(FALSE, FALSE),
      llama33_missing_label = c(FALSE, FALSE),
      llama33_positive_suggested_stance = c(FALSE, FALSE),
      llama33_basic_proposal_exclusion = c(FALSE, FALSE),
      stringsAsFactors = FALSE
    )
    write.csv(normalized_df, output_path, row.names = FALSE)
    write.csv(
      data.frame(
        proposal = c("prop-x", "prop-y"),
        action = c("normalized_to_other", "normalized_to_other"),
        stringsAsFactors = FALSE
      ),
      audit_path,
      row.names = FALSE
    )
    list(
      output_path = output_path,
      audit_path = audit_path,
      summary = list(normalized_proposals = 2L)
    )
  },
  save_provenance_fn = function(provenance) {
    orchestrator_calls$provenance <- provenance
    "saved"
  },
  refresh_stats_fn = function(analysis_root, python_bin) {
    orchestrator_calls$stats <- list(
      analysis_root = analysis_root,
      python_bin = python_bin
    )
    list(
      csv_path = file.path(stage5_test_dir, "latex_counts_candidate.csv"),
      json_path = file.path(stage5_test_dir, "latex_counts_candidate.json")
    )
  }
)

normalized_candidate_rows <- read.csv(stage5_candidate_output_path, stringsAsFactors = FALSE)
stopifnot(nrow(orchestrator_calls$persisted) == 2)
stopifnot(file.exists(stage5_canonical_output_path))
stopifnot(file.exists(stage5_coverage_audit_path))
stopifnot(file.exists(stage5_raw_output_path))
stopifnot(file.exists(stage5_candidate_output_path))
stopifnot(file.exists(stage5_audit_path))
stopifnot(all(normalized_candidate_rows$stance == "OTHER"))
stopifnot(identical(build_result$canonical_stage5_rows, 2L))
stopifnot(identical(build_result$raw_rows, 2L))
stopifnot(identical(build_result$candidate_rows, 2L))
stopifnot(identical(build_result$normalize_result$summary$normalized_proposals, 2L))
stopifnot(identical(orchestrator_calls$normalized$stage, "2_stance_v5"))
stopifnot(identical(orchestrator_calls$normalized$python_bin, "python-stub"))
stopifnot(identical(orchestrator_calls$stats$analysis_root, stage5_test_dir))
stopifnot(identical(orchestrator_calls$stats$python_bin, "python-stub"))
stopifnot(identical(orchestrator_calls$provenance$release_name[[1]], "candidate"))
stopifnot(identical(
  orchestrator_calls$provenance$choices_path[[1]],
  normalizePath(stage5_candidate_output_path, winslash = "/", mustWork = FALSE)
))

stage5_schema_error <- tryCatch(
  {
    buildStage5CandidateRelease(
      stage5_candidate_input = stage5_candidate_input_path,
      stage4_input = stage4_input_path,
      proposals_path = stage5_proposals_path,
      canonical_stage5_output_path = file.path(stage5_test_dir, "stage5_canonical_missing_cols.csv"),
      canonical_stage5_coverage_audit_path = file.path(stage5_test_dir, "stage5_coverage_missing_cols.csv"),
      raw_output_path = file.path(stage5_test_dir, "raw_release_missing_cols.csv"),
      candidate_output_path = file.path(stage5_test_dir, "candidate_release_missing_cols.csv"),
      normalization_audit_path = file.path(stage5_test_dir, "candidate_audit_missing_cols.csv"),
      analysis_root = stage5_test_dir,
      python_bin = "python-stub",
      llama33_label_paths = character(),
      current_release_path = NULL,
      persist_canonical_stage5_fn = function(df_stage5_canonical, output_path) {
        write.csv(df_stage5_canonical, output_path, row.names = FALSE)
        output_path
      },
      persist_canonical_coverage_audit_fn = function(stage5_coverage_audit, output_path) {
        write.csv(stage5_coverage_audit, output_path, row.names = FALSE)
        output_path
      },
      persist_raw_release_fn = function(df_raw_candidate, raw_output_path) {
        write.csv(df_raw_candidate, raw_output_path, row.names = FALSE)
        raw_output_path
      },
      normalize_release_fn = function(input_path, output_path, audit_path, stage, python_bin) {
        write.csv(
          data.frame(
            proposal = c("prop-x", "prop-y"),
            stance = c("OTHER", "OTHER"),
            stringsAsFactors = FALSE
          ),
          output_path,
          row.names = FALSE
        )
        write.csv(
          data.frame(
            proposal = c("prop-x", "prop-y"),
            action = c("normalized_to_other", "normalized_to_other"),
            stringsAsFactors = FALSE
          ),
          audit_path,
          row.names = FALSE
        )
        list(
          output_path = output_path,
          audit_path = audit_path,
          summary = list(normalized_proposals = 2L)
        )
      },
      save_provenance_fn = function(provenance) provenance,
      refresh_stats_fn = function(analysis_root, python_bin) list(
        csv_path = file.path(stage5_test_dir, "latex_counts_candidate.csv"),
        json_path = file.path(stage5_test_dir, "latex_counts_candidate.json")
      )
    )
    NULL
  },
  error = function(e) conditionMessage(e)
)
stopifnot(!is.null(stage5_schema_error))
stopifnot(grepl(
  "Normalized candidate release is missing required columns",
  stage5_schema_error,
  fixed = TRUE
))
stopifnot(grepl("stance_hl", stage5_schema_error, fixed = TRUE))

current_validation_dir <- tempfile("stage5_current_validation_")
dir.create(current_validation_dir)
on.exit(unlink(current_validation_dir, recursive = TRUE), add = TRUE)
current_validation_current <- file.path(current_validation_dir, "current.csv")
current_validation_candidate <- file.path(current_validation_dir, "candidate.csv")
write.csv(
  data.frame(
    proposal = c("prop-a", "prop-a"),
    choice_idx = c(1L, 2L),
    prop_chid = c("prop-a1", "prop-a2"),
    stance = c("APPROVE", "REJECT"),
    stringsAsFactors = FALSE
  ),
  current_validation_current,
  row.names = FALSE
)
write.csv(
  data.frame(
    proposal = c("prop-a", "prop-a"),
    choice_idx = c(1L, 2L),
    prop_chid = c("prop-a1", "prop-a2"),
    stance = c("APPROVE", "REJECT"),
    suggested_stance_llama33 = c("APPROVE", "APPROVE"),
    llama33_reviewed = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  ),
  current_validation_candidate,
  row.names = FALSE
)
validation_summary <- validateStage5CandidateAgainstCurrent(
  candidate_path = current_validation_candidate,
  current_path = current_validation_current
)
stopifnot(identical(validation_summary$row_count, 2L))
stopifnot(identical(validation_summary$key_count, 2L))

write.csv(
  data.frame(
    proposal = c("prop-a", "prop-a"),
    choice_idx = c(1L, 2L),
    prop_chid = c("prop-a1", "prop-a2"),
    stance = c("APPROVE", "OTHER"),
    stringsAsFactors = FALSE
  ),
  current_validation_candidate,
  row.names = FALSE
)
validation_error <- tryCatch(
  {
    validateStage5CandidateAgainstCurrent(
      candidate_path = current_validation_candidate,
      current_path = current_validation_current
    )
    NULL
  },
  error = function(e) conditionMessage(e)
)
stopifnot(is.character(validation_error))
stopifnot(grepl("changed existing analytical columns", validation_error, fixed = TRUE))

overlay_summary <- overlayStage5CandidateWithCurrentColumns(
  candidate_path = current_validation_candidate,
  current_path = current_validation_current
)
stopifnot(identical(overlay_summary$overlaid_columns, 2L))
validation_summary_after_overlay <- validateStage5CandidateAgainstCurrent(
  candidate_path = current_validation_candidate,
  current_path = current_validation_current
)
stopifnot(identical(validation_summary_after_overlay$row_count, 2L))

llama33_sparse_test_dir <- tempfile("stage5_llama33_sparse_")
dir.create(llama33_sparse_test_dir)
on.exit(unlink(llama33_sparse_test_dir, recursive = TRUE), add = TRUE)

llama33_stage4_input <- file.path(llama33_sparse_test_dir, "stage4.csv")
llama33_stage5_input <- file.path(llama33_sparse_test_dir, "stage5.csv")
llama33_canonical_output <- file.path(llama33_sparse_test_dir, "stage5_canonical.csv")
llama33_coverage_output <- file.path(llama33_sparse_test_dir, "coverage.csv")

llama33_stage4 <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b", "prop-basic"),
  choice_idx = c(1L, 2L, 1L, 1L),
  space = c("space-a", "space-a", "space-b", "space-basic"),
  type = c("single-choice", "single-choice", "single-choice", "basic"),
  scores_total = c(10, 10, 20, 5),
  n_votes = c(2, 2, 4, 1),
  flagged = c(FALSE, FALSE, FALSE, FALSE),
  stance = c("APPROVE", "REJECT", "ABSTAIN", "OTHER"),
  stringsAsFactors = FALSE
)
write.csv(llama33_stage4, llama33_stage4_input, row.names = FALSE)

llama33_stage5_source <- llama33_stage4
llama33_stage5_source$effect_oai <- c("oai a", NA, "oai b", NA)
llama33_stage5_source$suggested_stance_oai <- c("APPROVE", NA, "ABSTAIN", NA)
llama33_stage5_source$stance_oai <- c("APPROVE", NA, "ABSTAIN", NA)
write.csv(llama33_stage5_source, llama33_stage5_input, row.names = FALSE)

llama33_sparse_labels <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b"),
  choice_idx = c(1L, 2L, 1L),
  prop_chid = c("prop-a1", "prop-a2", "prop-b1"),
  effect_llama33 = c("effect a", "effect a", "effect b"),
  suggested_stance_llama33 = c("APPROVE", NA, "REJECT"),
  stance_llama33 = c("APPROVE", "REJECT", "ABSTAIN"),
  source_file_llama33 = c("llama33.csv", "llama33.csv", "llama33.csv"),
  llama33_label_conflict = c(FALSE, FALSE, TRUE),
  conflict_columns_llama33 = c(NA, NA, "suggested_stance_llama33"),
  stringsAsFactors = FALSE
)

llama33_sparse_result <- assembleStage5CanonicalChoiceTable(
  stage4_input = llama33_stage4_input,
  stage5_source_input = llama33_stage5_input,
  output_path = llama33_canonical_output,
  coverage_audit_path = llama33_coverage_output,
  llama33_label_paths = "stub-llama33.csv",
  read_llama33_labels_fn = function(paths, conflict_audit_path = NULL) {
    stopifnot(identical(paths, "stub-llama33.csv"))
    llama33_sparse_labels
  },
  persist_choice_table_fn = function(df_stage5_canonical, output_path) {
    write.csv(df_stage5_canonical, output_path, row.names = FALSE)
    output_path
  },
  persist_coverage_audit_fn = function(stage5_coverage_audit, output_path) {
    write.csv(stage5_coverage_audit, output_path, row.names = FALSE)
    output_path
  }
)

llama33_sparse <- llama33_sparse_result$df_stage5_canonical
stopifnot(nrow(llama33_sparse) == nrow(llama33_stage4))
stopifnot(identical(llama33_sparse$prop_chid, paste0(llama33_stage4$proposal, llama33_stage4$choice_idx)))
stopifnot(all(c(
  "effect_llama33",
  "suggested_stance_llama33",
  "stance_llama33",
  "source_file_llama33",
  "llama33_reviewed",
  "llama33_not_reviewed",
  "llama33_no_suggestion",
  "llama33_label_conflict",
  "llama33_basic_proposal_exclusion"
) %in% colnames(llama33_sparse)))
stopifnot(isTRUE(llama33_sparse$llama33_reviewed[llama33_sparse$prop_chid == "prop-a1"]))
stopifnot(isTRUE(llama33_sparse$llama33_no_suggestion[llama33_sparse$prop_chid == "prop-a2"]))
stopifnot(isTRUE(llama33_sparse$llama33_label_conflict[llama33_sparse$prop_chid == "prop-b1"]))
stopifnot(isTRUE(llama33_sparse$llama33_not_reviewed[llama33_sparse$prop_chid == "prop-basic1"]))
stopifnot(isTRUE(llama33_sparse$llama33_basic_proposal_exclusion[llama33_sparse$prop_chid == "prop-basic1"]))

llama33_coverage <- read.csv(llama33_sparse_result$coverage_audit_path, stringsAsFactors = FALSE)
llama33_coverage_row <- llama33_coverage[llama33_coverage$label_source == "llama33", ]
stopifnot(nrow(llama33_coverage_row) == 1)
stopifnot(identical(llama33_coverage_row$reviewed_rows[[1]], 3L))
stopifnot(identical(llama33_coverage_row$not_reviewed_rows[[1]], 1L))
stopifnot(identical(llama33_coverage_row$no_suggestion_rows[[1]], 1L))
stopifnot(identical(llama33_coverage_row$conflict_rows[[1]], 1L))
stopifnot(identical(llama33_coverage_row$basic_exclusion_rows[[1]], 1L))

stage5_canonicalization_test_dir <- file.path(tempdir(), "stage5_candidate_release_canonicalization")
dir.create(stage5_canonicalization_test_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(stage5_canonicalization_test_dir, recursive = TRUE), add = TRUE)

filtered_stage4_input_path <- file.path(stage5_canonicalization_test_dir, "stage4_input.csv")
filtered_stage5_input_path <- file.path(stage5_canonicalization_test_dir, "stage5_filtered_input.csv")
filtered_proposals_path <- file.path(stage5_canonicalization_test_dir, "proposals.csv")
filtered_canonical_output_path <- file.path(stage5_canonicalization_test_dir, "stage5_canonical.csv")
filtered_coverage_audit_path <- file.path(stage5_canonicalization_test_dir, "stage5_coverage_audit.csv")
filtered_raw_output_path <- file.path(stage5_canonicalization_test_dir, "raw_release.csv")
filtered_candidate_output_path <- file.path(stage5_canonicalization_test_dir, "candidate_release.csv")
filtered_normalization_audit_path <- file.path(stage5_canonicalization_test_dir, "candidate_audit.csv")

write.csv(
  data.frame(
    proposal = c("prop-a", "prop-a", "prop-b"),
    choice_idx = c(1, 2, 1),
    prop_chid = c("prop-a1", "prop-a2", "prop-b1"),
    type = c("weighted", "weighted", "single-choice"),
    is_author_choice = c(1, 0, 1),
    author_vp = c(4, 4, 2),
    choice_score = c(12, 10, 7),
    quorum = c(5, 5, 4),
    stringsAsFactors = FALSE
  ),
  filtered_stage4_input_path,
  row.names = FALSE
)
write.csv(
  data.frame(
    proposal = c("prop-a", "prop-a"),
    choice_idx = c(1, 2),
    prop_chid = c("prop-a1", "prop-a2"),
    type = c("weighted", "weighted"),
    is_author_choice = c(1, 0),
    author_vp = c(4, 4),
    choice_score = c(12, 10),
    quorum = c(5, 5),
    stance_oai = c("APPROVE", "REJECT"),
    suggested_stance_oai = c("APPROVE", "REJECT"),
    effect_oai = c("E1", "E1"),
    stance_hl = c("APPROVE", NA_character_),
    suggested_stance_hl = c("APPROVE", NA_character_),
    valid_GPT4o_v1 = c(1, 1),
    stringsAsFactors = FALSE
  ),
  filtered_stage5_input_path,
  row.names = FALSE
)
write.csv(
  data.frame(
    id = c("prop-a", "prop-b"),
    stringsAsFactors = FALSE
  ),
  filtered_proposals_path,
  row.names = FALSE
)

canonicalized_build_result <- buildStage5CandidateRelease(
  stage5_candidate_input = filtered_stage5_input_path,
  stage4_input = filtered_stage4_input_path,
  proposals_path = filtered_proposals_path,
  canonical_stage5_output_path = filtered_canonical_output_path,
  canonical_stage5_coverage_audit_path = filtered_coverage_audit_path,
  raw_output_path = filtered_raw_output_path,
  candidate_output_path = filtered_candidate_output_path,
  normalization_audit_path = filtered_normalization_audit_path,
  analysis_root = stage5_canonicalization_test_dir,
  python_bin = "python-stub",
  llama33_label_paths = character(),
  current_release_path = NULL,
  persist_canonical_stage5_fn = function(df_stage5_canonical, output_path) {
    write.csv(df_stage5_canonical, output_path, row.names = FALSE)
    output_path
  },
  persist_canonical_coverage_audit_fn = function(stage5_coverage_audit, output_path) {
    write.csv(stage5_coverage_audit, output_path, row.names = FALSE)
    output_path
  },
  persist_raw_release_fn = function(df_raw_candidate, raw_output_path) {
    write.csv(df_raw_candidate, raw_output_path, row.names = FALSE)
    raw_output_path
  },
  normalize_release_fn = function(input_path, output_path, audit_path, stage, python_bin) {
    normalized_df <- read.csv(input_path, stringsAsFactors = FALSE)
    normalized_df$space <- c("space-a", "space-a", "space-b")
    normalized_df$scores_total <- c(12, 10, 7)
    normalized_df$n_votes <- c(12, 10, 7)
    normalized_df$flagged <- c(0, 0, 0)
    normalized_df$stance <- c("APPROVE", "REJECT", "OTHER")
    normalized_df$stance_chatgpt <- NA_character_
    normalized_df$suggested_stance_chatgpt <- NA_character_
    normalized_df$effect_chatgpt <- NA_character_
    normalized_df$stance_llama <- NA_character_
    normalized_df$suggested_stance_llama <- NA_character_
    normalized_df$effect_llama <- NA_character_
    normalized_df$stance_hl <- NA_character_
    normalized_df$suggested_stance_hl <- NA_character_
    normalized_df$n_evaluations <- c(0, 0, 0)
    normalized_df$n_non_low_effort_evaluations <- c(0, 0, 0)
    normalized_df$n_low_effort_evaluations <- c(0, 0, 0)
    normalized_df$low_effort_only <- c(0, 0, 0)
    write.csv(normalized_df, output_path, row.names = FALSE)
    write.csv(
      data.frame(
        proposal = c("prop-a", "prop-b"),
        action = c("kept", "kept"),
        stringsAsFactors = FALSE
      ),
      audit_path,
      row.names = FALSE
    )
    list(
      output_path = output_path,
      audit_path = audit_path,
      summary = list(normalized_proposals = 0L)
    )
  },
  save_provenance_fn = function(provenance) provenance,
  refresh_stats_fn = function(analysis_root, python_bin) list(
    csv_path = file.path(stage5_canonicalization_test_dir, "latex_counts_candidate.csv"),
    json_path = file.path(stage5_canonicalization_test_dir, "latex_counts_candidate.json")
  )
)

canonicalized_stage5 <- read.csv(filtered_canonical_output_path, stringsAsFactors = FALSE)
canonicalized_raw <- read.csv(filtered_raw_output_path, stringsAsFactors = FALSE)
canonicalized_coverage <- read.csv(filtered_coverage_audit_path, stringsAsFactors = FALSE)

stopifnot(identical(canonicalized_build_result$canonical_stage5_rows, 3L))
stopifnot(identical(canonicalized_build_result$raw_rows, 3L))
stopifnot(identical(canonicalized_build_result$candidate_rows, 3L))
stopifnot(identical(canonicalized_stage5$prop_chid, c("prop-a1", "prop-a2", "prop-b1")))
stopifnot(!("valid_GPT4o_v1" %in% colnames(canonicalized_stage5)))
stopifnot(identical(
  canonicalized_stage5$proposal_in_gpt4o_review_universe,
  c(1L, 1L, 0L)
))
stopifnot(identical(
  canonicalized_stage5$proposal_in_gpt4o_analysis_scope,
  c(1L, 1L, 0L)
))
stopifnot(is.na(canonicalized_stage5$stance_oai[[3]]))
stopifnot(identical(canonicalized_raw$prop_chid, canonicalized_stage5$prop_chid))
stopifnot(all(c("oai", "human") %in% canonicalized_coverage$label_source))
