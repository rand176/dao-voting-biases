source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
MAIN_DIR <- PACKAGE_ROOT
R_DIR <- file.path(PACKAGE_ROOT,"src/r")
DATA_DIR <- file.path(tempdir(), "dao-stage-pipeline")
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

initialize_test_analysis()
source(file.path(PACKAGE_ROOT, "src/r/preparation/stance_patterns.R"))

write.csv(
  data.frame(
    stage = c("stage-test", "stage-test"),
    proposal = c("prop-meta", "prop-other"),
    reason = c("metadata exclusion", "other stage exclusion"),
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "proposal_exclusions.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    id = c("prop-dup", "prop-safe"),
    flag_duplicate_ballots = c(1, 0),
    duplicate_pairs = c(2, 0),
    duplicate_ballot_rows = c(4, 0),
    duplicate_wallets = c(2, 0),
    flag_category = c("duplicate_ballots", "not_flagged"),
    flag_reason = c("duplicate", "clean"),
    stringsAsFactors = FALSE
  ),
  gzfile(file.path(PROPOSAL_FLAGS_DIR, "proposal_duplicate_ballot_flags.csv.gz")),
  row.names = FALSE
)

write.csv(
  data.frame(
    id = c("prop-dup", "prop-invalid", "prop-safe"),
    flag_duplicate_ballots = c(1, 0, 0),
    duplicate_pairs = c(2, 0, 0),
    duplicate_ballot_rows = c(4, 0, 0),
    duplicate_wallets = c(2, 0, 0),
    flag_invalid_votes = c(0, 1, 0),
    invalid_vote_rows = c(0, 3, 0),
    flag_any_vote_issues = c(1, 1, 0),
    issue_categories = c("duplicate_ballots", "invalid_votes", "not_flagged"),
    flag_reason = c("duplicate", "invalid", "clean"),
    stringsAsFactors = FALSE
  ),
  gzfile(file.path(PROPOSAL_FLAGS_DIR, "proposal_vote_issue_flags.csv.gz")),
  row.names = FALSE
)

ids_without_duplicates <- getProposalExclusionIds("stage-test")
stopifnot(identical(ids_without_duplicates, c("prop-meta", "prop-other")))

ids_with_duplicates <- getProposalExclusionIds("stage-test", include_duplicate_ballots = TRUE)
stopifnot(setequal(ids_with_duplicates, c("prop-meta", "prop-other", "prop-dup")))

issue_ids <- getProposalExclusionIds("stage-test", include_vote_issues = TRUE)
stopifnot(setequal(issue_ids, c("prop-meta", "prop-other", "prop-dup", "prop-invalid")))

df_props <- data.frame(
  proposal = c("prop-meta", "prop-dup", "prop-invalid", "prop-safe"),
  value = c(1, 2, 3, 4),
  stringsAsFactors = FALSE
)

filtered_without_duplicates <- applyProposalExclusions(df_props, stage = "stage-test")
stopifnot(identical(filtered_without_duplicates$proposal, c("prop-dup", "prop-invalid", "prop-safe")))

filtered_with_duplicates <- applyProposalExclusions(
  df_props,
  stage = "stage-test",
  include_duplicate_ballots = TRUE
)
stopifnot(identical(filtered_with_duplicates$proposal, c("prop-invalid", "prop-safe")))

filtered_with_issues <- applyProposalExclusions(
  df_props,
  stage = "stage-test",
  include_vote_issues = TRUE
)
stopifnot(identical(filtered_with_issues$proposal, "prop-safe"))

root_clean_vote_dir <- file.path(DATA_DIR, "clean_vote_outputs")
legacy_clean_vote_dir <- file.path(root_clean_vote_dir, "old_run_with_approval_discarded")
dir.create(legacy_clean_vote_dir, recursive = TRUE, showWarnings = FALSE)

legacy_vote_file <- file.path(legacy_clean_vote_dir, "prop_choices_favorite_counts.csv")
writeLines("proposal,choice_idx,n_votes,tot_votes,vote_mode", legacy_vote_file)
stopifnot(identical(cleanVoteOutputPath("prop_choices_favorite_counts.csv"), legacy_vote_file))

root_vote_file <- file.path(root_clean_vote_dir, "prop_choices_favorite_counts.csv")
writeLines("proposal,choice_idx,n_votes,tot_votes,vote_mode", root_vote_file)
stopifnot(identical(cleanVoteOutputPath("prop_choices_favorite_counts.csv"), root_vote_file))

candidate_path <- debugDataPath("choices_augmented_2025b_dup_excluded.csv")
candidate_df <- data.frame(proposal = "prop-safe", choice_idx = 1, stringsAsFactors = FALSE)
writeCandidateCsv(candidate_df, candidate_path)
stopifnot(file.exists(candidate_path))

managed_path_probe_root <- file.path(tempdir(), "managed_path_probe")
managed_allowed_dir <- file.path(managed_path_probe_root, "allowed")
managed_forbidden_dir <- file.path(managed_path_probe_root, "forbidden")
dir.create(managed_allowed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(managed_forbidden_dir, recursive = TRUE, showWarnings = FALSE)
managed_traversal_path <- file.path(managed_allowed_dir, "..", "forbidden", "pwned.txt")
stopifnot(!pathIsWithinDirectory(managed_traversal_path, managed_allowed_dir))
managed_traversal_error <- tryCatch(
  {
    assertManagedOutputPath(managed_traversal_path, managed_allowed_dir, "Managed outputs")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(managed_traversal_error))
stopifnot(grepl("Managed outputs must be written under", managed_traversal_error, fixed = TRUE))
stopifnot(pathIsWithinDirectory(file.path(managed_allowed_dir, "nested", "ok.txt"), managed_allowed_dir))

dir.create(file.path(DERIVED_DIR, "debugger"), recursive = TRUE, showWarnings = FALSE)
outside_debug_path <- file.path(DERIVED_DIR, "debugger", "should_not_write.csv")
outside_write_error <- tryCatch(
  {
    writeCandidateCsv(candidate_df, outside_debug_path)
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(outside_write_error))
stopifnot(grepl("Candidate outputs must be written under", outside_write_error, fixed = TRUE))
stopifnot(!file.exists(outside_debug_path))

write_error <- tryCatch(
  {
    writeCandidateCsv(candidate_df, candidate_path)
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(write_error))
stopifnot(grepl("already exists", write_error, fixed = TRUE))

release_source_path <- tempfile(fileext = ".csv")
write.csv(candidate_df, release_source_path, row.names = FALSE)
release_path <- activeDataPath("stage5_candidate_release_helper_test.csv")
if (file.exists(release_path)) {
  unlink(release_path)
}
copied_release <- ensureReleaseFileCopy(release_source_path, release_path, "normalized release source")
stopifnot(identical(copied_release, release_path))
stopifnot(file.exists(release_path))
copied_release_again <- ensureReleaseFileCopy(release_source_path, release_path, "normalized release source")
stopifnot(identical(copied_release_again, release_path))

traversal_release_real_path <- file.path(
  DERIVED_ACTIVE_DIR,
  "allowed",
  "..",
  "..",
  "archive_large",
  "should_not_traverse_release.csv"
)
traversal_release_escape_path <- file.path(DERIVED_DIR, "archive_large", "should_not_traverse_release.csv")
if (file.exists(traversal_release_escape_path)) {
  unlink(traversal_release_escape_path)
}
traversal_release_error <- tryCatch(
  {
    ensureReleaseFileCopy(release_source_path, traversal_release_real_path, "normalized release source")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(traversal_release_error))
stopifnot(grepl("Release outputs must be written under", traversal_release_error, fixed = TRUE))
stopifnot(!file.exists(traversal_release_escape_path))

outside_active_path <- file.path(DERIVED_DIR, "archive_large", "should_not_write_release.csv")
release_copy_error <- tryCatch(
  {
    ensureReleaseFileCopy(release_source_path, outside_active_path, "normalized release source")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(release_copy_error))
stopifnot(grepl("Release outputs must be written under", release_copy_error, fixed = TRUE))
stopifnot(!file.exists(outside_active_path))
unlink(release_source_path)
unlink(release_path)

stopifnot(identical(requireExistingInputPath(candidate_path, "candidate output"), candidate_path))

missing_input_error <- tryCatch(
  {
    requireExistingInputPath(file.path(DERIVED_DEBUG_DIR, "missing.csv"), "missing candidate")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(missing_input_error))
stopifnot(grepl("Required input is missing", missing_input_error, fixed = TRUE))

snapshot_source_path <- activeDataPath("choices_lang_len_fake_paper.csv.gz")
snapshot_df <- data.frame(proposal = "prop-safe", choice_idx = 1, stringsAsFactors = FALSE)
write.csv(
  snapshot_df,
  gzfile(snapshot_source_path),
  row.names = FALSE
)
snapshot_candidate_path <- debugDataPath("choices_lang_len_fake_paper_stage0_snapshot.csv.gz")
copied_snapshot <- ensureCandidateFileCopy(snapshot_source_path, snapshot_candidate_path, "stage 0 source")
stopifnot(identical(copied_snapshot, snapshot_candidate_path))
stopifnot(file.exists(snapshot_candidate_path))
snapshot_size_before <- file.info(snapshot_candidate_path)$size
write.csv(
  snapshot_df,
  gzfile(snapshot_source_path, compression = 1),
  row.names = FALSE
)
copied_snapshot_again <- ensureCandidateFileCopy(snapshot_source_path, snapshot_candidate_path, "stage 0 source")
stopifnot(identical(copied_snapshot_again, snapshot_candidate_path))
stopifnot(identical(file.info(snapshot_candidate_path)$size, snapshot_size_before))

traversal_snapshot_real_path <- file.path(
  DERIVED_DEBUG_DIR,
  "allowed",
  "..",
  "..",
  "active",
  "should_not_traverse_candidate.csv.gz"
)
traversal_snapshot_escape_path <- file.path(DERIVED_DIR, "active", "should_not_traverse_candidate.csv.gz")
if (file.exists(traversal_snapshot_escape_path)) {
  unlink(traversal_snapshot_escape_path)
}
traversal_snapshot_error <- tryCatch(
  {
    ensureCandidateFileCopy(snapshot_source_path, traversal_snapshot_real_path, "stage 0 source")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(traversal_snapshot_error))
stopifnot(grepl("Candidate outputs must be written under", traversal_snapshot_error, fixed = TRUE))
stopifnot(!file.exists(traversal_snapshot_escape_path))

write.csv(
  data.frame(proposal = "prop-updated", choice_idx = 2, stringsAsFactors = FALSE),
  gzfile(snapshot_source_path),
  row.names = FALSE
)
stale_snapshot_error <- tryCatch(
  {
    ensureCandidateFileCopy(snapshot_source_path, snapshot_candidate_path, "stage 0 source")
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(stale_snapshot_error))
stopifnot(grepl("different contents", stale_snapshot_error, fixed = TRUE))

classifications <- classifyChoiceIdxValues(c("1", "0", "-1", "{", "\"bad\"", "0xabc", NA, "abc"))
stopifnot(identical(
  classifications,
  c("positive_integer", "zero", "minus_one", "brace", "quoted", "hex", "na", "other_text")
))

classification_warning <- NULL
classifications_mixed <- withCallingHandlers(
  classifyChoiceIdxValues(c("1", "abc", "02", " 5 ", NA, "{")),
  warning = function(w) {
    classification_warning <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
stopifnot(is.null(classification_warning))
stopifnot(identical(
  classifications_mixed,
  c("positive_integer", "other_text", "other_text", "positive_integer", "na", "brace")
))

write.csv(
  data.frame(
    stage = "3_merging_N_votes_v2",
    proposal = "prop-drop",
    reason = "stage 3 exclusion",
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "proposal_exclusions.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    id = c("prop-dup", "prop-safe"),
    flag_duplicate_ballots = c(1, 0),
    duplicate_pairs = c(2, 0),
    duplicate_ballot_rows = c(4, 0),
    duplicate_wallets = c(2, 0),
    flag_category = c("duplicate_ballots", "not_flagged"),
    flag_reason = c("duplicate", "clean"),
    stringsAsFactors = FALSE
  ),
  gzfile(file.path(PROPOSAL_FLAGS_DIR, "proposal_duplicate_ballot_flags.csv.gz")),
  row.names = FALSE
)

vote_counts <- data.frame(
  proposal = c("prop-keep", "prop-keep", "prop-keep", "prop-keep", "prop-drop", "prop-dup"),
  choice_idx = c("1", "2", "0", "\"bad\"", "1", "2"),
  n_votes = c(4, 1, 2, 3, 7, 9),
  tot_votes = c(10, 10, 10, 10, 7, 9),
  stringsAsFactors = FALSE
)

cleaned <- cleanStage3VoteCounts(
  vote_counts,
  stage = "3_merging_N_votes_v2",
  include_vote_issues = TRUE
)

stopifnot(nrow(cleaned$data) == 2)
stopifnot(setequal(cleaned$data$proposal, "prop-keep"))
stopifnot(identical(cleaned$data$choice_idx, c(1L, 2L)))
stopifnot(all(cleaned$data$tot_votes == 5))

audit_summary <- subset(cleaned$audit, phase == "apply_stage_exclusions" & metric == "proposals_removed")
stopifnot(nrow(audit_summary) == 1)
stopifnot(audit_summary$value[1] == 2)

invalid_removed <- subset(cleaned$audit, phase == "remove_invalid_choice_idx" & metric == "rows_removed")
stopifnot(sum(invalid_removed$value) == 2)

df_choices <- data.frame(
  proposal = c("prop-keep", "prop-keep", "prop-keep"),
  choice_idx = c(1L, 2L, 3L),
  prop_chid = c("prop-keep1", "prop-keep2", "prop-keep3"),
  choice_share = c(0.8, 0.2, 0.0),
  stringsAsFactors = FALSE
)

zero_filled <- zeroFillMissingFavoriteCounts(cleaned$data, df_choices)
stopifnot(nrow(zero_filled) == 3)
stopifnot(all(c("prop-keep1", "prop-keep2", "prop-keep3") %in% zero_filled$prop_chid))
stopifnot(zero_filled$n_votes[zero_filled$choice_idx == 3] == 0)
stopifnot(zero_filled$tot_votes[zero_filled$choice_idx == 3] == 5)

write.csv(
  data.frame(
    stage = "2_stance_v5",
    space = "space-drop",
    reason = "legacy stage 2 space exclusion",
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "space_exclusions.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    stage = c("2_stance_v5", "2_stance_v5"),
    space = c("space-force-other", "space-force-other"),
    column_name = c("stance2", "notes"),
    value = c("OTHER", "space override"),
    source_line = c(1, 2),
    note = c("space override", "space note"),
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "space_value_overrides.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    stage = c("2_stance_v5", "2_stance_v5"),
    proposal = c("prop-proposal-wins", "prop-proposal-wins"),
    column_name = c("stance2", "notes"),
    value = c("REJECT", "proposal override"),
    source_line = c(1, 2),
    note = c("proposal override", "proposal note"),
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "proposal_value_overrides.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    stage = c("2_stance_v5", "2_stance_v5"),
    proposal = c("prop-sequence", "prop-sequence-undoc"),
    stance_sequence = c("APPROVE|ABSTAIN", "APPROVE|ABSTAIN"),
    source_line = c(1, 2),
    note = c("sequence override", "undocumented one-sided sequence override"),
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "stance_choice_overrides.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(
    stage = c("2_stance_v5", "2_stance_v5"),
    proposal = c("prop-proposal-wins", "prop-sequence"),
    allowed_state = c("reject_only", "approve_only"),
    source_line = c(1, 2),
    note = c("documented one-sided proposal exception", "documented one-sided sequence exception"),
    stringsAsFactors = FALSE
  ),
  file.path(EXCEPTIONS_DIR, "stance_one_sided_exceptions.csv"),
  row.names = FALSE
)

stage2_df <- data.frame(
  proposal = c(
    "prop-keep",
    "prop-space-only",
    "prop-proposal-wins",
    "prop-sequence",
    "prop-sequence",
    "prop-sequence-undoc",
    "prop-sequence-undoc",
    "prop-undoc-approve",
    "prop-undoc-approve",
    "prop-undoc-reject",
    "prop-undoc-reject",
    "prop-drop-space",
    "prop-typo-approve",
    "prop-typo-abstain"
  ),
  space = c(
    "space-keep",
    "space-force-other",
    "space-force-other",
    "space-force-other",
    "space-force-other",
    "space-force-other",
    "space-force-other",
    "space-keep",
    "space-keep",
    "space-keep",
    "space-keep",
    "space-drop",
    "space-keep",
    "space-keep"
  ),
  choice_idx = c(1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1),
  choice_text = c(
    "Yes",
    "Maybe",
    "No",
    "Choice A",
    "Choice B",
    "Choice C",
    "Choice D",
    "Approve only",
    "Other",
    "Reject only",
    "Abstain",
    "Drop",
    "Typo",
    "Typo"
  ),
  stance2 = c(
    "APPROVE",
    "APPROVE",
    "APPROVE",
    "REJECT",
    "REJECT",
    "REJECT",
    "REJECT",
    "APPROVE",
    "OTHER",
    "REJECT",
    "ABSTAIN",
    "OTHER",
    "A",
    "Abstain"
  ),
  notes = "",
  stringsAsFactors = FALSE
)

stage2_filtered <- applySpaceExclusions(stage2_df, stage = "2_stance_v5")
stopifnot(!("prop-drop-space" %in% stage2_filtered$proposal))

stage2_space_values <- applySpaceValueOverrides(stage2_filtered, stage = "2_stance_v5")
stopifnot(identical(
  stage2_space_values$stance2[stage2_space_values$proposal == "prop-space-only"],
  "OTHER"
))

stage2_updated <- applyStage2StanceOverrides(stage2_filtered, stage = "2_stance_v5")
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-space-only"],
  "OTHER"
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-proposal-wins"],
  "REJECT"
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-sequence"],
  c("APPROVE", "ABSTAIN")
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-sequence-undoc"],
  c("OTHER", "OTHER")
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-undoc-approve"],
  c("OTHER", "OTHER")
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-undoc-reject"],
  c("OTHER", "OTHER")
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-typo-approve"],
  "OTHER"
))
stopifnot(identical(
  stage2_updated$stance2[stage2_updated$proposal == "prop-typo-abstain"],
  "ABSTAIN"
))
stopifnot(identical(
  unique(stage2_updated$notes[stage2_updated$proposal == "prop-proposal-wins"]),
  "proposal override"
))

stage2_proposal_audit <- buildStanceProposalStateAudit(
  stage2_updated,
  stage = "2_stance_v5",
  checkpoint = "unit_test_final"
)
stopifnot(all(c(
  "is_one_sided_approve_reject",
  "is_one_sided_documented_exception",
  "one_sided_action"
) %in% colnames(stage2_proposal_audit)))
sequence_audit <- stage2_proposal_audit[stage2_proposal_audit$proposal == "prop-sequence", ]
stopifnot(identical(sequence_audit$stance_state, "approve_only"))
stopifnot(isTRUE(sequence_audit$has_approve))
stopifnot(!isTRUE(sequence_audit$has_reject))
stopifnot(isTRUE(sequence_audit$has_abstain))
stopifnot(isTRUE(sequence_audit$is_one_sided_approve_reject))
stopifnot(isTRUE(sequence_audit$is_one_sided_documented_exception))
stopifnot(identical(sequence_audit$one_sided_action, "kept_documented"))
undoc_sequence_audit <- stage2_proposal_audit[stage2_proposal_audit$proposal == "prop-sequence-undoc", ]
stopifnot(identical(undoc_sequence_audit$stance_state, "neither"))
stopifnot(isTRUE(undoc_sequence_audit$is_one_sided_approve_reject))
stopifnot(!isTRUE(undoc_sequence_audit$is_one_sided_documented_exception))
stopifnot(identical(undoc_sequence_audit$one_sided_action, "normalized_to_other"))

yes_match <- classifyStanceChoiceInstrumented(data.frame(
  choice_text = " Yes ",
  body = "Governance proposal body",
  stringsAsFactors = FALSE
))
stopifnot(identical(yes_match$resolved_stance, "APPROVE"))
stopifnot(identical(yes_match$match_source, "approve_exact"))
stopifnot(identical(yes_match$matched_pattern, "yes"))
stopifnot(identical(yes_match$canonical_pattern, "yes"))
stopifnot(!yes_match$has_conflicting_matches)

empty_body_match <- classifyStanceChoiceInstrumented(data.frame(
  choice_text = "Yes",
  body = "",
  stringsAsFactors = FALSE
))
stopifnot(identical(empty_body_match$resolved_stance, "OTHER"))
stopifnot(identical(empty_body_match$match_source, "empty_body"))

conflict_match <- classifyStanceChoiceInstrumented(data.frame(
  choice_text = "Against support",
  body = "Governance proposal body",
  stringsAsFactors = FALSE
))
stopifnot(identical(conflict_match$resolved_stance, "REJECT"))
stopifnot(identical(conflict_match$match_source, "reject_prefix"))
stopifnot(conflict_match$has_conflicting_matches)
stopifnot(grepl("approve_low_confidence_contains", conflict_match$all_match_sources, fixed = TRUE))

original_nos_begins <- nos_begins
nos_begins <- c(nos_begins, "registry length mismatch sentinel")
registry_length_error <- tryCatch(
  {
    make_stance_pattern_registry()
    FALSE
  },
  error = function(e) grepl("Pattern registry length mismatch", conditionMessage(e), fixed = TRUE)
)
nos_begins <- original_nos_begins
stopifnot(isTRUE(registry_length_error))

stage2_proposal_audit <- buildStanceProposalStateAudit(
  stage2_updated,
  stage = "2_stance_v5",
  checkpoint = "unit_test"
)
proposal_state <- setNames(stage2_proposal_audit$stance_state, stage2_proposal_audit$proposal)
stopifnot(identical(proposal_state[["prop-sequence"]], "approve_only"))
stopifnot(identical(proposal_state[["prop-proposal-wins"]], "reject_only"))
stopifnot(identical(proposal_state[["prop-keep"]], "neither"))
proposal_metadata <- setNames(stage2_proposal_audit$has_existing_stance_metadata, stage2_proposal_audit$proposal)
stopifnot(isTRUE(proposal_metadata[["prop-sequence"]]))
stopifnot(isTRUE(proposal_metadata[["prop-proposal-wins"]]))
stopifnot(isTRUE(proposal_metadata[["prop-space-only"]]))
stopifnot(!isTRUE(proposal_metadata[["prop-keep"]]))

choice_fixture <- data.frame(
  proposal = c("choice-audit", "choice-audit"),
  space = c("space-keep", "space-keep"),
  choice_idx = c(1, 2),
  choice_text = c("Yes", "Against support"),
  body = c("Governance proposal body", "Governance proposal body"),
  stance2 = c("APPROVE", "REJECT"),
  stringsAsFactors = FALSE
)
choice_match_audit <- classifyStanceChoicesInstrumented(choice_fixture)
choice_audit <- buildStanceChoiceMatchAudit(
  choice_fixture,
  choice_match_audit,
  stage = "2_stance_v5",
  checkpoint = "unit_test"
)
stopifnot(nrow(choice_audit) == nrow(choice_fixture))
stopifnot(all(c(
  "resolved_stance",
  "raw_resolved_stance",
  "raw_match_source",
  "raw_matched_pattern",
  "raw_canonical_pattern",
  "raw_all_match_sources",
  "raw_has_conflicting_matches"
) %in% colnames(choice_audit)))
stopifnot(identical(choice_audit$current_stance, choice_fixture$stance2))
stopifnot(identical(choice_audit$resolved_stance, choice_fixture$stance2))
stopifnot(identical(choice_audit$raw_resolved_stance, choice_match_audit$resolved_stance))

overridden_choice_fixture <- data.frame(
  proposal = "choice-audit-overridden",
  space = "space-keep",
  choice_idx = 1,
  choice_text = "Unmapped choice label",
  body = "Governance proposal body",
  stance2 = "APPROVE",
  stringsAsFactors = FALSE
)
overridden_raw_match_audit <- classifyStanceChoicesInstrumented(overridden_choice_fixture)
overridden_choice_audit <- buildStanceChoiceMatchAudit(
  overridden_choice_fixture,
  overridden_raw_match_audit,
  stage = "2_stance_v5",
  checkpoint = "unit_test_override"
)
stopifnot(identical(overridden_raw_match_audit$resolved_stance, "OTHER"))
stopifnot(identical(overridden_choice_audit$resolved_stance, "APPROVE"))
stopifnot(identical(overridden_choice_audit$raw_resolved_stance, "OTHER"))
stopifnot(identical(overridden_choice_audit$raw_match_source, "none"))

unknown_stance_error <- tryCatch(
  {
    applyStage2StanceOverrides(
      data.frame(
        proposal = "prop-unknown",
        space = "space-keep",
        choice_idx = 1,
        choice_text = "Unknown",
        stance2 = "MAYBE",
        stringsAsFactors = FALSE
      ),
      stage = "2_stance_v5"
    )
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(unknown_stance_error))
stopifnot(grepl("Unknown stance labels", unknown_stance_error, fixed = TRUE))

missing_choice_text_error <- tryCatch(
  {
    applyStage2StanceOverrides(
      data.frame(
        proposal = "prop-missing-choice",
        space = "space-keep",
        choice_idx = 1,
        choice_text = NA,
        stance2 = "APPROVE",
        stringsAsFactors = FALSE
      ),
      stage = "2_stance_v5"
    )
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(missing_choice_text_error))
stopifnot(grepl("missing choice_text", missing_choice_text_error, ignore.case = TRUE))

write.csv(
  data.frame(
    id = c("prop-safe", "prop-invalid", "prop-single-option", "prop-flagged"),
    created = c(1, 2, 3, 4),
    flagged = c(0, 0, 0, 1),
    stringsAsFactors = FALSE
  ),
  gzfile(rawDataPath("proposals_0.csv.gz")),
  row.names = FALSE
)

analysis_choices_path <- activeDataPath("analysis_choices.csv")
write.csv(
  data.frame(
    proposal = c(
      "prop-safe", "prop-safe",
      "prop-invalid", "prop-invalid",
      "prop-single-option",
      "prop-flagged", "prop-flagged"
    ),
    type = rep("single-choice", 7),
    choice_idx = c(1, 2, 1, 2, 1, 1, 2),
    tot_votes_2 = c(3, 3, 4, 4, 5, 6, 6),
    N_choices = c(2, 2, 2, 2, 1, 2, 2),
    stringsAsFactors = FALSE
  ),
  analysis_choices_path,
  row.names = FALSE
)

LOAD_ANALYSIS_DATA_ON_SOURCE <- FALSE
source(file.path(PACKAGE_ROOT, "src/r/data/load.R"))
stopifnot(identical(
  normalizePath(analysisDataReleasePath("current"), winslash = "/", mustWork = FALSE),
  normalizePath(
    file.path(MAIN_DIR, "data", "2024", "derived", "active", "df_choices_28_stance_normalized_no_author_vp_llama33.csv"),
    winslash = "/",
    mustWork = FALSE
  )
))
analysis_loaded <- loadAnalysisData(file = analysis_choices_path)
stopifnot(identical(unique(analysis_loaded$df_choices$proposal), c("prop-safe", "prop-invalid")))
stopifnot(identical(unique(df_choices$proposal), c("prop-safe", "prop-invalid")))
stopifnot(!"prop-single-option" %in% analysis_loaded$df_choices$proposal)
stopifnot(!"prop-flagged" %in% analysis_loaded$df_choices$proposal)

before_missing_load <- df_choices
missing_release_error <- tryCatch({loadAnalysisData(file=file.path(tempdir(),"absent-release.csv"));NULL},error=conditionMessage)
stopifnot(grepl("Required input is missing",missing_release_error,fixed=TRUE))
stopifnot(identical(df_choices,before_missing_load))

message("Synthetic stage-pipeline helper tests passed")
