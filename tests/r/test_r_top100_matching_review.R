source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
script_file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
test_dir <- if (length(script_file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file_arg[[1]]), winslash = "/", mustWork = FALSE))
} else {
  getwd()
}
project_root <- PACKAGE_ROOT

source(file.path(project_root, "src/r/data/model_paths.R"), local = FALSE)
source(file.path(project_root, "src/r/data/model_data.R"), local = FALSE)
source(file.path(project_root, "src/r/preparation/tvl.R"), local = FALSE)

test_default_top100_match_summary_path <- function() {
  temp_dir <- tempfile("top100-summary-path-")
  on.exit(unlink(temp_dir, recursive = TRUE))
  expected <- file.path(
    temp_dir,
    "metadata",
    "analysis_data_provenance",
    "defillama_top100_match_summary.csv"
  )
  old_path <- Sys.getenv("ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE", unset = NA_character_)
  on.exit({
    if (is.na(old_path)) Sys.unsetenv("ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE") else Sys.setenv(ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE = old_path)
  }, add = TRUE)

  Sys.unsetenv("ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE")
  stopifnot(identical(default_top100_match_summary_path(temp_dir), expected))

  override <- file.path(temp_dir, "custom-summary.csv")
  Sys.setenv(ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE = override)
  stopifnot(identical(default_top100_match_summary_path(temp_dir), override))
}

test_top100_review_columns_are_stable <- function() {
  expected <- c(
    "review_decision",
    "reviewed_snapshot_space",
    "release_proposals",
    "release_choices",
    "evidence_url_or_note",
    "reviewer",
    "review_date",
    "review_note"
  )
  stopifnot(identical(top100_review_columns(), expected))
}

test_default_top100_match_summary_path()
test_top100_review_columns_are_stable()

test_top100_match_summary_table_contract <- function() {
  slugs <- c("alpha", "beta", "gamma", "delta", paste0("extra-", 5:100))
  protocols <- data.table::data.table(
    protocol_rank = 1:100,
    protocol_slug = slugs,
    protocol_name = c("Alpha", "Beta", "Gamma", "Delta", paste("Extra", 5:100))
  )
  diagnostics <- data.table::data.table(
    protocol_rank = c(1L, 2L),
    protocol_slug = c("alpha", "beta"),
    protocol_name = c("Alpha", "Beta"),
    candidate_snapshot_space = c("alpha.eth", "beta.eth"),
    match_source = c("defillama_governance_id", "fuzzy_diagnostic"),
    match_status = c("accepted", "fuzzy_diagnostic"),
    proposals = c(3L, NA_integer_),
    choices = c(9L, NA_integer_)
  )
  existing <- data.table::data.table(
    protocol_rank = 4L,
    protocol_slug = "delta",
    review_decision = "no_snapshot_governance",
    reviewed_snapshot_space = "",
    release_proposals = NA_integer_,
    release_choices = NA_integer_,
    evidence_url_or_note = "manual review note",
    reviewer = "reviewer",
    review_date = "2026-06-12",
    review_note = "defunct product"
  )

  result <- top100_match_summary_table(protocols, diagnostics, existing_summary = existing)
  stopifnot(nrow(result) == 100L)
  stopifnot(identical(result$status[1:4], c("matched", "fuzzy_only", "unmatched", "unmatched")))
  stopifnot(identical(result$matched_spaces[[1]], "alpha.eth"))
  stopifnot(identical(result$fuzzy_candidates[[2]], "beta.eth"))
  stopifnot(identical(result$review_decision[1:4], c("already_matched", "pending_review", "pending_review", "no_snapshot_governance")))
  stopifnot(identical(result$reviewer[[4]], "reviewer"))

  stale_existing <- data.table::data.table(
    protocol_rank = 3L,
    protocol_slug = "gamma",
    review_decision = "already_matched"
  )
  stale_result <- top100_match_summary_table(protocols, diagnostics, existing_summary = stale_existing)
  stopifnot(identical(stale_result$review_decision[[3]], "pending_review"))

  bad_existing <- data.table::data.table(
    protocol_rank = 3L,
    protocol_slug = "gamma",
    review_decision = "maybe"
  )
  err <- tryCatch(
    top100_match_summary_table(protocols, diagnostics, existing_summary = bad_existing),
    error = function(e) e
  )
  stopifnot(inherits(err, "error"))
  stopifnot(grepl("Invalid Top 100 review_decision", conditionMessage(err), fixed = TRUE))
}

test_top100_match_summary_table_contract()

test_top100_match_summary_table_marks_override_accepts <- function() {
  protocols <- data.table::data.table(
    protocol_rank = 1:100,
    protocol_slug = c("alpha", paste0("extra-", 2:100)),
    protocol_name = c("Alpha", paste("Extra", 2:100))
  )
  diagnostics <- data.table::data.table(
    protocol_rank = 1L,
    protocol_slug = "alpha",
    protocol_name = "Alpha",
    candidate_snapshot_space = "alpha.eth",
    match_source = "override_accept",
    match_status = "accepted",
    override_rationale = "Manual review confirmed Alpha governance.",
    override_evidence_url_or_note = "https://snapshot.org/#/alpha.eth",
    override_reviewer = "Reviewer",
    override_review_date = "2026-06-12",
    proposals = 2L,
    choices = 5L
  )

  result <- top100_match_summary_table(protocols, diagnostics)
  stopifnot(identical(result$status[[1]], "matched"))
  stopifnot(identical(result$review_decision[[1]], "accept_override"))
  stopifnot(identical(result$reviewed_snapshot_space[[1]], "alpha.eth"))
  stopifnot(identical(result$release_proposals[[1]], 2L))
  stopifnot(identical(result$release_choices[[1]], 5L))
  stopifnot(identical(result$evidence_url_or_note[[1]], "https://snapshot.org/#/alpha.eth"))
  stopifnot(identical(result$reviewer[[1]], "Reviewer"))
  stopifnot(identical(result$review_date[[1]], "2026-06-12"))
  stopifnot(identical(result$review_note[[1]], "Manual review confirmed Alpha governance."))
}

test_top100_match_summary_table_marks_override_accepts()

test_top100_match_summary_table_live_override_replaces_stale_summary <- function() {
  protocols <- data.table::data.table(
    protocol_rank = 1:100,
    protocol_slug = c("alpha", paste0("extra-", 2:100)),
    protocol_name = c("Alpha", paste("Extra", 2:100))
  )
  diagnostics <- data.table::data.table(
    protocol_rank = 1L,
    protocol_slug = "alpha",
    protocol_name = "Alpha",
    candidate_snapshot_space = "alpha.eth",
    match_source = "override_accept",
    match_status = "accepted",
    override_rationale = "Current live override.",
    override_evidence_url_or_note = "https://snapshot.org/#/alpha.eth",
    override_reviewer = "Reviewer",
    override_review_date = "2026-06-12",
    proposals = 2L,
    choices = 5L
  )
  existing <- data.table::data.table(
    protocol_rank = 1L,
    protocol_slug = "alpha",
    review_decision = "accept_override",
    reviewed_snapshot_space = "old-alpha.eth",
    release_proposals = 1L,
    release_choices = 1L,
    evidence_url_or_note = "stale evidence",
    reviewer = "Old Reviewer",
    review_date = "2026-06-11",
    review_note = "Stale review."
  )

  result <- top100_match_summary_table(protocols, diagnostics, existing_summary = existing)
  stopifnot(identical(result$review_decision[[1]], "accept_override"))
  stopifnot(identical(result$reviewed_snapshot_space[[1]], "alpha.eth"))
  stopifnot(identical(result$release_proposals[[1]], 2L))
  stopifnot(identical(result$evidence_url_or_note[[1]], "https://snapshot.org/#/alpha.eth"))
  stopifnot(identical(result$reviewer[[1]], "Reviewer"))
  stopifnot(identical(result$review_note[[1]], "Current live override."))
}

test_top100_match_summary_table_live_override_replaces_stale_summary()

test_top100_match_summary_table_seeds_pending_review_candidates <- function() {
  protocols <- data.table::data.table(
    protocol_rank = 1:100,
    protocol_slug = c("alpha", "beta", "gamma", paste0("extra-", 4:100)),
    protocol_name = c("Alpha", "Beta", "Gamma", paste("Extra", 4:100))
  )
  diagnostics <- data.table::data.table(
    protocol_rank = 2L,
    protocol_slug = "beta",
    protocol_name = "Beta",
    candidate_snapshot_space = "beta.eth",
    match_source = "fuzzy_diagnostic",
    match_status = "fuzzy_diagnostic",
    proposals = NA_integer_,
    choices = NA_integer_
  )
  review_seed <- data.table::data.table(
    protocol_slug = "gamma",
    reviewed_snapshot_space = "gamma.eth",
    evidence_url_or_note = "seed evidence",
    review_note = "seed note"
  )
  result <- top100_match_summary_table(protocols, diagnostics, review_seed = review_seed)

  stopifnot(identical(result$review_decision[1:3], c("pending_review", "pending_review", "pending_review")))
  stopifnot(identical(result$reviewed_snapshot_space[[2]], "beta.eth"))
  stopifnot(identical(result$reviewed_snapshot_space[[3]], "gamma.eth"))
  stopifnot(identical(result$evidence_url_or_note[[3]], "seed evidence"))
  stopifnot(identical(result$review_note[[3]], "seed note"))

  existing <- data.table::data.table(
    protocol_rank = 3L,
    protocol_slug = "gamma",
    review_decision = "no_snapshot_governance",
    reviewed_snapshot_space = "",
    evidence_url_or_note = "manual evidence",
    reviewer = "reviewer",
    review_date = "2026-06-12"
  )
  preserved <- top100_match_summary_table(
    protocols,
    diagnostics,
    existing_summary = existing,
    review_seed = review_seed
  )
  stopifnot(identical(preserved$review_decision[[3]], "no_snapshot_governance"))
  stopifnot(identical(preserved$evidence_url_or_note[[3]], "manual evidence"))
}

test_top100_match_summary_table_seeds_pending_review_candidates()

test_top100_review_seed_from_markdown_stops_at_next_heading <- function() {
  temp_dir <- tempfile("top100-review-md-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))
  path <- file.path(temp_dir, "review.md")
  writeLines(c(
    "### whiteheart (rank 100)",
    "- **Name:** Whiteheart",
    "- **Research:** No known Snapshot space. **No override suggested.**",
    "",
    "## Draft overrides for reviewer sign-off",
    "### High-confidence",
    "- `convex-finance` -> `accept cvx.eth`"
  ), path)

  seed <- top100_review_seed_from_markdown(path)
  stopifnot(nrow(seed) == 1L)
  stopifnot(identical(seed$protocol_slug[[1]], "whiteheart"))
  stopifnot(identical(seed$reviewed_snapshot_space[[1]], ""))
  stopifnot(!grepl("Draft overrides", seed$review_note[[1]], fixed = TRUE))
  stopifnot(!grepl("cvx.eth", seed$review_note[[1]], fixed = TRUE))
}

test_top100_review_seed_from_markdown_stops_at_next_heading()

test_write_top100_match_summary_preserves_existing_review <- function() {
  temp_dir <- tempfile("top100-summary-writer-")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  protocols <- data.table::data.table(
    protocol_rank = 1:100,
    protocol_slug = paste0("protocol-", 1:100),
    protocol_name = paste("Protocol", 1:100)
  )
  diagnostics <- data.table::data.table(
    protocol_rank = 1L,
    protocol_slug = "protocol-1",
    protocol_name = "Protocol 1",
    candidate_snapshot_space = "protocol-1.eth",
    match_source = "defillama_governance_id",
    match_status = "accepted",
    proposals = 2L,
    choices = 4L
  )
  path <- file.path(temp_dir, "defillama_top100_match_summary.csv")

  first <- write_top100_match_summary(protocols, diagnostics, path = path)
  stopifnot(file.exists(first))

  edited <- utils::read.csv(path, stringsAsFactors = FALSE)
  edited$review_decision[edited$protocol_rank == 100] <- "no_snapshot_governance"
  edited$evidence_url_or_note[edited$protocol_rank == 100] <- "manual note"
  edited$reviewer[edited$protocol_rank == 100] <- "reviewer"
  edited$review_date[edited$protocol_rank == 100] <- "2026-06-12"
  utils::write.csv(edited, path, row.names = FALSE)

  write_top100_match_summary(protocols, diagnostics, path = path, existing_summary_path = path)
  reread <- utils::read.csv(path, stringsAsFactors = FALSE)
  stopifnot(identical(reread$review_decision[reread$protocol_rank == 100], "no_snapshot_governance"))
  stopifnot(identical(reread$evidence_url_or_note[reread$protocol_rank == 100], "manual note"))
}

test_write_top100_match_summary_preserves_existing_review()

write_gzip_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- gzfile(path, open = "wt")
  on.exit(close(con), add = TRUE)
  utils::write.csv(df, con, row.names = FALSE)
}

test_write_top100_matching_review_artifacts_without_memberships <- function() {
  temp_dir <- tempfile("top100-matching-artifacts-")
  provenance_dir <- file.path(temp_dir, "metadata", "analysis_data_provenance")
  snapshot_dir <- file.path(temp_dir, "data", "2024", "raw")
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(snapshot_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  protocol_file <- file.path(provenance_dir, "defillama_protocol_ethereum_tvl_top100.csv")
  overrides_file <- file.path(provenance_dir, "defillama_snapshot_overrides.csv")
  snapshot_file <- file.path(snapshot_dir, "spaces_0.csv.gz")
  diagnostics_file <- file.path(provenance_dir, "defillama_top_tvl_snapshot_match_diagnostics.csv")
  summary_file <- file.path(provenance_dir, "defillama_top100_match_summary.csv")
  membership_file <- file.path(provenance_dir, "defillama_top_tvl_snapshot_sample_memberships.csv")

  ranks <- 1:100
  slugs <- c("alpha", "beta", paste0("extra-", 3:100))
  utils::write.csv(
    data.frame(
      protocol_rank = ranks,
      protocol_slug = slugs,
      protocol_name = c("Alpha", "Beta", paste("Extra", 3:100)),
      category = "Dexes",
      parent_protocol = "",
      snapshot_governance_ids = c("alpha.eth", rep("", 99)),
      parent_snapshot_governance_ids = "",
      median_ethereum_tvl_usd = rev(ranks) * 1000,
      stringsAsFactors = FALSE
    ),
    protocol_file,
    row.names = FALSE
  )
  utils::write.csv(empty_defillama_snapshot_overrides(), overrides_file, row.names = FALSE)
  write_gzip_csv(
    data.frame(id = c("alpha.eth", "beta.eth"), deleted = 0, flagged = 0, stringsAsFactors = FALSE),
    snapshot_file
  )

  model_df <- data.frame(
    proposal = c("p1", "p1"),
    space = c("alpha.eth", "alpha.eth"),
    stringsAsFactors = FALSE
  )

  result <- write_top100_matching_review_artifacts(
    model_df,
    path = protocol_file,
    overrides_path = overrides_file,
    snapshot_spaces_path = snapshot_file,
    diagnostics_path = diagnostics_file,
    top100_summary_path = summary_file,
    sample_memberships_path = NA_character_,
    project_root = temp_dir
  )

  summary <- utils::read.csv(summary_file, stringsAsFactors = FALSE)
  diagnostics <- utils::read.csv(diagnostics_file, stringsAsFactors = FALSE)
  stopifnot(file.exists(diagnostics_file))
  stopifnot(file.exists(summary_file))
  stopifnot(!file.exists(membership_file))
  stopifnot(nrow(summary) == 100L)
  stopifnot(identical(summary$status[1:3], c("matched", "fuzzy_only", "unmatched")))
  stopifnot(any(diagnostics$candidate_snapshot_space == "alpha.eth"))
  stopifnot(identical(result$paths$top100_summary_path, summary_file))
}

test_write_top100_matching_review_artifacts_without_memberships()
