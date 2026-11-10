source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
MAIN_DIR <- PACKAGE_ROOT
R_DIR <- file.path(PACKAGE_ROOT,"src/r")
DATA_DIR <- file.path(tempdir(), "dao-stance-heuristic-comparison")
DERIVED_DIR <- file.path(DATA_DIR, "derived")
DERIVED_ACTIVE_DIR <- file.path(DERIVED_DIR, "active")
DERIVED_LEGACY_DIR <- file.path(DERIVED_DIR, "legacy")
DERIVED_DEBUG_DIR <- file.path(DERIVED_DIR, "debug")
EXCEPTIONS_DIR <- file.path(DATA_DIR, "metadata", "exceptions")

dir.create(DERIVED_ACTIVE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_LEGACY_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_DEBUG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(EXCEPTIONS_DIR, recursive = TRUE, showWarnings = FALSE)

initialize_test_analysis()
Sys.setenv(STANCE_HEURISTIC_COMPARE_SKIP_MAIN = "1")
source(file.path(PACKAGE_ROOT, "src/r/models/stance_comparison.R"))

before <- data.frame(
  proposal = c("p1", "p1", "p2", "p3", "p4"),
  choice_idx = c("1", "2", "1", "1", "1"),
  choice_text = c("Approve", "Reject", "Legacy yes", "Old reject", "Before only"),
  stance = c("APPROVE", "REJECT", "OTHER", "REJECT", "APPROVE"),
  stringsAsFactors = FALSE
)

after <- data.frame(
  proposal = c("p1", "p1", "p2", "p3", "p5"),
  choice_idx = c("1", "2", "1", "1", "1"),
  choice_text = c("Approve", "Reject", "Legacy yes", "Old reject", "After only"),
  stance2 = c("APPROVE", "OTHER", "APPROVE", "OTHER", "REJECT"),
  stringsAsFactors = FALSE
)

totals <- stance_value_counts(before, "stance", "before")
stopifnot(totals$rows[totals$stance == "APPROVE"] == 2)
stopifnot(totals$rows[totals$stance == "REJECT"] == 2)
stopifnot(totals$rows[totals$stance == "OTHER"] == 1)

proposal_counts <- proposal_state_counts(before, "stance", "before")
stopifnot(proposal_counts$proposals[proposal_counts$stance_state == "both"] == 1)
stopifnot(proposal_counts$proposals[proposal_counts$stance_state == "approve_only"] == 1)
stopifnot(proposal_counts$proposals[proposal_counts$stance_state == "reject_only"] == 1)
stopifnot(proposal_counts$proposals[proposal_counts$stance_state == "neither"] == 1)

with_single_option <- data.frame(
  proposal = c("p1", "p1", "p-single", "p-reported-one", "p-reported-one"),
  choice_idx = c("1", "2", "1", "1", "2"),
  N_choices = c("2", "2", "1", "1", "1"),
  stance = c("APPROVE", "REJECT", "APPROVE", "APPROVE", "REJECT"),
  stringsAsFactors = FALSE
)
without_single_option <- drop_single_option_choice_rows(with_single_option)
stopifnot(identical(without_single_option$proposal, c("p1", "p1")))

joined <- compare_choice_labels(before, after, before_stance_col = "stance", after_stance_col = "stance2")
stopifnot(nrow(subset(joined, row_status == "both" & before_stance != after_stance)) == 3)
stopifnot(nrow(subset(joined, row_status == "before_only")) == 1)
stopifnot(nrow(subset(joined, row_status == "after_only")) == 1)

to_other <- top_label_changes(joined, from_stances = c("APPROVE", "REJECT"), to_stances = "OTHER")
stopifnot(nrow(to_other) == 2)
stopifnot(to_other$rows[to_other$choice_text == "Reject"] == 1)

from_other <- top_label_changes(joined, from_stances = "OTHER", to_stances = c("APPROVE", "REJECT"))
stopifnot(nrow(from_other) == 1)
stopifnot(from_other$choice_text == "Legacy yes")

raw_audit <- data.frame(
  raw_resolved_stance = c("OTHER", "APPROVE", "REJECT"),
  raw_match_source = c("approve_low_confidence_contains", "approve_exact", "reject_contains"),
  raw_matched_pattern = c(" support", "yes", "do not support"),
  raw_canonical_pattern = c("support", "yes", "do not support"),
  raw_all_match_sources = c("approve_low_confidence_contains", "approve_exact", "reject_contains|approve_low_confidence_contains"),
  raw_has_conflicting_matches = c("FALSE", "FALSE", "TRUE"),
  choice_text = c("customer support", "yes", "We do not support X"),
  stringsAsFactors = FALSE
)

low_conf <- low_confidence_summary(raw_audit)
stopifnot(nrow(low_conf) == 2)
stopifnot(sum(low_conf$rows) == 2)
stopifnot(setequal(
  low_conf$low_confidence_role,
  c("selected_low_confidence_source", "also_matched_low_confidence")
))

low_conf_counts <- low_confidence_dependency_counts(raw_audit)
selected_dependency <- low_conf_counts[
  low_conf_counts$low_confidence_count_type == "selected_label_depends_on_low_confidence",
]
selected_source <- low_conf_counts[
  low_conf_counts$low_confidence_count_type == "selected_low_confidence_source",
]
also_matched_low_conf <- low_conf_counts[
  low_conf_counts$low_confidence_count_type == "also_matched_low_confidence",
]
any_low_conf <- low_conf_counts[
  low_conf_counts$low_confidence_count_type == "any_low_confidence_match",
]
stopifnot(selected_dependency$selected_label_depends_on_low_confidence)
stopifnot(!selected_source$selected_label_depends_on_low_confidence)
stopifnot(!also_matched_low_conf$selected_label_depends_on_low_confidence)
stopifnot(selected_dependency$rows == 0)
stopifnot(selected_source$rows == 1)
stopifnot(also_matched_low_conf$rows == 1)
stopifnot(any_low_conf$rows == 2)

conflicts <- conflict_summary(raw_audit)
stopifnot(nrow(conflicts) == 1)
stopifnot(conflicts$choice_text == "We do not support X")

examples <- known_example_summary()
stopifnot("Adam S" %in% examples$choice_text)
stopifnot(examples$resolved_stance[examples$choice_text == "Adam S"] == "OTHER")
stopifnot(examples$resolved_stance[examples$choice_text == "do not support"] == "REJECT")

clean_final_audit <- data.frame(
  proposal = c("p1", "p2", "p3"),
  stance_state = c("both", "neither", "neither"),
  is_one_sided_approve_reject = c(FALSE, FALSE, TRUE),
  is_one_sided_documented_exception = c(FALSE, FALSE, FALSE),
  one_sided_action = c("not_applicable", "not_applicable", "normalized_to_other"),
  stringsAsFactors = FALSE
)
stopifnot(assert_no_undocumented_one_sided(clean_final_audit))

dirty_final_audit <- data.frame(
  proposal = "p4",
  stance_state = "approve_only",
  is_one_sided_approve_reject = TRUE,
  is_one_sided_documented_exception = FALSE,
  one_sided_action = "not_applicable",
  stringsAsFactors = FALSE
)
failed <- FALSE
tryCatch(
  assert_no_undocumented_one_sided(dirty_final_audit),
  error = function(e) failed <<- grepl("Final audit still has", conditionMessage(e), fixed = TRUE)
)
stopifnot(failed)
