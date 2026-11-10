source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
MAIN_DIR <- PACKAGE_ROOT

source(file.path(MAIN_DIR, "src/r/preparation/labels.R"))

df_choices <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b", "prop-c"),
  choice_idx = c(1, 2, 1, 1),
  stringsAsFactors = FALSE
)
df_choices <- stage5EnsurePropChid(df_choices, "df_choices")

llama33_path <- tempfile(fileext = ".csv")
writeLines(
  c(
    "\"effect a\",\"['APPROVE', 'REJECT']\",\"APPROVE\",\"PROP-A\",0",
    "\"N/A\",\"['OTHER']\",\"N/A\",\"prop-b\",10"
  ),
  llama33_path
)
llama33 <- readStage5Llama33ChoiceLabels(llama33_path)

stopifnot(nrow(llama33) == 3)
stopifnot(setequal(
  c("effect_llama33", "suggested_stance_llama33", "stance_llama33", "source_file_llama33"),
  intersect(
    c("effect_llama33", "suggested_stance_llama33", "stance_llama33", "source_file_llama33"),
    colnames(llama33)
  )
))
stopifnot(identical(llama33$stance_llama33[llama33$prop_chid == "prop-a1"], "APPROVE"))
stopifnot(is.na(llama33$effect_llama33[llama33$prop_chid == "prop-b1"]))

llama33_duplicate_old_path <- tempfile(fileext = ".csv")
llama33_duplicate_new_path <- tempfile(fileext = ".csv")
writeLines(
  "\"old effect\",\"['APPROVE']\",\"APPROVE\",\"prop-dup\",0",
  llama33_duplicate_old_path
)
writeLines(
  "\"new effect\",\"['REJECT']\",\"REJECT\",\"PROP-DUP\",0",
  llama33_duplicate_new_path
)

llama33_conflict_error <- tryCatch(
  {
    readStage5Llama33ChoiceLabels(c(llama33_duplicate_old_path, llama33_duplicate_new_path))
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(llama33_conflict_error))
stopifnot(grepl("Conflicting duplicate Llama 3.3 labels", llama33_conflict_error, fixed = TRUE))

llama33_conflict_audit_path <- tempfile(fileext = ".csv")
llama33_conflict_warnings <- character()
llama33_conflict <- withCallingHandlers(
  readStage5Llama33ChoiceLabels(
    c(llama33_duplicate_old_path, llama33_duplicate_new_path),
    conflict_audit_path = llama33_conflict_audit_path
  ),
  warning = function(warn) {
    llama33_conflict_warnings <<- c(llama33_conflict_warnings, warn$message)
    invokeRestart("muffleWarning")
  }
)
stopifnot(any(grepl("Conflicting duplicate Llama 3.3 labels", llama33_conflict_warnings, fixed = TRUE)))
stopifnot(file.exists(llama33_conflict_audit_path))
llama33_conflict_audit <- read.csv(llama33_conflict_audit_path, stringsAsFactors = FALSE)
stopifnot(nrow(llama33_conflict_audit) == 2)
stopifnot(all(c(
  "proposal",
  "choice_idx",
  "prop_chid",
  "effect_llama33",
  "suggested_stance_llama33",
  "stance_llama33",
  "source_file_llama33",
  "source_rank_llama33",
  "conflict_columns_llama33"
) %in% colnames(llama33_conflict_audit)))
stopifnot(nrow(llama33_conflict) == 1)
stopifnot(identical(llama33_conflict$stance_llama33[[1]], "REJECT"))
stopifnot(identical(llama33_conflict$source_file_llama33[[1]], basename(llama33_duplicate_new_path)))
stopifnot(isTRUE(llama33_conflict$llama33_label_conflict[[1]]))
stopifnot(grepl("stance_llama33", llama33_conflict$conflict_columns_llama33[[1]], fixed = TRUE))

llama33_duplicate_same_new_path <- tempfile(fileext = ".csv")
writeLines(
  "\"old effect\",\"['APPROVE']\",\"APPROVE\",\"PROP-DUP\",0",
  llama33_duplicate_same_new_path
)
llama33_nonconflict <- readStage5Llama33ChoiceLabels(
  c(llama33_duplicate_old_path, llama33_duplicate_same_new_path)
)
stopifnot(nrow(llama33_nonconflict) == 1)
stopifnot(identical(llama33_nonconflict$source_file_llama33[[1]], basename(llama33_duplicate_same_new_path)))
stopifnot(isFALSE(llama33_nonconflict$llama33_label_conflict[[1]]))

oai_labels <- data.frame(
  prop_chid = c("prop-a1", "prop-b1"),
  effect_oai = c("effect a", "effect b"),
  suggested_stance_oai = c("APPROVE", "REJECT"),
  stance_oai = c("APPROVE", "REJECT"),
  stringsAsFactors = FALSE
)

joined <- stage5LeftJoinChoiceLabels(df_choices, oai_labels, "OAI")
stopifnot(nrow(joined) == nrow(df_choices))
stopifnot(is.na(joined$stance_oai[joined$prop_chid == "prop-a2"]))

duplicate_error <- tryCatch(
  {
    stage5LeftJoinChoiceLabels(
      df_choices,
      data.frame(prop_chid = c("prop-a1", "prop-a1"), stance_oai = c("APPROVE", "REJECT")),
      "duplicate OAI"
    )
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(duplicate_error))
stopifnot(grepl("Duplicate key rows found", duplicate_error, fixed = TRUE))

human_path <- tempfile(fileext = ".csv")
human_df <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-c"),
  choice_idx = c(1, 2, 1),
  prop_chid = c("prop-a1", "prop-a2", "prop-c1"),
  stance_hl = c("APPROVE", "REJECT", NA),
  suggested_stance_hl = c("APPROVE", "REJECT", "ABSTAIN"),
  issue_hl = c(NA, "ambiguous", NA),
  n_evaluations = c(2, 2, 1),
  n_non_low_effort_evaluations = c(2, 1, 1),
  n_low_effort_evaluations = c(0, 1, 0),
  n_approve = c(2, 0, 0),
  n_reject = c(0, 2, 0),
  n_abstain = c(0, 0, 1),
  n_other = c(0, 0, 0),
  n_effective_approve = c(2, 0, 0),
  n_effective_reject = c(0, 1, 0),
  n_effective_abstain = c(0, 0, 1),
  n_effective_other = c(0, 0, 0),
  low_effort_only = c(FALSE, FALSE, FALSE),
  label_conflict = c(FALSE, FALSE, FALSE),
  raw_label_conflict = c(FALSE, TRUE, FALSE),
  has_evaluator_label_conflict = c(FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)
write.csv(human_df, human_path, row.names = FALSE)

human_labels <- readStage5HumanChoiceLabels(human_path)
stopifnot(setequal(stage5HumanChoiceColumns(), colnames(human_labels)))
human_labels_m <- human_labels[, stage5HumanChoiceJoinColumns(), drop = FALSE]

missing_human_path <- tempfile(fileext = ".csv")
missing_human_warnings <- character()
missing_human_labels <- withCallingHandlers(
  readStage5HumanChoiceLabels(missing_human_path),
  warning = function(warn) {
    missing_human_warnings <<- c(missing_human_warnings, warn$message)
    invokeRestart("muffleWarning")
  }
)
stopifnot(any(grepl("Required human-label choice output is missing", missing_human_warnings, fixed = TRUE)))
stopifnot(any(grepl("python run.py prepare --stage human-labels --data-root INPUT", missing_human_warnings, fixed = TRUE)))
stopifnot(nrow(missing_human_labels) == 0)
stopifnot(setequal(stage5HumanChoiceColumns(), colnames(missing_human_labels)))

missing_human_labels_m <- missing_human_labels[, stage5HumanChoiceJoinColumns(), drop = FALSE]
missing_human_joined <- stage5LeftJoinChoiceLabels(df_choices, missing_human_labels_m, "missing human labels")
stopifnot(nrow(missing_human_joined) == nrow(df_choices))
stopifnot(all(is.na(missing_human_joined$stance_hl)))

missing_human_coverage <- buildStage5LabelCoverageAudit(
  missing_human_joined,
  list(human = c("stance_hl", "suggested_stance_hl"))
)
stopifnot(missing_human_coverage$labeled_rows == 0)
stopifnot(missing_human_coverage$labeled_proposals == 0)
stopifnot(missing_human_coverage$complete_proposals == 0)
stopifnot(missing_human_coverage$partial_proposals == 0)
stopifnot(missing_human_coverage$missing_proposals == length(unique(df_choices$proposal)))

joined <- stage5LeftJoinChoiceLabels(joined, human_labels_m, "human labels")
stopifnot(nrow(joined) == nrow(df_choices))
stopifnot(is.na(joined$stance_hl[joined$prop_chid == "prop-b1"]))
stopifnot(identical(joined$raw_label_conflict[joined$prop_chid == "prop-a2"], TRUE))

stage4_choices <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b", "prop-c"),
  choice_idx = c(1, 2, 1, 1),
  base_metric = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)
stage4_choices <- stage5EnsurePropChid(stage4_choices, "stage4_choices")

filtered_stage5_source <- data.frame(
  proposal = c("prop-a", "prop-a", "prop-b"),
  choice_idx = c(1, 2, 1),
  prop_chid = c("prop-a1", "prop-a2", "prop-b1"),
  choice_text_normalized = c("keep-a1", "keep-a2", "keep-b1"),
  stance_oai = c("APPROVE", "REJECT", "OTHER"),
  valid_GPT4o_v1 = c(1, 1, 0),
  stringsAsFactors = FALSE
)

canonical_stage5 <- buildCanonicalStage5ChoiceTable(stage4_choices, filtered_stage5_source)
stopifnot(nrow(canonical_stage5) == nrow(stage4_choices))
stopifnot(identical(canonical_stage5$prop_chid, stage4_choices$prop_chid))
stopifnot(!("valid_GPT4o_v1" %in% colnames(canonical_stage5)))
stopifnot(identical(
  canonical_stage5$proposal_in_gpt4o_review_universe,
  c(1L, 1L, 1L, 0L)
))
stopifnot(identical(
  canonical_stage5$proposal_in_gpt4o_analysis_scope,
  c(1L, 1L, 0L, 0L)
))
stopifnot(is.na(canonical_stage5$stance_oai[canonical_stage5$prop_chid == "prop-c1"]))

explicit_flags_stage5 <- buildCanonicalStage5ChoiceTable(
  stage4_choices,
  filtered_stage5_source[, setdiff(colnames(filtered_stage5_source), "valid_GPT4o_v1"), drop = FALSE],
  proposal_coverage_flags = list(
    proposal_in_gpt4o_review_universe = c("prop-a", "prop-b"),
    proposal_in_gpt4o_analysis_scope = "prop-a"
  )
)
stopifnot(identical(
  explicit_flags_stage5$proposal_in_gpt4o_review_universe,
  canonical_stage5$proposal_in_gpt4o_review_universe
))
stopifnot(identical(
  explicit_flags_stage5$proposal_in_gpt4o_analysis_scope,
  canonical_stage5$proposal_in_gpt4o_analysis_scope
))

invalid_flag_error <- tryCatch(
  {
    stage5NormalizeProposalCoverageFlags(list(
      proposal_in_gpt4o_review_universe = "prop-a",
      proposal_in_gpt4o_analysis_scope = "prop-b"
    ))
    NULL
  },
  error = function(err) err$message
)
stopifnot(is.character(invalid_flag_error))
stopifnot(grepl(
  "proposal_in_gpt4o_analysis_scope must be a subset",
  invalid_flag_error,
  fixed = TRUE
))

coverage <- buildStage5LabelCoverageAudit(
  joined,
  list(
    oai = c("stance_oai", "suggested_stance_oai", "effect_oai"),
    human = c("stance_hl", "suggested_stance_hl")
  )
)

oai_coverage <- coverage[coverage$label_source == "oai", ]
human_coverage <- coverage[coverage$label_source == "human", ]

stopifnot(oai_coverage$labeled_rows == 2)
stopifnot(oai_coverage$labeled_proposals == 2)
stopifnot(oai_coverage$complete_proposals == 1)
stopifnot(oai_coverage$partial_proposals == 1)
stopifnot(oai_coverage$missing_proposals == 1)

stopifnot(human_coverage$labeled_rows == 3)
stopifnot(human_coverage$labeled_proposals == 2)
stopifnot(human_coverage$complete_proposals == 2)
stopifnot(human_coverage$partial_proposals == 0)
stopifnot(human_coverage$missing_proposals == 1)

plot_model_labels <- stage5PlotModelLabels(
  c("stance_oai", "stance_llama", "stance_chatgpt", "stance_llama33", "stance_hl", "choice_share")
)
stopifnot(identical(
  plot_model_labels,
  c("OAI", "Llama", "ChatGPT", "Llama 3.3", "Human labels", "Heuristic")
))
