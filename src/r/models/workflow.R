################################################################################
## Robustness and mechanism checks for author-choice bias
################################################################################

robustness_script_dir <- file.path(PACKAGE_ROOT,"src/r")
source(file.path(PACKAGE_ROOT, "src/r/preparation/tvl.R"), local = TRUE)

source(file.path(PACKAGE_ROOT, "src/r/data/model_paths.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/data/model_data.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/models/samples.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/models/fitting.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/models/effects.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/inference/bootstrap_helpers.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/reporting/forest.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/reporting/tvl_tables.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/reporting/tables.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/reporting/outputs.R"), local = TRUE)

################################################################################
## Entrypoint
################################################################################

run_robustness_bias_mechanisms <- function(
    input_path = default_input_path(),
    output_dir = default_output_dir(),
    stance_mode = Sys.getenv("ROBUSTNESS_STANCE_MODE", unset = "block")) {
  require_packages()

  if (!stance_mode %in% c("block", "exclude_undocumented_one_sided")) {
    stop(
      "Unknown ROBUSTNESS_STANCE_MODE: ", stance_mode,
      ". Use 'block' or 'exclude_undocumented_one_sided'.",
      call. = FALSE
    )
  }

  message("Loading active choice-level data: ", input_path)
  raw_df <- load_choice_data(input_path)
  model_df <- prepare_model_dataframe(raw_df)
  documented_stance_proposals <- load_documented_stance_proposals()
  pre_normalization_stance_audit <- build_stance_audit(model_df, documented_stance_proposals)
  stance_summary_source_audit <- pre_normalization_stance_audit
  if (!any(pre_normalization_stance_audit$undocumented_one_sided)) {
    promoted_candidate_audit <- load_promoted_candidate_normalization_audit(
      input_path = input_path,
      project_root = find_project_root()
    )
    if (!is.null(promoted_candidate_audit) && nrow(promoted_candidate_audit) > 0) {
      stance_summary_source_audit <- promoted_candidate_audit
      message(
        "Using promoted candidate normalization audit to recover pre-recode ",
        "undocumented one-sided counts for the current release table."
      )
    }
  }
  model_df <- normalize_undocumented_one_sided_stance(model_df, documented_stance_proposals)
  normalized_rows <- attr(model_df, "normalized_undocumented_one_sided_rows")
  normalized_props <- attr(model_df, "normalized_undocumented_one_sided_proposals")
  if (!is.null(normalized_rows) && normalized_rows > 0) {
    message(
      "Normalized ", normalized_rows, " row(s) across ", length(normalized_props),
      " undocumented one-sided stance proposal(s) to OTHER."
    )
  }
  invisible(check_expected_counts(model_df))

  stance_audit <- build_stance_audit(model_df, documented_stance_proposals)
  stance_summary <- build_stance_summary(
    stance_audit,
    model_df,
    pre_normalization_stance_audit = stance_summary_source_audit
  )

  upstream_has_undocumented <- if ("undocumented_one_sided" %in% names(stance_summary_source_audit)) {
    any(stance_summary_source_audit$undocumented_one_sided)
  } else if (all(c("action", "original_stance_state") %in% names(stance_summary_source_audit))) {
    any(
      stance_summary_source_audit$action == "normalized_to_other" &
        stance_summary_source_audit$original_stance_state %in% c("approve_only", "reject_only")
    )
  } else {
    FALSE
  }

  if (identical(stance_mode, "block") && upstream_has_undocumented) {
    message(
      "Upstream pre-normalization audit found undocumented one-sided stance proposal(s); ",
      "current release normalizes them to OTHER before fitting."
    )
  }

  message("Fitting robustness models")
  models <- fit_model_set(model_df)
  coefficient_ladders <- fit_coefficient_ladders(model_df)
  effect_tables <- build_effect_tables(models)
  attrition <- build_attrition_table(
    model_df,
    stance_audit,
    stance_mode,
    models
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write_latex_table(
    format_attrition_table(attrition),
    file.path(output_dir, "table_model_attrition.tex"),
    "Robustness model sample attrition",
    "tab:robustness-model-attrition",
    align = "@{}>{\\RaggedRight\\arraybackslash}p{0.24\\linewidth}rrr>{\\RaggedRight\\arraybackslash}X@{}",
    notes = "Retained rows are after fixed-effect estimator exclusions where applicable."
  )

  write_outputs(output_dir, attrition, stance_summary, effect_tables, models, coefficient_ladders)

  invisible(list(
    model_df = model_df,
    attrition = attrition,
    stance_audit = stance_audit,
    pre_normalization_stance_audit = pre_normalization_stance_audit,
    models = models,
    coefficient_ladders = coefficient_ladders,
    effect_tables = effect_tables,
    output_dir = output_dir
  ))
}
