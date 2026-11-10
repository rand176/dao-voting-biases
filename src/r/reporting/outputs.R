write_outputs <- function(output_dir, attrition, stance_summary, effect_tables, models,
                          coefficient_ladders = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  effect_no_n_align <- paste0(
    "@{}>{\\RaggedRight\\arraybackslash}Xl",
    ">{\\RaggedRight\\arraybackslash}p{0.18\\linewidth}rrr@{}"
  )
  effect_notes <- c(
    "The coefficient column reports logit-scale coefficients with DAO-space clustered standard errors in parentheses.",
    "AMEs are response-scale average marginal effects in percentage points from marginaleffects::avg_slopes; fixed-effect models report clustered uncertainty through the coefficient column.",
    "All regressions include voting-type controls; proposal fixed effects absorb voting type because voting type is constant within proposal."
  )

  write_robustness_variation_key_maintext_table(
    models,
    effect_tables,
    file.path(output_dir, "table_robustness_variation_key.tex")
  )

  unlink(file.path(output_dir, "table_vote_count_share_outcome.tex"))
  unlink(file.path(output_dir, "table_author_voted_only.tex"))
  unlink(file.path(output_dir, "table_participation_control_sensitivity.tex"))
  unlink(file.path(output_dir, "table_primary_ame_inference.tex"))
  unlink(file.path(output_dir, "table_coefficient_ladder_full_sample.tex"))
  unlink(file.path(output_dir, "table_coefficient_ladder_all_vote_types.tex"))
  unlink(file.path(output_dir, "table_coefficient_ladder_no_author_vp.tex"))
  unlink(file.path(output_dir, "table_no_author_vp.tex"))
  unlink(file.path(output_dir, "table_proposal_fe.tex"))
  unlink(file.path(output_dir, "table_low_participation_and_space_exclusion.tex"))

  write_latex_table(
    format_three_bias_summary_ci_table(
      effect_tables$three_bias_summary,
      inference_source = default_three_bias_forest_inference_path(),
      models = models,
      use_short_names = TRUE
    ),
    file.path(output_dir, "table_three_bias_summary.tex"),
    "Three-bias AMEs across shared robustness specifications",
    "tab:robustness-three-bias-summary",
    align = three_bias_summary_ci_align(),
    header_override = three_bias_summary_header(),
    escape = FALSE,
    tabcolsep = "2pt",
    notes_escape = FALSE,
    notes = c(
      "Cells report response-scale average marginal effects (AMEs) in percentage points with Bonferroni/FWER-adjusted 95\\% DAO-space clustered-bootstrap confidence intervals in brackets.",
      "Intervals are adjusted across the 36 shared AMEs shown in this table.",
      "Author-choice AMEs are evaluated among rows from proposals where \\texttt{author\\_voted}=1.",
      "The author-choice column compares author-selected with non-author-selected choices among rows from proposals where the author voted.",
      "The first-vs-second column reports the predicted difference between choice rank 1 and choice rank 2 using the rank-plus-rank-squared position specification.",
      "The approve-stance column should be read as a conditional association after one-sided stance recoding, not as a standalone mechanism test."
    )
  )

  write_latex_table(
    format_effect_table(
      effect_tables$author_choice_shared_specs,
      use_short_names = TRUE,
      include_n = FALSE
    ),
    file.path(output_dir, "table_author_choice_shared_specs.tex"),
    "Author-choice AMEs across shared robustness specifications (shared across the three biases)",
    "tab:robustness-author-choice-shared-specs",
    align = effect_no_n_align,
    notes = c(
      effect_notes,
      "Rows follow the shared robustness specifications used in the three-bias summary table."
    )
  )

  write_author_choice_diagnostics_maintext_table(
    effect_tables$author_choice_diagnostics,
    file.path(output_dir, "table_author_choice_diagnostics.tex")
  )

  write_latex_table(
    format_three_bias_summary_table(
      effect_tables$first_choice_three_bias_summary,
      use_short_names = TRUE
    ),
    file.path(output_dir, "table_first_choice_sensitivity.tex"),
    "First-choice position-control three-bias AMEs across shared robustness specifications",
    "tab:robustness-first-choice-sensitivity",
    align = three_bias_summary_align(),
    header_override = first_choice_three_bias_summary_header(),
    notes = c(
      "Cells report response-scale average marginal effects (AMEs) in percentage points.",
      "This grid mirrors the main three-bias summary in Table \\ref{tab:robustness-three-bias-summary} but replaces the rank-plus-rank-squared position controls with a binary first-choice indicator.",
      "The first-vs-non-first column reports the predicted response-scale difference between first-choice and non-first-choice rows."
    ),
    notes_escape = FALSE
  )

  if (!is.null(coefficient_ladders)) {
    first_choice_ladder_notes <- paste0(
      "Coefficient estimates are on the logit scale with DAO-space clustered standard errors in parentheses. ",
      "Significance markers on coefficients: + p below 0.1, * p below 0.05, ** p below 0.01, *** p below 0.001."
    )
    write_modelsummary_regression_table(
      coefficient_ladders$single_preference_rank_quadratic,
      file.path(output_dir, "table_coefficient_ladder_single_preference.tex"),
      "Nested fractional-logit specifications for the main single-preference sample",
      "tab:robustness-nested-single-preference",
      compact_model_names = TRUE,
      section_break_after_terms = "Choice rank squared",
      section_break_before_terms = "N",
      notes = "Coefficient estimates are on the logit scale with DAO-space clustered standard errors in parentheses. Significance markers: + $p<0.1$, * $p<0.05$, ** $p<0.01$, *** $p<0.001$.",
      notes_escape = FALSE
    )
    write_modelsummary_regression_table(
      coefficient_ladders$first_choice_position,
      file.path(output_dir, "table_coefficient_ladder_first_choice.tex"),
      "First-choice-position sensitivity specifications",
      "tab:robustness-first-choice-specifications",
      compact_model_names = TRUE,
      section_break_after_terms = "First choice",
      section_break_before_terms = "N",
      notes = first_choice_ladder_notes
    )
  }

  write_latex_table(
    format_stance_table(stance_summary),
    file.path(output_dir, "table_stance_sensitivity.tex"),
    "Audit of one-sided stance recoding for the approval-stance sensitivity analysis",
    "tab:robustness-stance-sensitivity",
    align = "@{}>{\\RaggedRight\\arraybackslash}p{0.20\\linewidth}>{\\RaggedRight\\arraybackslash}Xrrr@{}",
    notes = c(
      "Before recoding rows count undocumented one-sided proposals that were normalized to Other.",
      "After recoding rows count the final post-recoding buckets used by the stance sensitivity audit."
    ),
    tabcolsep = "3pt"
  )

  write_latex_table(
    format_space_exclusion_table(models),
    file.path(output_dir, "table_top_space_exclusion.tex"),
    "Top spaces excluded in robustness model",
    "tab:robustness-top-space-exclusion",
    align = "@{}rl@{}",
    tabcolsep = "6pt",
    arraystretch = "1.08"
  )

  write_latex_table(
    format_top_tvl_dao_latex_table(models),
    file.path(output_dir, "table_top_tvl_dao_spaces.tex"),
    "Top 25 matched DAO Snapshot-space ranks used in the Top-TVL robustness subset",
    "tab:robustness-top-tvl-dao-spaces",
    align = "@{}r>{\\RaggedRight\\arraybackslash}p{0.18\\linewidth}>{\\RaggedRight\\arraybackslash}X>{\\RaggedRight\\arraybackslash}p{0.12\\linewidth}rrr@{}",
    header_override = top_tvl_dao_header(),
    notes = top_tvl_dao_notes(models),
    tabcolsep = "2pt",
    arraystretch = "1.05",
    escape = FALSE,
    font_size = "\\scriptsize"
  )

  top100_tvl_sample <- read_top_tvl_sample_dao_rows("top100")
  top100_tvl_align <- "@{}rr>{\\RaggedRight\\arraybackslash}p{0.16\\linewidth}>{\\RaggedRight\\arraybackslash}X>{\\RaggedRight\\arraybackslash}p{0.11\\linewidth}rrr@{}"
  write_latex_table(
    format_top_tvl_sample_dao_latex_table(top100_tvl_sample, rank_min = 1L, rank_max = 25L),
    file.path(output_dir, "table_top100_tvl_dao_spaces_part1.tex"),
    "Top 100 matched DAO Snapshot-space ranks used in the Top-TVL robustness subset (ranks 1-25)",
    "tab:robustness-top100-tvl-dao-spaces-part1",
    align = top100_tvl_align,
    header_override = top_tvl_sample_dao_header(),
    notes = top_tvl_sample_dao_notes("top100", rank_min = 1L, rank_max = 25L),
    tabcolsep = "2pt",
    arraystretch = "1.05",
    escape = FALSE,
    font_size = "\\scriptsize"
  )
  write_latex_table(
    format_top_tvl_sample_dao_latex_table(top100_tvl_sample, rank_min = 26L, rank_max = 100L),
    file.path(output_dir, "table_top100_tvl_dao_spaces_part2.tex"),
    "Top 100 matched DAO Snapshot-space ranks used in the Top-TVL robustness subset, continued (ranks 26-100)",
    "tab:robustness-top100-tvl-dao-spaces-part2",
    align = top100_tvl_align,
    header_override = top_tvl_sample_dao_header(),
    notes = top_tvl_sample_dao_notes("top100", rank_min = 26L, rank_max = 100L, continuation = TRUE),
    tabcolsep = "2pt",
    arraystretch = "1.05",
    escape = FALSE,
    font_size = "\\scriptsize"
  )

  write_three_bias_forest_plot(
    default_three_bias_forest_inference_path(),
    three_bias_summary = effect_tables$three_bias_summary,
    models = models
  )
  write_three_bias_forest_top100_plot(
    default_three_bias_forest_inference_path(),
    three_bias_summary = effect_tables$three_bias_summary,
    models = models
  )

}
