# Extracted analytical implementation; execution is explicit.
figure_approve_stance_agreement <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    latex_counts_path <- file.path(
      OUTPUT_ROOT,
      "metadata",
      "analysis_data_provenance",
      paste0("latex_counts_",ANALYSIS_DATA_VERSION,".csv")
    )
    suggested_stance_path <- file.path(
      OUTPUT_ROOT,
      "metadata",
      "analysis_data_provenance",
      "suggested_stance_diagnostics__latest.csv"
    )

    latex_counts_path <- requireExistingInputPath(latex_counts_path)
    suggested_stance_path <- requireExistingInputPath(suggested_stance_path)
    if (!file.exists(latex_counts_path)) {
      stop("Missing latex count macro data: ", latex_counts_path)
    }
    if (!file.exists(suggested_stance_path)) {
      stop("Missing suggested-stance diagnostics: ", suggested_stance_path)
    }

    latex_counts <- read.csv(latex_counts_path, stringsAsFactors = FALSE)
    suggested_stance_metrics <- read.csv(suggested_stance_path, stringsAsFactors = FALSE)

    macro_value <- function(name) {
      row <- latex_counts[latex_counts$macro == name, , drop = FALSE]
      if (nrow(row) != 1) {
        stop("Expected exactly one macro row for ", name, ", found ", nrow(row))
      }
      row$value[[1]]
    }

    macro_number <- function(name) {
      value <- suppressWarnings(as.numeric(macro_value(name)))
      if (is.na(value)) {
        stop("Expected numeric macro value for ", name)
      }
      value
    }

    llm_sources <- data.frame(
      model_key = c("llama", "llama33", "oai", "chatgpt"),
      model_label = c("Llama 3.1", "Llama 3.3", "OpenAI GPT-4o", "ChatGPT"),
      kappa_macro = c(
        "DaoBiasLlmKappaLlama",
        "DaoBiasLlmKappaLlamaThreePointThree",
        "DaoBiasLlmKappaOai",
        "DaoBiasLlmKappaChatgpt"
      ),
      agreement_macro = c(
        "DaoBiasLlmKappaLlamaAgreement",
        "DaoBiasLlmKappaLlamaThreePointThreeAgreement",
        "DaoBiasLlmKappaOaiAgreement",
        "DaoBiasLlmKappaChatgptAgreement"
      ),
      no_suggestion_macro = c(
        "DaoBiasSuggestedNoSuggestionPctLlama",
        "DaoBiasSuggestedNoSuggestionPctLlamaThreePointThree",
        "DaoBiasSuggestedNoSuggestionPctOai",
        "DaoBiasSuggestedNoSuggestionPctChatgpt"
      ),
      fill = c("#2A9D8F", "#457B9D", "#E76F51", "#6D597A"),
      stringsAsFactors = FALSE
    )

    kappa_rows <- llm_sources %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        value = macro_number(kappa_macro),
        agreement_label = macro_value(agreement_macro),
        no_suggestion_pct = macro_number(no_suggestion_macro)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        model_key,
        model_label,
        section = "Heuristic vs LLM stance classification",
        metric_label = "Cohen's kappa",
        value,
        agreement_label,
        no_suggestion_pct
      )

    approve_metric_source <- suggested_stance_metrics %>%
      dplyr::filter(.data$model_key %in% llm_sources$model_key, .data$stance == "APPROVE") %>%
      dplyr::select(
        model_key,
        precision,
        recall,
        f1
      )

    missing_approve_sources <- setdiff(llm_sources$model_key, approve_metric_source$model_key)
    if (length(missing_approve_sources) > 0) {
      stop("Missing Approve suggested-stance metrics for: ", paste(missing_approve_sources, collapse = ", "))
    }

    approve_rows <- llm_sources %>%
      dplyr::select(model_key, model_label) %>%
      dplyr::left_join(approve_metric_source, by = "model_key") %>%
      tidyr::pivot_longer(
        cols = c("precision", "recall", "f1"),
        names_to = "metric_key",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        section = "Approve suggested stance vs winning stance",
        metric_label = dplyr::recode(
          .data$metric_key,
          precision = "Precision",
          recall = "Recall",
          f1 = "F1"
        ),
        agreement_label = NA_character_,
        no_suggestion_pct = NA_real_
      ) %>%
      dplyr::select(
        model_key,
        model_label,
        section,
        metric_label,
        value,
        agreement_label,
        no_suggestion_pct
      )

    approve_metric_bounds <- data.frame(
      metric_key = c("precision", "recall", "f1"),
      min_macro = c(
        "DaoBiasSuggestedApprovePrecisionMin",
        "DaoBiasSuggestedApproveRecallMin",
        "DaoBiasSuggestedApproveFOneMin"
      ),
      max_macro = c(
        "DaoBiasSuggestedApprovePrecisionMax",
        "DaoBiasSuggestedApproveRecallMax",
        "DaoBiasSuggestedApproveFOneMax"
      ),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        macro_min = macro_number(min_macro),
        macro_max = macro_number(max_macro),
        diagnostic_min = round(min(approve_metric_source[[metric_key]], na.rm = TRUE), 2),
        diagnostic_max = round(max(approve_metric_source[[metric_key]], na.rm = TRUE), 2)
      ) %>%
      dplyr::ungroup()

    if (!all(approve_metric_bounds$macro_min == approve_metric_bounds$diagnostic_min) ||
        !all(approve_metric_bounds$macro_max == approve_metric_bounds$diagnostic_max)) {
      stop("Approve metric macro ranges do not match suggested-stance diagnostics.")
    }

    plot_rows <- dplyr::bind_rows(kappa_rows, approve_rows) %>%
      dplyr::mutate(
        model_label = factor(model_label, levels = llm_sources$model_label),
        section = factor(
          section,
          levels = c(
            "Heuristic vs LLM stance classification",
            "Approve suggested stance vs winning stance"
          )
        ),
        metric_label = factor(
          metric_label,
          levels = c("Cohen's kappa", "Precision", "Recall", "F1")
        ),
        value_label = sprintf("%.2f", value),
        plot_label = ifelse(
          metric_label == "Cohen's kappa",
          paste0(value_label, "\n", agreement_label),
          value_label
        )
      )

    no_suggestion_subtitle <- paste0(
      "Model-specific denominators; no-suggestion labels range from ",
      macro_value("DaoBiasSuggestedNoSuggestionPctMin"),
      "% (",
      macro_value("DaoBiasSuggestedNoSuggestionPctMinSource"),
      ") to ",
      macro_value("DaoBiasSuggestedNoSuggestionPctMax"),
      "% (",
      macro_value("DaoBiasSuggestedNoSuggestionPctMaxSource"),
      ")."
    )

    plot_rows


    llm_stance_agreement_approve_plot <- plot_rows %>%
      ggplot(aes(metric_label, value, fill = model_label)) +
      geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
      geom_col(
        position = position_dodge(width = 0.78),
        width = 0.68,
        color = "white",
        linewidth = 0.3
      ) +
      geom_text(
        aes(label = plot_label),
        position = position_dodge(width = 0.78),
        vjust = -0.25,
        size = 3.6,
        lineheight = 0.92
      ) +
      scale_fill_manual(
        name = "Source",
        values = stats::setNames(llm_sources$fill, llm_sources$model_label)
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        labels = function(x) sprintf("%.1f", x),
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(
        x = NULL,
        y = "Diagnostic value",
        title = "LLM agreement with heuristic stance classification",
        subtitle = no_suggestion_subtitle,
        caption = paste(
          "Sources:",
          "metadata/analysis_data_provenance/latex_counts_current.csv;",
          "metadata/analysis_data_provenance/suggested_stance_diagnostics__latest.csv."
        )
      ) +
      facet_wrap(~section, ncol = 1, scales = "free_x") +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        strip.background = element_rect(fill = "gray92", color = "gray70"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 0)
      )

    llm_stance_agreement_approve_plot


    gggsave("llm_stance_agreement_approve_metrics.svg", plot = llm_stance_agreement_approve_plot, height = 7, width = 11)
    gggsave("llm_stance_agreement_approve_metrics.pdf", plot = llm_stance_agreement_approve_plot, height = 7, width = 11)
  }),envir)
  invisible(envir)
}
