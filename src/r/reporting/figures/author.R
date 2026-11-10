# Extracted analytical implementation; execution is explicit.
figure_author <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    author_choice_colors <- c(
      "Author Choice" = "#56B1F7",
      "Other Choices" = "#132B43"
    )

    labelAuthorChoice <- function(value) {
      factor(
        ifelse(value == 1, "Author Choice", "Other Choices"),
        levels = c("Author Choice", "Other Choices")
      )
    }

    saveMainTextFigure <- function(filename, plot, ...) {
      output_path <- file.path(FIGURE_OUTPUT_ROOT, filename)
      if (SAVEIMG) {
        dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
        ggsave(filename = output_path, plot = plot, ...)
      }
      print(output_path)
    }

    authorChoiceLineTheme <- function(show_legend = FALSE) {
      theme(
        legend.position = if (show_legend) "bottom" else "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.tag = element_text(face = "bold", size = 18),
        plot.tag.position = c(0, 1)
      )
    }



    ## All proposals with more than 1 vote.
    mydata <- df_choices_gt1vote

    author_choice_by_count_summary <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "N_choices5", "is_author_choice"),
      na.rm = TRUE
    )
    author_choice_by_count_summary <- author_choice_by_count_summary[
      author_choice_by_count_summary$N > 10,
    ]
    author_choice_by_count_summary <- author_choice_by_count_summary[
      !is.na(author_choice_by_count_summary$is_author_choice),
    ]
    author_choice_by_count_summary$author_choice_label <- labelAuthorChoice(
      author_choice_by_count_summary$is_author_choice
    )

    author_choice_by_count_plot <- author_choice_by_count_summary %>%
      ggplot(aes(
        factor(choice_idx5),
        excess_share,
        group = interaction(N_choices5, author_choice_label),
        fill = author_choice_label,
        color = author_choice_label
      )) +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), width = 0.4, alpha = 0.6) +
      geom_point() +
      geom_line() +
      scale_fill_manual(values = author_choice_colors) +
      scale_color_manual(values = author_choice_colors) +
      xlab("") +
      ylab("Avg. excess voting power") +
      authorChoiceLineTheme() +
      facet_grid(~N_choices5, scales = "free", drop = TRUE) +
      labs(tag = "A")

    author_choice_by_count_plot


    gggsave(
      "excess_vp_author_choice_by_choice_sc_2-5_lines.svg",
      plot = author_choice_by_count_plot,
      height = 5,
      width = 10
    )


    mydata <- df_choices_gt1vote
    # mydata <- mydata[mydata$n_votes > 10, ]

    author_choice_by_stance_summary <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "is_author_choice", "stance"),
      na.rm = TRUE
    )
    author_choice_by_stance_summary <- author_choice_by_stance_summary[
      author_choice_by_stance_summary$N > 10,
    ]
    author_choice_by_stance_summary <- author_choice_by_stance_summary[
      !is.na(author_choice_by_stance_summary$is_author_choice),
    ]
    author_choice_by_stance_summary$author_choice_label <- labelAuthorChoice(
      author_choice_by_stance_summary$is_author_choice
    )
    author_choice_by_stance_summary$stance <- factor(
      author_choice_by_stance_summary$stance,
      levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER")
    )

    author_choice_by_stance_plot <- author_choice_by_stance_summary %>%
      ggplot(aes(
        factor(choice_idx5),
        excess_share,
        fill = author_choice_label,
        color = author_choice_label,
        group = author_choice_label
      )) +
      geom_point() +
      geom_line() +
      xlab("Choice Index") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), width = 0.4, alpha = 0.6) +
      scale_fill_manual(values = author_choice_colors) +
      scale_color_manual(values = author_choice_colors) +
      authorChoiceLineTheme() +
      facet_grid(~stance, scales = "free", drop = TRUE) +
      labs(tag = "B")

    author_choice_by_stance_plot


    gggsave(
      "excess_vp_author_choice_by_stance_2-5_lines.svg",
      plot = author_choice_by_stance_plot,
      height = 5,
      width = 10
    )


    author_choice_by_stance_plot_with_legend <- author_choice_by_stance_plot +
      authorChoiceLineTheme(show_legend = TRUE) +
      guides(
        fill = "none",
        color = guide_legend(
          nrow = 1,
          override.aes = list(size = 5, linetype = 0)
        )
      )

    author_bias_figure <- gridExtra::arrangeGrob(
      author_choice_by_count_plot,
      author_choice_by_stance_plot_with_legend,
      ncol = 1,
      heights = c(1, 1.17)
    )

    grid::grid.newpage()
    grid::grid.draw(author_bias_figure)


    saveMainTextFigure(
      "Fig4_AuthorBias_v2.svg",
      plot = author_bias_figure,
      height = 10,
      width = 10
    )
    saveMainTextFigure(
      "Fig4_AuthorBias_v2.pdf",
      plot = author_bias_figure,
      height = 10,
      width = 10
    )


    author_lift_by_stance_position <- df_choices_gt1vote %>%
      dplyr::filter(!is.na(is_author_choice), !is.na(excess_share)) %>%
      dplyr::group_by(stance, choice_idx5, is_author_choice) %>%
      dplyr::summarise(
        n = dplyr::n(),
        mean_excess = mean(excess_share, na.rm = TRUE),
        var_excess = var(excess_share, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(n > 10) %>%
      dplyr::mutate(author_choice_label = labelAuthorChoice(is_author_choice)) %>%
      dplyr::select(-is_author_choice) %>%
      tidyr::pivot_wider(
        names_from = author_choice_label,
        values_from = c(n, mean_excess, var_excess),
        names_sep = "__"
      ) %>%
      dplyr::mutate(
        author_lift =
          `mean_excess__Author Choice` - `mean_excess__Other Choices`,

        se_lift = sqrt(
          `var_excess__Author Choice` / `n__Author Choice` +
            `var_excess__Other Choices` / `n__Other Choices`
        ),

        df = (
          `var_excess__Author Choice` / `n__Author Choice` +
            `var_excess__Other Choices` / `n__Other Choices`
        )^2 / (
          (`var_excess__Author Choice` / `n__Author Choice`)^2 /
            (`n__Author Choice` - 1) +
            (`var_excess__Other Choices` / `n__Other Choices`)^2 /
            (`n__Other Choices` - 1)
        ),

        ci_lift = qt(0.975, df) * se_lift,
        lift_low = author_lift - ci_lift,
        lift_high = author_lift + ci_lift
      ) %>%
      dplyr::arrange(dplyr::desc(author_lift))


    author_lift_by_stance_position %>%
      mutate(
        stance = factor(stance, levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER"))
      ) %>%
      ggplot(aes(
        x = factor(choice_idx5),
        y = author_lift,
        group = stance
      )) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_point() +
      geom_errorbar(aes(ymin = lift_low, ymax = lift_high), width = 0.25, alpha = 0.6) +
      facet_grid(~ stance, scales = "free_x", drop = TRUE) +
      xlab("Choice Index") +
      ylab("Author-choice lift in\navg. excess voting power") +
      authorChoiceLineTheme()


    # gggsave(
    #   "excess_vp_author_lift_by_stance_and_pos.svg",
    #   height = 5,
    #   width = 10
    # )
    gggsave(
      "excess_vp_author_lift_by_stance_and_pos.pdf",
      height = 5,
      width = 10
    )
  }),envir)
  invisible(envir)
}
