# Extracted analytical implementation; execution is explicit.
figure_teaser <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    df_choices_biases <- df_choices_full %>%
      pivot_longer(
        cols = c("is_author_choice_str2", "is_infavor_str", "is_first_choice_str2"),
        names_to = "bias",
        values_to = "bias_value"
      )

    df_choices_biases$bias <- factor(
      df_choices_biases$bias,
      levels = c("is_first_choice_str2", "is_infavor_str", "is_author_choice_str2"),
      labels = c("Position Bias", "Approve Bias", "Author Bias")
    )


    mydata <- df_choices_biases
    mydata <- mydata[!(mydata$bias_value %in% c("No Auth.\nVote")), ]

    mysm <- summarySE(mydata, "choice_share_2", c("bias", "bias_value"), na.rm = TRUE)
    mysm$bias_value <- factor(mysm$bias_value, levels = c("First", "Approve", "Author", "Others"))

    mysm %>%
      ggplot(aes(bias_value, choice_share_2, fill = choice_share_2)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), width = 0.5) +
      xlab("") + ylab("Avg. Share voting power") +
      theme(legend.position = "none") +
      facet_grid(~bias, scales = "free_x")


    gggsave("teaser_biases.svg", height = 5, width = 7)


    mydata <- df_choices_biases
    mydata <- mydata[!(mydata$bias_value %in% c("No Auth.\nVote")), ]

    mysm <- summarySE(mydata, "excess_share", c("bias", "bias_value"), na.rm = TRUE)
    mysm$bias_value <- factor(mysm$bias_value, levels = c("First", "Approve", "Author", "Others"))

    mysm %>%
      ggplot(aes(bias_value, excess_share, fill = excess_share)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), width = 0.5) +
      xlab("") + ylab("Avg. Excess voting power") +
      theme(legend.position = "none") +
      facet_grid(~bias, scales = "free_x")


    gggsave("teaser_excess_biases.svg", height = 5, width = 7)


    df_pos_per_proposal <- df_choices_full %>%
      dplyr::filter(N_choices > 1) %>%
      dplyr::mutate(
        is_first = ifelse(choice_idx == 1, "First", "Others"),
        teaser_excess_share = choice_share_2 - (1 / N_choices)
      ) %>%
      dplyr::group_by(proposal, is_first) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(teaser_excess_share, na.rm = TRUE),
        .groups    = "drop"
      )

    df_pos_per_proposal$is_first <- factor(
      df_pos_per_proposal$is_first,
      levels = c("First", "Others")
    )


    mysm <- summarySE(df_pos_per_proposal, "share_sum", c("is_first"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_first, share_sum, fill = share_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = share_sum - ci, ymax = share_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Share voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_biases_position_aggregate.svg", height = 5, width = 3)


    mysm <- summarySE(df_pos_per_proposal, "excess_sum", c("is_first"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_first, excess_sum, fill = excess_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = excess_sum - ci, ymax = excess_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Excess voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_excess_biases_position_aggregate.svg", height = 5, width = 3)


    df_author_per_proposal <- df_choices_full %>%
      dplyr::filter(N_choices > 1, !is.na(is_author_choice)) %>%
      dplyr::mutate(
        is_author = ifelse(is_author_choice == 1, "Author", "Others"),
        teaser_excess_share = choice_share_2 - (1 / N_choices)
      ) %>%
      dplyr::group_by(proposal, is_author) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(teaser_excess_share, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      tidyr::complete(
        proposal, is_author,
        fill = list(share_sum = 0, excess_sum = 0)
      )

    df_author_per_proposal$is_author <- factor(
      df_author_per_proposal$is_author,
      levels = c("Author", "Others")
    )


    mysm <- summarySE(df_author_per_proposal, "share_sum", c("is_author"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_author, share_sum, fill = share_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = share_sum - ci, ymax = share_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Share voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_biases_author_aggregate.svg", height = 5, width = 3)


    mysm <- summarySE(df_author_per_proposal, "excess_sum", c("is_author"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_author, excess_sum, fill = excess_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = excess_sum - ci, ymax = excess_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Excess voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_excess_biases_author_aggregate.svg", height = 5, width = 3)


    df_approve_per_proposal <- df_choices_full %>%
      dplyr::filter(N_choices > 1) %>%
      dplyr::group_by(proposal) %>%
      dplyr::filter(any(stance == "APPROVE", na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        is_approve = ifelse(!is.na(stance) & stance == "APPROVE",
                            "Approve", "Others"),
        teaser_excess_share = choice_share_2 - (1 / N_choices)
      ) %>%
      dplyr::group_by(proposal, is_approve) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(teaser_excess_share, na.rm = TRUE),
        .groups    = "drop"
      ) %>%
      tidyr::complete(
        proposal, is_approve,
        fill = list(share_sum = 0, excess_sum = 0)
      )

    df_approve_per_proposal$is_approve <- factor(
      df_approve_per_proposal$is_approve,
      levels = c("Approve", "Others")
    )


    mysm <- summarySE(df_approve_per_proposal, "share_sum", c("is_approve"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_approve, share_sum, fill = share_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = share_sum - ci, ymax = share_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Share voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_biases_approve_aggregate.svg", height = 5, width = 3)


    mysm <- summarySE(df_approve_per_proposal, "excess_sum", c("is_approve"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(is_approve, excess_sum, fill = excess_sum)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = excess_sum - ci, ymax = excess_sum + ci), width = 0.5) +
      xlab("") + ylab("Avg. Excess voting power") + #  (per-proposal aggregate)
      theme(legend.position = "none")


    gggsave("teaser_excess_biases_approve_aggregate.svg", height = 5, width = 3)


    df_teaser_companion_aggregate <- dplyr::bind_rows(
      df_pos_per_proposal %>%
        dplyr::transmute(
          proposal,
          bias = "Position Bias",
          bias_value = as.character(is_first),
          share_sum,
          excess_sum
        ),
      df_approve_per_proposal %>%
        dplyr::transmute(
          proposal,
          bias = "Approve Bias",
          bias_value = as.character(is_approve),
          share_sum,
          excess_sum
        ),
      df_author_per_proposal %>%
        dplyr::transmute(
          proposal,
          bias = "Author Bias",
          bias_value = as.character(is_author),
          share_sum,
          excess_sum
        )
    )

    df_teaser_companion_aggregate$bias <- factor(
      df_teaser_companion_aggregate$bias,
      levels = c("Position Bias", "Approve Bias", "Author Bias")
    )
    df_teaser_companion_aggregate$bias_value <- factor(
      df_teaser_companion_aggregate$bias_value,
      levels = c("First", "Approve", "Author", "Others")
    )


    plotTeaserCompanionAggregate <- function(summary_data, metric_col, y_label, tag) {
      ggplot(
        summary_data,
        aes(bias_value, .data[[metric_col]], fill = .data[[metric_col]])
      ) +
        geom_bar(stat = "identity") +
        geom_errorbar(
          aes(
            ymin = .data[[metric_col]] - ci,
            ymax = .data[[metric_col]] + ci
          ),
          width = 0.5
        ) +
        xlab("") +
        ylab(y_label) +
        labs(tag = tag) +
        theme(
          legend.position = "none",
          plot.tag = element_text(face = "bold", size = 16),
          plot.tag.position = c(0, 1)
        ) +
        facet_grid(~bias, scales = "free_x")
    }


    teaser_companion_share_summary <- summarySE(
      df_teaser_companion_aggregate,
      "share_sum",
      c("bias", "bias_value"),
      na.rm = TRUE
    )
    teaser_companion_excess_summary <- summarySE(
      df_teaser_companion_aggregate,
      "excess_sum",
      c("bias", "bias_value"),
      na.rm = TRUE
    )

    teaser_companion_share_plot <- plotTeaserCompanionAggregate(
      teaser_companion_share_summary,
      "share_sum",
      "Avg. Share voting power",
      "A"
    )
    teaser_companion_excess_plot <- plotTeaserCompanionAggregate(
      teaser_companion_excess_summary,
      "excess_sum",
      "Avg. Excess voting power",
      "B"
    )

    teaser_companion_aggregate_figure <- gridExtra::arrangeGrob(
      teaser_companion_share_plot,
      teaser_companion_excess_plot,
      ncol = 2
    )

    grid::grid.newpage()
    grid::grid.draw(teaser_companion_aggregate_figure)


    gggsave(
      "teaser_companion_aggregate.svg",
      plot = teaser_companion_aggregate_figure,
      height = 5,
      width = 14
    )
    gggsave(
      "teaser_companion_aggregate.pdf",
      plot = teaser_companion_aggregate_figure,
      height = 5,
      width = 14
    )
  }),envir)
  invisible(envir)
}
