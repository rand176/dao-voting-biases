# Extracted analytical implementation; execution is explicit.
figure_metric_test <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    df_pos_metric_test <- df_choices_full %>%
      dplyr::filter(N_choices > 1) %>%
      dplyr::mutate(is_first = ifelse(choice_idx == 1, "First", "Others")) %>%
      dplyr::group_by(proposal, N_choices, N_choices5, is_first) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(excess_share, na.rm = TRUE),
        K          = dplyr::n(),
        .groups    = "drop"
      ) %>%
      dplyr::filter(is_first == "First") %>%
      dplyr::mutate(
        mult_excess = (N_choices / K) * share_sum - 1
      ) %>%
      tidyr::pivot_longer(
        cols      = c(excess_sum, mult_excess),
        names_to  = "metric_type",
        values_to = "metric_value"
      ) %>%
      dplyr::mutate(
        metric_type = factor(
          metric_type,
          levels = c("excess_sum", "mult_excess"),
          labels = c("Additive excess", "Multiplicative excess")
        )
      )

    mysm <- summarySE(df_pos_metric_test, "metric_value",
                      c("N_choices5", "metric_type"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(N_choices5, metric_value, fill = metric_value)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = metric_value - ci, ymax = metric_value + ci),
                    width = 0.4) +
      xlab("N_choices (binned)") + ylab("Excess value (First bucket)") +
      theme(legend.position = "none") +
      facet_wrap(~metric_type, scales = "free_y")


    gggsave("teaser_metric_test_position.svg", height = 5, width = 8)


    df_author_metric_test <- df_choices_full %>%
      dplyr::filter(N_choices > 1, !is.na(is_author_choice)) %>%
      dplyr::mutate(
        is_author = ifelse(is_author_choice == 1, "Author", "Others")
      ) %>%
      dplyr::group_by(proposal, N_choices, N_choices5, is_author) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(excess_share, na.rm = TRUE),
        K          = dplyr::n(),
        .groups    = "drop"
      ) %>%
      dplyr::filter(is_author == "Author") %>%
      dplyr::mutate(
        mult_excess = (N_choices / K) * share_sum - 1
      ) %>%
      tidyr::pivot_longer(
        cols      = c(excess_sum, mult_excess),
        names_to  = "metric_type",
        values_to = "metric_value"
      ) %>%
      dplyr::mutate(
        metric_type = factor(
          metric_type,
          levels = c("excess_sum", "mult_excess"),
          labels = c("Additive excess", "Multiplicative excess")
        )
      )

    mysm <- summarySE(df_author_metric_test, "metric_value",
                      c("N_choices5", "metric_type"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(N_choices5, metric_value, fill = metric_value)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = metric_value - ci, ymax = metric_value + ci),
                    width = 0.4) +
      xlab("N_choices (binned)") + ylab("Excess value (Author bucket)") +
      theme(legend.position = "none") +
      facet_wrap(~metric_type, scales = "free_y")


    gggsave("teaser_metric_test_author.svg", height = 5, width = 8)


    df_approve_metric_test <- df_choices_full %>%
      dplyr::filter(N_choices > 1) %>%
      dplyr::group_by(proposal) %>%
      dplyr::filter(any(stance == "APPROVE", na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        is_approve = ifelse(!is.na(stance) & stance == "APPROVE",
                            "Approve", "Others")
      ) %>%
      dplyr::group_by(proposal, N_choices, N_choices5, is_approve) %>%
      dplyr::summarise(
        share_sum  = sum(choice_share_2, na.rm = TRUE),
        excess_sum = sum(excess_share, na.rm = TRUE),
        K          = dplyr::n(),
        .groups    = "drop"
      ) %>%
      dplyr::filter(is_approve == "Approve") %>%
      dplyr::mutate(
        mult_excess = (N_choices / K) * share_sum - 1
      ) %>%
      tidyr::pivot_longer(
        cols      = c(excess_sum, mult_excess),
        names_to  = "metric_type",
        values_to = "metric_value"
      ) %>%
      dplyr::mutate(
        metric_type = factor(
          metric_type,
          levels = c("excess_sum", "mult_excess"),
          labels = c("Additive excess", "Multiplicative excess")
        )
      )

    mysm <- summarySE(df_approve_metric_test, "metric_value",
                      c("N_choices5", "metric_type"), na.rm = TRUE)

    mysm %>%
      ggplot(aes(N_choices5, metric_value, fill = metric_value)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = metric_value - ci, ymax = metric_value + ci),
                    width = 0.4) +
      xlab("N_choices (binned)") + ylab("Excess value (Approve bucket)") +
      theme(legend.position = "none") +
      facet_wrap(~metric_type, scales = "free_y")


    gggsave("teaser_metric_test_approve.svg", height = 5, width = 8)
  }),envir)
  invisible(envir)
}
