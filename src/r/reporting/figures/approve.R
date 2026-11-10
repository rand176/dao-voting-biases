# Extracted analytical implementation; execution is explicit.
figure_approve <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    mydata <- df_choices

    mysm <- summarySE(mydata, "choice_share_2", c("choice_idx5", "stance"), na.rm = TRUE)
    mysm$choice_idx2 <- factor(mysm$choice_idx5)

    approve_choice_share_plot <- mysm %>%
      ggplot(aes(choice_idx2, choice_share_2, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge") +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.title = element_text(size = 14)) +
      facet_grid(~stance, scales = "free_x")

    approve_choice_share_plot


    gggsave("vp_by_stance_by_choice_by_pos_hist.svg", plot = approve_choice_share_plot, height = 6, width = 16)


    mydata <- df_choices

    mysm <- summarySE(mydata, "excess_share", c("N_choices5", "choice_idx5", "stance"), na.rm = TRUE)
    mysm$choice_idx2 <- factor(mysm$choice_idx5)
    mysm$N_choices2 <- factor(mysm$N_choices5)

    approve_excess_by_choice_count_plot <- mysm %>%
      ggplot(aes(choice_idx2, excess_share, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Avg. Excess share of voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none") +
      facet_grid(stance ~ N_choices5, scales = "free_x")

    approve_excess_by_choice_count_plot


    gggsave("excess_vp_by_stance_by_choice_by_pos_hist.pdf", plot = approve_excess_by_choice_count_plot, height = 7, width = 10)


    mydata <- df_choices

    mysm <- summarySE(mydata, "excess_share", c("choice_idx5", "stance"), na.rm = TRUE)
    mysm$choice_idx2 <- factor(mysm$choice_idx5)

    approve_excess_by_stance_plot <- mysm %>%
      ggplot(aes(choice_idx2, excess_share, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Avg. Excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none") +
      facet_grid(~stance, scales = "free_x")

    approve_excess_by_stance_plot


    gggsave("excess_vp_by_choice_by_stance_hist.pdf", plot = approve_excess_by_stance_plot, height = 6, width = 11)


    mydata <- df_choices

    mysm <- summarySE(mydata, "excess_share", c("stance"), na.rm = TRUE)

    approve_excess_by_stance_plot <- mysm %>%
      ggplot(aes(stance, excess_share, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      ylab("Avg. Excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none")

    approve_excess_by_stance_plot


    mydata <- df_choices

    stance_position_counts <- as.data.frame(
      table(mydata$stance, mydata$choice_idx5),
      stringsAsFactors = FALSE
    )
    colnames(stance_position_counts) <- c("stance", "choice_idx5", "n")
    stance_position_counts$stance <- factor(
      stance_position_counts$stance,
      levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER")
    )
    stance_position_counts$choice_idx5 <- factor(
      stance_position_counts$choice_idx5,
      levels = c("1", "2", "3", "4", "5", "5+")
    )

    stance_distribution_by_position_plot <- stance_position_counts %>%
      ggplot(aes(choice_idx5, n, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Number of choices") +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "bottom")

    stance_distribution_by_position_plot


    gggsave("stance_distribution_by_choice_position_hist.pdf", plot = stance_distribution_by_position_plot, height = 5, width = 8)


    ## 10+

    mydata = df_choices
    mydata$pos1 = ifelse(mydata$choice_idx == 1, 1, 0)

    mysm <- summarySE(mydata, "pos1", c("stance", "N_choices5"), na.rm=TRUE)

    mysm %>%
      ggplot(aes(stance, pos1, fill=stance)) +
      geom_bar(stat="identity", position="dodge") +
      xlab('') + ylab('% of times in position 1') +
      geom_errorbar(aes(ymin=pos1-ci, ymax=pos1+ci), width=0.5) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position = "none") +
       scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      facet_grid(~N_choices5)


    gggsave("choice_pos1_stance_hist_5choices.svg", width=10)
    gggsave("choice_pos1_stance_hist_5choices.pdf", width=10)
  }),envir)
  invisible(envir)
}
