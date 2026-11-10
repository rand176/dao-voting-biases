# Extracted analytical implementation; execution is explicit.
figure_approve_appendix <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()



    mydata = df_choices


    mysm <- summarySE(mydata, "choice_share_2", c("stance"), na.rm=TRUE)

    p_pos1 <- mysm %>%
      ggplot(aes(stance, choice_share_2, fill=stance)) +
      geom_bar(stat="identity", position="dodge") +
      xlab('') + ylab('Share of voting power') +
      geom_errorbar(aes(ymin=choice_share_2-ci, ymax=choice_share_2+ci), width=0.5) +
       scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) + theme(legend.position = "none",
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    p_pos1


    # gggsave("vp_by_stance_hist.svg")
    # gggsave("vp_by_stance_hist.pdf")


    ## 10+

    mydata = df_choices
    mydata$pos1 = ifelse(mydata$choice_idx == 1, 1, 0)

    mysm <- summarySE(mydata, "pos1", c("stance", "N_choices5"), na.rm=TRUE)

    p_pos2 <- mysm %>%
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

    p_pos2


    # gggsave("choice_pos1_stance_hist_5choices.svg", width=10)
    # gggsave("choice_pos1_stance_hist_5choices.pdf", width=10)


    library(patchwork)

    combined_plot <- p_pos1 | p_pos2

    combined_plot <- combined_plot +
      plot_layout(widths = c(1, 2.5)) +
      plot_annotation(tag_levels = "A")

    combined_plot


    gggsave("approve_bias_stance_panel.svg", plot = combined_plot, width = 13)
    gggsave("approve_bias_stance_panel.pdf", plot = combined_plot, width = 13)



    # mydata = df_choices[df_choices$N_choices <= 10 & df_choices$N_choices > 1,]
    mydata <- df_choices

    mydata %>%
      ggplot(aes(stance, fill=stance)) +
      geom_histogram(stat = "count") +
      xlab('') + ylab('Count') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      theme(legend.position = "none") +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      facet_grid(~N_choices10)



    gggsave("choice_stance_hist.pdf", height=6, width=12)
  }),envir)
  invisible(envir)
}
