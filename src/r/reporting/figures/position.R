# Extracted analytical implementation; execution is explicit.
figure_position <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    # mydata = df_choices[df_choices$N_choices <= 5, ]
    mydata = df_choices

    N <- 5
    # N <- 10

    choice_idx_var <- c(paste0("choice_idx", N))
    choice_N_var <- c(paste0("N_choices", N))

    mysm <- summarySE(mydata, "choice_share_2", c(choice_idx_var, choice_N_var), na.rm=TRUE)
    mysm$choice_idx2 = mysm[, choice_idx_var]
    # mysm$choice_idx2 <- factor(mysm$choice_idx2, levels=c('1','2','3','4','5','6','7','8','9','10','10+'))
    mysm$N_choices2 = mysm[, choice_N_var]


    mysm %>%
      ggplot(aes(choice_idx2, choice_share_2, group=N_choices2, color=N_choices2), alpha=0.5) +
      geom_point(size=2.2) + geom_line(size=2,alpha=0.5) +
      geom_errorbar(aes(ymin=choice_share_2-ci, ymax=choice_share_2+ci), width=0.1) +
      xlab('Choice Index') + ylab('Avg. Share of voting power') +
      scale_color_discrete(name="Num. of\navailable\nchoices") +
      theme(
            legend.title = element_text(size = 20),
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification = c("right", "top")
            )


    gggsave("lines_choice_idx_vp.svg", height = 7)
    gggsave("lines_choice_idx_vp.pdf", height = 7)


    ## Update 5+

    mydata = df_choices[df_choices$N_choices > 1,]


    mysm <- summarySE(mydata, "excess_share", c("N_choices5", "choice_idx5"), na.rm=TRUE)
    # mysm$N_choices2 = factor(mysm$N_choices)

    mysm %>%
      ggplot(aes(choice_idx5, excess_share, fill=excess_share)) +
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymin=excess_share-ci, ymax=excess_share+ci), width=0.5)  +
      xlab('Choice Index') + ylab('Avg. Excess voting power') +
      theme(legend.position = "none") + facet_grid(~N_choices5, scales="free_x")



    gggsave("excess_vp_by_choice_v2.svg", width = 7)
    gggsave("excess_vp_by_choice_v2.pdf", width = 7)
  }),envir)
  invisible(envir)
}
