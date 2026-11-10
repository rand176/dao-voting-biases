# Extracted analytical implementation; execution is explicit.
figure_author_supplemental <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)
    configure_figures()
    printAnalysisDataProvenance()


    mydata <- df_choices_gt1vote

    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "is_author_choice_str2", "stance"),
      na.rm = TRUE
    )

    mysm %>%
      ggplot(aes(choice_idx5, excess_share, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Avg. Excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none") +
      facet_grid(is_author_choice_str2 ~ stance, scales = "free_x")


    gggsave("excess_vp_by_choice_by_stance_by_authorchoice_hist.pdf", height = 9, width = 14)


    mydata <- df_choices_gt1vote

    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "is_author_choice_str2", "stance"),
      na.rm = TRUE
    )

    mysm %>%
      ggplot(aes(is_author_choice_str2, excess_share, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      ylab("Avg. Excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_grid(choice_idx5 ~ stance, scales = "free_x")


    gggsave("excess_vp_by_choice_by_stance_by_authorchoice_hist_v2.pdf", height = 12, width = 10)


    mydata <- df_choices_gt1vote

    mysm <- summarySE(
      mydata,
      "choice_share_2",
      c("choice_idx5", "is_author_choice_str2", "stance"),
      na.rm = TRUE
    )

    mysm <- mysm[mysm$N > 10, ]

    mysm %>%
      ggplot(aes(is_author_choice_str2, choice_share_2, fill = stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      ylab("Avg. Excess voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge", width = 0.5) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      ) +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_grid(choice_idx5 ~ stance, scales = "free_x")


    mydata <- df_choices_gt1vote

    ## There are some weird values for choice
    mydata$author_choice2 <- as.integer(mydata$author_choice)
    mydata$author_choice2 <- factor(mydata$author_choice2)

    mydata$author_choice_1 <- ifelse(mydata$author_choice == 1, 1, 0)
    mydata$author_choice_1b <- ifelse(mydata$author_choice == 1, "Author ~ 1", "Author ~ 2-10")

    mysm <- summarySE(
      mydata,
      "choice_share_2",
      c("choice_idx10", "N_choices10", "author_choice_1b"),
      na.rm = TRUE
    )
    mysm$choice_idx2 <- factor(mysm$choice_idx10, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "10+"))
    mysm$N_choices2 <- factor(mysm$N_choices)
    mysm$author_choice_1b <- factor(mysm$author_choice_1b)

    ## Author choice 1 only
    mysm[mysm$author_choice_1b %in% c("Author ~ 1"), ] %>%
      ggplot(aes(choice_idx2, choice_share_2, fill = author_choice_1b)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge") +
      theme(legend.position = "none") +
      facet_grid(~N_choices10, scales = "free_x")


    gggsave("author_choice_hist_choice1_sc.pdf", height = 4, width = 16)


    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "N_choices5", "author_choice_1b"),
      na.rm = TRUE
    )
    mysm$author_choice_1b <- factor(mysm$author_choice_1b)

    ## Author choice 1 only
    mysm[mysm$author_choice_1b %in% c("Author ~ 1"), ] %>%
      ggplot(aes(choice_idx5, excess_share, fill = author_choice_1b)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge") +
      theme(legend.position = "none") +
      facet_grid(~N_choices5, scales = "free_x")


    gggsave("excess_vp_author_choice1_sc.pdf", height = 4, width = 16)


    mydata <- df_choices_gt1vote

    ## There are some weird values for choice
    mydata$author_choice2 <- as.integer(mydata$author_choice)
    mydata$author_choice2 <- factor(mydata$author_choice2)

    mydata$author_choice_1 <- ifelse(mydata$author_choice == 1, 1, 0)
    mydata$author_choice_1b <- ifelse(mydata$author_choice == 1, "Author ~ 1", "Author ~ 2-10")

    mysm <- summarySE(
      mydata,
      "choice_share_2",
      c("choice_idx5", "N_choices5", "author_choice"),
      na.rm = TRUE
    )
    mysm$author_choice <- factor(mysm$author_choice)

    mmysm <- mysm[mysm$N > 1, ]
    mmysm <- mmysm[!is.na(mmysm$author_choice), ]

    mysm[!is.na(mysm$author_choice), ] %>%
      ggplot(aes(choice_idx5, choice_share_2, fill = author_choice)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge") +
      theme(legend.position = "none") +
      facet_grid(author_choice ~ N_choices5, scales = "free")


    gggsave("author_choice_hist_sc_2-5.pdf", height = 6, width = 8)
    # gggsave("author_choice_hist_sc_6-10.pdf", height=12, width=8)


    mydata <- df_choices_gt1vote
    # mydata = mydata[mydata$N_choices <= 5 & mydata$N_choices > 1,]

    ## There are some weird values for choice
    mydata$author_choice2 <- as.integer(mydata$author_choice)
    mydata$author_choice2 <- factor(mydata$author_choice2)

    mydata$author_choice5 <- ifelse(mydata$author_choice <= 5, mydata$author_choice, "5+")
    mydata$author_choice5 <- factor(mydata$author_choice5)

    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "N_choices5", "author_choice5"),
      na.rm = TRUE
    )
    mysm$author_choice <- factor(mysm$author_choice5)

    mmysm <- mysm[mysm$N > 1, ]
    mmysm <- mmysm[!is.na(mmysm$author_choice5), ]

    mmysm %>%
      ggplot(aes(factor(choice_idx5), excess_share, fill = author_choice5)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge", width = 0.4) +
      theme(legend.position = "none") +
      facet_grid(author_choice5 ~ N_choices5, scales = "free", drop = TRUE)


    # gggsave("excess_vp_author_choice_by_choice_sc_2-5_v2.pdf", height=8, width=10)
    gggsave("excess_vp_author_choice_by_choice_sc_2-5_v2.svg", height = 8, width = 10)


    mmysm %>%
      ggplot(aes(
        x = factor(choice_idx5),
        y = excess_share,
        color = N_choices5,
        group = author_choice5
      )) +
      # geom_line() +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      geom_errorbar(
        aes(ymin = excess_share - ci, ymax = excess_share + ci),
        position = position_dodge(width = 0.4),
        width = 0.15
      ) +
      facet_wrap(~author_choice5, scales = "free_x") +
      xlab("Choice Index") +
      ylab("Avg. excess voting power")


    mydata <- df_choices_gt1vote
    mydata <- mydata[mydata$n_votes > 10, ]

    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "is_author_choice", "stance"),
      na.rm = TRUE
    )
    mysm$author_choice <- factor(mysm$is_author_choice)

    mmysm <- mysm[mysm$N > 10, ]
    mmysm <- mmysm[!is.na(mmysm$is_author_choice), ]

    mmysm %>%
      ggplot(aes(factor(choice_idx5), excess_share, fill = is_author_choice)) +
      geom_bar(stat = "identity", alpha = 0.8, position = position_dodge()) +
      xlab("Choice Index") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = position_dodge(), width = 0.4, alpha = 0.6) +
      theme(legend.position = "none") +
      facet_grid(~stance, scales = "free", drop = TRUE)


    # gggsave("excess_vp_author_choice_by_stance_2-5.pdf", height=5, width=10)
    gggsave("excess_vp_author_choice_by_stance_2-5.svg", height = 5, width = 10)


    mmysm %>%
      dplyr::filter(N > 10) %>%
      ggplot(aes(factor(stance), excess_share, fill = is_author_choice, color = is_author_choice, group = is_author_choice)) +
      # geom_bar(stat="identity", alpha=0.8, position=position_dodge()) +
      geom_point() +
      geom_line() +
      xlab("Choice Index") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), width = 0.4, alpha = 0.6) +
      theme(legend.position = "none") +
      facet_grid(~choice_idx5, scales = "free", drop = TRUE)


    ## NO AUTHOR VP

    mydata <- df_choices_gt1vote
    mydata$excess_share_no_author_vp <- mydata$excess_share - mydata$choice_share_no_author_vp

    mysm <- summarySE(
      mydata,
      "excess_share_no_author_vp",
      c("choice_idx5", "is_author_choice", "stance"),
      na.rm = TRUE
    )
    mysm$author_choice <- factor(mysm$is_author_choice)

    mmysm <- mysm[mysm$N > 10, ]
    mmysm <- mmysm[!is.na(mmysm$is_author_choice), ]

    mmysm %>%
      dplyr::filter(N > 10) %>%
      ggplot(aes(factor(choice_idx5), excess_share_no_author_vp, fill = is_author_choice, color = is_author_choice, group = is_author_choice)) +
      # geom_bar(stat="identity", alpha=0.8, position=position_dodge()) +
      geom_point() +
      geom_line() +
      xlab("Choice Index") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share_no_author_vp - ci, ymax = excess_share_no_author_vp + ci), width = 0.4, alpha = 0.6) +
      theme(legend.position = "none") +
      facet_grid(~stance, scales = "free", drop = TRUE)


    mysm <- summarySE(
      mydata,
      "excess_share",
      c("choice_idx5", "N_choices5", "is_author_choice", "stance"),
      na.rm = TRUE
    )
    mysm$author_choice <- factor(mysm$is_author_choice)

    mmysm <- mysm[mysm$N > 1, ]
    mmysm <- mmysm[!is.na(mmysm$is_author_choice), ]

    mmysm %>%
      ggplot(aes(factor(choice_idx5), excess_share, fill = is_author_choice)) +
      geom_bar(stat = "identity", alpha = 0.8, position = position_dodge()) +
      xlab("") +
      ylab("Avg. excess voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = position_dodge(), width = 0.4, alpha = 0.6) +
      theme(legend.position = "none") +
      facet_grid(N_choices5 ~ stance, scales = "free", drop = TRUE)


    mysm <- summarySE(mydata, "excess_share", c("choice_idx5", "N_choices5", "author_choice"), na.rm = TRUE)

    mysm[!is.na(mysm$author_choice), ] %>%
      ggplot(aes(choice_idx5, excess_share, fill = author_choice)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("Choice Index") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = excess_share - ci, ymax = excess_share + ci), position = "dodge") +
      theme(legend.position = "none") +
      # facet_grid(author_choice~N_choices, scales = "free_x")
      facet_grid(author_choice ~ N_choices5, scales = "free")


    gggsave("excess_vp_author_choice_by_choice.pdf", height = 12, width = 8)


    mydata <- df_choices[df_choices$N_choices <= 5 & df_choices$N_choices > 1, ]
    mydata <- mydata[mydata$is_single_preference == 1, ]
    mydata <- mydata[!is.na(mydata$author_choice_stance), ]

    mydata$author_choice_stance <- factor(mydata$author_choice_stance, levels = c("APPROVE", "REJECT", "ABSTAIN", "OTHER"))

    nrow(mydata)

    mysm <- summarySE(mydata, "choice_share_2", c("choice_idx", "author_choice_stance"), na.rm = TRUE)
    mysm$choice_idx2 <- factor(mysm$choice_idx)

    mysm %>%
      ggplot(aes(author_choice_stance, choice_share_2, fill = author_choice_stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge") +
      theme(legend.position = "none") +
      facet_grid(~choice_idx2, scales = "free") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      )


    mysm <- summarySE(mydata, "choice_share_2", c("choice_idx", "author_choice_stance", "N_choices"), na.rm = TRUE)
    mysm$choice_idx2 <- factor(mysm$choice_idx)

    mysm %>%
      ggplot(aes(author_choice_stance, choice_share_2, fill = author_choice_stance)) +
      geom_bar(stat = "identity", position = "dodge") +
      xlab("") +
      ylab("Share of voting power") +
      geom_errorbar(aes(ymin = choice_share_2 - ci, ymax = choice_share_2 + ci), position = "dodge") +
      theme(legend.position = "none") +
      facet_grid(choice_idx2 ~ N_choices, scales = "free") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_fill_manual(
        name = "Stance",
        values = stance_colors
      )


    gggsave("author_choice_vs_stance_auc-1_choices2-5.pdf", height = 6, width = 8)


    gggsave("vp_by_author_choice_n_choices_choice_idx.pdf", height = 6, width = 12)
  }),envir)
  invisible(envir)
}
