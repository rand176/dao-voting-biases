# Extracted analytical implementation; execution is explicit.
prepare_features <- function(context) {
  envir <- new_analysis_environment(context)
  evalq({

    ## check also: proposals_choices_25_v2.csv
    df_proposals <-  read.csv(rawDataPath('proposals_0.csv.gz'), colClasses = c(id = "character"))
    df_proposals$proposal <- tolower(df_proposals$id)
    nrow(df_proposals)



    stage3_candidate_input <- debugDataPath("choices_25-nov-17_C_stage3_reanalysis.csv")
    stage4_candidate_output <- debugDataPath("df_choices_25B_more_vars_stage4.csv")

    df_choices <- read.csv(requireExistingInputPath(stage3_candidate_input, "stage 3 candidate output"), colClasses = c(proposal = "character"))
    df_choices$proposal <- trimws(tolower(df_choices$proposal))
    df_choices$type = factor(df_choices$type, levels = c(
        "single-choice", "basic", "quadratic", "weighted",
        "ranked-choice", "approval"
    ))
    df_choices <- dropSingleOptionProposals(df_choices)



    nrow(df_choices) # 340135



    # df_choices$is_single <- ifelse(df_choices$type %in% c("basic", "single-choice"), 1, 0)
    # df_choices$is_first_choice <- ifelse(df_choices$choice_idx == 1, 1, 0)
    #

    df_choices$quorum.x <- NULL
    df_choices$quorum <- df_choices$quorum.y
    df_choices$quorum.y <- NULL

    df_choices$stance <- df_choices$stance2

    df_choices$is_infavor <- df_choices$is_infavor2
    df_choices$is_against <- df_choices$is_against2
    df_choices$is_abstain <- df_choices$is_abstain2
    df_choices$is_other <- df_choices$is_other2

    df_choices$stance2 <- NULL
    df_choices$is_infavor2 <- NULL
    df_choices$is_abstain2 <- NULL
    df_choices$is_against2 <- NULL
    df_choices$is_other2 <- NULL




    get_author_stance <- function(x, stance_col) {
      ac <- x[x$is_author_choice == 1, ]
      nac <- nrow(ac)

      if (nac == 0) {
        res <- NA
      } else {
        stances <- ac[[stance_col]]
        uniq <- unique(stances)

        if (length(uniq) == 1) {
          res <- uniq
        } else {
          res <- paste(sort(uniq), collapse = "-")
        }
      }

      return(res)
    }


    add_author_stance_stats <- function(df, stance_field) {
      if (!(stance_field %in% colnames(df))) {
        stop(paste("Missing stance field:", stance_field))
      }

      author_choice_stance <- df[df$is_author_choice %in% c(1), ]
      # nrow(author_choice_stance)
      # 52510
      # length(unique(author_choice_stance$proposal))
      # [1] 49438

      ## There are fewer unique proposals than author_choices. Probably because
      ## of authors voting for more choices.


      ## All proposals with more than one author choice.
      filtered_df <- df %>%
        filter(is_author_choice == 1) %>%
        group_by(proposal) %>%
        filter(n() > 1) %>%
        ungroup()

      ## ADD AUTHOR_MULTI_VOTE
      df$author_multi_vote <- ifelse(df$proposal %in% filtered_df$proposal, 1, 0)

      ## Find proposals with more than one approve vote.
      df_multiyes <- df %>%
        filter(.data[[stance_field]] == "APPROVE") %>%
        group_by(proposal) %>%
        filter(n() > 1) %>%
        ungroup()

      ## ADD AUTHOR_MULTI_APPROVE
      df$author_multi_approve <- ifelse(df$proposal %in% df_multiyes$proposal, 1, 0)

      ## When there are multiple author choices, they are with the same stance.
      # author_choice_stance <- ddply(df, 'proposal', get_author_stance)

      author_choice_stance <- do.call(
        rbind,
        lapply(
          split(df, df$proposal),
          function(x) {
            data.frame(
              proposal = x$proposal[1],
              author_choice_stance = get_author_stance(x, stance_field),
              stringsAsFactors = FALSE
            )
          }
        )
      )

      colnames(author_choice_stance)[2] <- "author_choice_stance"

      df <- audit_left_join_df(df, author_choice_stance, "proposal", "df_choices", "author_choice_stance")

      return(df)

    }

    df2 <- add_author_stance_stats(df_choices, "stance")

    df_choices = df2
    df2 <- NULL

    df_choices <- df_choices %>%
      mutate(author_choice_stance_compact =
              ifelse(author_choice_stance %in% c("ABSTAIN","APPROVE","REJECT","OTHER"),
                     author_choice_stance, ifelse(is.na(author_choice_stance), NA, "MIXED")))

    table(df_choices$author_choice_stance_compact)

    # ABSTAIN APPROVE   MIXED   OTHER  REJECT
    #    1170   42172     308   93897    2973




    ## There seem to be some issues in choice_share, let's check and fix them.
    ## (see also the plots in the next cell)

    ## CHECK
    res <- ddply(df_choices, "proposal", function(x) {
      ssum = sum(x$choice_share)
      if (length(x$choice_share) > 0 && abs(ssum-1) > 0.0000001) {
        return(1)
      }
      return(0)
    })
    table(res$V1)


    ## Issues confirmed and tests:

    # head(res[res$V1 == 1,])
    # p <- "0x002890c3d01f492eaa7fcf01da2341eee9fba707403b17cc7508222ef46d63d2"
    # df_tmp <- df_choices[df_choices$proposal == p,]
    # df_tmp$choice_score
    # df_tmp$choice_share
    # df_tmp$scores_total

    ## The problem is that scores_total is somehow different from the sum of the
    ## individual scores.

    # df_choices_wrong_share <- merge(df_choices, res)
    # nrow(df_choices_wrong_share)
    #
    # df_wrong = df_choices_wrong_share[df_choices_wrong_share$V1 == 1,]

    ## It happens for all voting types.
    # table(df_wrong$type)

    ###########################

    ## FIX
    res_sum <- ddply(df_choices, "proposal", function(x) {
      ssum = sum(x$choice_score)
      return(ssum)
    })
    colnames(res_sum)[2] <- "scores_total_2"
    # head(res_sum)

    table(df_choices$scores_total_2)

    assert_unique_key(res_sum, "proposal", "res_sum")
    df_choices <- audit_left_join_df(df_choices, res_sum, "proposal", "df_choices", "res_sum")

    df_choices$choice_share_2 <- df_choices$choice_score / df_choices$scores_total_2

    df_tmp <- df_choices[is.na(df_choices$choice_share_2),]
    nrow(df_tmp)

    df_tmp$proposal
    df_tmp$choice_text

    ## All zeros.
    df_tmp$choice_score

    ## Same as choice_share
    df_choices <- applyProposalValueOverrides(df_choices, stage = "4_more_vars_for_regr_v2")



    ## Check choice_share again.
    res2 <- ddply(df_choices, "proposal", function(x) {
      ssum = sum(x$choice_share_2)
      if (length(x$choice_share_2) > 0 && abs(ssum-1) > 0.0000001) {
        return(1)
      }
      return(0)
    })
    table(res2$V1) #OK
    res2[res2$V1 == 1,]




    ## Check the sum of choice_share again

    ## CHECK
    res <- ddply(df_choices, c("proposal", "N_choices"), function(x) {
      return(sum(x$choice_share))
    })

    res2 <- ddply(df_choices, c("proposal", "N_choices"), function(x) {
      return(sum(x$choice_share_2))
    })

    ## Choice share2 is fixed.
    my_res = res2

    # Summarize mean and 95% CI
    summary_df <- my_res %>%
      group_by(N_choices) %>%
      summarise(
        mean = mean(V1),
        se = sd(V1) / sqrt(n()),                  # standard error
        ci = qt(0.975, df = n() - 1) * se            # 95% CI
      )

    ## The sum of choice_share is progressively more wrong.

    # Plot
    ggplot(summary_df[summary_df$N_choices <= 10,], aes(x = N_choices, y = mean)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
      geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.2) +
      labs(y = "Mean Value", x = "Group", title = "Bar Plot with 95% CI") +
      theme_minimal()





    ## Keep this as a check.

    # df_choices$is_author_choice2 <- ifelse(is.na(df_choices$author_choice), NA,
    #   ifelse((df_choices$author_choice == df_choices$choice_idx), 1, 0))

    ## There is some problem.
    # summary(df_choices$is_author_choice2 - df_choices$is_author_choice)
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max
     # -1.000   0.000   0.000  -0.022   0.000   0.000  199615

    # Find rows with differences
    # df_diff <- df_choices[df_choices$is_author_choice != df_choices$is_author_choice2 &
                           # !is.na(df_choices$is_author_choice2), ]
    # nrow(df_diff) # 3073

    # table(df_diff$type)
    ## They all come from multiple-preference proposals.
    ## author_choice is a single index, but there are multiple choices with the same stance.
    ## is_author_choice field seem accurate.




    writeCandidateCsv(
      df_choices,
      stage4_candidate_output,
      row.names = FALSE
    )
  }, envir)
  invisible(envir)
}
