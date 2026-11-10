# Extracted analytical implementation; execution is explicit.
run_descriptive_analysis <- function(context) {
  envir <- new_analysis_environment(context)
  evaluate_analysis(substitute({
    library(modelsummary)
    loadAnalysisData(version=ANALYSIS_DATA_VERSION)


    printAnalysisDataProvenance()


    nrow(df_proposals)



    df_choices_all <- read.csv(requireExistingInputPath(archiveLargeDataPath('choices.csv.gz'), "archive choices snapshot"), colClasses = c(proposal = "character"))
    df_choices_all$proposal <- trimws(tolower(df_choices_all$proposal))
    nrow(df_choices_all) # 654682


    df_choices_pre <- read.csv(
      requireExistingInputPath(
        debugDataPath("choices_lang_len_fake_paper_stage0_snapshot.csv.gz"),
        "stage 0 choices snapshot"
      ),
      colClasses = c(proposal = "character")
    )
    df_choices_pre$proposal <- trimws(tolower(df_choices_pre$proposal))
    nrow(df_choices_pre)


    df_choices_first_screen <- read.csv(
      requireExistingInputPath(
        debugDataPath("choices_augmented_2025b_dup_excluded.csv"),
        "stage 1 candidate output"
      ),
      colClasses = c(proposal = "character")
    )
    df_choices_first_screen$proposal <- trimws(tolower(df_choices_first_screen$proposal))
    nrow(df_choices_first_screen)


    df_choices_after_stance <- read.csv(
      requireExistingInputPath(
        debugDataPath("choices_25-nov-17_B_dup_excluded.csv"),
        "stage 2 candidate output"
      ),
      colClasses = c(proposal = "character")
    )
    df_choices_after_stance$proposal <- trimws(tolower(df_choices_after_stance$proposal))
    nrow(df_choices_after_stance)


    ## The actual final dataset.
    nrow(df_choices_full)
    nrow(df_choices_gt1vote)
    nrow(df_choices_multichoice)
    nrow(df_choices_multichoice_gt1vote)


    ## There should not be proposals with only 1 choice
    df_choices1 <- df_choices_full[df_choices_full$N_choices == 1,]
    nrow(df_choices1) # 0


    df_proposals_unique_spaces <- df_proposals[!duplicated(df_proposals$space),]
    nrow(df_proposals_unique_spaces)


    df_all_unique_spaces <- df_choices_all[!duplicated(df_choices_all$space),]
    nrow(df_all_unique_spaces)


    ## ALL
    table(df_choices_all$paper)

    #    0      1
    # 225647 429035

    table(df_all_unique_spaces$paper)


    #     0     1
    # 26947  6048



    ## FINAL

    table(df_choices$paper)

    #    0      1
    # 225647 429035

    df_unique_spaces <- df_choices[!duplicated(df_choices$space),]
    table(df_unique_spaces$paper)

    #    0     1
    # 13854  5347




    df_eliminated_spaces <- df_all_unique_spaces[!df_all_unique_spaces$space %in% df_unique_spaces$space,]
    nrow(df_eliminated_spaces) # 18777

    table(df_eliminated_spaces$paper)
    #     0     1
    # 13093   701

    table(df_eliminated_spaces$paper) / nrow(df_eliminated_spaces)
    #         0         1
    # 0.9491808 0.0508192




    mydata <- df_choices

    ## Percent and N are identical for Voting Power and Number of votes (same
    ## denominator), so we deduplicate them into a single trailing pair of columns,
    ## matching the hand-crafted reference table format.
    ##
    ## Strategy: pull mean/SD for both vars + Percent/N once as a dataframe,
    ## then render a tblr table directly.

    ds_df <- datasummary(
      (`Choice Position` = choice_idx10) ~
        ((`Voting Power` = choice_share_2) + (`Number of votes` = n_votes_share_2)) *
          (Mean + SD) +
        (`_pct_n` = choice_share_2) * (Percent() + N),
      data = mydata,
      output = "dataframe"
    )

    ## Columns: row label | VP Mean | VP SD | Votes Mean | Votes SD | Pct (Percent) | N
    names(ds_df) <- c("pos", "vp_mean", "vp_sd", "nv_mean", "nv_sd", "pct", "n")

    fmt2  <- function(x) sprintf("\\num{%.2f}", as.numeric(x))
    fmt_n <- function(x) formatC(as.integer(as.numeric(x)), format = "d", big.mark = ",")
    fmt_p <- function(x) sprintf("\\num{%.2f}", as.numeric(x))

    rows <- apply(ds_df, 1, function(r) {
      paste0(
        r["pos"], " & ",
        fmt2(r["vp_mean"]), " & ", fmt2(r["vp_sd"]), " & ",
        fmt2(r["nv_mean"]), " & ", fmt2(r["nv_sd"]), " & ",
        fmt_p(r["pct"]), " & ",
        fmt_n(r["n"]), " \\\\"
      )
    })

    cat(
    "\\begin{table}\n",
    "\\centering\n",
    "\\begin{tblr}{\n",
    "colspec={\n",
    "Q[l]\n",
    "Q[r, colsep=6pt]\n",
    "Q[r, colsep=6pt]\n",
    "Q[r, colsep=20pt]\n",
    "Q[r, colsep=6pt]\n",
    "Q[r, colsep=10pt]\n",
    "Q[r]\n",
    "},\n",
    "hline{1}={1-7}{solid, black, 0.1em},\n",
    "hline{2}={2-3}{solid, black, 0.05em},\n",
    "hline{2}={4-5}{solid, black, 0.05em},\n",
    "hline{3}={1-7}{solid, black, 0.05em},\n",
    paste0("hline{", nrow(ds_df) + 3, "}={1-7}{solid, black, 0.1em},\n"),
    "cell{1}{2}={c=2}{halign=c},\n",
    "cell{1}{4}={c=2}{halign=c},\n",
    "}\n",
    "& Voting Power &  & Number of votes &  & Percent & N \\\\\n",
    "Choice Pos. & Mean & SD & Mean & SD &  &  \\\\\n",
    paste(rows, collapse = "\n"), "\n",
    "\\end{tblr}\n",
    "\\end{table}\n",
    sep = ""
    )



    ## Correlation between votes and voting power.
    aa <- df_choices
    aa <- aa[!duplicated(df_choices$proposal),]
    corr.test(aa$choice_share_2, aa$n_votes_share_2)



    mydata <- df_choices

    datasummary(
      # continent *
       (`Choice Position` = choice_idx10) ~
          # (
        # (`Voting Power` = choice_share_2)
        # # (`Number of votes` = n_votes_share_2) +
        # ) * (Mean + SD + Percent() + N)
        # +
       (
        (`Approve` = is_infavor) +
        (`Reject` = is_against) +
        (`Abstain` = is_abstain) +
        (`Other` = is_other)
        ) * (Mean + SD )
         +
        (`All` = 1) * (Percent() + N)
        # +
        # (`Author Choice` =  is_author_choice2) * (Mean + SD)
      , mydata,
        # output = "gt"
        output = "latex"
        # output = "main_vars_by_nft_and_continent.html"
      )




    # mydata <- df_choices
    # mydata <- df_choices[df_choices$choice_idx %in% c(1),]
    mydata <- df_choices[df_choices$choice_idx %in% c(1) &
                           df_choices$N_choices %in% c(2,3)
                           ,]


    mydata$rnd_share = 1 / mydata$N_choices
    mydata$excess_share = mydata$choice_share_2 - mydata$rnd_share


    datasummary(
      # type *
       # (`Choice` = choice_idx10) ~
       (`Type` = type) ~
         # (
        # (`Voting Power` = choice_share_2) +
        # (`Excess Voting Power` = choice_share_2) +

        # (`Approve` = is_infavor)
        # (`Reject` = is_against) +
        # (`Abstain` = is_abstain)
        # (`Other` = is_other)
        # ) * (Mean + SD + Percent())
        # +
         # (`Other` = is_other) * (Mean + SD + Percent() + N)
        # (`Number of votes` = n_votes_share_2) * (Mean + SD + Percent() + N)
        # (`Author Choice` =  is_author_choice) * (Mean + SD + Percent() + N)
          (`Excess Voting Power` = excess_share)* (Mean + SD + Percent() + N)
      , mydata,
        output = "gt"
      # output = "latex",
        # output = "file.html"
      )






    ## Note: It was here, not used? Commented out.

    # # Count the number of unique subgroups in each main group
    # subgroup_counts <- aggregate(proposal ~ space, data = mydata, function(x) length(unique(x)))
    #
    # prop_per_space = summarySE(subgroup_counts, "proposal")
    #
    #
    # # Count the number of unique subgroups in each main group
    # subgroup_counts <- aggregate(choice_idx ~ proposal, data = mydata, function(x) length(unique(x)))
    #
    #
    # choice_per_proposal = summarySE(subgroup_counts, "choice_idx")


    mydata <- df_choices
    ## Only 2-3 choices proposals.
    # mydata <- df_choices[df_choices$N_choices %in% c(2,3),]


    datasummary(
      (`Choice` = choice_idx10) ~
      (
        (`Voting Power` = choice_share_2) +
        (`Approve` = is_infavor)
      ) * (Mean + SD)
      +
      (`Author Choice` = is_author_choice2) * (Mean + SD)
      +
      (`All` = 1) * (Percent() + N),
      mydata,
      # output = "gt"
      output = "latex"
    )



    mydata <- df_choices[df_choices$choice_idx == 1,]


    datasummary(
      type ~
        (`Voting Power` = choice_share_2) * (Mean + SD) +
        (`Approve` = is_infavor) * (Mean + SD) +
        (`Author Choice` = is_author_choice2) * (Mean + SD) +
        1 * (Percent() + N),
      data = mydata,
      output = "latex"
    )

  }),envir,report=file.path(context$OUTPUT_ROOT,"reports","descriptive.txt"))
  invisible(envir)
}
