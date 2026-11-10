# Extracted analytical implementation; execution is explicit.
prepare_stance <- function(context) {
  envir <- new_analysis_environment(context)
  evalq({
    df_proposals <-  read.csv(rawDataPath('proposals_0.csv.gz'), colClasses = c(id = "character"))
    # write.csv(df_proposals, 'data/2024/proposals_0.csv', row.names=FALSE)
    nrow(df_proposals)


    stage1_candidate_input <- debugDataPath("choices_augmented_2025b_dup_excluded.csv")
    stage2_candidate_output <- debugDataPath("choices_25-nov-17_B_dup_excluded_m1_instrumented.csv")
    stage2_stage <- "2_stance_v5"
    df_choices <- read.csv(requireExistingInputPath(stage1_candidate_input, "stage 1 candidate output"), colClasses = c(proposal = "character"))
    df_choices$proposal = tolower(df_choices$proposal)
    df_choices$type = factor(df_choices$type, levels = c(
        "single-choice", "basic", "quadratic", "weighted",
        "ranked-choice", "approval"
    ))

    nrow(df_choices[is.na(df_choices$choice_text),]) ## no more NAs

    df_choices$proposal <- trimws(tolower(df_choices$proposal))

    df_choices <- dropSingleOptionProposals(df_choices)
    nrow(df_choices)

    df_choices$body_is_zero <- ifelse(df_choices$body_mod_len == 0, 1, 0)

    table(df_choices$body_is_zero)

    df_choices$prop_short <- str_extract(df_choices$proposal, "^.{6}")

    nrow(df_choices)
    # [1] 423048



    ## Proposal-specific exclusions now live in metadata/exceptions/proposal_exclusions.csv
    nrow(df_choices)
    df_choices <- applyProposalExclusions(
      df_choices,
      stage = stage2_stage
    )
    nrow(df_choices)

    ## Space-level stage-2 exclusions now live in metadata/exceptions/space_exclusions.csv
    df_choices <- applySpaceExclusions(
      df_choices,
      stage = stage2_stage
    )
    nrow(df_choices)


    ## Load stance detection patterns from external file
    ## This significantly improves RStudio performance by avoiding massive inline pattern generation

    source(file.path(PACKAGE_ROOT,"src/r/preparation/stance_patterns.R"),local=TRUE)

    # Verify patterns are loaded
    cat("Loaded pattern counts:\n")
    cat("  REJECT exact:", length(nos), "\n")
    cat("  REJECT begins:", length(nos_begins), "\n")
    cat("  REJECT contains:", length(nos_contains), "\n")
    cat("  APPROVE exact:", length(yess), "\n")
    cat("  APPROVE begins:", length(yess_begins), "\n")
    cat("  APPROVE contains:", length(yess_contains), "\n")
    cat("  ABSTAIN exact:", length(abstains), "\n")




    ## Callback function for stance detection. The instrumented helper uses the
    ## same priority order as the prior callback and adds match metadata for audits.
    stance_cb <- function(row, verbose=FALSE) {
      classifyStanceChoice(row, verbose = verbose)
    }

    raw_heuristic_match_audit <- classifyStanceChoicesInstrumented(df_choices)
    df_choices$stance2 <- raw_heuristic_match_audit$resolved_stance

    table(df_choices$stance2)

    # ABSTAIN APPROVE   OTHER  REJECT
    #   16364   62559  364646   64425



    raw_choice_audit <- buildStanceChoiceMatchAudit(
      df_choices,
      raw_heuristic_match_audit,
      stage = stage2_stage,
      checkpoint = "raw_heuristic"
    )
    raw_proposal_audit <- buildStanceProposalStateAudit(
      df_choices,
      stage = stage2_stage,
      checkpoint = "raw_heuristic"
    )

    writeCandidateCsv(
      raw_choice_audit,
      debugDataPath("stance_audit_raw_heuristic_choice_matches.csv"),
      row.names=FALSE
    )
    writeCandidateCsv(
      raw_proposal_audit,
      debugDataPath("stance_audit_raw_heuristic_proposal_state.csv"),
      row.names=FALSE
    )



    # df_choices_orig <- read.csv('data/2024/choices_lang_len_fake_paper.csv.gz')
    # df_choices_orig[trimws(tolower(df_choices_orig$proposal)) == trimws(tolower(p)),]
    #
    # p <- "QmQE89PaNkTeS111PqwuNHAoq8TwgiucYZ2FxKAjCsec3x"
    # df_choices[trimws(tolower(df_choices$proposal)) == trimws(tolower(p)),]$title
    # p <- "Qmd1zkgLEhQwUkY5oAQxznb2U8ZDkcmRpVBCZHjafvoD69"
    # df_choices[trimws(tolower(df_choices$proposal)) == trimws(tolower(p)),]$title
    #
    # unique(df_choices[df_choices$space == "gnosis.eth",]$title)
    #
    # df_proposals[df_proposals$title == "Extend GNO Token Lock by another 2 weeks for GIP29",]$scores
    #
    # df_proposals[trimws(tolower(df_proposals$id)) == trimws(tolower(p)),]
    #
    #
    # df_miss <- read.csv("data/2024/proposals/QmQE89PaNkTeS111PqwuNHAoq8TwgiucYZ2FxKAjCsec3x_votes.csv", header=FALSE, stringsAsFactors = TRUE, colClasses = c(V4 = "character"))
    # # df_miss$voter <- as.character(df_miss$voter)
    # # df_miss$V4 <- as.character(df_miss$V4)
    # df_miss
    #
    # p <- "QmQE89PaNkTeS111PqwuNHAoq8TwgiucYZ2FxKAjCsec3x"
    # author_prop = df_proposals[trimws(tolower(df_proposals$id)) == trimws(tolower(p)),]$author
    # author_prop
    #
    # author_prop %in% df_miss$voter ## NOT IN THERE.


    ## Proposal-specific stance corrections now live in
    ## metadata/exceptions/stance_choice_overrides.csv and are applied in one pass
    ## near the end of the notebook.






    # bb1 <- df_choices[grepl("Do not ", df_choices$choice_text),]
    # nrow(bb1) ## 365
    #
    #
    # bb2 <- df_choices[grepl("Don't ", df_choices$choice_text),]
    # nrow(bb2) ## 259
    #
    # bb <- rbind(bb1, bb2)
    # nrow(bb) # 624

    # bb <- df_choices[df_choices$proposal %in% bb$proposal,]
    # nrow(df_donts)


    donts <- c("dont")
    pattern <- paste0("^(", paste(donts, collapse="|"), ")")

    donts <- c("do not", "don't")
    pattern <- paste0("^(", paste(donts, collapse="|"), ")")

    bb <- df_choices[grepl(pattern, tolower(df_choices$choice_text)),]
    nrow(bb) # 772


    df_donts <- df_choices[df_choices$proposal %in% bb$proposal,]
    nrow(df_donts) # 2177

    # aa <- df_choices[df_choices$proposal %in% df_donts$proposal,
    #                  c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)

    manual_stage_overrides <- loadExceptionTable(
      "stance_choice_overrides.csv",
      c("stage", "proposal", "stance_sequence", "source_line", "note")
    )
    manual_override_proposals <- unique(
      trimws(tolower(
        manual_stage_overrides$proposal[
          manual_stage_overrides$stage == "2_stance_v5" &
            nzchar(trimws(manual_stage_overrides$proposal))
        ]
      ))
    )







    bb <- df_choices[grepl(pattern, tolower(df_choices$choice_text)),]
    nrow(bb) # 762


    df_donts <- df_choices[df_choices$proposal %in% bb$proposal,]
    nrow(df_donts) # 2153


    df_to_fix = ddply(df_donts, "proposal", function(x) {
        # print(x$proposal[1])
        # print(x$choice_text)
        # print(x$stance2)
        rej <- sum(x$stance2 == "REJECT")
        app <- sum(x$stance2 == "APPROVE")

        if (rej > 0 && app == 0) {
          # print(paste0('====>  ', x$title[1]))
          # print(x$choice_text)
          # print(x$stance2)
          # print(paste0('////  ', x$body_is_zero[1]))
          # print("")
          return(1)
        }
        return(0)
    })


    prop_to_fix = df_to_fix[df_to_fix$V1 == 1,]$proposal
    df_to_fix = df_choices[df_choices$proposal %in% prop_to_fix,]
    nrow(df_to_fix) # 1064

    table(df_choices$stance2)
    df_choices[df_choices$proposal %in% df_donts$proposal &
                 !(df_choices$proposal %in% manual_override_proposals) &
                  df_choices$N_choices > 2 & df_choices$stance2 == "OTHER",]$stance2 = "APPROVE"
    table(df_choices$stance2)

    # ABSTAIN APPROVE   OTHER  REJECT
    #   15044   60576  288491   58745





    bb <- df_choices[grepl(pattern, tolower(df_choices$choice_text)),]
    nrow(bb) # 762


    df_donts <- df_choices[df_choices$proposal %in% bb$proposal,]
    nrow(df_donts) # 2153

    df_to_fix = ddply(df_donts, "proposal", function(x) {
        # print(x$proposal[1])
        # print(x$choice_text)
        # print(x$stance2)
        rej <- sum(x$stance2 == "REJECT")
        app <- sum(x$stance2 == "APPROVE")

        if (rej > 0 && app == 0) {
          # print(paste0('====>  ', x$title[1]))
          # print(x$choice_text)
          # print(x$stance2)
          # print(paste0('////  ', x$body_is_zero[1]))
          # print("")
          return(1)
        }
        return(0)
    })


    prop_to_fix = df_to_fix[df_to_fix$V1 == 1,]$proposal
    df_to_fix = df_choices[df_choices$proposal %in% prop_to_fix,]
    nrow(df_to_fix) # 3

    ## There is an issue with non-native: just two choices.
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "non-native"),]
    nrow(bb) # 2






    ## "Invalid question/options"
    #############################


    invalid_question_options <- !is.na(df_choices$choice_text) &
      df_choices$choice_text == "Invalid question/options"

    bb <- df_choices[invalid_question_options,]
    nrow(bb) ## 844

    ## Always at least 3 options.
    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices")]
    # print(aa, n=100)

    df_choices[invalid_question_options,]$stance2 = "ABSTAIN"


    ## More discussion needed.
    ##########################

    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "more discussion"),]
    nrow(bb) ## 5

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## further discussion
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "further discussion"),]
    nrow(bb) ## 7

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## further research
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "further research"),]
    nrow(bb) ## 3

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)



    ## I'm not sure
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "i'm not sure"),]
    nrow(bb) ## 6

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## Make it a regular vote
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "make it a regular"),]
    nrow(bb) ## 6

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## Require more info
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "require more info"),]
    nrow(bb) ## 5

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)

    ## Require more information
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "require more information"),]
    nrow(bb) ## 4

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## Need more info
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "need more info"),]
    nrow(bb) ## 28

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)

    ## All good.


    ## Need more details
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "need more details"),]
    nrow(bb) ## 5

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)

    ## All good.


    ## Needs more information
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), "needs more information"),]
    nrow(bb) ## 20

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)

    ## All good.

    ## Skip (could be too restrictive)
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "skip"),]
    nrow(bb) ## 149

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## idk
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "idk"),]
    nrow(bb) ## 16

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)



    ## "Both above / Both below / Both"
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "both"),]
    nrow(bb) ## 146

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## None
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "none"),]
    nrow(bb) ## 72

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)



    ## Neither
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "neither"),]
    nrow(bb) ## 40

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)



    ## other
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "other"),]
    nrow(bb) ## 112

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## All of the above
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "all of the above"),]
    nrow(bb) ## 21

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)


    ## All the above
    bb <- df_choices[str_starts(tolower(df_choices$choice_text), "all the above"),]
    nrow(bb) ## 6

    aa <- df_choices[df_choices$proposal %in% bb$proposal,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=100)




    ## Multiple choice question.
    multiple_choice_question <- !is.na(df_choices$choice_text) &
      str_starts(tolower(df_choices$choice_text), "multiple choice question")

    bb <- df_choices[multiple_choice_question,]
    nrow(bb) ## 0

    multiple_choice_question_proposals <- df_choices$proposal %in% bb$proposal

    aa <- df_choices[multiple_choice_question_proposals,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    print(aa, n=100)

    ## All OTHER.
    df_choices$stance2[multiple_choice_question_proposals] <- "OTHER"


    ## This is a poll, there is not
    this_is_a_poll <- !is.na(df_choices$choice_text) &
      str_starts(tolower(df_choices$choice_text), "this is a poll, there is not")

    bb <- df_choices[this_is_a_poll,]
    nrow(bb) ## 0

    this_is_a_poll_proposals <- df_choices$proposal %in% bb$proposal

    aa <- df_choices[this_is_a_poll_proposals,
                     c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    print(aa, n=100)

    ## All OTHER.
    df_choices$stance2[this_is_a_poll_proposals] <- "OTHER"




    ## Proposal-specific keyword exceptions are maintained through the metadata
    ## override tables, so this chunk only keeps the exploratory checks.


    ## Check with only 2 choices that are not accept/reject

    aa <- df_choices[df_choices$body_is_zero == 0,]

    df_counts = ddply(aa, "proposal", function(x) {
      nrow(x)
    })

    p2 <- df_counts[df_counts$V1 == 2,]$proposal

    aa <- df_choices[df_choices$proposal %in% p2,]
    nrow(aa)

    ## Check if both are other

    df_two_other = ddply(aa, "proposal", function(x) {
        # print(x$proposal[1])
        # print(x$choice_text)
        # print(x$stance2)
        ot <- sum(x$stance2 == "OTHER")

        if (ot == 2) {
          # print(paste0('====>  ', x$title[1]))
          # print(x$choice_text)
          # print(x$stance2)
          # print(paste0('////  ', x$body_is_zero[1]))
          # print("")
          return(1)
        }
        return(0)
    })


    prop_to_fix = df_two_other[df_two_other$V1 == 1,]$proposal
    df_to_fix = df_choices[df_choices$proposal %in% prop_to_fix,]
    nrow(df_to_fix) # 1040

    aa <- df_to_fix[, c("prop_short", "choice_text", "stance2", "N_choices", "body_is_zero")]
    # print(aa, n=200)



    nrow(df_choices) # 340614




    ## Check others

    pattern <- tolower("Abstenção")
    pattern <- tolower("Mi astengo")
    pattern <- tolower("n/a")
    pattern <- tolower("no opinion")
    pattern <- tolower("Aprovo")
    pattern <- tolower("Aprovar")
    pattern <- tolower("Abster")
    pattern <- tolower("no opinion")

    ## CHECK
    pattern <- tolower("me abstengo")
    pattern <- tolower("Je ne sais pas")
    pattern <- tolower("je m'abstiens")
    pattern <- tolower("No objection")
    pattern <- tolower("赞同")
    pattern <- tolower("with")


     # TODO: Temp Check, Temperature Check
     # author: realism
     # rabbithole


    aa <- df_choices[
      df_choices$body_is_zero == 0 &
      # df_choices$stance2 == "OTHER" &
      tolower(df_choices$choice_text) == pattern,]
    nrow(aa)

    aa$choice_text
    aa$stance2


    aa <- df_choices[
      df_choices$body_is_zero == 0 &
      # df_choices$stance2 == "OTHER" &
      str_starts(tolower(df_choices$choice_text), pattern),]

    aa$choice_text


    aa <- df_choices[
      df_choices$body_is_zero == 0 &
      # df_choices$stance2 == "OTHER" &
      str_detect(tolower(df_choices$choice_text), pattern),]

    aa$choice_text

    bb <- df_choices[df_choices$proposal %in% aa$proposal,]
    nrow(bb)

    bb[,c("choice_text", "stance2", "title", "body_is_zero", "space", "proposal")]


    ## "je m'abstiens", "赞同", and similar proposal-level exceptions are applied
    ## through the documented metadata override pass below.




    # aa <- table(str_trim(tolower(df_choices$choice_text)))
    #
    # aa <- as.data.frame(aa)
    #
    # aa <- aa[aa$Freq >= 10,]
    # print(aa[order(aa$Freq, decreasing = TRUE),], n=100)
    #
    #
    # aa <- table(str_trim(tolower(df_choices$choice_text)), df_choices$choice_idx)
    # aa <- as.data.frame(aa)
    # aa <- aa[aa$Freq >= 10,]
    #
    # aa$Var2 <- as.numeric(aa$Var2)
    # aa <- aa[aa$Var2 <= 5,]
    # print(aa[order(aa$Freq, decreasing = TRUE),], n=100)
    #
    # require(modelsummary)
    # # datasummary(aa)
    #
    # df_choices$choice_text2 <- str_trim(tolower(df_choices$choice_text))
    # cols <- c("choice_idx", "choice_text2")
    # md <- df_choices[,cols]
    # md <- md[md$choice_idx <= 5,]
    #
    # datasummary_balance(choice_text2 ~ choice_idx, md)
    #
    # datasummary_skim(aa)
    #
    # library(xtable)
    #
    # cols <- c("choice_idx", "choice_text2")
    # md <- df_choices[,cols]
    # md <- md[md$choice_idx <= 5,]
    #
    # tab <- table(md$choice_text2, md$choice_idx)
    #
    #
    # tab <- as.data.frame(tab)
    # print(tab[order(aa$Freq, decreasing = TRUE),], n=100)
    #
    #
    # t1 = xtable(tab, caption = "Male", digits = 0)
    #
    # print(t1, caption.placement = "top", comment = FALSE)
    #
    # cols <- c("choice_idx", "choice_text2")
    # md <- df_choices[,cols]
    # md <- md[md$choice_idx <= 3,]
    #
    # datasummary_crosstab(choice_text2 ~ choice_idx, data = md[1:1000,])





    ## Final.


    prop_imbalance <- ddply(df_choices, "proposal", function(x) {
      rej <- sum(x$stance2 == "REJECT")
      app <- sum(x$stance2 == "APPROVE")
      abs <- sum(x$stance2 == "ABSTAIN")

      if (app >= 1 && rej == 0 && abs == 0) return("app_alone")
      if (app >= 1 && rej == 0 && abs > 0) return("app_abs")
      if (rej >= 1 && app == 0 && abs == 0) return("rej_alone")
      if (rej >= 1 && app == 0 && abs > 0) return("rej_abs")
      if (rej == 0 && app == 0) return("no_rej_no_app")

      return("app_rej")
    })


    nrow(prop_imbalance[prop_imbalance$V1 == "app_alone",]) / nrow(prop_imbalance)
    nrow(prop_imbalance[prop_imbalance$V1 == "app_abs",]) / nrow(prop_imbalance)
    nrow(prop_imbalance[prop_imbalance$V1 == "rej_alone",]) / nrow(prop_imbalance)
    nrow(prop_imbalance[prop_imbalance$V1 == "rej_abs",]) / nrow(prop_imbalance)
    nrow(prop_imbalance[prop_imbalance$V1 == "no_rej_no_app",]) / nrow(prop_imbalance)
    nrow(prop_imbalance[prop_imbalance$V1 == "app_rej",]) / nrow(prop_imbalance)


    # [1] 0.007296927
    # [1] 0.001313447
    # [1] 0.002870124
    # [1] 0.002344746
    # [1] 0.4900713
    # [1] 0.4961034






    aa <- df_choices[df_choices$proposal %in% prop_imbalance[prop_imbalance$V1 == "app_alone",]$proposal,]
    nrow(aa)

    # print(aa[, c("choice_text", "stance2", "space",
    #              # "title"
    #              "body_is_zero",
    #              "proposal"
    #              )], n=100)

    ## not a pattern
    # c <- "nao"
    # bb <- df_choices[str_starts(tolower(df_choices$choice_text), c),]
    # nrow(bb)

    # aa$proposal


    ## At least 2 choices to be approve/reject

    low_choice_count <- !is.na(df_choices$N_choices) & df_choices$N_choices < 2
    df_choices$stance2[low_choice_count] <- "OTHER"

    ## not a pattern
    # c <- "recusar"
    # bb <- df_choices[str_starts(tolower(df_choices$choice_text), c),]
    # nrow(bb)




    aa <- df_choices[df_choices$proposal %in% prop_imbalance[prop_imbalance$V1 == "rej_alone",]$proposal,]
    nrow(aa)

    # aa$choice_text
    # aa$stance2
    # aa$proposal
    #
    # aa$title
    # aa$body_is_zero
    ## Remaining proposal-level exceptions are applied once below from
    ## metadata/exceptions/stance_choice_overrides.csv.

    c <- ' <---> '
    bb <- df_choices[str_detect(tolower(df_choices$choice_text), tolower(c)),]
    nrow(bb)





    post_notebook_choice_audit <- buildStanceChoiceMatchAudit(
      df_choices,
      raw_heuristic_match_audit,
      stage = stage2_stage,
      checkpoint = "post_notebook_rules"
    )
    post_notebook_proposal_audit <- buildStanceProposalStateAudit(
      df_choices,
      stage = stage2_stage,
      checkpoint = "post_notebook_rules"
    )

    writeCandidateCsv(
      post_notebook_choice_audit,
      debugDataPath("stance_audit_post_notebook_rules_choice_matches.csv"),
      row.names=FALSE
    )
    writeCandidateCsv(
      post_notebook_proposal_audit,
      debugDataPath("stance_audit_post_notebook_rules_proposal_state.csv"),
      row.names=FALSE
    )


    ## Whole-space exclusions, whole-proposal overrides, per-proposal stance sequences,
    ## and final stance normalization now live in documented metadata / helper code.
    ## Preserve rows with absent raw labels while making the final handoff explicit.
    df_choices$choice_text <- as.character(df_choices$choice_text)
    choice_text_trimmed <- trimws(df_choices$choice_text)
    missing_choice_text <- is.na(df_choices$choice_text) |
      is.na(choice_text_trimmed) |
      choice_text_trimmed == ""
    nrow(df_choices[missing_choice_text,]) ## 40
    df_choices$choice_text[missing_choice_text] <- "[missing choice text]"

    df_choices <- applyStage2StanceOverrides(df_choices, stage = stage2_stage)

    df_choices$is_infavor2 <- ifelse(df_choices$stance2 %in% ("APPROVE"), 1, 0)
    df_choices$is_against2 <- ifelse(df_choices$stance2 %in% ("REJECT"), 1, 0)
    df_choices$is_abstain2 <- ifelse(df_choices$stance2 %in% ("ABSTAIN"), 1, 0)
    df_choices$is_other2 <- ifelse(df_choices$stance2 %in% ("OTHER"), 1, 0)


    final_choice_audit <- buildStanceChoiceMatchAudit(
      df_choices,
      raw_heuristic_match_audit,
      stage = stage2_stage,
      checkpoint = "final_after_overrides"
    )
    final_proposal_audit <- buildStanceProposalStateAudit(
      df_choices,
      stage = stage2_stage,
      checkpoint = "final_after_overrides"
    )

    writeCandidateCsv(
      final_choice_audit,
      debugDataPath("stance_audit_final_after_overrides_choice_matches.csv"),
      row.names=FALSE
    )
    writeCandidateCsv(
      final_proposal_audit,
      debugDataPath("stance_audit_final_after_overrides_proposal_state.csv"),
      row.names=FALSE
    )



    ## Safe reanalysis writes candidate output only; the legacy stage-2 artifacts stay unchanged.
    writeCandidateCsv(df_choices, stage2_candidate_output, row.names=FALSE)
  }, envir)
  invisible(envir)
}
