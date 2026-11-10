# Extracted analytical implementation; execution is explicit.
prepare_dataset <- function(context) {
  envir <- new_analysis_environment(context)
  evalq({
    df_proposals <-  read.csv(rawDataPath('proposals_0.csv.gz'), colClasses = c(id = "character"))
    # write.csv(df_proposals, 'data/2024/proposals_0.csv', row.names=FALSE)
    nrow(df_proposals)


    strict_fake_ids <- getFakeProposalIds(strict_only = TRUE)
    stage1_candidate_output <- debugDataPath("choices_augmented_2025b_dup_excluded.csv")

    df_props_strict_fake = df_proposals[trimws(tolower(df_proposals$id)) %in% strict_fake_ids,]
    nrow(df_props_strict_fake)

    ## Fake/test exclusions stay separate from generic stage exclusions.
    ## Proposal vote-issue exclusions are available through applyProposalExclusions()
    ## but are not enabled by default.

    nrow(df_proposals)
    df_proposals <- applyFakeProposalExclusions(
      df_proposals,
      proposal_col = "id",
      strict_only = TRUE
    )
    nrow(df_proposals)

    ## Important! Otherwise matching is weird.
    df_proposals$id = trimws(tolower(df_proposals$id))
    nrow(df_proposals)
    df_proposals <- applyProposalExclusions(
      df_proposals,
      stage = "1_pos_bias_data_preparation_v3",
      proposal_col = "id"
    )
    nrow(df_proposals)

    ## FINAL PROPOSALS
    ##################
    df_props_final = df_proposals[df_proposals$scores_state == "final",]
    nrow(df_props_final) ## 169672


    ## NO VOTES PROPOSALS
    #####################
    df_props_no_votes <- df_proposals %>%
      filter(grepl("^\\[0*(,\\s*0*)*\\]$", scores))
    nrow(df_props_no_votes) ## 29913


    df_proposals$no_votes <- ifelse(df_proposals$id %in% df_props_no_votes$id, 1, 0)

    df_proposals_orig = df_proposals




    stage0_choices_snapshot <- ensureCandidateFileCopy(
      activeDataPath('choices_lang_len_fake_paper.csv.gz'),
      debugDataPath("choices_lang_len_fake_paper_stage0_snapshot.csv.gz"),
      "pre-stage choices snapshot"
    )

    df_choices <- read.csv(stage0_choices_snapshot, colClasses = c(proposal = "character", author = "character"))


    df_choices$proposal = trimws(tolower(df_choices$proposal))

    df_choices$type = factor(df_choices$type, levels = c(
      "single-choice", "approval", "basic", "quadratic", "ranked-choice", "weighted"
    ))
    nrow(df_choices) # 538504

    # ## Remove fake/test proposals first, then generic stage exclusions.
    df_choices <- applyFakeProposalExclusions(
      df_choices,
      strict_only = TRUE
    )
    nrow(df_choices)

    df_choices <- applyProposalExclusions(
      df_choices,
      stage = "1_pos_bias_data_preparation_v3"
    )
    nrow(df_choices)

    df_choices_orig = df_choices



    assert_unique_key <- function(df, key, df_name) {
      dup_n <- sum(duplicated(df[[key]]))
      if (dup_n > 0) {
        stop(paste0(df_name, " has duplicated values for key ", key, ": ", dup_n))
      }
    }

    audit_left_join <- function(left_df, right_df, by, left_name, right_name) {
      before_n <- nrow(left_df)
      missing_rows <- anti_join(left_df, right_df, by = by)
      joined_df <- left_join(left_df, right_df, by = by)
      after_n <- nrow(joined_df)

      cat(
        paste0(
          "[JOIN AUDIT] ", left_name, " <- ", right_name,
          " | before=", before_n,
          " after=", after_n,
          " unmatched=", nrow(missing_rows), "\n"
        )
      )

      if (after_n != before_n) {
        stop(paste0(
          "Row count changed while joining ", right_name,
          " into ", left_name, ". Check the right-hand key uniqueness."
        ))
      }

      joined_df
    }





    df_choices <- addStage1ChoiceShare(df_choices)
    nrow(df_choices)

    ## Quorum stats

    quorum_stats <- function() {
      df_proposals$has_quorum <- factor(ifelse(df_proposals$quorum > 0, 1, 0))

      df_proposals$relative_quorum <- ifelse(df_proposals$scores_total == 0, 0,
                                             df_proposals$quorum / df_proposals$scores_total)


      sum_scores_total <- summarySE(df_proposals, "scores_total", "space")
      sum_scores_total
      colnames(sum_scores_total)[3] <- "scores_total_avg"


      df_proposals <- merge(df_proposals, sum_scores_total[, c("space", "scores_total_avg")])

      df_proposals$relative_avg_quorum <- ifelse(df_proposals$scores_total_avg == 0, 0,
                                                 df_proposals$quorum / df_proposals$scores_total_avg)


      df_prop_quorum <- df_proposals[,c("id", "quorum", "has_quorum", "relative_quorum", "relative_avg_quorum")]

    }


    df_quorum_stats = quorum_stats()
    colnames(df_quorum_stats)[1] <- "proposal"

    nrow(df_choices) # 538422
    df_choices = merge(df_choices, df_quorum_stats, by="proposal")
    nrow(df_choices) # 538422




    ## Add number of choices. Count choice rows, not non-missing choice shares:
    ## zero-vote proposals have undefined shares but still have choices.
    df_choices <- addStage1ChoiceCounts(df_choices)
    nrow(df_choices)

    ## One-option proposals do not contribute to choice-position comparisons.
    df_choices <- dropSingleOptionProposals(df_choices)
    nrow(df_choices)


    ## Choice len stats

    mysm_choice_len <- summarySE(df_choices, "choice_text_len", "proposal")
    colnames(df_quorum_stats)[1] <- "proposal"

    mysm_choice_len <- mysm_choice_len[,c("proposal", "choice_text_len", "sd")]
    colnames(mysm_choice_len) <- c("proposal", "choice_text_len_avg", "choice_text_len_sd")

    df_choices = merge(df_choices, mysm_choice_len, by="proposal")
    df_choices$choice_text_len_rel = df_choices$choice_text_len / df_choices$choice_text_len_avg

    nrow(df_choices) # 538422




    ## Save a copy
    # df_choices_orig = df_choices
    # df_choices = df_choices_orig

    # ## Add spaces settings

    df_spaces_settings = read.csv(requireExistingInputPath(activeDataPath("spaces_settings.csv.gz")))
    colnames(df_spaces_settings)[1] <- "space"
    assert_unique_key(df_spaces_settings, "space", "df_spaces_settings")
    df_choices <- audit_left_join(df_choices, df_spaces_settings, "space", "df_choices", "df_spaces_settings")

    nrow(df_choices) # 538411


    ## Add spaces delegates

    df_delegates = read.csv(requireExistingInputPath(activeDataPath('proposals_delegates.csv.gz')), colClasses = c(id = "character"))
    colnames(df_delegates)[1] <- "proposal"
    df_delegates$proposal = trimws(tolower(df_delegates$proposal))
    df_delegates = df_delegates[df_delegates$proposal %in% df_choices$proposal,]
    assert_unique_key(df_delegates, "proposal", "df_delegates")

    df_choices <- audit_left_join(df_choices, df_delegates, "proposal", "df_choices", "df_delegates")
    nrow(df_choices) # 538411




    df_prop_dates = read.csv(requireExistingInputPath(activeDataPath('proposals_dates.csv.gz')), colClasses = c(id = "character"))
    colnames(df_prop_dates)[1] = "proposal"
    nrow(df_prop_dates) # 223760
    df_prop_dates$proposal = trimws(tolower(df_prop_dates$proposal))
    df_prop_dates = df_prop_dates[df_prop_dates$proposal %in% df_choices$proposal,]
    assert_unique_key(df_prop_dates, "proposal", "df_prop_dates")

    df_choices <- audit_left_join(df_choices, df_prop_dates, "proposal", "df_choices", "df_prop_dates")
    nrow(df_choices) # 538411





    # df_choices = read.csv('data/2024/choices_augmented_v3.csv')
    # df_choices = read.csv('data/2024/choices_augmented_NEW.csv')

    ## Prerequisite: run `python run.py prepare --stage author-votes --data-root INPUT`
    ## and reduce it into `data/2024/clean_author_vote_outputs/` before
    ## executing this notebook.

    author_vote_outputs <- loadAuthorVoteOutputs()
    df_author_choices <- author_vote_outputs$favorite
    df_author_support <- author_vote_outputs$support

    nrow(df_author_choices) ## 151754

    head(df_author_choices)

    summary(df_author_choices$author_pos)

    table(df_author_choices$author_pos)


    ## Mark rows with multiple author choices.

    df_author_choices$author_multi <- ifelse(
      duplicated(df_author_choices[, c("proposal", "author")]) | duplicated(df_author_choices[, c("proposal", "author")], fromLast = TRUE), 1, 0)

    table(df_author_choices$author_multi) ## 4800 multi


    df_author_choices_no_dupl <-
      df_author_choices[!duplicated(df_author_choices[, c("proposal", "author")]),]

    nrow(df_author_choices_no_dupl) / nrow(df_author_choices)

    ## 2.4% cases in which an author voted for more choices, and we took only the
    ## first one.

    table(df_author_choices_no_dupl$author_multi)

    ## Author vote 61%
    df_author_voted = df_author_choices_no_dupl[!is.na(df_author_choices_no_dupl$author_pos),]
    nrow(df_author_voted) / nrow(df_author_choices_no_dupl)
    ## Authors vote ~62% of the time.

    nrow(df_author_voted[df_author_voted$author_pos <= 5,]) / nrow(df_author_voted)

    ## When they vote, they do so ~91% of the times within the first 5 votes.

    df_author_voted_multi <-
      df_author_choices[duplicated(df_author_choices[, c("proposal", "author")]),]

    df_prop_auth_multi = df_proposals[df_proposals$id %in% df_author_voted_multi$proposal,]

    table(df_prop_auth_multi$type)
    ## Only approval, quadratic, weighted.



    df_author_voted_multi_all <-
      df_author_choices[duplicated(df_author_choices[, c("proposal", "author")]) | duplicated(df_author_choices[, c("proposal", "author")], fromLast = TRUE),]


    ## How many multiple votes per proposal.
    aa = sort(table(df_author_voted_multi_all$proposal))
    aa = as.data.frame(aa)
    nrow(aa[aa$Freq > 2,])
    ggplot(aa, aes(Freq)) + geom_histogram()




    ## Copy
    # df_choices_orig <- df_choices
    # df_choices <- df_choices_orig

    ## Merge author_choices in df_choices

    nrow(df_choices)

    ## The new author-vote job already preserves proposals with missing or unusable
    ## author ballots, so the old manual additions are no longer needed.

    ## Keep only proposals present in df_choices before merging.
    df_author_choices <- df_author_choices[df_author_choices$proposal %in% df_choices$proposal,]
    df_author_support <- df_author_support[df_author_support$proposal %in% df_choices$proposal,]

    df_author_choices_no_dupl <-
      df_author_choices[!duplicated(df_author_choices[, c("proposal", "author")]),]

    df_tmp = df_choices

    nrow(df_choices) # 430141
    df_choices <- mergeAuthorVoteOutputs(df_choices, df_author_choices, df_author_support)
    nrow(df_choices) # 430141
    stopifnot(nrow(df_choices) == nrow(df_tmp))

    summary(df_choices$author_support_fraction)
    table(df_choices$is_author_supported_choice, useNA = "ifany")

    ## 94% is within the first 6 choices.
    nrow(df_choices[df_choices$author_pos <= 6,]) / nrow(df_choices)




    ## Position bias by author's vote position

    df_choices$author_pos_cut <- cut(df_choices$author_pos, c(0, 3, 6, 10, 100000000))

    table(df_choices$author_pos_cut)

    df_choices$author_pos6 <- ifelse(df_choices$author_pos <= 6, 1, 0)


    table(df_choices$author_pos6)

    table(df_choices$is_author_choice)





    df_choices$choices = NULL

    df_choices$choice_idx10 <- ifelse(df_choices$choice_idx <= 10, df_choices$choice_idx, "10+")
    table(df_choices$choice_idx10)




    # aa = df_choices[df_choices$choice_text == "Skip",]
    # # aa[1,c("title", "choices")]
    # table(aa$space)

    ## Whole-space exclusions are now documented in
    ## metadata/exceptions/space_exclusions.csv for this stage.

    ## These are bit weird, but there is some activity and have larger userbase,
    ## we keep them
    ##       nadiva.eth            raisaa.eth







    df_choices$is_test_space <- ifelse(str_starts(df_choices$space, "test"), 1, 0)

    table(df_choices$is_test_space)

    # df_choices[df_choices$is_test_space == 1,]$space
    #
    #
    # df_choices[df_choices$is_test_space == 1,]$title
    #
    #
    # df_choices[df_choices$is_test_space == 1,]$body
    #
    # ## Mostly 0-1 votes
    # df_choices[df_choices$is_test_space == 1,]$n_votes

    ## "test6.shot.eth" has two large proposals, but they are also test
    # df_choices[df_choices$is_test_space == 1 & df_choices$n_votes > 100 ,]$space

    ## Let's remove all test spaces.
    nrow(df_choices)
    df_choices <- df_choices[df_choices$is_test_space == 0,]
    nrow(df_choices) #


    # df_test <- df_choices %>%
    #   filter(grepl("^tests", space))
    # nrow(df_test)

    # bb <- df_choices[df_choices$body != "" & df_choices$choice_text == "Maybe",]
    # nrow(bb) ## 3 from stevyhacker.eth
    # ## Remove it
    #
    # ## Check body == "Desc"
    #
    # bb <- df_choices[df_choices$body == "Desc",]
    # nrow(bb) ## 3 from stevyhacker.eth
    # ## Remove it
    #
    # bb <- df_choices[df_choices$space == "stevyhacker.eth",]
    # nrow(bb) ## Two proposals, one is a test, the other one seems legit, but with
    # ## very few votes. The space cannot be found on the interface of Snapshot.
    # ## Removed.


    # bb <- df_choices[df_choices$title == "New DAO proposal",]
    # nrow(bb) ## 0


    ## The low-vote "existential question" spaces are also handled through the
    ## documented stage-1 space exclusions metadata now.







    # aa <- df_other[df_other$choice_text == "Option 1",]
    # aa


    ## 26, 63 is right from p12.eth
    ## 0xfa167982a4a1a1739999df097b0e8df4381dd46466ee34c28dcbca6880ddeaee
    ## 0x64ce7a71c9987151eb1962d3e7ab853ba287b403f3b607a6cdb8465732312db4
    ## 5 from egvote.eth is right
    ## 0x17806c02f5e9ca0a588d6930261b847f52af514e8b60153c8fc6b66d11182ef9
    # aa <- df_other[df_other$choice_text == "a",]
    # aa

    # table(aa$space)
    # aa[aa$space == "fabien.eth",]$proposal

    ## Fabien is an admin at Snapshot; that whole-space exclusion now lives in the
    ## stage-1 metadata table together with the other maintained space removals.


    # dappwave.eth usebraintrust.eth       wim-dao.eth
    #                 6                28                57

    ## Among the critical ones, some are left with very few proposals. Let's check
    ## those.

    ## fegvote.eth ok
    ## hbot-dev.eth might, mostly wrong, 1 borderline case, remove
    ## some cases borderline, mostly test, remove
    ## xpang.eth some borderline, remove
    ## amywhittaker.eth some test, mostly surveys, but they make sense.
    ## p12.eth ok
    ## dongfang.eth ok
    ## htiffirg.eth mostly test
    ## thehong.eth ok
    ## eth88888888.eth ok
    ## haitang.eth ok




    ## Some titles contain "Title of your proposal goes here", remove those

    nrow(df_choices) ##
    t <- "Title of your proposal goes here"
    df_choices = df_choices[!(grepl(t, df_choices$title)),]
    nrow(df_choices) ##


    ## Until then, the rest is wrong.

    new_props = c("New Proposal", "new proposal", "New proposal")

    tmp = df_choices[df_choices$title %in% new_props,]
    nrow(tmp)


    ## Remove "New proposal"
    nrow(df_choices) ##
    df_choices = df_choices[!(df_choices$title %in% new_props),]
    nrow(df_choices) ##



    nrow(df_choices[df_choices$title %in% c("r", "aa", "mm"),])

    nrow(df_choices) ##
    df_choices = df_choices[!(df_choices$title %in% c("r", "aa", "mm")),]
    nrow(df_choices) ## 426253




    ## Keep a copy.
    # df_choices_copy = df_choices

    # aa <- df_other[df_other$choice_text == "A",]
    # aa

    # sort(table(aa$space))

    ## TO REMOVE entirel
    # harmony-testnet.eth 0xyqwang.eth aptosmovement.eth asianndragon.eth
    # joyfi.eth neoleo.eth photoyogi.eth porogaugoda.eth stagecf.eth eclipsex.eth
    # excito.eth victoshi.eth


    # 杭州马pk深圳马.eth

    ## Todo: continue.

    ## apeass.eth maybe
    ## shtonsummers.eth
    ## cherideal.eth some weird ones, mostly surveys
    ## dao.ilnureth.eth surveys
    ## dehorizondao.eth glmrapes.eth


    # aa[aa$space == "streamingfast.eth",]$title
    # df_choices[df_choices$space == "streamingfast.eth",]$title
    # df_choices[df_choices$space == "thecultdao.eth",]$title

    # aa[aa$space == "thecultdao.eth",]$title

    ## These whole-space removals are now documented in
    ## metadata/exceptions/space_exclusions.csv and applied later in one pass.






    aa = df_choices[df_choices$choice_text == "1️⃣",]


    aa = df_choices[df_choices$choice_text == " 1️⃣",]



    aa = df_choices[df_choices$choice_text == "会",]

    # "会" is yes, it will. It is a survey, not approving the proposal


    ## check
    aa = df_choices[df_choices$choice_text == "有",]

    # "会" is yes, it will. It is a survey, not approving the proposal



    aa = df_choices[df_choices$choice_text == "0",]

    ## all from space goodmorningnews.eth

    aa = df_choices[df_choices$title == "new title",]
    nrow(aa)

    ## Only from wdefi. That whole-space exclusion is now documented in metadata.


    aa = df_choices[df_choices$title == "jghjgghfg",]
    nrow(aa)

    # "shootingx.eth" is a test space and is now excluded via metadata.


    ## Seems OK
    aa = df_choices[df_choices$choice_text == "Option 1",]


    ## Seems OK
    aa = df_choices[df_choices$choice_text == "1",]

    titles <- c("fhftghfg", "t", "BB",
                "yujhtgfd", "gj", "hygfbdvs", "fgfg", "lk,ujhmgnf", "ujhygfbdv",
                "yrthgefds", "yhgfbdvss", "dgs", "dsfs", "rhtygefd", "yhgtfds",
                "。", "cccccc", "tyhrgfe", "tgrfsdcaa", "fhftghfg",
                "chgiffffg", "xx", "hygrftdvsa", "juhgnf", "hgfbd", "hygfdvs",
                "yujghtfds", "yujhtgfrd", "tyhrgfede" , "tuyjhgrfrd",
                "vdssvd",";kljhgf", "dsfwer", "hfgbdvs", "jjjghfhfhf", "yujhtgfvd",
                "fwer", "awefawef", "hainengxing", "fsdhgh", "fsdf",
                "ssssssad", "jhgfdvs", "fdsfgg", "tyhgfd", "jtyhgfds",
                "yujhtgrfds", "yhrtgfde", "dsbdsb", "ghfdvc", "c",
                "tITLE", "hfgbdvs", "dsafsa", "htrgfdvs", "lko,ujimyhgnf",
                "ujhgyfdvsa",
                "uikjhgfd", "chgiffffg", "hjgtbfvd", "ghfbdd", "eee",
                "fdsafdsfa", "ldfjeih",
                "chgiffffg", "uyjhtgfbrvds", "dfvcxd",
                "uyjhtrr", "chgiffffg", "uyjhtgrfds" ,                                                                       "uyjhtgrfd" ,
                "hgfbds",
                "gfhtdvs", "afdasfas",  "tt",
                "fsadfdas"    ,                                                               "gdfvsa", "fadsfasf", "dadw",

                "fdsaffas"                    ,
                "wokeyi",
                "yjuhgtfd",                                                                         "sddc", "uqdbnajguy",  "ghjfdvs", "yuhjtgfd", "utjyhrgfd", "ffsd",
                "tyhrgfedse", "ghgh", "uyjthggrf", "gabyuu",
                "yhtrgfdv", "tyhrgfd",
                "utjyhgrfvd", "dfsdafdsaf", "tetetetette", "dsfsfds",
                "ijyuhtgfr", "sadas", "trhgfdes", "sdfsd"   ,                                       "ikuyjthgrfede", "kjhygnfbdds", "uikjyhtgrfrd", "first", "ythrgf",
                "title", "fsdfsad", "yuijhtgbfdv", "fgdvs", "tytle", "fdsdsfs"  ,                   "eeeer", "sdafsaf", "T", "sdfwer", "fsdfsdf", "oiu8lkyjthg",
                "gsdgsdgxd", "eeeee", "fdsf", "tyjhgfrbdvrsc", "fsdfas"
    )

    # "Heyo. New proposal.", "TItle" , "Title", "title", "Example Proposal", "question"  ,

    ## check
    # [62] "dahh  hoaihwodhhppd"
    # "Joker Today" "LAST TIME"

    aa = df_choices[df_choices$title %in% titles,]

    nrow(aa)


    nrow(df_choices) ## 424004
    df_choices = df_choices[!(df_choices$title %in% titles),]
    nrow(df_choices) ## 423399

    ## Apply all documented stage-1 whole-space exclusions in one auditable pass.
    df_choices <- applySpaceExclusions(
      df_choices,
      stage = "1_pos_bias_data_preparation_v3"
    )

    sort(table(aa$space))




    # write.csv(df_choices, 'data/2024/choices_augmented_v5.csv', row.names=FALSE)

    df_choices <- addStage1RandomAndExcessShare(df_choices)


    df_choices <- addStage1AuthorShare(df_choices)

    summary(df_choices$author_share)

    ## Author share of vp cannot be above 1.
    not_right = df_choices[df_choices$author_share > 1 & !is.na(df_choices$author_choice), ]

    table(not_right$space)
    ## Actually one space is basically 1, it's an approximation error.

    ## Check one proposal.
    aa <- df_choices[df_choices$proposal == "0x6018d479f4ecf3098ef71b2e694570cc34d9f4a369271cb29fc0e91c064968f8",]

    nrow(aa) ## 248, but only 1 vote in the interface, because there are 248 choices
    ## and the type is weighted.

    table(aa$choice_idx)

    nrow(df_choices[df_choices$space == "bentfinance.eth",]) ## 7042 choices
    nrow(df_proposals[df_proposals$space == "bentfinance.eth",]) ## 474 proposals


    ## bentfinance and onchainlabs.eth are a rounding error. Check the others

    not_right = df_choices[df_choices$author_share > 1.01 & !is.na(df_choices$author_choice), ]
    nrow(not_right) ## 79

    summary(not_right$author_share)

    table(not_right$space)
    #  huangwenchao.eth thalescouncil.eth
    #                2                 8

    ## Check one proposal.
    pid <- not_right$proposal[1]

    aa <- df_choices[df_choices$proposal == pid,]

    ## Author did not vote, just to reach the quorum.
    df_choices[df_choices$proposal ==
                 "0xaea959af894d6aa2c783196571012bd60c754ad0f23a529d6e833a55b2404575",]$author_share = NA


    ## bentfinance and onchainlabs.eth are a rounding error. Check the others
    not_right = df_choices[!is.na(df_choices$author_share) & df_choices$author_share > 1.01, ]
    nrow(not_right)
    table(not_right$space)
    #  huangwenchao.eth thalescouncil.eth
    #                2                 8


    ## Check one proposal.
    aa <- df_choices[df_choices$proposal == "0xf20b5d8c83af19a2d0c7a11b17860335c3c676c739db205f93c3c29ecb06fa5e",]

    aa

    ## Author did not vote again.

    ## Author did not vote, just to reach the quorum.
    df_choices[df_choices$proposal ==
                 "0xf20b5d8c83af19a2d0c7a11b17860335c3c676c739db205f93c3c29ecb06fa5e",]$author_share = NA



    aa <- df_choices[df_choices$proposal == "0x8237312b74450cdbd1dca54a72ef9ee1db1dd876780802c6a623f158e10a6b14",]
    aa

    df_choices[df_choices$proposal ==
                 "0x8237312b74450cdbd1dca54a72ef9ee1db1dd876780802c6a623f158e10a6b14",]$author_share = NA


    ## bentfinance and onchainlabs.eth are a rounding error. Check the others
    not_right = df_choices[!is.na(df_choices$author_share) & df_choices$author_share > 1.01, ]
    nrow(not_right)
    # 0

    not_right[!is.na(not_right$space),]$author_vp

    ## Somehow the number do not add up. We recompute the total vp and re-assingn
    ## the author vp share.
    df_wrong_vp_sum <- read.csv(debugDataPath("proposals", "0x63ab2605ffb55cc9f2c7e54beecb522ebebbe38534647e2dc183d3632b454516_votes.csv"), header=FALSE)

    ## VP
    real_tot_vp <- sum(df_wrong_vp_sum$V12, na.rm = TRUE)

    pid = "0x63ab2605ffb55cc9f2c7e54beecb522ebebbe38534647e2dc183d3632b454516"
    df_choices[df_choices$proposal == pid,]$author_share <-
      stage1SafeDivide(df_choices[df_choices$proposal == pid,]$author_vp, real_tot_vp)


    # scores = c(420.94803912806384, 362.7659176672357, 987.4748183950944, 427.4659728585668, 305.95811552644557, 507.04028863912464, 1235.7795605573542, 404.31428661583175)
    # scores_total = sum(scores)

    # not_right[!is.na(not_right$space),]$scores
    # not_right[!is.na(not_right$space),]$scores_total
    #
    # df_wrong_vp_sum




    df_auth_prop = df_author_choices

    author_choice_flag <- stage1AuthorChoiceFlag(df_choices, df_auth_prop)
    matched_author_choice_rows <- sum(author_choice_flag == 1, na.rm = TRUE)
    matched_author_choice_rows / nrow(df_auth_prop)

    missing = df_auth_prop[
      !is.na(df_auth_prop$author_choice) &
        !(stage1ProposalChoiceKey(df_auth_prop$proposal, df_auth_prop$author_choice) %in%
            stage1ProposalChoiceKey(df_choices$proposal, df_choices$choice_idx)),
    ]
    nrow(missing)


    # df_choices[df_choices$proposal %in% missing$proposal,]$space
    # df_choices[df_choices$proposal %in% missing$proposal,]$proposal


    # NOT FOUND.
    # ## Somehow the author choice is missing
    # aa = df_choices[df_choices$proposal == "0x6fb1d194182769dc6776a2d44ddc6836b53946e58e8984713e970c9214b83859",]
    #
    # ## Recreate missing choice.
    # miss_auth = aa[1,]
    #
    # miss_auth$choice_idx = 1
    # miss_auth$choice_text = "Choice 1"
    # miss_auth$choice_score = 5550
    # miss_auth$choice_won = 1
    # miss_auth$n_votes = 1
    # miss_auth$n_votes_share = 1
    # miss_auth$n_votes_share_2 = 1
    # miss_auth$choice_idx10 = 1
    # miss_auth$is_author_choice = 1
    # miss_auth$excess_share = 1 - (1/3)
    # miss_auth$is_first_choice = 1
    #
    # df_choices = rbind(df_choices, miss_auth)

    ## Legacy test/sample exclusions are now documented in
    ## metadata/exceptions/proposal_exclusions.csv and applied earlier.

    df_choices_copy = df_choices

    df_choices$is_author_choice2 <- author_choice_flag

    aa = df_choices$is_author_choice - df_choices$is_author_choice2
    summary(aa)

    summary(df_choices$is_author_choice)

    summary(df_choices$is_author_choice2)


    ## There should be no difference for basic and single-choice
    aa = df_choices[df_choices$type %in% c("basic", "single-choice"),]
    aa = aa$is_author_choice - aa$is_author_choice2
    summary(aa)
    ## 0...

    ## Max is zero, so we have increased the num of author_choices, as wanted.
    aa = df_choices[!(df_choices$type %in% c("basic", "single-choice")),]
    aa = aa$is_author_choice - aa$is_author_choice2
    summary(aa)

    df_choices$is_author_choice <- df_choices$is_author_choice2
    df_choices$is_author_choice2 <- NULL

    writeCandidateCsv(df_choices, stage1_candidate_output, row.names=FALSE)
  }, envir)
  invisible(envir)
}
