# Extracted analytical implementation; execution is explicit.
prepare_labels <- function(context) {
  envir <- new_analysis_environment(context)
  evalq({
    rStoragePath <- function(...) file.path(INPUT_ROOT,"r/storage",...)

    ## check also: proposals_choices_25_v2.csv
    df_proposals <-  read.csv(rawDataPath('proposals_0.csv.gz'), colClasses = c(id = "character"))
    df_proposals$id <- trimws(tolower(df_proposals$id))
    nrow(df_proposals)



    stage4_candidate_input <- debugDataPath("df_choices_25B_more_vars_stage4.csv")
    stage5_candidate_output <- debugDataPath("df_choices_27_stage5_canonical.csv")
    stage5_coverage_audit_output <- debugDataPath("df_choices_27_stage5_canonical_label_coverage.csv")

    # df_choices$proposal <- trimws(tolower(df_choices$proposal))
    df_choices <- read.csv(requireExistingInputPath(stage4_candidate_input, "stage 4 candidate output"), colClasses = c(proposal = "character"))
    df_choices$proposal <- trimws(tolower(df_choices$proposal))
    df_choices <- dropSingleOptionProposals(df_choices)

    if (!"choice_text" %in% colnames(df_choices)) {
      stop("Missing required `choice_text` column in stage 4 candidate output.")
    }
    df_choices$choice_text_normalized <- str_trim(tolower(df_choices$choice_text))



    nrow(df_choices)


    # Fix dupli across files.
    # f = 'choices_25_llama_7_5_25.csv'
    # Fix dupli within file.
    # f = 'choices_25_llama_20_5_25.csv'
    f = 'choices_25_llama_27_6_25.csv'
    f = llmDataPath(f)

    df_llm_llama = read.csv(f, colClasses = c(proposal = "character"))
    df_llm_llama$proposal = trimws(tolower(df_llm_llama$proposal))
    # str(df_llm_llama$choice_idx) # int

    if (!"prop_chid" %in% colnames(df_llm_llama)) {
      df_llm_llama$prop_chid = paste0(df_llm_llama$proposal, df_llm_llama$choice_idx)
    }

    normalize_llm_stance <- function(x) {
      x <- trimws(as.character(x))
      x[x == ""] <- NA_character_
      x
    }

    has_raw_llama_stance <- "stance" %in% colnames(df_llm_llama)
    has_canonical_llama_stance <- "stance_llama" %in% colnames(df_llm_llama)
    if (!has_raw_llama_stance && !has_canonical_llama_stance) {
      stop("Missing Llama stance column: expected `stance_llama` or raw `stance`.")
    }

    if (has_raw_llama_stance) {
      df_llm_llama$stance <- normalize_llm_stance(df_llm_llama$stance)
    }
    if (has_canonical_llama_stance) {
      df_llm_llama$stance_llama <- normalize_llm_stance(df_llm_llama$stance_llama)
    } else {
      df_llm_llama$stance_llama <- NA_character_
    }

    llama_stance_missing_before_fill <- is.na(df_llm_llama$stance_llama)
    llama_stance_filled_from_raw <- rep(FALSE, nrow(df_llm_llama))
    if (has_raw_llama_stance) {
      stance_conflict <- !is.na(df_llm_llama$stance) &
        !is.na(df_llm_llama$stance_llama) &
        df_llm_llama$stance != df_llm_llama$stance_llama
      if (any(stance_conflict)) {
        stop(paste("Conflicting raw `stance` and canonical `stance_llama` values in", sum(stance_conflict), "Llama rows."))
      }

      llama_stance_filled_from_raw <- llama_stance_missing_before_fill & !is.na(df_llm_llama$stance)
      df_llm_llama$stance_llama[llama_stance_filled_from_raw] <- df_llm_llama$stance[llama_stance_filled_from_raw]
    }

    llama_stance_audit <- data.frame(
      metric = c(
        "missing stance_llama before raw backfill",
        "non-missing raw stance values",
        "stance_llama labels filled from raw stance",
        "missing stance_llama after raw backfill"
      ),
      n = c(
        sum(llama_stance_missing_before_fill),
        if (has_raw_llama_stance) sum(!is.na(df_llm_llama$stance)) else 0,
        sum(llama_stance_filled_from_raw),
        sum(is.na(df_llm_llama$stance_llama))
      )
    )
    llama_stance_audit

    nrow(df_llm_llama)



    f = llmDataPath('choices_25_oai.csv')

    df_llm_oai = read.csv(f, colClasses = c(proposal = "character"))
    df_llm_oai$proposal = trimws(tolower(df_llm_oai$proposal))
    # str(df_llm_oai$choice_idx) # int

    if (!"prop_chid" %in% colnames(df_llm_oai)) {
      df_llm_oai$prop_chid = paste0(df_llm_oai$proposal, df_llm_oai$choice_idx)
    }

    nrow(df_llm_oai)



    df_llm_err <- df_llm_llama[df_llm_llama$effect == "ERR",]
    nrow(df_llm_err)

    df_llm_llama <- df_llm_llama[df_llm_llama$effect != "ERR",]
    nrow(df_llm_llama)




    llama_duplicate_key_cols <- c("proposal", "choice_idx")
    llama_duplicate_cols <- c(llama_duplicate_key_cols, "stance_llama")
    missing_llama_duplicate_cols <- setdiff(llama_duplicate_cols, colnames(df_llm_llama))
    if (length(missing_llama_duplicate_cols) > 0) {
      stop(paste("Missing Llama duplicate-check columns:", paste(missing_llama_duplicate_cols, collapse = ", ")))
    }

    llama_duplicate_audit <- data.frame(
      metric = c(
        "duplicate proposal-choice rows before",
        "duplicate proposal-choice-stance_llama rows before"
      ),
      n = c(
        sum(duplicated(df_llm_llama[,llama_duplicate_key_cols, drop = FALSE])),
        sum(duplicated(df_llm_llama[,llama_duplicate_cols, drop = FALSE]))
      )
    )
    llama_duplicate_audit

    aa = df_llm_llama[duplicated(df_llm_llama[,llama_duplicate_key_cols, drop = FALSE]),]
    nrow(aa)

    aa = df_llm_llama[duplicated(df_llm_llama[,llama_duplicate_cols, drop = FALSE]),]
    nrow(aa)

    ## Remove duplicates in LLama
    df_llm_llama = df_llm_llama[!duplicated(df_llm_llama[,llama_duplicate_cols, drop = FALSE]),]
    llama_duplicate_keys_after_exact_dedup <- duplicated(df_llm_llama[,llama_duplicate_key_cols, drop = FALSE]) |
      duplicated(df_llm_llama[,llama_duplicate_key_cols, drop = FALSE], fromLast = TRUE)
    llama_duplicate_audit <- rbind(
      llama_duplicate_audit,
      data.frame(
        metric = c(
          "duplicate proposal-choice rows after",
          "duplicate proposal-choice-stance_llama rows after"
        ),
        n = c(
          sum(duplicated(df_llm_llama[,llama_duplicate_key_cols, drop = FALSE])),
          sum(duplicated(df_llm_llama[,llama_duplicate_cols, drop = FALSE]))
        )
      )
    )
    llama_duplicate_audit

    if (any(llama_duplicate_keys_after_exact_dedup)) {
      llama_conflicting_keys <- unique(df_llm_llama[llama_duplicate_keys_after_exact_dedup, llama_duplicate_key_cols, drop = FALSE])
      print(head(llama_conflicting_keys, 20))
      stop(
        paste(
          "Conflicting Llama stance_llama values remain for",
          nrow(llama_conflicting_keys),
          "proposal-choice keys after exact duplicate removal."
        )
      )
    }
    nrow(df_llm_llama)




    props_unique = unique(df_choices$proposal)
    length(props_unique)

    props_unique_oai = unique(df_llm_oai$proposal)
    length(props_unique_oai)

    props_unique_llama = unique(df_llm_llama$proposal)
    length(props_unique_llama)




    ## Checks.

    props_not_in_oai = props_unique[!(props_unique %in% props_unique_oai)]
    length(props_not_in_oai)

    props_not_in_llama = props_unique[!(props_unique %in% props_unique_llama)]
    length(props_not_in_llama)

    props_not_in_oai_and_llama = props_not_in_oai[props_not_in_oai %in% props_not_in_llama]
    length(props_not_in_oai_and_llama)

    ## Just a few hundreds proposals are missing in LLMs.



    ## Diagnostic overlap only: inner joins here are coverage checks, not the
    ## production merge path used for downstream analysis datasets.

    df_llm_oai$stance_oai <- df_llm_oai$stances
    df_llm_oai$stances <- NULL
    assert_unique_key(df_choices, c("proposal", "choice_idx"), "df_choices")
    assert_unique_key(df_llm_oai, c("proposal", "choice_idx"), "df_llm_oai")
    df_choices_oai_overlap <- merge(df_choices, df_llm_oai, by=c("proposal", "choice_idx"))
    nrow(df_choices_oai_overlap) / nrow(df_choices)

    if ("stance" %in% colnames(df_llm_llama)) {
      df_llm_llama$stance <- NULL
    }
    assert_unique_key(df_llm_llama, c("proposal", "choice_idx"), "df_llm_llama")
    df_choices_llama_overlap <- merge(df_choices, df_llm_llama, by=c("proposal", "choice_idx"))
    nrow(df_choices_llama_overlap) / nrow(df_choices)

    ## About 80% of data matches. 20 % of proposal-choice_idx are missing




    cols <- c("effect", "suggested_stance", "stance_oai", "prop_chid")
    df_llm_oai_m <- df_llm_oai[,cols]
    colnames(df_llm_oai_m) <- c("effect_oai", "suggested_stance_oai", "stance_oai", "prop_chid")
    assert_unique_key(df_llm_oai_m, "prop_chid", "df_llm_oai_m")

    df_choices_oai <- audit_left_join_df(df_choices, df_llm_oai_m, "prop_chid", "df_choices", "df_llm_oai_m")
    nrow(df_choices_oai) / nrow(df_choices)


    cols <- c("effect", "suggested_stance", "stance_llama", "prop_chid")
    df_llm_llama_m <- df_llm_llama[,cols]
    colnames(df_llm_llama_m) <- c("effect_llama", "suggested_stance_llama", "stance_llama", "prop_chid")
    assert_unique_key(df_llm_llama_m, "prop_chid", "df_llm_llama_m")

    df_choices_llama <- audit_left_join_df(df_choices, df_llm_llama_m, "prop_chid", "df_choices", "df_llm_llama_m")
    nrow(df_choices_llama) / nrow(df_choices)

    ## Same proportions using variable prop_chid, as expected

    ## Without all.x
    # [1] 0.8331619 OAI
    # [1] 0.7827994 LLAMA



    ## We keep those choices without match in llama or oai
    df_choices_m <- audit_left_join_df(df_choices_oai, df_llm_llama_m, "prop_chid", "df_choices_oai", "df_llm_llama_m")
    nrow(df_choices_m)

    nrow(df_choices_m) / nrow(df_choices)


    llama33_files <- llmDataPath(
      "llama3.3",
      c(
        "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r1.csv",
        "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r2.csv",
        "stance_v3_stance_v3_25_Llama-3.3-70B-Instruct_r3.csv"
      )
    )

    llama33_conflict_audit_path <- debugDataPath("llama33_choice_label_conflicts.csv")
    df_llm_llama33 <- readStage5Llama33ChoiceLabels(
      llama33_files,
      conflict_audit_path = llama33_conflict_audit_path
    )
    nrow(df_llm_llama33)

    df_llm_llama33_m <- df_llm_llama33[, c(
      "effect_llama33",
      "suggested_stance_llama33",
      "stance_llama33",
      "source_file_llama33",
      "prop_chid"
    )]
    df_choices_m <- stage5LeftJoinChoiceLabels(df_choices_m, df_llm_llama33_m, "Llama 3.3")
    nrow(df_choices_m) / nrow(df_choices)


    human_choice_label_path <- preparedDataPath("clean_human_label_outputs", "human_label_choice_labels.csv")
    df_human_choice_labels <- readStage5HumanChoiceLabels(human_choice_label_path)
    df_human_choice_labels_m <- df_human_choice_labels[, stage5HumanChoiceJoinColumns(), drop = FALSE]

    df_choices_m <- stage5LeftJoinChoiceLabels(df_choices_m, df_human_choice_labels_m, "human labels")
    nrow(df_choices_m) / nrow(df_choices)


    ## Fixing stance

    table(df_choices_m$stance)

    table(df_choices_m$stance_llama)

    ## LLAMA
    df_choices_m$stance_llama <- ifelse(df_choices_m$stance_llama == "AGAINST", "REJECT", df_choices_m$stance_llama)

    table(df_choices_m$suggested_stance_llama)

    df_choices_m$suggested_stance_llama <- ifelse(df_choices_m$suggested_stance_llama %in% c("Approve", "FOR"), "APPROVE", df_choices_m$suggested_stance_llama)

    table(df_choices_m$suggested_stance_llama)

    ## No stance
    # df_choices_m$suggested_stance_llama[is.na(df_choices_m$suggested_stance_llama)] <- "NONE"

    table(df_choices_m$suggested_stance_llama)


    ## OAI

    table(df_choices_m$stance_oai)


    table(df_choices_m$suggested_stance_oai)

    df_choices_m$suggested_stance_oai <- ifelse(df_choices_m$suggested_stance_oai %in% c("Approve", "FOR"), "APPROVE", df_choices_m$suggested_stance_oai)


    df_choices_m$suggested_stance_oai <- ifelse(df_choices_m$suggested_stance_oai %in% c("AGAINST"), "REJECT", df_choices_m$suggested_stance_oai)

    table(df_choices_m$suggested_stance_oai)

    ## No stance
    # df_choices_m$suggested_stance_llama[is.na(df_choices_m$suggested_stance_llama)] <- "NONE"

    table(df_choices_m$suggested_stance_oai)

    df_choices_m[df_choices_m$suggested_stance_oai == "Use Funds for Rewards Pool",]

    valid_suggested_stance_oai <- c("", "APPROVE", "REJECT", "ABSTAIN", "OTHER")
    nonstandard_suggested_stance_oai <- !is.na(df_choices_m$suggested_stance_oai) &
      !(df_choices_m$suggested_stance_oai %in% valid_suggested_stance_oai)

    df_check = df_choices_m[nonstandard_suggested_stance_oai,]


    df_check[, c("choice_text", "stance_oai", "suggested_stance_oai")]

    #                                 choice_text stance_oai
    # 23443          Continue with Casino Project      OTHER
    # 23444            Use Funds for Rewards Pool      OTHER
    # 35870                    100% Tether (USDT)      OTHER
    # 35871                  100% USD Coin (USDC)      OTHER
    # 35872               100% Binance USD (BUSD)      OTHER
    # 35873             50% Tether / 50% USD Coin      OTHER
    # 35874          50% Tether / 50% Binance USD      OTHER
    # 35875        50% USD Coin / 50% Binance USD      OTHER
    # 35876           33% Each (USDT, USDC, BUSD)      OTHER
    # 75028            Option A: weekly + monthly      OTHER
    # 75029                     Option B: monthly      OTHER
    # 159636                Handover to community      OTHER
    # 159637     Sunset & airdrop $10k to holders      OTHER
    # 203539              A. No change (保持现状)      OTHER
    # 203540 B. Build Owned Liquidity(自建流动性)      OTHER
    # 205412                          Option Wait      OTHER
    # 205413                        Option Reveal      OTHER
    # 240732                         ay, Option 1      OTHER
    # 240733                         ay, Option 2      OTHER
    # 240734                  I don't understand!      OTHER
    # 241728             Option 1: Buy & Pay AQUA      OTHER
    # 241729            Option 2: Buy & Burn AQUA      OTHER
    #                                                                                                                                suggested_stance_oai
    # 23443                                                                                                                    Use Funds for Rewards Pool
    # 23444                                                                                                                    Use Funds for Rewards Pool
    # 35870                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35871                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35872                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35873                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35874                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35875                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 35876                               The proposal suggests diversifying stable coin holdings 50% to USDC and 50% to BUSD as a precautionary measure.
    # 75028                                                                                     Option B is suggested as preferable in the proposal body.
    # 75029                                                                                     Option B is suggested as preferable in the proposal body.
    # 159636                                                              The proposal suggests voting for the 'Sunset & airdrop $10k to holders' option.
    # 159637                                                              The proposal suggests voting for the 'Sunset & airdrop $10k to holders' option.
    # 203539                                                                                                        B. Build Owned Liquidity (自建流动性)
    # 203540                                                                                                        B. Build Owned Liquidity (自建流动性)
    # 205412 The proposal suggests a preference for the 'Wait' option by stating the founders' preference and the potential risks of the 'Reveal' option.
    # 205413 The proposal suggests a preference for the 'Wait' option by stating the founders' preference and the potential risks of the 'Reveal' option.
    # 240732                                                                                                      Option 2 is recommended for efficiency.
    # 240733                                                                                                      Option 2 is recommended for efficiency.
    # 240734                                                                                                      Option 2 is recommended for efficiency.
    # 241728                                                                                                                    Option 2: Buy & Burn AQUA
    # 241729                                                                                                                    Option 2: Buy & Burn AQUA


    df_choices_m$suggested_stance_oai[nonstandard_suggested_stance_oai] <- "APPROVE_LIKE"


    table(df_choices_m$suggested_stance_oai)






    df_choices_not_in_oai <- df_choices[!df_choices$prop_chid %in% df_llm_oai_m$prop_chid,]
    nrow(df_choices_not_in_oai)



    df_choices_not_in_llama <- df_choices[!df_choices$prop_chid %in% df_llm_llama_m$prop_chid,]
    nrow(df_choices_not_in_llama)


    df_choices_in_llama <- df_choices[df_choices$prop_chid %in% df_llm_llama_m$prop_chid,]
    nrow(df_choices_in_llama)


    nrow(df_choices_not_in_llama[df_choices_not_in_llama$prop_chid %in% df_choices_not_in_oai$prop_chid,])
    ## A good number of choices is missing from both.


    df_llama_not_in_choices <- df_llm_llama[!df_llm_llama$prop_chid %in% df_choices$prop_chid,]
    nrow(df_llama_not_in_choices)
    ## A large number of choices in llama not in choices. Why?



    ## Checking which proposals are missing in LLMs
    # table(df_choices_not_in_oai$proposal)
    # table(df_choices_not_in_oai$space)

    prop_miss_oai = unique(df_choices_not_in_oai$proposal)
    length(prop_miss_oai)

    length(prop_miss_oai[prop_miss_oai %in% df_llm_oai$proposal]) / length(prop_miss_oai)

    ## 73%+ of the missing prop_chid are from proposals that are in the LLM
    ## datasets, but apparently the specific choice_idx is missing.

    length(props_not_in_oai)




    ## All the proposals that have at least one choice missing
    prop_miss_llama = unique(df_choices_not_in_llama$proposal)
    length(prop_miss_llama)

    nrow(df_choices_not_in_llama)

    ## These are proposals with many choices.
    summary((df_choices_not_in_llama$choice_idx))

    props_with_miss_choices <- prop_miss_llama[prop_miss_llama %in% df_llm_llama$proposal]
    length(props_with_miss_choices) / length(prop_miss_llama)

    ## 51%+ of the missing prop_chid are from proposals that are in the LLM
    ## datasets, but apparently the specific choice_idx is missing.

    ## We need to check why they are not found.


    length(props_not_in_llama)



    df_choices_not_in_llama_uniq = df_choices_not_in_llama[!duplicated(df_choices_not_in_llama$proposal),]

    df_choices_in_llama_uniq = df_choices_in_llama[!duplicated(df_choices_in_llama$proposal),]

    summary(df_choices_not_in_llama_uniq$N_choices)

    summary(df_choices_in_llama_uniq$N_choices)


    table(df_choices_not_in_llama$N_choices)




    df_choices3 <- df_choices[df_choices$N_choices <= 3,]
    nrow(df_choices3)

    df_llm_llama3 = df_llm_llama[df_llm_llama$proposal %in% df_choices3$proposal,]
    nrow(df_llm_llama3)




    df_llama_in_choices3_props = df_llm_llama3[df_llm_llama3$proposal %in% df_choices3$proposal,]
    nrow(df_llama_in_choices3_props)

    ## All proposals in df_llm_llama3 are in df_choices3 by construction.

    df_in_llama_not_in_choices = df_llm_llama3[!df_llm_llama3$prop_chid %in% df_choices3$prop_chid,]
    nrow(df_in_llama_not_in_choices)

    ## Many prop_chid in Llama are not found in choices!

    head(df_in_llama_not_in_choices$prop_chid)

    head(df_in_llama_not_in_choices$proposal)

    ## THis proposal has choice 3 in llama but not in choices
    ## Inside the output file stances_manual_25_B_Llama-3.1-70B-Instruct_r1.csv
    ## it appears twice, once with three choices and the other with one with
    ## one choice ("OTHER")
    p <- "0x9443494b5219f857bba128a851688b76df0c077af3e2008ac746e61b14985ece"

    pp <- df_proposals[df_proposals$id == p,]
    ## Same choice
    pp$choices
    df_llm_llama[df_llm_llama$proposal == p,]

    ## It appears three times, but it is actually only one choice.
    p <- "0x00047595aaa56cf9bc7dbf80f2d12beccd27b370ef008ea0fd87d5c3a6746744"

    pp <- df_proposals[df_proposals$id == p,]
    ## One choice
    pp$choices
    df_llm_llama[df_llm_llama$proposal == p,]


    ## THIS CASE WAS SOLVED. IT WAS CAUSED BY DUPLICATION (DIFFERENT BATCH IDs)
    ## IN THE SAME LLAMA FILE
    ## Three choices, it appears in the file in two batches, each of which
    ## with three choices.
    # p <- "0x001358ceaa2a33039893974f2f57097681ee382076b048745df8293c8af25dc8"
    #
    # pp <- df_proposals[df_proposals$id == p,]
    # ## One choice
    # pp$choices
    # df_llm_llama[df_llm_llama$proposal == p,]


    ## It appears three times, but it is actually only one choice.
    p <- "0x004071eb2a8e37adb462597c47934c709fea8fff1c345eb666c15026438965c6"

    pp <- df_proposals[df_proposals$id == p,]
    ## One choice
    pp$choices
    df_llm_llama[df_llm_llama$proposal == p,]

    ## Many proposals have only one choice.
    df_choices_check = df_choices[df_choices$proposal %in% df_in_llama_not_in_choices$proposal,]
    summary(df_choices_check$N_choices)

    nrow(df_choices_check[df_choices_check$N_choices == 1,])

    ## How many proposals have identical values for choice_text?

    df_choices_check %>%
      group_by(proposal) %>%
      summarise(unique_choices = n_distinct(choice_text)) %>%
      filter(unique_choices == 1) %>%
      tally(name = "num_identical_choice_proposals")


    nrow(df_choices_check)

    head(df_choices_check[, c("proposal", "title")], 200)




    ## This proposal has nonsensical title it
    p <- "0x00770b02a955b2869398a8ac213fd32938dc4fdc0b0948f19f3a2fbf87c61610"

    df_choices[df_choices$proposal == p,]$title


    ## Finding random characters titles. With help of ChatGPT.
    df_choices_check[df_choices_check$title_mod_len > 10 &
                       df_choices_check$title_mod_len < 12 , c("title")]



    random_titles <- c(
      "sadasfsgsdg", "sdgadfhzdfh", "SDFbZSDFvgs", "ertertwwert", "jfgjgffghdf",
      "fafeafadadf", "srtghsfthst", "jfghfggdsnb", "gddgshgdsf‘", "rqwerqwrqwr",
      "yujthgrfdse", "tgtrtfrfrtt", "sdfgsfgsdgs", "fvfgfhtrhtr", "gadadgsasgd",
      "42342354365", "qwqsdsdsdsd", "bsghbxfbxcb", "JFFGEGDFDFH", "tyktyktulul",
      "，hmngfbvdcx", "tgyhjcgtykc", "gsgddxghsdg", "hghdfghdfdf", " hrtg egr th",
      "hasdthsdfgh", "12345asdasd", "bfewbferwbg", "12341234123", "hfdgdfgfsad",
      "lkfawieiawe", "zxcyabolboy", "uyjhtgfrdrs", "hdrgrdgsfas", "fwg rthrtng",
      "dertfhyder3", "kjghfdfghdf", "w4ywesrhyws", "zdfghsdrfth", "bdfghertfsd",
      "MNXBCVJKM,B", "GTEFR489G56", "shtfghadgfh", "hdrgrdgjkgh", "hjgfhfghdfg",
      "safasdfsadf", "vgdferyhjgh", "12312312323", "uikjhygdfvs", "sdfhgfjkogh",
      "dfhdfhjdfhn", "dfadfasdfsf", "JHDFBVUJGXS", "DFJEDJDZFJN", "planet_1940",
      "gsgddxjghdf", "jfgjgffgkgj", "gerhgfjngdf", "53454235342", "SIILljsgdp'",
      "trherthgrth", "dgasdfgearg", "rhtygdfesca", "swerwefsdvg", "deu5te4357u",
      "hfdgdfghgfd", "gjhl,fgjrty", "tjtyhtrhgaf", "wfrgegvbdsa", "acsadwwecxs",
      "hfdgdfgkhdf", "sdfgdsfgbad", "sdgdfgfhfsd", "jfgjgffgjgh", "ethtrthergt",
      "qqqqfefssdf", "fgsfgtrhfhg", "L;JHLJHLHJK", "dfgfdasfadf", "dsfasfdasdf",
      "hghdfgfgdas", "adssadasdas", "dsfsdfdsfsd", "afewgebtrsj", "xczdasdaget"
    )


    df_choices_check[df_choices_check$title_mod_len > 12 &
                       df_choices_check$title_mod_len < 14 , c("title")]

    random_titles <- c(
      "bdxfgbndfgrbn", "sdadsadaaadsc", "tykrtjryktykt", "hfdgdfgdgsdsg", "DGFSDGFDFGDFG",
      "afdhvbsrtyurt", "5t6ytrhdfhhds", "fuyjhfjjjncgv", "hsfdgbadfghas", "rtuyrtfghghfj",
      "sdfgadfyhatey", "bbbbbbbeeeeee", "gythkcdgtyhuk", "ed45dryhder7y", "hfgddgsfgdsds",
      "3ererrgrfgvfg", "SDGDFGHSH9999", "jmghnfgbdfvds", "5343455432534", "DASDSGFADSGFS",
      "DFSDGRFGTHFGH", "gwerhgwerghwe"
    )


    df_choices_check[df_choices_check$title_mod_len > 14 &
                       df_choices_check$title_mod_len < 16 , c("title")]




    # Regex to match "random character" titles
    greped_random_titles <- grep("^[a-zA-Z0-9!@#$%]{8,}$", df_choices$title, value = TRUE)
    length(greped_random_titles)

    # print(greped_random_titles[1:1000])
    # print(greped_random_titles[1000:2000])



    ## Valid words
    non_random_lines_me <- c("Redemption", "pabloescobar", "approvalMore", "0x72e8288873d1A795BA39dd607A20502DF68ab8b4", "TheCur4tor", "MohnadMostafa2", "goooooooooooooooooooooooo", "Undeniably", "Gothicizing", "whosyourdaddy", "Kenpro2014",
      "PinnacleDAO", "Cryptopraveen", "Allegeance", "Fornicator", "alexpavlovsliy", "LuanCrypto", "cothuikhong", "shapeelement", "rumann1855", "ambassador", "Interpolate", "Distribution", "subculture", "chelseycolbert", "CloneForce", "VeeFriends", "KONAMItywter", "wronghorstlamprey", "TitusWinters", "Aplication", "soulsgames", "cryptopedia")


    ## Valid words according to Claude Sonnet 4

    non_random_lines_claude <- c(
      # Meaningful English words
      "Subordinate", "ambassador", "Blockchain", "Management", "Destination",
      "Empowering", "Discussion", "Complexity", "Description", "Notification",
      "Advertising", "Announcements", "Merchandise", "Partnerships",
      "Interdimensional", "Translator", "Whitepaper", "transactions",
      "Suggestion", "Copywriter", "translations", "multichain", "multichain2",
      "Contiguous", "Centralism", "ChatRoulette", "blindhelpful", "Succeeding",
      "Distribution", "subculture", "Undeniably", "Gothicizing", "controlleast",
      "Allegeance", "Fornicator", "Insectation", "Infinitesimal", "Interpolate",
      "Crocheting", "Diversification", "Helminthiasis", "beautifull", "Rebranding",
      "Tubulicole", "Noctambulism", "Excommunication", "Heptagonal", "Microscopist",
      "fractional", "Concatenating", "Attendance", "Permanable", "vegetables",
      "Mycoremediation", "Quadratojugal", "Retardation", "decentralised",
      "ordinarysink", "Aplication", "usingworried", "drivingbrave", "Regardless",
      "Redemption", "Emphasizing", "processing", "Retirement", "Girlfriend",
      "Billionaire", "Onboarding", "Disseminate", "Billboards", "basketball",
      "instruction", "Cheesesticks", "consisting", "Leadership", "Overaction",
      "MonthVoting", "Indication", "Confederate", "Doorkeeper", "rabbithole",
      "RabbitHole", "Enthusiasm", "USMortality", "Discontinued", "Ayuntamiento",
      "cryptoqueen", "Luckyteram", "choseoutline", "Funebrious", "Consolidation",
      "Teresional", "Cybersport", "someonedirty", "BIRMINGHAM", "Melliphagan",
      "guessproduce", "foresightnews", "Gemmuliferous", "frequently", "globecommand",
      "Correlative", "Nuciferous", "Catalystic", "Transprose", "Lagmanuiry",
      "Substitution", "barbershop", "Procedures", "uppercareful", "Periwinkle",
      "Polynomial", "Ultraviolet", "Draftkings", "Housekeeping", "Liberation",
      "Irrecognition", "Silicotungstic", "Monstration", "Volborthite", "Concavation",
      "Improvisate", "Mechanographic", "Monopteron", "Jackpudding", "Industriya",
      "airfeather", "jungleexcept", "rapidlydrawn", "compasschose", "importantpay",
      "regulardeep", "batgetlast", "Trovenomics", "cooooooool", "fasterlonely",
      "wildchanging", "somelikeyou", "Underpeopled", "Inergetical", "thebestcryptos",
      "tinkerbell", "FileValidator", "CoinNewsExtraTv", "audioupmedia", "pankajontech",
      "usualprevent", "berlogavolka", "pensionerskaya", "snappyshot", "ennethalme",
      "speechsafety", "visitorboard", "showmethemoney", "driverlittle", "mcdonaldsw",
      "tropicalflow", "Pancakeswap", "magamonday", "cryptographic", "tuborggold",
      "christopher", "bigboyslime", "Nikkimunderwood", "karachaevskoe", "golosovaniE",
      "Orklympics", "Fuckthepopulation", "Razrepovka", "Badmonday", "ColorStyle",
      "Cryptomeme", "youtubeXyz", "dotaunderlords", "Asalamalekum", "Jackpudding",

      # Meaningful names and usernames
      "ViolentCanadian", "Kenpro2014", "PinnacleDAO", "FriendTech", "Biblioteka",
      "ApprovalMore", "Eleganci10", "TestingProposal4", "TestingProposal3", "ArtifactReal",
      "InnovationHub", "VisioTherapy", "Stakenomics", "Bienvenida", "DAOConnect",
      "MetaverseGameHub", "HealthLink", "NeilSanchezz", "DoubleDoor",
      "Heartforbithes", "theADAghost", "KualaLumpur", "Prashan81966298",
      "longdiceshi", "helliogrow", "ceshiyixia", "whosyourdaddy", "rumann1855",
      "Mandongzhang", "Mandongzhang11", "shapeelement", "Cryptopraveen",
      "symbolstrong", "nimenzhenlihai", "LuanCrypto", "alexpavlovsliy",
      "mingtiantianqiruhe", "cothuikhong", "Loahhhhhhhhh", "pabloescobar00",
      "plovlavash", "chelseycolbert", "VeeFriends", "CloneForce", "KONAMItywter",
      "FinaParinaz", "TerrySeamas", "CrytoRoshi", "bieronghuanle", "huangyingni",
      "wronghorstlamprey", "SpaceRiders", "liangwenqiang", "KONAMIytkuyklyu",
      "ZUOGERENWU", "Zalupareserch", "niaodiceshi", "ZUOTIANKEWEI", "erdinand22",
      "TitusWinters", "STARBOYSODMG", "paodiceshi", "CryptoNews24", "Mamasaodsads",
      "blockchain", "xiaopengcar", "cheburecheck", "yiqilaitougepiao", "bitcoiner2",
      "matumbaman", "polihvoynik", "GalickGunFire", "PancakeInMetaverse",
      "RugPullIndex", "MySelfProposal", "JIARUWOMEN", "Kalkulated", "twelvepalace",
      "EraSurantika", "polygonzkevm", "wwwwkkkktest", "escorza240", "MikePasteli",
      "hoangthien", "portalbridge", "Bubbleworld", "gangsterallstar",
      "Moonrocknugget", "NickEiselArt", "cyberconnect", "Cyberconnect", "CyberConnect",
      "lusilizenmeban", "panxiuling", "HareCrypta", "BlessedL2DAO", "SolomonSimon",
      "Kontryshka", "SirarthurBlack0", "jixujiayou", "aydnve2011", "Xingluxing",
      "baotousugarfc", "ThomasSamuel", "huanhuanshizhong", "dackieswap",
      "LateXClone17707", "microsoftxyz", "Minotaurus", "AbrodolfLinkler",
      "MANTSCROLL", "blackbeard33", "crossvhain", "XimiyaXimiyaVsya",
      "uglitokioshi", "Majeed2337M", "realitybox", "XIAOLAOBAN", "astwoodinsgroup",
      "Bingbingdao01", "crosschain", "Meggiefbrs", "bratiknenado", "bishangyanjing",
      "Konnichiwa", "bibliotekk", "tianhaorea", "Clairvoyance", "ProfMartinP",
      "FreshApple", "Tetradymite", "PROGRAMMES", "TomasEdin1", "minhphucngau",
      "orbitcapital", "Greenercoin", "Heavensmile", "Stablecoins", "VenturesDAO",
      "SUPERSTARTER", "BinkleyOnStyle", "WallaceRodney", "golosovaniE", "airfeather",
      "Amin6Hatem", "TestingProposal4", "yoyovote001", "iamlouispaull", "xindegushi",
      "andeishifu", "KONAMIhert", "deyangyuwenle", "pubgmobile1", "DustinGouker",
      "zuotianchishenme", "tempesta3011", "airdropped", "seydisehir", "WilfredRick",
      "Basiknatusik", "niahaodjuirjlxcifgkshfgSKJGLH", "dotaunderlords", "Playboii123sdasd",
      "frangomant5", "wildchanging", "CryptoBrekkie", "AstroZeroNFT", "vascencio74",
      "VoyaCrypto", "EnahSerlika", "BinaRunida", "Grosse1213", "ronnie44256",
      "CoinNewsExtraTv", "FileValidator", "LusiRastani", "Margery111", "siennaheets",
      "FENGQIYUNYONG", "SHENMEWANYU", "TheCur4tor", "XTraEvil103", "XTraEvil102",
      "approvalMore", "discussion", "speechsafety", "kuiqianclub", "rapidlydrawn",
      "CarterSchmidt7", "fengzibiji", "#JustDeleteIt", "WinstonRobbie", "new32237434",
      "CHENGSHUDEXIANGFA", "lycasantafe", "drywonderful", "vincenttaglia", "FilaNuhata",
      "rucidongting", "Nefatazinc", "compasschose", "DaraPalwina", "PANCAKESWAP",
      "TestingWhitelist", "KaraSweeptakes", "importantpay", "Billionaire", "GCTigerTracker",
      "Krungatina", "PEOPLE", "batgetlast", "Trovenomics", "Razrepovka", "Badmonday",
      "Merchandise", "Confederate", "Doorkeeper", "rabbithole", "cooooooool",
      "ZahranaBarlin", "Enthusiasm", "abbyrances", "AlisaRolina", "TimothySaul",
      "USMortality", "Marichrisssss", "eaufort666", "arjori1988", "electraffa",
      "cheson2055", "shediceshi", "yessurrrrr", "Jackpudding", "JoinCoinBase",
      "Industriya", "Discontinued", "ArtifactReal", "welikethejpegs", "somelikeyou",
      "LightsCrypto", "TheodoricReuel", "christopher", "Underpeopled", "Inergetical",
      "DontGiveUpOnFit", "thebestcryptos", "AntCaveDAO", "Heartforbithes", "crosschain",
      "tinkerbell", "CYBERCONNECT", "everbirthday", "amsden2045", "KONAWAMIERQWE",
      "irebakainplaut", "watanabetest", "xiaolaoban", "Long37657006", "lenadunham",
      "chelseiahbilog", "HealingVibes", "Invitating", "CryptoSheep", "Abstention",
      "hesterton233", "LgarciaLucy", "mingtianwanshangchishenme", "Mandongzhang",
      "TestnetProposal1", "roivaysaochoi", "supersiemmetry", "dekafinance", "Provero4ka",
      "AlinaBunikah", "mmmaaasd2016", "SpeedTheSpartan", "WHOSYOURDADY", "Cheesesticks",
      "VinceRonald", "humanhundred", "woshirucixiangxin", "enjoyhammock2", "wintercool",
      "Concatenating", "Attendance", "escorza240", "MikePasteli", "Permanable",
      "hoangthien", "portalbridge", "Bubbleworld", "gangsterallstar", "Moonrocknugget",
      "NickEiselArt", "cyberconnect", "Cyberconnect", "lusilizenmeban", "panxiuling",
      "HareCrypta", "BlessedL2DAO", "SolomonSimon", "Kontryshka", "SirarthurBlack0",
      "Quadratojugal", "Mycoremediation", "jixujiayou", "Contiguous", "twelvepalace",
      "EraSurantika", "polygonzkevm", "wwwwkkkktest", "MySelfProposal", "JIARUWOMEN",
      "Kalkulated", "blindhelpful", "xiaopengcar", "cheburecheck", "yiqilaitougepiao",
      "bitcoiner2", "matumbaman", "polihvoynik", "GalickGunFire", "PancakeInMetaverse",
      "RugPullIndex", "multichain2", "HealthLink", "Announcements", "Advertising",
      "MetaverseGameHub", "Stakenomics", "VisioTherapy", "KualaLumpur", "Partnerships",
      "WHITEPAPER", "Merchandise", "Blockchain", "translator", "Notification",
      "Interdimensional", "Bienvenida", "multichain", "DAOConnect",

      # Repetitive/expressive entries
      "ANTOOOOOOOOOOOOOOOOOOOOON", "goooooooooooooooooooooooo", "hooooocccca",
      "nononononono", "wqqqqqqqqq", "hhhhhhhaaa", "wefewwwwwww", "biubiubiubiu",
      "YOUDIANYISI", "xxxxbbbbbbg", "gogogogogo", "gooooooooooood", "REDISTRIBUTION",
      "hhhihihihi", "fefefefefef", "cooooooool", "hiiiiiiiiiiiiiiiii", "collllllllll",
      "bbbbbbbeeeeee", "dedededede", "sheshsheshshesh", "cccccccoppppppp",
      "SuckSuckSuckSuckSuck", "SuckSuckSuckSuck", "SuckSuckSuck",



      # They seem to have all empty body.

      # Discord/gaming usernames (with # format)
      # "Tompq#6072", "Cazrd#2788", "Yimhu#8908", "Mqthv#5563", "Mrfed#6705",
      # "Jahkn#5063", "Olryk#7526", "Dbgyl#4310", "Nucbs#3808", "Mkqly#3464",
      # "tg992#5769", "Szvns#9362", "Caidb#8405", "Ndtzc#7679", "steven610#7561",
      # "Krmqb#7024", "Omboz#0224", "Niroa#1590", "Tgplz#3006", "Qsuyd#4015",
      # "Idylx#3701", "Xrglz#2844", "Laihr#4632", "Ulprb#6332", "Mrojy#4580",
      # "Khkro#3536", "Igakj#3028", "Vilbj#6938", "Rtvdm#0855", "Efskc#1920",
      # "Mzula#5845", "Fosrq#1822", "laoliu#2733", "Cnjai#7599", "Yxitg#1842",
      # "Gjchs#2753", "Qqbgi#3097", "PUNCX#17707", "Ylrve#5662", "Kfmjw#1782",
      # "Kvkog#3324", "eva1978#7273", "Esamx#6119", "Ozyxm#2673", "Vdcja#4249",
      # "Gwjvt#5431", "Fqtph#7486", "Lgkde#7370", "Cnkam#9333", "Vrmxb#4999",
      # "Hlfco#6888", "Ffibx#6204", "Uzlpd#2468", "Emrjt#9122", "Nzfyw#6061",
      # "Elagu#8567", "duquee55#2807", "Qdhwr#3946", "Sykni#1203", "Dypcl#7265",
      # "Cbfkp#4618", "Lctvr#8986", "Vhnsb#9181", "Blygq#7493", "Axqfm#0671",
      # "Aoxbp#8315", "Mrhjz#9722", "Lschp#9111", "Zldoz#8084", "Ndrlc#6478",
      # "Louis#Bloor2", "Rcghu#5998", "Grudb#2860", "Mweti#0448", "Whcio#7295",
      # "Sskfc#2377", "Csgan#6506", "Txvgp#4152", "Afzap#9697", "Jfwsm#1482",
      # "Kzimx#7005", "Vzkve#9512", "EdisonMarshall#7268", "mhug254#8122",
      # "Vsjlt#2826", "Rumkf#0664", "Mkbjn#6203", "Fgfec#6906", "Szubp#9118",
      # "Wcavx#0051", "Zzqet#8040", "Bexrz#9227", "Ipufe#5838", "Tdbji#7881",
      # "Zufdj#7597", "Khcbx#2341", "Bebqu#1217", "Aehxd#1373", "Wndkj#0452",
      # "Teqwh#6214", "Favtp#2903", "Kxkli#8491", "Npiey#5299", "Wtqpj#9721",
      # "lshepard#1460", "aidoumeng#9408", "Wtjzc#9337", "Xpnhz#4763", "Jciyp#1433",
      # "Drsou#0135", "Lmyln#6688", "Kiyhb#4881", "Bquzj#3361", "Qtnmy#3922",

      # They seem to have all empty body.


      # Twitter/social handles (with @ format)
      # "@donzion1987", "@esanders2469", "@yakiddant", "@UcheOgbeifun", "@Bajo8853Skyball",
      # "@tarek2020741", "@monybustamante1", "@yinyangbuffs", "@artararat", "@piyushpine",
      # "@RomaYaryko", "@meccopower210",

      # Ethereum addresses
      "0x67b9D58678acc76df005916f57b51D51E5aeb64D",
      "0x9c8fa353F5bD8d4c39a307865EcA5b6A5810142C",
      "0xba728cb1967a2a17be1fedacc4a60143627c385b",
      "0xe5a040FA08D417E41A8DB0Ed7ceA39A2fa25B8e9",
      "0x91418640cc7984b5Ee526a02b18989cF06cF6Fd2",
      "0x72e8288873d1A795BA39dd607A20502DF68ab8b4",
      "0xE57dA14A7Ee5b1499cf153ebf318e7D567125B07",
      "0x0f9Ece6Ae3B6da4110216E65D4AbaA017dD21A30",
      "0x1fb7f5Ef990257d8f7017ebbB4046ED34E612D9a",
      "0x2b7808926E358cd0e1c2b9E09FaAa4829aFB9Bb6",
      "0x37073055Ca61FDe771EC40B3873f9eeD7CFFF7F4",
      "0x485a78C49F246e829a9ccA89d83E9b472607758b",
      "0x55d398326f99059ff775485246999027b3197955",
      "0x671Fd3f6Ab26e02aBc21cf4647B28dCE3235A4F",
      "0x0000000000000000000000000000000000001010",
      "0x5a07C61F",
      "0x1111111111",

      # Other meaningful entries
      "Bienvenidos!", "Desarrollos", "Allhamdulillah", "MohnadMostafa2", "hongshanculture",
      "jiadiceshi", "moreopposite", "bitcoinomar", "autosniper", "TitanSagaGame",
      "BiraJelita", "Julioblandon14", "GreenBuilding", "Adver"
    )




    df_choices_updated <- df_choices[!df_choices$title %in% greped_random_titles |
                                      df_choices$title %in% non_random_lines_me |
                                      df_choices$title %in% non_random_lines_claude, ]

    nrow(df_choices_updated)
    nrow(df_choices_updated) / nrow(df_choices)



    # Regex to match "random character" titles
    greped_random_bodies <- grep("^[a-zA-Z0-9!@#$%]{8,}$", df_choices$body, value = TRUE)
    length(greped_random_bodies)


    # greped_random_bodies




    df_very_short <- df_choices[df_choices$body_is_very_short == 1,]






    ## USED AND PASSED TO GPT4o interface.

    df_with_choices <- df_choices %>%
      group_by(proposal) %>%
      mutate(choices_all = paste(choice_text_normalized, collapse = " <SEP> ")) %>%
      ungroup()

    head(df_with_choices$choices_all)

    # ## This fails to upload via copilot. Let's use body is zero first.
    # # df_tmp <- df_with_choices[df_with_choices$body_is_very_short == 1,]
    # df_tmp <- df_with_choices[df_with_choices$body_is_zero == 1,]

    df_tmp <- df_with_choices[df_with_choices$body_is_very_short == 0,]

    df_tmp <- df_tmp[!duplicated(df_tmp$proposal),]
    #


    #


    ## Load variable valid_short_body_proposals

    source(requireExistingInputPath(
      rStoragePath("valid_short_body_proposals_GPT4o.R"),
      "valid short-body GPT4o proposal list"
    ),local=TRUE)




    df_choices_updated2 <-
      df_choices_updated[!df_choices_updated$proposal %in% df_very_short$proposal |
                          df_choices_updated$proposal %in% valid_short_body_proposals,]

    nrow(df_choices_updated2)
    nrow(df_choices_updated2) / nrow(df_choices_updated)
    nrow(df_choices_updated2) / nrow(df_choices)




    props_unique2 = unique(df_choices_updated2$proposal)
    length(props_unique2)

    ## Checks.

    props_not_in_oai = props_unique2[!(props_unique2 %in% props_unique_oai)]
    length(props_not_in_oai)

    props_not_in_llama = props_unique2[!(props_unique2 %in% props_unique_llama)]
    length(props_not_in_llama)

    ## Just a few hundreds proposals are missing in LLMs.



    ## Load variable valid_nonshort_body_proposals

    source(requireExistingInputPath(
      rStoragePath("valid_non-short_body_proposals_GPT4o.R"),
      "valid non-short-body GPT4o proposal list"
    ),local=TRUE)




    df_choices_updated3 <-
      df_choices_updated2[df_choices_updated2$body_is_very_short == 1 |
                          df_choices_updated2$proposal %in% valid_nonshort_body_proposals,]

    nrow(df_choices_updated3)
    nrow(df_choices_updated3) / nrow(df_choices_updated2)
    nrow(df_choices_updated3) / nrow(df_choices)

    ## This is super-restrictive, let's add a flag.

    gpt4o_review_universe_proposals <- unique(df_choices_updated2$proposal)
    gpt4o_analysis_scope_proposals <- unique(df_choices_updated3$proposal)




    ### SAVE






    # install.packages("vcd")
    library(vcd)

    ## Computing Cramer's V

    # Create a contingency table
    table_xy <- table(df_choices_m$stance_oai, df_choices_m$stance_llama)

    # Compute Cramér's V
    assocstats(table_xy)

    # Cramér's V	Strength of Association
    # 0.00–0.10	Negligible
    # 0.10–0.20	Weak
    # 0.20–0.40	Moderate
    # 0.40–0.60	Relatively strong
    # 0.60–0.80	Strong
    # 0.80–1.00	Very strong




    # install.packages('irr')
    library(irr)

    kappa2(df_choices_m[,c("stance_oai", "stance_llama")])

    # Common Interpretation Scale (Landis & Koch, 1977):
    #
    # Kappa Range	Strength of Agreement
    # < 0	Poor
    # 0.01–0.20	Slight
    # 0.21–0.40	Fair
    # 0.41–0.60	Moderate
    # 0.61–0.80	Substantial
    # 0.81–1.00	Almost perfect
    # So, your value (0.394) falls in the fair-to-moderate range.





    kappa2(df_choices_m[,c("stance", "stance_llama")])



    kappa2(df_choices_m[,c("stance", "stance_oai")])



    df_with_choices <- df_choices_m %>%
      group_by(proposal) %>%
      mutate(choices_all = paste(choice_text_normalized, collapse = " <SEP> ")) %>%
      ungroup()

    head(df_with_choices$choices_all)

    df_tmp <- df_with_choices
    df_tmp <- df_tmp[!duplicated(df_tmp$proposal),]


    df_tmp$idx <- seq(1,nrow(df_tmp))

    writeCandidateCsv(
      df_tmp[,c("proposal", "idx")],
      debugStoragePath("props_info_6_27_id_map.csv"),
      row.names = FALSE
    )

    writeCandidateCsv(
      df_tmp[,c("idx", "title", "body", "choices_all")],
      debugStoragePath("props_info_6_27.csv"),
      row.names = FALSE
    )


    writeCandidateCsv(
      df_tmp[1:100,c("idx", "title", "body", "choices_all")],
      debugStoragePath("props_info_6_27_sample.csv"),
      row.names = FALSE
    )



    ## Load file returned by ChatGPT

    f = llmDataPath('chatgpt', 'choices_chatgpt_v1.csv')
    df_llm_chatgpt = read.csv(f, colClasses = c(proposal = "character"))
    df_llm_chatgpt$proposal = trimws(tolower(df_llm_chatgpt$proposal))
    # str(df_llm_oai$choice_idx) # int

    if (!"prop_chid" %in% colnames(df_llm_chatgpt)) {
      df_llm_chatgpt$prop_chid = paste0(df_llm_chatgpt$proposal, df_llm_chatgpt$choice_idx)
    }

    nrow(df_llm_chatgpt)



    ## Fix


    # [1] "0x53d9351e52048c74b701ee540ee966c661459a29d41b79deacdc5c404b046eef"
    # [2] "0xd8d0085e10c0c33286bdad6cf5ae446a520c8298810eeb6003b3d7ea31803cce"
    # [3] "0xd8d0085e10c0c33286bdad6cf5ae446a520c8298810eeb6003b3d7ea31803cce"
    # [4] "0xfc85965fe8cf8d724d35630a717b1941edf48b8e0b63383fd7063f8c08a8d76a"
    # [5] "qmcinf3dhnwwhvydxhzsqpmbg4rpzm1yxkxkpmta1zacv7"
    # [6] "qmw8a4cchkwq1kprkwaw3ugdfzgyakazg3yuy1opthh7bq"

    # $choice_text
    # [1] "n/a"  " "    " "    "null" ""     "  "

    df_llm_chatgpt$stances = ifelse(df_llm_chatgpt$stances == "", "OTHER", df_llm_chatgpt$stances)

    # df_llm_chatgpt[df_llm_chatgpt$stances == "",]$stances



    ## Diagnostic overlap only. ChatGPT-specific columns remain optional diagnostics.

    ## Merging.

    props_unique_chatgpt = unique(df_llm_chatgpt$proposal)
    length(props_unique_chatgpt)

    props_not_in_chatgpt = props_unique[!(props_unique %in% props_unique_chatgpt)]
    length(props_not_in_chatgpt)


    df_llm_chatgpt$stance_chatgpt <- df_llm_chatgpt$stances
    df_llm_chatgpt$stances <- NULL
    assert_unique_key(df_llm_chatgpt, c("proposal", "choice_idx"), "df_llm_chatgpt")
    df_choices_chatgpt_overlap <- merge(df_choices, df_llm_chatgpt, by=c("proposal", "choice_idx"))
    nrow(df_choices_chatgpt_overlap) / nrow(df_choices)


    cols <- c("effect", "suggested_stance", "stance_chatgpt", "prop_chid")
    df_llm_chatgpt_m <- df_llm_chatgpt[,cols]
    colnames(df_llm_chatgpt_m) <- c("effect_chatgpt", "suggested_stance_chatgpt", "stance_chatgpt", "prop_chid")
    assert_unique_key(df_llm_chatgpt_m, "prop_chid", "df_llm_chatgpt_m")


    df_choices_chatgpt <- audit_left_join_df(df_choices_m, df_llm_chatgpt_m, "prop_chid", "df_choices_m", "df_llm_chatgpt_m")
    nrow(df_choices_chatgpt) / nrow(df_choices_m)



    head(df_choices_chatgpt)


    kappa2(df_choices_chatgpt[,c("stance", "stance_chatgpt")])
    ## Relatively strong 0.412



    canonical_stage5_choices <- buildCanonicalStage5ChoiceTable(
      df_stage4 = df_choices,
      df_stage5_source = df_choices_chatgpt,
      proposal_coverage_flags = list(
        proposal_in_gpt4o_review_universe = gpt4o_review_universe_proposals,
        proposal_in_gpt4o_analysis_scope = gpt4o_analysis_scope_proposals
      )
    )
    nrow(canonical_stage5_choices)

    writeCandidateCsv(canonical_stage5_choices, stage5_candidate_output, row.names = FALSE)



    stage5_label_coverage <- buildStage5LabelCoverageAudit(canonical_stage5_choices)
    stage5_label_coverage

    writeCandidateCsv(
      stage5_label_coverage,
      stage5_coverage_audit_output,
      row.names = FALSE
    )
  }, envir)
  invisible(envir)
}
