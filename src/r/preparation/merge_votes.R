# Extracted analytical implementation; execution is explicit.
prepare_vote_merge <- function(context) {
  envir <- new_analysis_environment(context)
  evalq({
stage2_candidate_input <- debugDataPath("choices_25-nov-17_B_dup_excluded_m1_instrumented.csv")
stage3_candidate_output <- debugDataPath("choices_25-nov-17_C_stage3_reanalysis.csv")
stage3_audit_output <- debugDataPath("stage3_vote_merge_audit.csv")

stage3_vote_audit <- data.frame(
  phase = character(0),
  metric = character(0),
  value = numeric(0),
  detail = character(0),
  stringsAsFactors = FALSE
)

append_stage3_audit <- function(phase, metric, value, detail = "") {
  stage3_vote_audit <<- rbind(
    stage3_vote_audit,
    data.frame(
      phase = phase,
      metric = metric,
      value = as.numeric(value),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  )
}

    df_proposals <- read.csv(rawDataPath("proposals_0.csv.gz"), colClasses = c(id = "character"))
    df_proposals$id <- trimws(tolower(df_proposals$id))
    nrow(df_proposals)

    df_props_final <- df_proposals[df_proposals$scores_state == "final", ]
    nrow(df_props_final)
    append_stage3_audit("load_proposals", "final_proposals", nrow(df_props_final))


    choices_input_path <- requireExistingInputPath(stage2_candidate_input, "stage 2 candidate output")
    df_choices <- read.csv(choices_input_path, colClasses = c(proposal = "character"))
    df_choices$proposal <- trimws(tolower(df_choices$proposal))
    df_choices$type <- factor(
      df_choices$type,
      levels = c("single-choice", "basic", "quadratic", "weighted", "ranked-choice", "approval")
    )
    rows_before_single_option_drop <- nrow(df_choices)
    df_choices <- dropSingleOptionProposals(df_choices)
    nrow(df_choices)
    append_stage3_audit("load_choices", "rows", nrow(df_choices), basename(choices_input_path))
    append_stage3_audit(
      "drop_single_option_proposals",
      "rows_removed",
      rows_before_single_option_drop - nrow(df_choices),
      basename(choices_input_path)
    )


    favorite_counts_path <- requireExistingInputPath(
      preparedDataPath("clean_vote_outputs", "prop_choices_favorite_counts.csv"),
      "current clean vote output prop_choices_favorite_counts.csv"
    )
    df_votes_exp_counts <- read.csv(
      favorite_counts_path,
      col.names = c("proposal", "choice_idx", "n_votes", "tot_votes", "vote_mode"),
      header = TRUE,
      colClasses = c(proposal = "character")
    )
    df_votes_exp_counts$proposal <- trimws(tolower(df_votes_exp_counts$proposal))
    df_votes_exp_counts$vote_mode <- NULL

    df_votes_exp_counts_orig <- df_votes_exp_counts
    nrow(df_votes_exp_counts_orig)
    append_stage3_audit("load_vote_counts", "rows", nrow(df_votes_exp_counts_orig), basename(favorite_counts_path))


    ## Only final proposals should enter the stage-3 handoff.
    nrow(df_votes_exp_counts)
    df_votes_exp_counts <- df_votes_exp_counts[df_votes_exp_counts$proposal %in% df_props_final$id, ]
    nrow(df_votes_exp_counts)
    append_stage3_audit("filter_final_proposals", "rows_after", nrow(df_votes_exp_counts))

    latin_fake_ids <- getFakeProposalIds(categories = c("latin_placeholder"))
    df_props_latin <- df_proposals[trimws(tolower(df_proposals$id)) %in% latin_fake_ids, ]
    nrow(df_props_latin)

    nrow(df_votes_exp_counts)
    df_votes_exp_counts <- df_votes_exp_counts[!(df_votes_exp_counts$proposal %in% df_props_latin$id), ]
    nrow(df_votes_exp_counts)
    append_stage3_audit("remove_latin_placeholder", "rows_after", nrow(df_votes_exp_counts))

    cleaned_vote_counts <- cleanStage3VoteCounts(
      df_votes_exp_counts,
      stage = "3_merging_N_votes_v2"
    )
    df_votes_exp_counts_clean <- cleaned_vote_counts$data
    stage3_vote_audit <- rbind(stage3_vote_audit, cleaned_vote_counts$audit)
    nrow(df_votes_exp_counts_clean)


    ## Use only proposals still present in the stage-2 analytical file.
    nrow(df_votes_exp_counts_clean)
    df_votes_exp_counts_clean <- df_votes_exp_counts_clean[df_votes_exp_counts_clean$proposal %in% df_choices$proposal, ]
    nrow(df_votes_exp_counts_clean)
    append_stage3_audit("align_votes_to_choices", "rows_after", nrow(df_votes_exp_counts_clean))

    missing_choice_proposals <- unique(df_choices$proposal[!(df_choices$proposal %in% df_votes_exp_counts_clean$proposal)])
    length(missing_choice_proposals)
    append_stage3_audit("drop_choices_without_vote_counts", "proposals_removed", length(missing_choice_proposals))

    if (length(missing_choice_proposals) > 0) {
      df_choices <- df_choices[!(df_choices$proposal %in% missing_choice_proposals), ]
    }

    nrow(df_choices)
    append_stage3_audit("drop_choices_without_vote_counts", "rows_after", nrow(df_choices))


    df_votes_exp_counts_clean$prop_chid <- paste0(df_votes_exp_counts_clean$proposal, df_votes_exp_counts_clean$choice_idx)
    df_choices$prop_chid <- paste0(df_choices$proposal, df_choices$choice_idx)

    unmatched_vote_rows <- df_votes_exp_counts_clean[
      !(df_votes_exp_counts_clean$prop_chid %in% df_choices$prop_chid),
    ]
    unmatched_vote_proposals <- unique(unmatched_vote_rows$proposal)

    append_stage3_audit(
      "remove_vote_counts_without_choices",
      "rows_before",
      nrow(unmatched_vote_rows),
      "vote-count choice_idx absent from stage-2 choices"
    )
    append_stage3_audit(
      "remove_vote_counts_without_choices",
      "vote_mass_removed",
      sum(unmatched_vote_rows$n_votes, na.rm = TRUE),
      "vote-count choice_idx absent from stage-2 choices"
    )
    append_stage3_audit(
      "remove_vote_counts_without_choices",
      "proposals_removed",
      length(unmatched_vote_proposals),
      paste(head(unmatched_vote_proposals, 10), collapse = ";")
    )

    if (length(unmatched_vote_proposals) > 0) {
      rows_before_unmatched_choice_drop <- nrow(df_choices)
      rows_before_unmatched_vote_drop <- nrow(df_votes_exp_counts_clean)

      df_choices <- df_choices[!(df_choices$proposal %in% unmatched_vote_proposals), ]
      df_votes_exp_counts_clean <- df_votes_exp_counts_clean[
        !(df_votes_exp_counts_clean$proposal %in% unmatched_vote_proposals),
      ]

      append_stage3_audit(
        "remove_vote_counts_without_choices",
        "choice_rows_removed",
        rows_before_unmatched_choice_drop - nrow(df_choices)
      )
      append_stage3_audit(
        "remove_vote_counts_without_choices",
        "vote_rows_removed",
        rows_before_unmatched_vote_drop - nrow(df_votes_exp_counts_clean)
      )
    }

    remaining_unmatched_vote_rows <- nrow(df_votes_exp_counts_clean[
      !(df_votes_exp_counts_clean$prop_chid %in% df_choices$prop_chid),
    ])
    if (remaining_unmatched_vote_rows != 0) {
      stop("Stage 3 still has vote-count choice rows without matching stage-2 choices")
    }
    append_stage3_audit(
      "remove_vote_counts_without_choices",
      "rows_after",
      remaining_unmatched_vote_rows
    )

    all_missing <- df_choices[!(df_choices$prop_chid %in% df_votes_exp_counts_clean$prop_chid), ]
    nrow(all_missing)
    append_stage3_audit("zero_fill_missing_choices", "missing_rows_before", nrow(all_missing))

    df_votes_exp_counts_clean2 <- zeroFillMissingFavoriteCounts(df_votes_exp_counts_clean, df_choices)
    nrow(df_votes_exp_counts_clean2)
    append_stage3_audit(
      "zero_fill_missing_choices",
      "rows_added",
      nrow(df_votes_exp_counts_clean2) - nrow(df_votes_exp_counts_clean)
    )

    missing_after_zero_fill <- nrow(df_choices[!(df_choices$prop_chid %in% df_votes_exp_counts_clean2$prop_chid), ])
    if (missing_after_zero_fill != 0) {
      stop("Some retained prop_chid values are still missing after zero-fill in stage 3")
    }
    append_stage3_audit("zero_fill_missing_choices", "missing_rows_after", missing_after_zero_fill)


    duplicate_prop_chid_n <- sum(duplicated(df_votes_exp_counts_clean2$prop_chid))
    append_stage3_audit("deduplicate_vote_counts", "duplicate_prop_chid_before", duplicate_prop_chid_n)

    df_votes_exp_counts_clean3 <- df_votes_exp_counts_clean2 %>%
      group_by(prop_chid) %>%
      arrange(desc(n_votes), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup()

    sum(duplicated(df_votes_exp_counts_clean3$prop_chid))
    assert_unique_key(df_votes_exp_counts_clean3, "prop_chid", "df_votes_exp_counts_clean3")
    append_stage3_audit(
      "deduplicate_vote_counts",
      "rows_removed",
      nrow(df_votes_exp_counts_clean2) - nrow(df_votes_exp_counts_clean3)
    )


    assert_unique_key(df_choices, "prop_chid", "df_choices")
    missing_prop_chid_before_merge <- nrow(df_choices[!(df_choices$prop_chid %in% df_votes_exp_counts_clean3$prop_chid), ])

    df_choices_n <- merge(
      df_choices,
      df_votes_exp_counts_clean3[, c("prop_chid", "n_votes", "tot_votes")],
      by = c("prop_chid")
    )

    if (nrow(df_choices_n) != nrow(df_choices) - missing_prop_chid_before_merge) {
      stop("Choice-level vote merge dropped an unexpected number of rows in stage 3")
    }

    nrow(df_choices_n)
    append_stage3_audit("merge_vote_counts", "rows_after", nrow(df_choices_n))


    aa <- df_choices_n[is.na(df_choices_n$choice_share), ]
    nrow(aa)

    aa <- df_choices_n[is.na(df_choices_n$scores_total), ]
    nrow(aa)

    aa <- df_choices_n[is.na(df_choices_n$choice_score), ]
    nrow(aa)

    ## Proposals with zero voting power stay out of the analytical output.
    rows_before_zero_scores <- nrow(df_choices_n)
    df_choices_n <- df_choices_n[df_choices_n$scores_total != 0, ]
    nrow(df_choices_n)
    append_stage3_audit(
      "filter_zero_scores_total",
      "rows_removed",
      rows_before_zero_scores - nrow(df_choices_n)
    )

    df_choices_n$choice_N_votes <- NULL
    df_choices_n$prop_N_votes <- NULL
    df_choices_n$N_votes_share <- NULL

    df_choices_n$n_votes_share <- df_choices_n$n_votes / df_choices_n$tot_votes

    ## Approval, weighted, and quadratic favorite-count rows can sum above tot_votes.
    tt <- ddply(df_choices_n, "proposal", function(x) {
      tot_votes <- sum(x$n_votes)
      tot_votes2 <- x$tot_votes[1]
      if (tot_votes != tot_votes2) {
        type <- x$type[1]
        if (!(type %in% c("weighted", "quadratic", "approval"))) {
          print(x$proposal[1])
        }
      }
    })

    rows_before_testsnap <- nrow(df_choices_n)
    df_choices_n <- df_choices_n[df_choices_n$space != "testsnap.eth", ]
    nrow(df_choices_n)
    append_stage3_audit(
      "filter_testsnap_space",
      "rows_removed",
      rows_before_testsnap - nrow(df_choices_n)
    )

    tt <- ddply(df_choices_n, "proposal", function(x) {
      tot_votes <- sum(x$n_votes)
      return(tot_votes)
    })
    colnames(tt)[2] <- "tot_votes_2"

    assert_unique_key(tt, "proposal", "tt")
    df_choices_n <- audit_left_join_df(df_choices_n, tt, "proposal", "df_choices_n", "tt")
    df_choices_n$n_votes_share_2 <- df_choices_n$n_votes / df_choices_n$tot_votes_2

    if (any(is.na(df_choices_n$tot_votes))) {
      stop("Proposal-level tot_votes is missing after stage-3 merge")
    }

    proposal_totals <- unique(df_choices_n[, c("proposal", "tot_votes")])
    assert_unique_key(proposal_totals, "proposal", "proposal_totals")

    vote_total_check <- df_choices_n %>%
      group_by(proposal) %>%
      summarise(
        sum_n_votes = sum(n_votes),
        tot_votes_2 = first(tot_votes_2),
        .groups = "drop"
      )

    if (any(vote_total_check$sum_n_votes != vote_total_check$tot_votes_2)) {
      stop("sum(n_votes) does not match tot_votes_2 after stage-3 cleanup")
    }

    append_stage3_audit("final_output", "rows", nrow(df_choices_n))
    append_stage3_audit("final_output", "proposals", length(unique(df_choices_n$proposal)))


    writeCandidateCsv(df_choices_n, stage3_candidate_output, row.names = FALSE)
    writeCandidateCsv(stage3_vote_audit, stage3_audit_output, row.names = FALSE)
  }, envir)
  invisible(envir)
}
