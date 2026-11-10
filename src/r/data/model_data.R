################################################################################
## Data load and validation
################################################################################

canonical_column_specs <- list(
  choice_share = c("choice_share_2", "choice_share"),
  is_author_choice = c("is_author_choice2", "is_author_choice"),
  created_rel = c("created_rel", "created_relative")
)

required_base_columns <- c(
  "proposal", "space", "type", "choice_idx", "N_choices",
  "choice_share_no_author_vp", "tot_votes_2", "stance", "is_infavor",
  "author_choice", "author_pos6", "choice_text_len_rel", "n_votes"
)

optional_columns <- c(
  "is_against", "is_abstain", "is_other", "author_vp", "author_rank",
  "author_multi", "author_multi_vote", "author_multi_approve"
)

resolve_column <- function(header, canonical_name, alternatives) {
  matches <- alternatives[alternatives %in% header]
  if (length(matches) == 0) {
    stop(
      "Missing required column for ", canonical_name, ". Expected one of: ",
      paste(alternatives, collapse = ", "),
      call. = FALSE
    )
  }
  matches[[1]]
}

assert_required_columns <- function(header, require_flagged = FALSE) {
  missing <- setdiff(required_base_columns, header)
  if (isTRUE(require_flagged) && !("flagged" %in% header)) {
    missing <- c(missing, "flagged")
  }
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(lapply(
    names(canonical_column_specs),
    function(name) resolve_column(header, name, canonical_column_specs[[name]])
  ))
}

load_choice_data <- function(input_path = default_input_path(), raw_data_dir = NULL) {
  require_packages("data.table")
  if (!file.exists(input_path)) {
    stop("Required input is missing: ", input_path, call. = FALSE)
  }

  header <- names(data.table::fread(input_path, nrows = 0, showProgress = FALSE))
  assert_required_columns(header, require_flagged = TRUE)

  selected_columns <- unique(c(
    required_base_columns,
    optional_columns[optional_columns %in% header],
    "flagged",
    unlist(canonical_column_specs, use.names = FALSE)
  ))
  selected_columns <- selected_columns[selected_columns %in% header]

  dt <- data.table::fread(
    input_path,
    select = selected_columns,
    colClasses = list(character = c("proposal", "space", "type", "stance")),
    showProgress = TRUE
  )
  reconstruct_table6_outcome(dt, raw_data_dir)
}

# Table 6 must use actual remaining ballots, not the difference between rounded
# Snapshot scores and author power. Other outcomes and sample flags are unchanged.
# This runs on a future data load; sourcing this module does not scan or write data.
reconstruct_table6_outcome <- function(df, raw_data_dir = NULL) {
  dt <- data.table::copy(data.table::as.data.table(df))
  prepared <- prepare_model_dataframe(dt)
  target_choices <- prepared[author_voted_regression_row %in% TRUE,
    .(proposal, choice_idx, N_choices, tot_votes_2, is_author_choice_expanded)]
  if (nrow(target_choices) == 0L) return(dt)
  if (anyDuplicated(target_choices[, .(proposal, choice_idx)])) {
    stop("Duplicate proposal-choice keys in Table 6 input.", call. = FALSE)
  }
  target <- target_choices[, .(
    N_choices = N_choices[1L], release_ballots = tot_votes_2[1L],
    author_choice = choice_idx[is_author_choice_expanded == 1L][1L]
  ), by = proposal]
  # An author-voted proposal with only one accepted ballot cannot contain an
  # additional voter. Keep its choices in the data, with no removed-share outcome.
  target <- target[release_ballots > 1]
  scores <- data.table::copy(target_choices[, .(proposal, choice_idx)])
  scores[, removed_share := NA_real_]
  if (nrow(target) > 0L) {
    if (is.null(raw_data_dir)) {
      root <- find_project_root()
      candidates <- c(file.path(root, "analysis", "data", "2024", "raw"),
                      file.path(root, "data", "2024", "raw"))
      existing <- candidates[dir.exists(candidates)]
      if (length(existing) == 0L) {
        stop("Table 6 requires raw proposal and vote files; supply raw_data_dir.", call. = FALSE)
      }
      raw_data_dir <- existing[[1L]]
    }
    proposals <- data.table::fread(file.path(raw_data_dir, "proposals_0.csv.gz"),
      select = c("id", "author"), colClasses = "character", showProgress = FALSE)
    proposals[, `:=`(id = trimws(tolower(id)), author = trimws(tolower(author)))]
    proposals <- proposals[id %in% target$proposal]
    if (anyDuplicated(proposals$id) || !setequal(proposals$id, target$proposal) ||
        anyNA(proposals$author) || any(!nzchar(proposals$author))) {
      stop("Missing or ambiguous proposal authors for Table 6.", call. = FALSE)
    }
    target <- merge(target, proposals, by.x = "proposal", by.y = "id", sort = FALSE)
    files <- sort(list.files(file.path(raw_data_dir, "votes"),
      pattern = "[.]csv[.]gz$", full.names = TRUE))
    if (length(files) == 0L) stop("No raw vote shards found for Table 6.", call. = FALSE)
    totals <- vector("list", length(files))
    quality <- vector("list", length(files))
    for (i in seq_along(files)) {
      votes <- data.table::fread(files[[i]],
        select = c("proposal", "voter", "choice", "vp"),
        colClasses = list(character = c("proposal", "voter", "choice")),
        showProgress = FALSE)
      votes[, `:=`(proposal = trimws(tolower(proposal)), voter = trimws(tolower(voter)))]
      votes <- merge(votes[proposal %in% target$proposal], target,
                     by = "proposal", sort = FALSE)
      if (nrow(votes) == 0L) next
      if (anyNA(votes$voter) || any(!nzchar(votes$voter)) || any(!is.finite(votes$vp))) {
        stop("Missing voter or nonfinite voting power in Table 6 raw ballots.", call. = FALSE)
      }
      # Same scalar/singleton-list parser as the reviewed reconstruction.
      parsed_choice <- trimws(votes$choice)
      parsed_choice <- sub('^"(.*)"$', '\\1', parsed_choice)
      parsed_choice <- sub("^\\[\\s*([^,]+)\\s*\\]$", "\\1", parsed_choice)
      votes[, choice_idx := suppressWarnings(as.numeric(parsed_choice))]
      votes <- votes[is.finite(choice_idx) & choice_idx == floor(choice_idx) &
                       choice_idx >= 1 & choice_idx <= N_choices]
      votes[, is_author := voter == author]
      quality[[i]] <- votes[, .(
        accepted_ballots = .N, author_ballots = sum(is_author),
        author_choice_matches = sum(is_author & choice_idx == author_choice)
      ), by = proposal]
      # Preserve finite signed power and the existing VP-state policy. In
      # particular, do not replace genuine tiny power with an absolute epsilon.
      totals[[i]] <- votes[is_author == FALSE, .(
        other_power = sum(vp), positive_ballots = sum(vp > 0)
      ), by = .(proposal, choice_idx)]
    }
    quality <- data.table::rbindlist(quality)[, lapply(.SD, sum), by = proposal]
    quality <- merge(target, quality, by = "proposal", all.x = TRUE, sort = FALSE)
    if (anyNA(quality$accepted_ballots) ||
        any(quality$accepted_ballots != quality$release_ballots) ||
        any(quality$author_ballots != 1L) || any(quality$author_choice_matches != 1L)) {
      stop("Raw accepted ballots do not match Table 6 proposal/author records.", call. = FALSE)
    }
    totals <- data.table::rbindlist(totals)[,
      .(other_power = sum(other_power), positive_ballots = sum(positive_ballots)),
      by = .(proposal, choice_idx)]
    if (any(!is.finite(totals$other_power)) || any(totals$other_power < 0)) {
      stop("Invalid remaining choice power in Table 6.", call. = FALSE)
    }
    totals[, `:=`(other_total = sum(other_power),
                  other_positive_ballots = sum(positive_ballots)), by = proposal]
    totals <- totals[other_total > 0 & other_positive_ballots > 0]
    retained <- scores[proposal %in% totals$proposal]
    retained <- merge(retained[, .(proposal, choice_idx)], totals,
                      by = c("proposal", "choice_idx"), all.x = TRUE, sort = FALSE)
    # Choices with no remaining ballots have zero share, not missing share.
    retained[, removed_share := data.table::fifelse(is.na(other_power), 0,
                                                   other_power / other_total)]
    sums <- retained[, .(share_sum = sum(removed_share)), by = proposal]
    if (any(!is.finite(sums$share_sum)) || any(abs(sums$share_sum - 1) > 1e-12)) {
      stop("Remaining shares do not sum to one in Table 6.", call. = FALSE)
    }
    scores[retained, on = .(proposal, choice_idx), removed_share := i.removed_share]
  }
  # Update only the Table 6 author-voted Main choices; preserve observed shares,
  # all other voting systems, and all proposal-choice keys.
  dt[, table6_proposal_key := trimws(tolower(as.character(proposal))) ]
  dt[scores, on = .(table6_proposal_key = proposal, choice_idx),
     choice_share_no_author_vp := i.removed_share]
  dt[, table6_proposal_key := NULL]
  dt[]
}

as_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) || is.integer(x)) {
    out <- rep(NA_integer_, length(x))
    out[!is.na(x)] <- as.integer(x[!is.na(x)] != 0)
    return(out)
  }

  lower <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(lower))
  out[lower %in% c("true", "t", "1", "yes", "y")] <- 1L
  out[lower %in% c("false", "f", "0", "no", "n")] <- 0L
  out
}

assert_unit_interval <- function(x, column_name) {
  bad <- !is.na(x) & (x < 0 | x > 1)
  if (any(bad)) {
    stop(
      "Column ", column_name, " has ", sum(bad),
      " impossible share value(s) outside [0, 1].",
      call. = FALSE
    )
  }
}

prepare_model_dataframe <- function(df) {
  require_packages("data.table")
  dt <- data.table::copy(data.table::as.data.table(df))
  header <- names(dt)
  assert_required_columns(header, require_flagged = TRUE)

  share_col <- resolve_column(header, "choice_share", canonical_column_specs$choice_share)
  author_col <- resolve_column(header, "is_author_choice", canonical_column_specs$is_author_choice)
  created_col <- resolve_column(header, "created_rel", canonical_column_specs$created_rel)

  dt[, proposal := trimws(tolower(as.character(proposal)))]
  dt[, space := trimws(as.character(space))]
  dt[, type := trimws(as.character(type))]
  dt[, stance := trimws(as.character(stance))]
  if ("flagged" %in% names(dt)) {
    dt[, flagged := as_binary01(flagged)]
    flagged_proposals <- unique(dt[
      flagged == 1L & !is.na(proposal) & nzchar(proposal),
      proposal
    ])
    if (length(flagged_proposals) > 0) {
      dt <- dt[!(proposal %in% flagged_proposals)]
    }
  }

  dt[, choice_share := as.numeric(get(share_col))]
  dt[, choice_share_no_author_vp := as.numeric(choice_share_no_author_vp)]
  dt[, is_author_choice := as_binary01(get(author_col))]
  dt[, created_rel := as.numeric(get(created_col))]
  dt[, is_infavor := as_binary01(is_infavor)]
  dt[, choice_idx := as.numeric(choice_idx)]
  dt[, N_choices := as.numeric(N_choices)]
  dt[, author_pos6 := as_binary01(author_pos6)]
  dt[, choice_text_len_rel := as.numeric(choice_text_len_rel)]
  dt[, n_votes := as.numeric(n_votes)]
  dt[, tot_votes_2 := as.numeric(tot_votes_2)]
  dt[, vote_count_share := data.table::fifelse(
    !is.na(tot_votes_2) & tot_votes_2 > 0,
    n_votes / tot_votes_2,
    NA_real_
  )]
  dt[, log_tot_votes_2 := data.table::fifelse(
    !is.na(tot_votes_2) & tot_votes_2 >= 0,
    log1p(tot_votes_2),
    NA_real_
  )]

  dt[, proposal_choice_rows := .N, by = proposal]
  dt <- dt[
    !is.na(proposal) &
      nzchar(proposal) &
      proposal_choice_rows > 1 &
      (is.na(N_choices) | N_choices > 1)
  ]
  dt[, proposal_choice_rows := NULL]

  assert_unit_interval(dt$choice_share, "choice_share")
  assert_unit_interval(dt$choice_share_no_author_vp, "choice_share_no_author_vp")
  assert_unit_interval(dt$vote_count_share, "vote_count_share")

  dt[, author_voted := as.integer(any(!is.na(is_author_choice))), by = proposal]
  dt[, is_author_choice_expanded := data.table::fifelse(
    author_voted == 1L,
    is_author_choice,
    0L
  )]
  dt[, author_pos6_expanded := data.table::fifelse(
    author_voted == 1L,
    author_pos6,
    0L
  )]
  dt[, is_first_choice := as.integer(choice_idx == 1)]
  dt[, first_author_choice := as.integer(is_author_choice == 1 & choice_idx == 1)]
  dt[, nonfirst_author_choice := as.integer(is_author_choice == 1 & choice_idx != 1)]
  dt[, first_author_choice_expanded := as.integer(is_author_choice_expanded == 1 & choice_idx == 1)]
  dt[, nonfirst_author_choice_expanded := as.integer(is_author_choice_expanded == 1 & choice_idx != 1)]
  dt[, infavor_author_choice_expanded := as.integer(
    is_author_choice_expanded == 1 & is_infavor == 1
  )]
  dt[, noninfavor_author_choice_expanded := as.integer(
    is_author_choice_expanded == 1 & is_infavor == 0
  )]
  dt[, single_preference_row := type %in% c("single-choice", "basic")]
  dt[, full_sample_supported_row := type %in% c("single-choice", "basic", "ranked-choice", "approval")]

  main_vars <- c(
    "proposal", "space", "type", "choice_idx", "N_choices", "choice_share",
    "author_voted", "is_author_choice_expanded", "is_infavor", "created_rel",
    "author_pos6_expanded", "choice_text_len_rel"
  )
  dt[, full_sample_regression_row := full_sample_supported_row & stats::complete.cases(.SD),
     .SDcols = main_vars]
  dt[, all_vote_types_regression_row := stats::complete.cases(.SD), .SDcols = main_vars]
  dt[, main_regression_row := single_preference_row & full_sample_regression_row]
  dt[, author_voted_regression_row := main_regression_row & author_voted == 1L]
  dt[, vote_count_share_complete := all(!is.na(vote_count_share)), by = proposal]
  dt[, vote_count_share_sum := sum(vote_count_share, na.rm = TRUE), by = proposal]
  dt[, vote_count_share_valid_proposal := vote_count_share_complete &
       abs(vote_count_share_sum - 1) <= 1e-6]
  dt[, vote_count_outcome_row := main_regression_row & vote_count_share_valid_proposal]
  used_vote_count <- dt[vote_count_outcome_row %in% TRUE]
  if (nrow(used_vote_count) > 0) {
    bad_vote_count_sums <- used_vote_count[
      ,
      .(vote_count_share_sum = sum(vote_count_share, na.rm = TRUE)),
      by = proposal
    ][abs(vote_count_share_sum - 1) > 1e-6]
    if (nrow(bad_vote_count_sums) > 0) {
      stop(
        "vote_count_share does not sum to 1 within ",
        nrow(bad_vote_count_sums),
        " proposal(s) used by the vote-count outcome model.",
        call. = FALSE
      )
    }
  }
  dt[, participation_control_row := main_regression_row & !is.na(log_tot_votes_2)]
  dt[, no_author_vp_row := author_voted_regression_row & !is.na(choice_share_no_author_vp)]
  dt[, low_participation_row := main_regression_row & !is.na(tot_votes_2) & tot_votes_2 > 1]

  dt[]
}

check_expected_counts <- function(model_df) {
  expected <- data.frame(
    name = c(
      "active rows",
      "expanded main regression sample",
      "author-voted-only regression sample",
      "vote-count-share outcome sample",
      "participation-control sensitivity sample",
      "no-author-vp sensitivity"
    ),
    observed = c(
      nrow(model_df),
      sum(model_df$main_regression_row, na.rm = TRUE),
      sum(model_df$author_voted_regression_row, na.rm = TRUE),
      sum(model_df$vote_count_outcome_row, na.rm = TRUE),
      sum(model_df$participation_control_row, na.rm = TRUE),
      sum(model_df$no_author_vp_row, na.rm = TRUE)
    ),
    expected = c(398218, 293816, 172171, 293816, 293816, 118483),
    tolerance = c(2500, 2500, 2500, 2500, 2500, 2500),
    stringsAsFactors = FALSE
  )

  drift <- expected[abs(expected$observed - expected$expected) > expected$tolerance, ]
  if (nrow(drift) > 0) {
    warning(
      "Observed sample counts differ from expected sanity-check values: ",
      paste(
        paste0(drift$name, " observed=", drift$observed, " expected~", drift$expected),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
  expected
}
