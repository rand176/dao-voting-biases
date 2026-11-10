format_int <- function(x) {
  ifelse(is.na(x), "", format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

format_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

format_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3)))
}

format_currency <- function(x) {
  ifelse(
    is.na(x),
    "",
    paste0("$", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE, trim = TRUE))
  )
}

format_match_snapshot_compact <- function(match_status, snapshot_status) {
  match_flag <- ifelse(as.character(match_status) == "accepted", "Y", "N")
  snapshot_flag <- ifelse(as.character(snapshot_status) == "Active", "Y", "N")
  paste(match_flag, snapshot_flag, sep = "/")
}

format_coef_se <- function(estimate, std_error) {
  ifelse(
    is.na(estimate) | is.na(std_error),
    "",
    paste0(format_num(estimate, 3), " (", format_num(std_error, 3), ")")
  )
}

robustness_short_name_map <- function() {
  c(
    "Single preference" = "Main",
    "Observed share, expanded single-preference sample" = "Main",
    "Author-voted only" = "Auth-voted",
    "Observed share, author-voted-only sample" = "Auth-voted",
    "Compatible full sample" = "Compat full",
    "Observed share, compatible full sample" = "Compat full",
    "FULL sample" = "All systems",
    "Observed share, FULL all-voting-system sample" = "All systems",
    "Participation control" = "Vote-control",
    "Observed share, participation-control sensitivity" = "Vote-control",
    "Top 25 Ethereum-TVL DAO subset" = "TVL-25",
    "Observed share, Top 25 Ethereum-TVL DAO subset" = "TVL-25",
    "Top 50 Ethereum-TVL DAO subset" = "TVL-50",
    "Observed share, Top 50 Ethereum-TVL DAO subset" = "TVL-50",
    "Top 100 Ethereum-TVL DAO subset" = "TVL-100",
    "Observed share, Top 100 Ethereum-TVL DAO subset" = "TVL-100",
    "Proposal FE" = "Proposal FE",
    "Observed share, proposal fixed effects" = "Proposal FE",
    "Low-participation exclusion" = "Low-part.",
    "Observed share, excluding tot_votes_2 <= 1" = "Low-part.",
    "Top-space exclusion" = "No top-5",
    "Observed share, excluding top 5 spaces" = "No top-5",
    "Observed share, matched no-author-vp sample" = "Matched obs.",
    "No-author-vp share, matched sample" = "No-author VP",
    "Vote-count share, expanded single-preference sample" = "Vote-count"
  )
}

robustness_short_name <- function(x) {
  x <- as.character(x)
  map <- robustness_short_name_map()
  out <- unname(map[x])
  top_space_rows <- is.na(out) & grepl("^Observed share, excluding top [0-9]+ spaces$", x)
  out[top_space_rows] <- sub(
    "^Observed share, excluding top ([0-9]+) spaces$",
    "No top-\\1",
    x[top_space_rows]
  )
  ifelse(is.na(out), x, out)
}

robustness_variation_key_rows <- function(models, effect_tables) {
  n_for_model <- function(model_name) {
    if (is.null(models[[model_name]])) return(NA_integer_)
    as.integer(stats::nobs(models[[model_name]]))
  }

  short_names <- c(
    "Main",
    "Vote-count",
    "Auth-voted", "Low-part.", "Vote-control",
    "TVL-25", "TVL-50", "TVL-100",
    "Proposal FE",
    "No top-5",
    "Expanded", "Full"
  )
  midrule_before_rows <- c(2L, 3L, 6L, 9L, 10L, 11L)

  data.frame(
    order = seq_along(short_names),
    short_name = short_names,
    section_break_before = seq_along(short_names) %in% midrule_before_rows,
    dimension = c(
      "Baseline",
      "Outcome",
      "Participation", "Participation", "Participation",
      "TVL", "TVL", "TVL",
      "Fixed effects",
      "Space influence",
      "Proposal type", "Proposal type"
    ),
    definition = c(
      "Single-preference proposals: single-choice and basic voting systems.",
      "Vote-count share (number of votes divided by total proposal votes) as outcome.",
      "Restricted to proposals where the author voted.",
      "Excludes proposals with fewer than two votes.",
      "Adds log total proposal votes.",
      "Restricted to Top 25 Ethereum-TVL DAOs matched against DefiLlama ranking.",
      "Restricted to Top 50 Ethereum-TVL DAOs matched against DefiLlama ranking.",
      "Restricted to Top 100 Ethereum-TVL DAOs matched against DefiLlama ranking.",
      "Uses proposal fixed effects.",
      "Excludes the five largest contributing spaces.",
      "Includes approval and ranked-choice voting systems.",
      "Includes all voting systems, including weighted and quadratic."
    ),
    n = c(
      n_for_model("single_preference_any"),
      n_for_model("vote_count_share_any"),
      n_for_model("author_voted_only_any"),
      n_for_model("low_participation_any"),
      n_for_model("participation_control_any"),
      n_for_model("top_tvl_dao_any"),
      n_for_model("top_tvl_dao_top50_any"),
      n_for_model("top_tvl_dao_top100_any"),
      n_for_model("proposal_fe_any"),
      n_for_model("top_space_excluded_any"),
      n_for_model("full_sample_any"),
      n_for_model("all_vote_types_any")
    ),
    stringsAsFactors = FALSE
  )
}

format_robustness_variation_key_table <- function(models, effect_tables) {
  rows <- robustness_variation_key_rows(models, effect_tables)
  out <- data.frame(
    `#` = rows$order,
    `Short name` = rows$short_name,
    Dimension = rows$dimension,
    Definition = rows$definition,
    N = format_int(rows$n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "midrule_before_rows") <- rows$order[rows$section_break_before]
  out
}

format_effect_table <- function(df, use_short_names = FALSE, include_n = TRUE) {
  specification <- if (isTRUE(use_short_names)) robustness_short_name(df$model) else df$model
  out <- data.frame(
    Specification = specification,
    FE = df$fixed_effect,
    Contrast = df$term_label,
    `Coef. (SE)` = format_coef_se(df$estimate, df$std_error),
    p = format_p(df$p_value),
    `AME (pp)` =
      format_num(df$average_adjusted_prediction_difference_pp, 2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_n)) {
    out <- data.frame(
      Specification = out$Specification,
      FE = out$FE,
      N = format_int(df$model_nobs),
      out[, setdiff(names(out), c("Specification", "FE")), drop = FALSE],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  out
}

format_author_choice_diagnostics_matrix <- function(df) {
  required_columns <- c(
    "model", "fixed_effect", "model_nobs", "term_label",
    "average_adjusted_prediction_difference_pp"
  )
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      "Author-choice diagnostics missing column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  required_cells <- data.frame(
    row_id = c(
      "matched_observed", "matched_observed", "matched_observed", "matched_observed", "matched_observed",
      "matched_no_author", "matched_no_author", "matched_no_author", "matched_no_author", "matched_no_author",
      "proposal_fe", "proposal_fe", "proposal_fe", "proposal_fe", "proposal_fe"
    ),
    column = rep(c("Pooled", "First pos.", "Non-first", "In favor", "Not in favor"), 3),
    model = c(
      "Observed share, matched no-author-vp sample",
      "Observed share, first/non-first split: first position",
      "Observed share, first/non-first split: non-first position",
      "Observed share, approve/non-approve split: in favor",
      "Observed share, approve/non-approve split: not in favor",
      "No-author-vp share, matched sample",
      "No-author-vp share, first/non-first split: first position",
      "No-author-vp share, first/non-first split: non-first position",
      "No-author-vp share, approve/non-approve split: in favor",
      "No-author-vp share, approve/non-approve split: not in favor",
      "Observed share, proposal fixed effects",
      "Observed share, proposal fixed effects split: first position",
      "Observed share, proposal fixed effects split: non-first position",
      "Observed share, proposal fixed effects approve/non-approve split: in favor",
      "Observed share, proposal fixed effects approve/non-approve split: not in favor"
    ),
    fixed_effect = c(rep("space", 10), rep("proposal", 5)),
    term_label = c(
      "Author choice",
      "Author choice, first position",
      "Author choice, non-first position",
      "Author choice, in favor",
      "Author choice, not in favor",
      "Author choice",
      "Author choice, first position",
      "Author choice, non-first position",
      "Author choice, in favor",
      "Author choice, not in favor",
      "Author choice",
      "Author choice, first position",
      "Author choice, non-first position",
      "Author choice, in favor",
      "Author choice, not in favor"
    ),
    stringsAsFactors = FALSE
  )

  matched_cells <- lapply(seq_len(nrow(required_cells)), function(row_idx) {
    required <- required_cells[row_idx, , drop = FALSE]
    matches <- df[
      df$model == required$model &
        df$fixed_effect == required$fixed_effect &
        df$term_label == required$term_label,
      ,
      drop = FALSE
    ]
    if (nrow(matches) != 1L) {
      stop(
        "Author-choice diagnostics matrix expected exactly one source row for ",
        required$row_id, " / ", required$column, " but found ", nrow(matches), ".",
        call. = FALSE
      )
    }
    data.frame(
      row_id = required$row_id,
      column = required$column,
      model_nobs = matches$model_nobs[[1]],
      value = matches$average_adjusted_prediction_difference_pp[[1]],
      stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, matched_cells)

  row_ids <- c("matched_observed", "matched_no_author", "proposal_fe")
  row_n <- vapply(row_ids, function(row_id) {
    n_values <- unique(cells$model_nobs[cells$row_id == row_id])
    if (length(n_values) != 1L) {
      stop(
        "Author-choice diagnostics matrix has inconsistent N for row: ",
        row_id,
        call. = FALSE
      )
    }
    n_values[[1]]
  }, numeric(1))

  cell_value <- function(row_id, column) {
    value <- cells$value[cells$row_id == row_id & cells$column == column]
    if (length(value) != 1L) {
      stop(
        "Author-choice diagnostics matrix has ambiguous cell for ",
        row_id, " / ", column,
        call. = FALSE
      )
    }
    format_num(value[[1]], 2)
  }

  active_row_ids <- c("matched_observed", "matched_no_author")
  out <- data.frame(
    author_vp = c("Included", "Removed"),
    N = format_int(row_n[active_row_ids]),
    Pooled = vapply(active_row_ids, cell_value, character(1), column = "Pooled"),
    `First pos.` = vapply(active_row_ids, cell_value, character(1), column = "First pos."),
    `Non-first` = vapply(active_row_ids, cell_value, character(1), column = "Non-first"),
    Approve = vapply(active_row_ids, cell_value, character(1), column = "In favor"),
    `Non-Approve` = vapply(active_row_ids, cell_value, character(1), column = "Not in favor"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(out)[[1]] <- "Author's VP"
  attr(out, "proposal_fe_comment") <- paste0(
    "% Main, proposal FE & Included & ",
    format_int(row_n[["proposal_fe"]]),
    " & ",
    paste(
      vapply(
        c("Pooled", "First pos.", "Non-first", "In favor", "Not in favor"),
        function(column) cell_value("proposal_fe", column),
        character(1)
      ),
      collapse = " & "
    ),
    " \\\\"
  )
  out
}

format_three_bias_summary_table <- function(df, use_short_names = FALSE) {
  value_cols <- setdiff(names(df), c("Bias contrast", "Interpretation"))
  out <- data.frame(
    Specification = if (isTRUE(use_short_names)) robustness_short_name(value_cols) else value_cols,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (row_idx in seq_len(nrow(df))) {
    out[[df[["Bias contrast"]][[row_idx]]]] <-
      format_num(as.numeric(df[row_idx, value_cols]), digits = 2)
  }
  out
}

format_ame_ci_cell <- function(ame_pp, ci_low_pp, ci_high_pp) {
  paste0(
    "\\makecell[r]{",
    format_num(ame_pp, 2),
    "\\\\{\\scriptsize ",
    format_ci_pp(ci_low_pp, ci_high_pp),
    "}}"
  )
}

format_three_bias_summary_ci_table <- function(df,
                                               inference_source = default_three_bias_forest_inference_path(),
                                               models = NULL,
                                               use_short_names = FALSE) {
  source <- validate_three_bias_forest_inference_source(
    inference_source,
    three_bias_summary = df,
    models = models
  )
  specs <- shared_bias_summary_specifications()
  contrasts <- three_bias_contrast_specifications()
  out <- data.frame(
    Specification = if (isTRUE(use_short_names)) {
      robustness_short_name(specs$specification)
    } else {
      specs$specification
    },
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (contrast_idx in seq_len(nrow(contrasts))) {
    contrast <- contrasts$bias_contrast[[contrast_idx]]
    rows <- source[source$bias_contrast == contrast, , drop = FALSE]
    rows <- rows[order(rows$specification_order), , drop = FALSE]
    if (!identical(rows$specification, specs$specification)) {
      stop(
        "Three-bias CI source order does not match shared specifications for ",
        contrast,
        ".",
        call. = FALSE
      )
    }
    out[[contrast]] <- format_ame_ci_cell(
      rows$ame_pp,
      rows$fwer_ci_low_pp,
      rows$fwer_ci_high_pp
    )
  }
  out
}

format_ci_pp <- function(lower, upper) {
  paste0("[", format_num(lower, 2), ", ", format_num(upper, 2), "]")
}


three_bias_summary_align <- function() {
  "@{}>{\\RaggedRight\\arraybackslash}Xrrr@{}"
}

three_bias_summary_ci_align <- function() {
  "@{}>{\\RaggedRight\\arraybackslash}X>{\\RaggedLeft\\arraybackslash}p{0.19\\linewidth}>{\\RaggedLeft\\arraybackslash}p{0.19\\linewidth}>{\\RaggedLeft\\arraybackslash}p{0.19\\linewidth}@{}"
}

three_bias_summary_header <- function() {
  paste(
    c(
      "Specification",
      "\\makecell[r]{Author\\\\choice}",
      "\\makecell[r]{First vs\\\\second}",
      "\\makecell[r]{Approve\\\\stance}"
    ),
    collapse = " & "
  )
}

first_choice_three_bias_summary_header <- function() {
  paste(
    c(
      "Specification",
      "\\makecell[r]{Author\\\\choice}",
      "\\makecell[r]{First vs\\\\non-first}",
      "\\makecell[r]{Approve\\\\stance}"
    ),
    collapse = " & "
  )
}


format_attrition_table <- function(df) {
  data.frame(
    Sample = df$sample,
    Rows = format_int(df$rows),
    Proposals = format_int(df$proposals),
    Spaces = format_int(df$spaces),
    Note = df$note,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_stance_table <- function(df) {
  data.frame(
    When = df$when,
    `Proposal pattern` = df$proposal_pattern,
    Proposals = format_int(df$proposals),
    Rows = format_int(df$rows),
    Spaces = format_int(df$spaces),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_space_exclusion_table <- function(models) {
  spaces <- models$excluded_spaces
  if (length(spaces) == 0) {
    spaces <- NA_character_
  }
  data.frame(
    Rank = seq_along(spaces),
    Space = spaces,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


latex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("<", "\\textless{}", x, fixed = TRUE)
  x <- gsub(">", "\\textgreater{}", x, fixed = TRUE)
  x
}

latex_makecell_left <- function(x, split_semicolon = FALSE) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  vapply(x, function(value) {
    value <- trimws(value)
    if (!nzchar(value)) {
      return("")
    }
    parts <- if (split_semicolon) {
      trimws(strsplit(value, ";", fixed = TRUE)[[1]])
    } else {
      value
    }
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) {
      return("")
    }
    paste0("\\makecell[l]{", paste(latex_escape(parts), collapse = "\\\\"), "}")
  }, character(1), USE.NAMES = FALSE)
}

latex_url_arg <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\", "/", x)
  gsub("([{}])", "\\\\\\1", x, perl = TRUE)
}

latex_url_same_style <- function(x) {
  x <- latex_url_arg(x)
  ifelse(nzchar(x), paste0("{\\urlstyle{same}\\url{", x, "}}"), "")
}


sanitize_latex_label <- function(label) {
  label <- as.character(label)[[1]]
  gsub("[^A-Za-z0-9:.-]+", "-", label)
}

default_latex_align <- function(df, use_tabularx) {
  if (use_tabularx) {
    return(paste0(
      "@{}>{\\RaggedRight\\arraybackslash}X",
      paste(rep("r", max(ncol(df) - 1, 0)), collapse = ""),
      "@{}"
    ))
  }
  paste0("@{}", paste(rep("l", ncol(df)), collapse = ""), "@{}")
}

latex_table <- function(df, caption, label, align = NULL, notes = NULL,
                        tabcolsep = NULL, arraystretch = "1.10",
                        header_override = NULL, escape = TRUE,
                        font_size = "\\small",
                        notes_escape = TRUE) {
  midrule_before_rows <- attr(df, "midrule_before_rows", exact = TRUE)
  midrule_before_rows <- sort(unique(as.integer(midrule_before_rows)))
  midrule_before_rows <- midrule_before_rows[!is.na(midrule_before_rows)]
  use_tabularx <- ncol(df) >= 7
  if (is.null(align)) {
    align <- default_latex_align(df, use_tabularx)
  } else if (grepl("X", align, fixed = TRUE)) {
    use_tabularx <- TRUE
  }
  if (is.null(tabcolsep)) {
    tabcolsep <- if (ncol(df) >= 7) "3pt" else "4pt"
  }
  escaped <- if (escape) {
    as.data.frame(lapply(df, latex_escape), stringsAsFactors = FALSE)
  } else {
    as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  }
  names(escaped) <- if (escape) latex_escape(names(df)) else names(df)

  header <- if (is.null(header_override)) {
    paste(names(escaped), collapse = " & ")
  } else {
    header_override
  }
  body <- if (nrow(escaped) > 0) {
    apply(escaped, 1, function(row) paste(row, collapse = " & "))
  } else {
    character()
  }
  body_lines <- paste0(body, " \\\\")
  midrule_before_rows <- midrule_before_rows[
    midrule_before_rows >= 1L & midrule_before_rows <= length(body_lines)
  ]
  if (length(body_lines) > 0 && length(midrule_before_rows) > 0) {
    body_lines <- unlist(
      lapply(seq_along(body_lines), function(row_idx) {
        if (row_idx %in% midrule_before_rows) {
          c("\\midrule", body_lines[[row_idx]])
        } else {
          body_lines[[row_idx]]
        }
      }),
      use.names = FALSE
    )
  }
  begin_table <- if (use_tabularx) {
    paste0("\\begin{tabularx}{\\linewidth}{", align, "}")
  } else {
    paste0("\\begin{tabular}{", align, "}")
  }
  end_table <- if (use_tabularx) "\\end{tabularx}" else "\\end{tabular}"
  note_text <- paste(notes, collapse = " ")
  if (notes_escape) {
    note_text <- latex_escape(note_text)
  }
  note_lines <- if (is.null(notes) || length(notes) == 0) {
    character()
  } else {
    c(
      "\\vspace{0.25em}",
      paste0(
        "\\parbox{\\linewidth}{\\footnotesize Notes: ",
        note_text,
        "}"
      )
    )
  }

  c(
    "% Generated by src/r/reporting/tables.R (python run.py models).",
    "\\begin{table}[!tbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", sanitize_latex_label(label), "}"),
    font_size,
    paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}"),
    paste0("\\renewcommand{\\arraystretch}{", arraystretch, "}"),
    begin_table,
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    body_lines,
    "\\bottomrule",
    end_table,
    note_lines,
    "\\end{table}"
  )
}

write_latex_table <- function(df, path, caption, label, align = NULL, notes = NULL,
                              tabcolsep = NULL, arraystretch = "1.10",
                              header_override = NULL, escape = TRUE,
                              font_size = "\\small",
                              notes_escape = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    latex_table(
      df,
      caption,
      label,
      align,
      notes,
      tabcolsep,
      arraystretch,
      header_override = header_override,
      escape = escape,
      font_size = font_size,
      notes_escape = notes_escape
    ),
    path
  )
  message("Wrote ", path)
  invisible(path)
}

write_robustness_variation_key_maintext_table <- function(models, effect_tables, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- latex_table(
    format_robustness_variation_key_table(models, effect_tables),
    "Robustness specification key",
    "tab:robustness-variation-key",
    align = "@{}r>{\\RaggedRight\\arraybackslash}p{0.12\\linewidth}>{\\RaggedRight\\arraybackslash}p{0.14\\linewidth}>{\\RaggedRight\\arraybackslash}Xr@{}",
    notes = paste0(
      "Variations 2-10 are variations of the main single-preferences sample; variations 11-12 expands the sample to multi-preference proposals. ",
      "The TVL samples contain the DAOs matched against the Top 100 Decentralized Finance (DeFi) protocols ranked by DefiLlama ",
      "(TVL refers to the median daily Ethereum total value locked by DeFi protocols over the study period); accordingly TVL-100 contains less than 100 DAOs because not every protocol can be matched ",
      "(see Appendix \\ref{sec:app:defillama} for details)."
    ),
    tabcolsep = "3pt",
    notes_escape = FALSE
  )
  expanded_idx <- grep("^11 & Expanded & Proposal type", lines)
  if (length(expanded_idx) == 1L) {
    lines <- append(lines, "\\midrule", after = expanded_idx[[1]] - 1L)
  }
  writeLines(lines, path)
  message("Wrote ", path)
  invisible(path)
}

write_author_choice_diagnostics_maintext_table <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  formatted <- format_author_choice_diagnostics_matrix(df)
  proposal_fe_comment <- attr(formatted, "proposal_fe_comment", exact = TRUE)
  row_line <- function(row_idx) {
    row <- formatted[row_idx, , drop = FALSE]
    paste0(
      " ",
      row[["Author's VP"]],
      " & ",
      row[["N"]],
      " & ",
      paste(
        unname(unlist(row[, c("Pooled", "First pos.", "Non-first", "Approve", "Non-Approve")])),
        collapse = " & "
      ),
      " \\\\"
    )
  }
  lines <- c(
    "% Generated by src/r/reporting/tables.R (python run.py models).",
    "\\begin{table}[!tbp]",
    "\\centering",
    "\\caption{Author-specific diagnostics for author-choice AMEs}",
    "\\label{tab:robustness-author-choice-diagnostics}",
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{2.5pt}",
    "\\renewcommand{\\arraystretch}{1.10}",
    "\\begin{tabularx}{\\linewidth}{@{}>{\\RaggedRight\\arraybackslash}X>{\\RaggedRight\\arraybackslash}p{0.18\\linewidth}r|rr|rr}",
    "\\toprule",
    " & & & \\multicolumn{2}{c|}{Author-Position Split} & \\multicolumn{2}{c}{Author-Stance Split} \\\\",
    "Author's VP & N & Pooled & First pos. & Non-first & Approve & Non-Approve \\\\",
    "\\midrule",
    row_line(1L),
    row_line(2L),
    "% \\midrule",
    proposal_fe_comment,
    "\\bottomrule",
    "\\end{tabularx}",
    "\\vspace{0.25em}",
    paste0(
      "\\parbox{\\linewidth}{\\footnotesize Notes: the sample includes all proposals in which the author voted and  with at least one more vote casted with positive voting power. Cells report response-scale author-choice AMEs in percentage points.}",
      " "
    ),
    "\\end{table}"
  )
  writeLines(lines, path)
  message("Wrote ", path)
  invisible(path)
}

wrap_modelsummary_latex <- function(tabular_lines, caption, label, notes = NULL,
                                    notes_escape = TRUE) {
  note_lines <- if (is.null(notes) || length(notes) == 0) {
    character()
  } else {
    note_text <- paste(notes, collapse = " ")
    if (isTRUE(notes_escape)) {
      note_text <- latex_escape(note_text)
    }
    c(
      "\\vspace{0.25em}",
      paste0(
        "\\parbox{\\linewidth}{\\footnotesize Notes: ",
        note_text,
        "}"
      )
    )
  }

  c(
    "% Generated by src/r/reporting/tables.R (python run.py models).",
    "\\begin{table}[!tbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", sanitize_latex_label(label), "}"),
    "\\small",
    "\\setlength{\\tabcolsep}{4pt}",
    "\\renewcommand{\\arraystretch}{1.10}",
    tabular_lines,
    note_lines,
    "\\end{table}"
  )
}

add_booktabs_to_modelsummary_tabular <- function(tabular_lines) {
  if (any(grepl("\\\\toprule", tabular_lines))) {
    return(tabular_lines)
  }
  begin_idx <- grep("\\\\begin\\{tabular\\}", tabular_lines)
  end_idx <- grep("\\\\end\\{tabular\\}", tabular_lines)
  if (length(begin_idx) == 0 || length(end_idx) == 0 || end_idx[[1]] <= begin_idx[[1]]) {
    return(tabular_lines)
  }

  header_idx <- begin_idx[[1]] + 1L
  c(
    tabular_lines[seq_len(begin_idx[[1]])],
    "\\toprule",
    tabular_lines[seq.int(header_idx, header_idx)],
    "\\midrule",
    tabular_lines[seq.int(header_idx + 1L, end_idx[[1]] - 1L)],
    "\\bottomrule",
    tabular_lines[seq.int(end_idx[[1]], length(tabular_lines))]
  )
}

modelsummary_tabularx_lines <- function(tabular_lines, model_count) {
  align <- paste0(
    "@{}>{\\RaggedRight\\arraybackslash}X",
    paste(rep("r", model_count), collapse = ""),
    "@{}"
  )
  begin_idx <- grep("\\\\begin\\{tabular\\}", tabular_lines)
  end_idx <- grep("\\\\end\\{tabular\\}", tabular_lines)
  if (length(begin_idx) > 0) {
    tabular_lines[begin_idx] <- paste0("\\begin{tabularx}{\\linewidth}{", align, "}")
  }
  if (length(end_idx) > 0) {
    tabular_lines[end_idx] <- "\\end{tabularx}"
  }
  tabular_lines
}

compact_modelsummary_header <- function(tabular_lines, model_count) {
  top_idx <- grep("\\\\toprule", tabular_lines)
  if (length(top_idx) == 0 || top_idx[[1]] >= length(tabular_lines)) {
    return(tabular_lines)
  }

  header_idx <- top_idx[[1]] + 1L
  tabular_lines[[header_idx]] <- paste0(
    "& ",
    paste(sprintf("(%d)", seq_len(model_count)), collapse = " & "),
    " \\\\"
  )
  tabular_lines
}

latex_row_body <- function(line) {
  sub("[[:space:]]*\\\\\\\\[[:space:]]*$", "", line, perl = TRUE)
}

latex_row_cells <- function(line) {
  strsplit(latex_row_body(line), "[[:space:]]*&[[:space:]]*", perl = TRUE)[[1]]
}

latex_table_row <- function(cells) {
  paste0(paste(cells, collapse = " & "), " \\\\")
}

is_modelsummary_statistic_row <- function(line) {
  cells <- latex_row_cells(line)
  if (length(cells) <= 1 || nzchar(trimws(cells[[1]]))) {
    return(FALSE)
  }

  values <- trimws(cells[-1])
  any(nzchar(values)) &&
    all(!nzchar(values) | grepl("^\\(-?[0-9]+(\\.[0-9]+)?\\)$", values))
}

combine_modelsummary_estimate_statistic_rows <- function(tabular_lines) {
  out <- character()
  line_idx <- 1L
  while (line_idx <= length(tabular_lines)) {
    line <- tabular_lines[[line_idx]]
    if (line_idx < length(tabular_lines) &&
        is_modelsummary_statistic_row(tabular_lines[[line_idx + 1L]])) {
      estimate_cells <- latex_row_cells(line)
      statistic_cells <- latex_row_cells(tabular_lines[[line_idx + 1L]])
      if (length(estimate_cells) == length(statistic_cells) &&
          length(estimate_cells) > 1L &&
          nzchar(trimws(estimate_cells[[1]]))) {
        for (cell_idx in seq.int(2L, length(estimate_cells))) {
          estimate <- trimws(estimate_cells[[cell_idx]])
          statistic <- trimws(statistic_cells[[cell_idx]])
          if (nzchar(estimate) && nzchar(statistic)) {
            estimate_cells[[cell_idx]] <- paste0(
              "\\makecell[r]{",
              estimate,
              "\\\\",
              statistic,
              "}"
            )
          }
        }
        out <- c(out, latex_table_row(estimate_cells))
        line_idx <- line_idx + 2L
        next
      }
    }

    out <- c(out, line)
    line_idx <- line_idx + 1L
  }
  out
}

add_modelsummary_section_breaks <- function(tabular_lines, after_terms,
                                            before_terms = character()) {
  if (length(after_terms) == 0 && length(before_terms) == 0) return(tabular_lines)

  out <- character()
  pending_break <- FALSE
  for (line_idx in seq_along(tabular_lines)) {
    line <- tabular_lines[[line_idx]]
    if (any(vapply(
      before_terms,
      function(term) startsWith(line, paste0(term, " &")),
      logical(1)
    ))) {
      out <- c(out, "\\hline")
    }
    out <- c(out, line)
    if (pending_break && is_modelsummary_statistic_row(line)) {
      out <- c(out, "\\hline")
      pending_break <- FALSE
    }
    after_match <- any(vapply(
      after_terms,
      function(term) startsWith(line, paste0(term, " &")),
      logical(1)
    ))
    if (after_match) {
      if (line_idx < length(tabular_lines) &&
          is_modelsummary_statistic_row(tabular_lines[[line_idx + 1L]])) {
        pending_break <- TRUE
      } else {
        out <- c(out, "\\hline")
      }
    }
  }
  out
}

add_voting_type_controls_row <- function(tabular_lines, model_count) {
  n_rows <- which(startsWith(tabular_lines, "N &"))
  if (length(n_rows) == 0) return(tabular_lines)
  n_row <- n_rows[[1]]
  controls_row <- paste(
    c("Voting-type controls", rep("Yes", model_count)),
    collapse = " & "
  )
  controls_row <- paste0(controls_row, " \\\\")
  append(tabular_lines, controls_row, after = n_row - 1)
}

write_modelsummary_regression_table <- function(models, path, caption, label, notes = NULL,
                                                compact_model_names = FALSE,
                                                section_break_after_terms = character(),
                                                section_break_before_terms = character(),
                                                notes_escape = TRUE) {
  require_packages(c("modelsummary"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  old_options <- options(modelsummary_format_numeric_latex = "plain")
  on.exit(options(old_options), add = TRUE)
  coef_map <- c(
    is_author_choice = "Author choice",
    is_author_choice_expanded = "Author choice",
    first_author_choice = "Author choice, first position",
    first_author_choice_expanded = "Author choice, first position",
    nonfirst_author_choice = "Author choice, non-first position",
    nonfirst_author_choice_expanded = "Author choice, non-first position",
    is_infavor = "Approving choice",
    choice_idx = "Choice rank",
    `I(choice_idx^2)` = "Choice rank squared",
    `I(I(choice_idx^2))` = "Choice rank squared",
    is_first_choice = "First choice",
    author_voted = "Author voted",
    created_rel = "Relative proposal date",
    N_choices = "Number of choices",
    author_pos6 = "Author choice in first six",
    author_pos6_expanded = "Author choice in first six",
    `is_author_choice:author_pos6` = "Author choice x first-six visibility",
    `author_pos6:is_author_choice` = "Author choice x first-six visibility",
    `is_author_choice_expanded:author_pos6_expanded` = "Author choice x first-six visibility",
    `author_pos6_expanded:is_author_choice_expanded` = "Author choice x first-six visibility",
    log_tot_votes_2 = "Log total votes",
    choice_text_len_rel = "Relative choice text length"
  )
  gof_map <- data.frame(
    raw = c("nobs"),
    clean = c("N"),
    fmt = c(0),
    stringsAsFactors = FALSE
  )
  tabular <- modelsummary::modelsummary(
    models,
    coef_map = coef_map,
    gof_map = gof_map,
    vcov = ~space,
    statistic = "({std.error})",
    stars = c("+" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
    fmt = 3,
    output = "latex_tabular",
    escape = TRUE
  )
  tabular_lines <- unlist(strsplit(paste(as.character(tabular), collapse = "\n"), "\n", fixed = TRUE))
  tabular_lines <- add_booktabs_to_modelsummary_tabular(tabular_lines)
  if (compact_model_names) {
    tabular_lines <- compact_modelsummary_header(tabular_lines, length(models))
  }
  tabular_lines <- combine_modelsummary_estimate_statistic_rows(tabular_lines)
  tabular_lines <- add_modelsummary_section_breaks(
    tabular_lines,
    section_break_after_terms,
    section_break_before_terms
  )
  tabular_lines <- add_voting_type_controls_row(tabular_lines, length(models))
  tabular_lines <- modelsummary_tabularx_lines(tabular_lines, length(models))
  writeLines(
    wrap_modelsummary_latex(tabular_lines, caption, label, notes, notes_escape = notes_escape),
    path
  )
  message("Wrote ", path)
  invisible(path)
}
