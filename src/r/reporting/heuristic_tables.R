source_stance_patterns <- function() {
  registry_path <- file.path(PACKAGE_ROOT, "src/r/preparation/stance_patterns.R")
  if (!file.exists(registry_path)) {
    stop(
      paste(
        "Could not find",
        registry_path,
        "Run python run.py tables --kind heuristics."
      ),
      call. = FALSE
    )
  }
  source(registry_path, local = .GlobalEnv)
}

source_stance_patterns()

validate_terms_per_row <- function(terms_per_row) {
  if (
    length(terms_per_row) != 1L ||
      is.na(terms_per_row) ||
      !is.finite(terms_per_row) ||
      terms_per_row < 1 ||
      terms_per_row != as.integer(terms_per_row)
  ) {
    stop("terms_per_row must be a positive integer.", call. = FALSE)
  }
  as.integer(terms_per_row)
}

latex_escape <- function(x) {
  escape_one <- function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    chars <- strsplit(as.character(value), "", useBytes = FALSE)[[1]]
    escaped <- vapply(chars, function(ch) {
      if (identical(ch, "\\")) return("\\textbackslash{}")
      if (identical(ch, "_")) return("\\_")
      if (identical(ch, "%")) return("\\%")
      if (identical(ch, "&")) return("\\&")
      if (identical(ch, "$")) return("\\$")
      if (identical(ch, "#")) return("\\#")
      if (identical(ch, "{")) return("\\{")
      if (identical(ch, "}")) return("\\}")
      if (identical(ch, "~")) return("\\textasciitilde{}")
      if (identical(ch, "^")) return("\\textasciicircum{}")
      ch
    }, character(1))
    paste(escaped, collapse = "")
  }

  vapply(as.character(x), escape_one, character(1), USE.NAMES = FALSE)
}

contains_non_ascii <- function(value) {
  if (is.na(value)) {
    return(FALSE)
  }
  any(as.integer(charToRaw(enc2utf8(as.character(value)))) > 127L)
}

non_english_canonical_terms <- function() {
  tolower(c(
    "a favor",
    "abstenção",
    "abster",
    "approvo",
    "aprovar",
    "apruebo",
    "contra",
    "je m'abstiens",
    "não",
    "no apruebo",
    "non",
    "non approvo",
    "oui",
    "si",
    "sí",
    "sim",
    "да",
    "нет",
    "不会",
    "不可以",
    "不同意",
    "不支持",
    "不是",
    "不能",
    "会",
    "反对",
    "反対",
    "可以",
    "同意",
    "否",
    "弃权",
    "支持",
    "是",
    "有",
    "棄権",
    "没有",
    "能",
    "賛成",
    "赞成"
  ))
}

display_group_for_term <- function(term) {
  term <- as.character(term)
  term_lower <- tolower(term)
  if (contains_non_ascii(term) || term_lower %in% non_english_canonical_terms()) {
    return("Non-English")
  }
  "English"
}

sort_terms_for_display <- function(terms) {
  terms <- as.character(terms)
  terms <- terms[!is.na(terms)]
  terms <- unique(terms)
  if (length(terms) == 0L) {
    return(terms)
  }
  terms[order(tolower(enc2utf8(terms)), enc2utf8(terms), method = "radix")]
}

group_terms_for_display <- function(terms) {
  terms <- sort_terms_for_display(terms)
  groups <- c("English", "Non-English")
  out <- lapply(groups, function(group) {
    term_groups <- vapply(terms, display_group_for_term, character(1), USE.NAMES = FALSE)
    group_terms <- terms[term_groups == group]
    list(group = group, terms = sort_terms_for_display(group_terms))
  })
  out[vapply(out, function(item) length(item$terms) > 0L, logical(1))]
}

latex_escape_multiscript_term <- function(x) {
  escaped <- latex_escape(x)
  vapply(seq_along(x), function(idx) {
    if (is.na(x[[idx]])) {
      return(NA_character_)
    }
    if (!contains_non_ascii(x[[idx]])) {
      return(escaped[[idx]])
    }
    paste0("{\\fontspec{Noto Sans CJK SC}", escaped[[idx]], "}")
  }, character(1), USE.NAMES = FALSE)
}

render_canonical_terms <- function(value) {
  if (is.na(value) || identical(value, "None")) {
    return(latex_escape(value))
  }
  value <- as.character(value)
  group_prefixes <- c("English: ", "Non-English: ")
  group_prefix <- group_prefixes[startsWith(value, group_prefixes)]
  if (length(group_prefix) > 0L) {
    prefix <- group_prefix[[1]]
    terms_value <- substr(value, nchar(prefix) + 1L, nchar(value))
    terms <- strsplit(terms_value, ", ", fixed = TRUE)[[1]]
    return(paste0(
      latex_escape(prefix),
      paste(latex_escape_multiscript_term(terms), collapse = ", ")
    ))
  }
  terms <- strsplit(value, ", ", fixed = TRUE)[[1]]
  paste(latex_escape_multiscript_term(terms), collapse = ", ")
}

stance_label <- function(stance) {
  stance <- toupper(as.character(stance))
  switch(
    stance,
    REJECT = "Reject",
    APPROVE = "Approve",
    ABSTAIN = "Abstain",
    stop(paste("Unsupported stance:", stance), call. = FALSE)
  )
}

stance_match_sources <- function(stance) {
  stance <- toupper(as.character(stance))
  prefix <- switch(
    stance,
    REJECT = "reject",
    APPROVE = "approve",
    ABSTAIN = "abstain",
    stop(paste("Unsupported stance:", stance), call. = FALSE)
  )
  paste0(prefix, c(
    "_exact",
    "_prefix",
    "_suffix",
    "_contains",
    "_low_confidence_contains"
  ))
}

tier_type_from_match_source <- function(match_source) {
  if (grepl("_low_confidence_contains$", match_source)) {
    return("low_confidence_contains")
  }
  if (grepl("_contains$", match_source)) {
    return("contains")
  }
  if (grepl("_suffix$", match_source)) {
    return("suffix")
  }
  if (grepl("_prefix$", match_source)) {
    return("prefix")
  }
  if (grepl("_exact$", match_source)) {
    return("exact")
  }
  stop(paste("Unsupported match source:", match_source), call. = FALSE)
}

tier_label <- function(match_type) {
  switch(
    match_type,
    exact = "Exact",
    prefix = "Prefix",
    suffix = "Suffix",
    contains = "High-confidence contains",
    low_confidence_contains = "Low-confidence contains",
    stop(paste("Unsupported match type:", match_type), call. = FALSE)
  )
}

tier_note <- function(match_type) {
  notes <- c(
    exact = "Trimmed lower-case choice equals the canonical term.",
    prefix = "Left-trimmed choice starts with the term followed by space or punctuation.",
    suffix = "Right-trimmed choice ends with a space plus the term.",
    contains = "Choice contains a space-delimited or punctuation-delimited registry term. Confident matches resolve to the stance unless a higher-priority tier already matched.",
    low_confidence_contains = "Low-confidence-only matches resolve to Other; conflicts remain visible in the audit trail."
  )
  if (!match_type %in% names(notes)) {
    stop(paste("Unsupported match type:", match_type), call. = FALSE)
  }
  unname(notes[[match_type]])
}

chunk_terms <- function(terms, terms_per_row) {
  terms_per_row <- validate_terms_per_row(terms_per_row)
  terms <- as.character(terms)
  if (length(terms) == 0L) {
    return(list(character()))
  }
  split(terms, ceiling(seq_along(terms) / terms_per_row))
}

validate_registry <- function(registry) {
  required_columns <- c(
    "stance",
    "match_source",
    "matched_pattern",
    "canonical_pattern",
    "match_type"
  )
  missing_columns <- setdiff(required_columns, names(registry))
  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Registry missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

build_stance_heuristic_rows <- function(registry, stance, terms_per_row = 18) {
  validate_registry(registry)
  terms_per_row <- validate_terms_per_row(terms_per_row)
  stance <- toupper(as.character(stance))

  rows <- list()
  for (match_source in stance_match_sources(stance)) {
    match_type <- tier_type_from_match_source(match_source)
    source_registry <- registry[
      registry$stance == stance & registry$match_source == match_source,
      ,
      drop = FALSE
    ]
    terms <- unique(as.character(source_registry$canonical_pattern))
    terms <- terms[!is.na(terms)]
    grouped_terms <- group_terms_for_display(terms)
    if (length(terms) == 0L) {
      grouped_terms <- list(list(group = "", terms = "None"))
    }

    is_first_tier_row <- TRUE
    for (group_item in grouped_terms) {
      chunks <- chunk_terms(group_item$terms, terms_per_row)
      for (idx in seq_along(chunks)) {
        rows[[length(rows) + 1L]] <- data.frame(
          tier = if (is_first_tier_row) tier_label(match_type) else "",
          match_source = if (is_first_tier_row) match_source else "",
          terms = if (identical(group_item$group, "")) {
            paste(chunks[[idx]], collapse = ", ")
          } else {
            paste0(group_item$group, ": ", paste(chunks[[idx]], collapse = ", "))
          },
          rule = if (is_first_tier_row) tier_note(match_type) else "",
          stringsAsFactors = FALSE
        )
        is_first_tier_row <- FALSE
      }
    }
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$canonical_terms <- out$terms
  out$note <- out$rule
  out
}

stance_table_labels <- function() {
  c(
    REJECT = "tab:choice-stance-heuristics-reject",
    APPROVE = "tab:choice-stance-heuristics-approve",
    ABSTAIN = "tab:choice-stance-heuristics-abstain"
  )
}

stance_table_filenames <- function() {
  c(
    reject = "table_stance_heuristics_reject.tex",
    approve = "table_stance_heuristics_approve.tex",
    abstain = "table_stance_heuristics_abstain.tex"
  )
}

render_table_row <- function(row) {
  paste0(
    latex_escape(row[["tier"]]),
    " & ",
    latex_escape(row[["rule"]]),
    " & ",
    render_canonical_terms(row[["terms"]]),
    " \\\\"
  )
}

render_table_body <- function(rows) {
  lines <- character()
  for (idx in seq_len(nrow(rows))) {
    match_source <- rows$match_source[[idx]]
    if (!is.na(match_source) && nzchar(match_source)) {
      lines <- c(lines, paste0("% match_source: ", match_source))
    }
    lines <- c(lines, render_table_row(rows[idx, , drop = FALSE]))
  }
  lines
}

build_stance_heuristic_table <- function(registry, stance, terms_per_row = 18) {
  validate_registry(registry)
  terms_per_row <- validate_terms_per_row(terms_per_row)
  stance <- toupper(as.character(stance))
  label <- stance_table_labels()[[stance]]
  label_text <- stance_label(stance)
  rows <- build_stance_heuristic_rows(registry, stance, terms_per_row)

  c(
    "% Generated by src/r/reporting/heuristic_tables.R.",
    "% Do not edit by hand; run python run.py tables --kind heuristics.",
    "\\begingroup",
    "\\small",
    "\\setlength{\\LTleft}{0pt}",
    "\\setlength{\\LTright}{0pt}",
    "\\setlength{\\LTcapwidth}{0.92\\textwidth}",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.12}",
    "\\begin{longtable}{@{}>{\\RaggedRight\\arraybackslash}p{0.13\\textwidth}>{\\RaggedRight\\arraybackslash}p{0.31\\textwidth}>{\\RaggedRight\\arraybackslash}p{0.50\\textwidth}@{}}",
    paste0(
      "\\caption{",
      label_text,
      " stance heuristic registry}\\label{",
      label,
      "}\\\\"
    ),
    "\\toprule",
    "Tier & Rule & Terms \\\\",
    "\\midrule",
    "\\endfirsthead",
    paste0(
      "\\caption[]{",
      label_text,
      " stance heuristic registry (continued)}\\\\"
    ),
    "\\toprule",
    "Tier & Rule & Terms \\\\",
    "\\midrule",
    "\\endhead",
    "\\bottomrule",
    "\\endfoot",
    render_table_body(rows),
    "\\end{longtable}",
    "\\endgroup"
  ) |>
    paste(collapse = "\n")
}

build_stance_heuristic_tables <- function(
  registry = make_stance_pattern_registry(),
  terms_per_row = 18
) {
  validate_registry(registry)
  terms_per_row <- validate_terms_per_row(terms_per_row)
  tables <- list(
    reject = build_stance_heuristic_table(registry, "REJECT", terms_per_row),
    approve = build_stance_heuristic_table(registry, "APPROVE", terms_per_row),
    abstain = build_stance_heuristic_table(registry, "ABSTAIN", terms_per_row)
  )
  tables
}

write_stance_heuristic_tables <- function(
  output_dir = file.path(Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=file.path(PACKAGE_ROOT,"outputs")),"tables"),
  registry = make_stance_pattern_registry(),
  terms_per_row = 18
) {
  tables <- build_stance_heuristic_tables(registry, terms_per_row)
  filenames <- stance_table_filenames()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  written <- vapply(names(tables), function(name) {
    path <- file.path(output_dir, filenames[[name]])
    writeLines(tables[[name]], path, useBytes = TRUE)
    path
  }, character(1), USE.NAMES = FALSE)

  written
}
