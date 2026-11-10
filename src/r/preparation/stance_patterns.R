# Stance Detection Pattern Definitions
# This file contains all pattern definitions for detecting APPROVE, REJECT, and ABSTAIN stances
# Generated from 2_stance_v5.Rmd refactoring

## REJECT PATTERNS
###################

# Exact matches for REJECT
nos <- c(
  "No", "iNO", "NAY", "NAE", "Nah", "Non", "нет", "Nope", "não", "n",
  "Against",
  "否", "不是", "反对", "不支持", "不会", "反対", "不能", "不同意", "没有", "不可以",
  "Do not", "Don't",
  "Do not support", "Don't support", "I don't support", "I do not support",
  "Do not approve", "Don't approve", "I don't approve", "I do not approve",
  "Dont support", "no support", "not support",
  "I dont support",
  "Disagree", "Disagreed", "I do not agree", "I don't agree", "Don't agree",
  "I disagree", "Not Agree",
  "dont agree", "I dont agree",
  "Decline", "Declined",
  "Oppose", "Opposed",
  "Disapprove", "Disapproved", "Not approved", "Not approve",
  "Disapporve", "Dissaprove",
  "Dissent", "I dissent",
  "Refuse", "Refused", "I refuse",
  "Deny", "Denied", "I deny",
  "Reject", "Rejected", "I reject",
  "Not in favor",
  "Againist", "Aganist",
  "do not proceed", "dont't proceed",
  "no apruebo",
  "non approvo",
  "do not accept", "don't accept", "I do not accept", "I don't accept", "not accept",
  "Do nothing",
  "contra",
  "negative",
  "negative vote"
)
nos <- tolower(nos)

# Contains patterns for REJECT (subset of exact matches - more specific ones)
nos_contains_base <- c(
  "iNO", "NAY", "NAE", "Nah", "нет", "Nope", "não",
  "Against",
  "否", "不是", "反对", "不支持", "不会", "反対", "不能", "不同意", "没有", "不可以",
  "Do nothing",
  "do not support", "don't support", "I don't support", "I do not support",
  "do not approve", "don't approve", "I don't approve", "I do not approve",
  "Dont support", "no support", "not support",
  "Disagree", "Disagreed", "I do not agree", "I don't agree", "Don't agree", "I disagree", "Not Agree",
  "dont agree",
  "Decline", "Declined",
  "Oppose", "Opposed",
  "Disapprove", "Disapproved", "Not approve",
  "Disapporve", "Dissaprove",
  "Dissent", "I dissent",
  "Refuse", "Refused", "I refuse",
  "Deny", "Denied", "I deny",
  "Reject", "Rejected", "I reject",
  "Not in favor",
  "Againist", "Aganist",
  "not proceed", "dont't proceed",
  "non approvo",
  "not accept", "don't accept",
  "contra",
  "negative vote"
)

## APPROVE PATTERNS
####################

# Exact matches for APPROVE
yess <- c(
  "Yes", "For", "Approve", "y", "Agree", "YAE",
  "是", "同意", "賛成", "支持", "赞成", "会", "可以", "有",
  "Support", "Accept", "能", "Pass", "In favor",
  "s", "ok", "Si", "Sim", "Oui", "Proceed",
  "I support",
  "sYES", "Yay", "Yes!", "yay", "yes!", "Agreed", "Aye",
  "Approved", "Approve!", "да", "Apruebo", "Sí", "I agree", "approvo", "aprovar", "sim",
  "a favor",
  "affirmative",
  "positive",
  "positive vote"
)
yess <- tolower(yess)

# Contains patterns for APPROVE (subset - more specific ones)
yess_contains_base <- c(
  "yes", "approve", "agree", "yae",
  "是", "同意", "賛成", "赞成", "有", "支持",
  "support", "accept", "能", "in favor", "ok", "oui",
  "syes", "yay", "yes!", "Agreed", "Aye", "Approved", "Approve!",
  "да", "Apruebo", "Sí", "agree", "approvo", "aprovar", "sim", "a favor",
  "affirmative",
  "positive vote"
)

## ABSTAIN PATTERNS
####################

# Exact matches for ABSTAIN
abstains <- c(
  "abstain", "棄権", "弃权", "maybe", "not sure",
  "I abstain", "Abstenção", "abster", "je m'abstiens"
)
abstains <- tolower(abstains)

# Contains patterns for ABSTAIN
abstains_contains_base <- c(
  "abstain", "棄権", "弃权", "Abstenção", "abster", "je m'abstiens"
)

## GENERATE PATTERN VARIANTS
#############################
# Helper function to generate pattern variants with punctuation

generate_pattern_variants <- function(base_patterns, variant_type = "begins") {

  base_patterns <- tolower(base_patterns)
  if (length(base_patterns) == 0) {
    return(character())
  }

  if (variant_type == "begins") {
    # Patterns that start with the word
    v1 <- paste0(base_patterns, " ")
    v2 <- paste0(base_patterns, ",")
    v3 <- paste0(base_patterns, "!")
    v4 <- paste0(base_patterns, "\\.")
    v5 <- paste0(base_patterns, ";")
    v6 <- paste0(base_patterns, "-")
    v7 <- paste0(base_patterns, ":")
    v8 <- paste0(base_patterns, "/")
    return(c(v1, v2, v3, v4, v5, v6, v7, v8))

  } else if (variant_type == "contains") {
    # Patterns that contain the word (with spaces/punctuation around)
    v1 <- paste0(" ", base_patterns, " ")
    v2 <- paste0(" ", base_patterns, ",")
    v3 <- paste0(" ", base_patterns, "!")
    v4 <- paste0(" ", base_patterns, "\\.")
    v5 <- paste0(" ", base_patterns, ";")
    v6 <- paste0(" ", base_patterns, "-")
    v7 <- paste0(" ", base_patterns, ":")
    v8 <- paste0(" ", base_patterns, "/")
    return(c(v1, v2, v3, v4, v5, v6, v7, v8))

  } else if (variant_type == "ends") {
    # Patterns that end with the word
    return(paste0(" ", base_patterns))
  }

  return(base_patterns)
}

generate_low_confidence_contains_variants <- function(base_patterns) {
  base_patterns <- tolower(base_patterns)
  if (length(base_patterns) == 0) {
    return(character())
  }
  c(
    generate_pattern_variants(base_patterns, "contains"),
    paste0(" ", base_patterns)
  )
}

short_or_ambiguous_expansion_terms <- tolower(c(
  "s", "y", "n", "ok", "有", "是", "会", "能"
))

approve_low_confidence_contains_base <- tolower(c(
  "support", "approve", "agree", "accept", "yes"
))

reject_low_confidence_contains_base <- character()
abstain_low_confidence_contains_base <- character()

nos_prefix_base <- setdiff(nos, short_or_ambiguous_expansion_terms)
nos_suffix_base <- setdiff(nos, short_or_ambiguous_expansion_terms)
nos_high_confidence_contains_base <- setdiff(
  tolower(nos_contains_base),
  short_or_ambiguous_expansion_terms
)

yess_prefix_base <- setdiff(yess, short_or_ambiguous_expansion_terms)
yess_suffix_base <- setdiff(
  yess_prefix_base,
  c("support", "approve", "agree", "accept", "可以")
)
yess_high_confidence_contains_base <- setdiff(
  tolower(yess_contains_base),
  c(short_or_ambiguous_expansion_terms, approve_low_confidence_contains_base)
)

abstain_prefix_base <- setdiff(abstains, short_or_ambiguous_expansion_terms)
abstain_suffix_base <- setdiff(abstains, short_or_ambiguous_expansion_terms)
abstain_high_confidence_contains_base <- setdiff(
  tolower(abstains_contains_base),
  short_or_ambiguous_expansion_terms
)

# Generate all pattern variants
nos_begins <- generate_pattern_variants(nos_prefix_base, "begins")
nos_contains <- generate_pattern_variants(nos_high_confidence_contains_base, "contains")
nos_low_confidence_contains <- generate_low_confidence_contains_variants(
  reject_low_confidence_contains_base
)
nos_ends <- generate_pattern_variants(nos_suffix_base, "ends")

yess_begins <- generate_pattern_variants(yess_prefix_base, "begins")
yess_contains <- generate_pattern_variants(
  yess_high_confidence_contains_base,
  "contains"
)
yess_low_confidence_contains <- generate_low_confidence_contains_variants(
  approve_low_confidence_contains_base
)
yess_ends <- generate_pattern_variants(yess_suffix_base, "ends")

abstain_begins <- generate_pattern_variants(abstain_prefix_base, "begins")
abstains_contains <- generate_pattern_variants(
  abstain_high_confidence_contains_base,
  "contains"
)
abstains_low_confidence_contains <- generate_low_confidence_contains_variants(
  abstain_low_confidence_contains_base
)
abstains_ends <- generate_pattern_variants(abstain_suffix_base, "ends")

# Return list of all patterns
stance_patterns <- list(
  # REJECT patterns
  nos = nos,
  nos_begins = nos_begins,
  nos_contains = nos_contains,
  nos_low_confidence_contains = nos_low_confidence_contains,
  nos_ends = nos_ends,
  reject_exact = nos,
  reject_prefix = nos_begins,
  reject_suffix = nos_ends,
  reject_contains = nos_contains,
  reject_low_confidence_contains = nos_low_confidence_contains,

  # APPROVE patterns
  yess = yess,
  yess_begins = yess_begins,
  yess_contains = yess_contains,
  yess_low_confidence_contains = yess_low_confidence_contains,
  yess_ends = yess_ends,
  approve_exact = yess,
  approve_prefix = yess_begins,
  approve_suffix = yess_ends,
  approve_contains = yess_contains,
  approve_low_confidence_contains = yess_low_confidence_contains,

  # ABSTAIN patterns
  abstains = abstains,
  abstain_begins = abstain_begins,
  abstains_contains = abstains_contains,
  abstains_low_confidence_contains = abstains_low_confidence_contains,
  abstains_ends = abstains_ends,
  abstain_exact = abstains,
  abstain_prefix = abstain_begins,
  abstain_suffix = abstains_ends,
  abstain_contains = abstains_contains,
  abstain_low_confidence_contains = abstains_low_confidence_contains
)

empty_stance_match_rows <- function() {
  data.frame(
    stance = character(),
    match_source = character(),
    matched_pattern = character(),
    canonical_pattern = character(),
    stringsAsFactors = FALSE
  )
}

make_stance_pattern_registry <- function() {
  pattern_rows <- function(stance, match_source, patterns, canonical_patterns, match_type) {
    if (length(patterns) != length(canonical_patterns)) {
      stop(paste(
        "Pattern registry length mismatch for",
        match_source,
        paste0("patterns=", length(patterns)),
        paste0("canonical_patterns=", length(canonical_patterns))
      ))
    }
    if (length(patterns) == 0) {
      return(data.frame(
        stance = character(),
        match_source = character(),
        matched_pattern = character(),
        canonical_pattern = character(),
        match_type = character(),
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      stance = stance,
      match_source = match_source,
      matched_pattern = as.character(patterns),
      canonical_pattern = as.character(canonical_patterns),
      match_type = match_type,
      stringsAsFactors = FALSE
    )
  }

  rbind(
    pattern_rows("REJECT", "reject_exact", nos, nos, "exact"),
    pattern_rows("APPROVE", "approve_exact", yess, yess, "exact"),
    pattern_rows("ABSTAIN", "abstain_exact", abstains, abstains, "exact"),

    pattern_rows("REJECT", "reject_prefix", nos_begins, rep(nos_prefix_base, times = 8), "prefix"),
    pattern_rows("APPROVE", "approve_prefix", yess_begins, rep(yess_prefix_base, times = 8), "prefix"),
    pattern_rows("ABSTAIN", "abstain_prefix", abstain_begins, rep(abstain_prefix_base, times = 8), "prefix"),

    pattern_rows("REJECT", "reject_suffix", nos_ends, nos_suffix_base, "suffix"),
    pattern_rows("APPROVE", "approve_suffix", yess_ends, yess_suffix_base, "suffix"),
    pattern_rows("ABSTAIN", "abstain_suffix", abstains_ends, abstain_suffix_base, "suffix"),

    pattern_rows("REJECT", "reject_contains", nos_contains, rep(nos_high_confidence_contains_base, times = 8), "contains"),
    pattern_rows("APPROVE", "approve_contains", yess_contains, rep(yess_high_confidence_contains_base, times = 8), "contains"),
    pattern_rows("ABSTAIN", "abstain_contains", abstains_contains, rep(abstain_high_confidence_contains_base, times = 8), "contains"),

    pattern_rows("REJECT", "reject_low_confidence_contains", nos_low_confidence_contains, rep(reject_low_confidence_contains_base, times = 9), "low_confidence_contains"),
    pattern_rows("APPROVE", "approve_low_confidence_contains", yess_low_confidence_contains, rep(approve_low_confidence_contains_base, times = 9), "low_confidence_contains"),
    pattern_rows("ABSTAIN", "abstain_low_confidence_contains", abstains_low_confidence_contains, rep(abstain_low_confidence_contains_base, times = 9), "low_confidence_contains")
  )
}

match_stance_patterns <- function(row, registry = NULL) {
  choice_text <- as.character(row[["choice_text"]])
  choice_lower <- tolower(choice_text)
  choice_trim <- str_trim(choice_lower)
  choice_trim_left <- str_trim(choice_lower, "left")
  choice_trim_right <- str_trim(choice_lower, "right")

  if (is.null(registry)) {
    registry <- make_stance_pattern_registry()
  }
  matches <- empty_stance_match_rows()
  source_order <- unique(registry$match_source)

  for (match_source in source_order) {
    source_registry <- registry[registry$match_source == match_source, ]
    if (nrow(source_registry) == 0) {
      next
    }

    match_type <- source_registry$match_type[1]
    matched <- switch(
      match_type,
      exact = !is.na(choice_trim) & source_registry$matched_pattern == choice_trim,
      prefix = !is.na(choice_trim_left) & str_starts(choice_trim_left, source_registry$matched_pattern),
      suffix = !is.na(choice_trim_right) & str_ends(choice_trim_right, source_registry$matched_pattern),
      contains = !is.na(choice_lower) & str_detect(choice_lower, source_registry$matched_pattern),
      low_confidence_contains = !is.na(choice_lower) & str_detect(choice_lower, source_registry$matched_pattern),
      rep(FALSE, nrow(source_registry))
    )

    if (any(matched, na.rm = TRUE)) {
      matched_rows <- source_registry[which(matched), c("stance", "match_source", "matched_pattern", "canonical_pattern")]
      matches <- rbind(matches, matched_rows)
    }
  }

  rownames(matches) <- NULL
  matches
}

empty_stance_instrumentation <- function(resolved_stance, match_source) {
  data.frame(
    resolved_stance = resolved_stance,
    match_source = match_source,
    matched_pattern = "",
    canonical_pattern = "",
    all_match_sources = match_source,
    has_conflicting_matches = FALSE,
    stringsAsFactors = FALSE
  )
}

classifyStanceChoiceInstrumented <- function(row, verbose = FALSE, registry = NULL) {
  choice_text <- as.character(row[["choice_text"]])
  choice_lower <- tolower(choice_text)
  choice_trim <- str_trim(choice_lower)

  body_text <- as.character(row[["body"]])
  body_trim <- str_trim(body_text)
  if (is.na(body_trim) || body_trim == "") {
    if (verbose) print("no body")
    return(empty_stance_instrumentation("OTHER", "empty_body"))
  }

  if (is.na(choice_trim) || choice_trim == "") {
    if (verbose) print("no choice text")
    return(empty_stance_instrumentation("OTHER", "empty_choice_text"))
  }

  matches <- match_stance_patterns(row, registry = registry)
  if (nrow(matches) == 0) {
    if (verbose) print("exit")
    return(empty_stance_instrumentation("OTHER", "none"))
  }

  confident_matches <- matches[!grepl("low_confidence", matches$match_source, fixed = TRUE), ]
  if (nrow(confident_matches) == 0) {
    first_match <- matches[1, ]
    resolved_stance <- "OTHER"
  } else {
    first_match <- confident_matches[1, ]
    resolved_stance <- first_match$stance
  }
  if (verbose) print(first_match$match_source)

  matched_stances <- unique(matches$stance)
  data.frame(
    resolved_stance = resolved_stance,
    match_source = first_match$match_source,
    matched_pattern = first_match$matched_pattern,
    canonical_pattern = first_match$canonical_pattern,
    all_match_sources = paste(unique(matches$match_source), collapse = "|"),
    has_conflicting_matches = all(c("APPROVE", "REJECT") %in% matched_stances),
    stringsAsFactors = FALSE
  )
}

classifyStanceChoice <- function(row, verbose = FALSE) {
  classifyStanceChoiceInstrumented(row, verbose = verbose)$resolved_stance
}

classifyStanceChoicesInstrumented <- function(df, verbose = FALSE) {
  if (nrow(df) == 0) {
    return(data.frame(
      resolved_stance = character(),
      match_source = character(),
      matched_pattern = character(),
      canonical_pattern = character(),
      all_match_sources = character(),
      has_conflicting_matches = logical(),
      stringsAsFactors = FALSE
    ))
  }

  registry <- make_stance_pattern_registry()
  rows <- lapply(seq_len(nrow(df)), function(idx) {
    classifyStanceChoiceInstrumented(df[idx, , drop = FALSE], verbose = verbose, registry = registry)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Make patterns available in global environment when sourced
list2env(stance_patterns, envir = .GlobalEnv)
