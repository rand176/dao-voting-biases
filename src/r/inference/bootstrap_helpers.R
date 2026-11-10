primary_ame_focal_contrasts <- function() {
  data.frame(
    contrast = c("Author choice", "First vs second", "Approve stance"),
    three_bias_contrast = c("Author choice", "First vs second choice", "Approve stance"),
    stringsAsFactors = FALSE
  )
}

assert_primary_ame_focal_contrasts <- function(source) {
  expected <- primary_ame_focal_contrasts()
  has_expected <- nrow(source) == nrow(expected) &&
    identical(source$contrast, expected$contrast) &&
    identical(source$three_bias_contrast, expected$three_bias_contrast)
  if (!has_expected) {
    stop(
      "Primary AME inference source must contain exactly the focal three contrasts ",
      "in order: ",
      paste(expected$three_bias_contrast, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(source)
}

read_primary_ame_inference_source <- function(path) {
  if (length(path) == 0 || is.na(path) || !file.exists(path)) {
    stop("Primary AME inference source is missing: ", path, call. = FALSE)
  }
  source <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c(
    "contrast",
    "three_bias_contrast",
    "ame_pp",
    "ordinary_ci_low_pp",
    "ordinary_ci_high_pp",
    "bootstrap_p",
    "bonferroni_ci_low_pp",
    "bonferroni_ci_high_pp",
    "bootstrap_replicates",
    "cluster_unit",
    "bootstrap_seed"
  )
  missing <- setdiff(required, names(source))
  if (length(missing) > 0) {
    stop(
      "Primary AME inference source missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(source$contrast) || anyDuplicated(source$three_bias_contrast)) {
    stop("Primary AME inference source has duplicated contrast rows.", call. = FALSE)
  }

  numeric_cols <- c(
    "ame_pp",
    "ordinary_ci_low_pp",
    "ordinary_ci_high_pp",
    "bootstrap_p",
    "bonferroni_ci_low_pp",
    "bonferroni_ci_high_pp",
    "bootstrap_replicates",
    "bootstrap_seed"
  )
  for (col in numeric_cols) {
    source[[col]] <- suppressWarnings(as.numeric(source[[col]]))
    if (any(is.na(source[[col]]))) {
      stop("Primary AME inference source has non-numeric values in ", col, ".", call. = FALSE)
    }
  }
  if (any(source$bootstrap_p < 0 | source$bootstrap_p > 1)) {
    stop("Primary AME inference source bootstrap_p values must be in [0, 1].", call. = FALSE)
  }
  if (length(unique(source$bootstrap_replicates)) != 1 ||
      source$bootstrap_replicates[[1]] <= 0) {
    stop(
      "Primary AME inference source must use one positive bootstrap_replicates value.",
      call. = FALSE
    )
  }
  source$contrast <- trimws(as.character(source$contrast))
  source$three_bias_contrast <- trimws(as.character(source$three_bias_contrast))
  source$cluster_unit <- trimws(as.character(source$cluster_unit))
  if (length(unique(source$cluster_unit)) != 1 ||
      !identical(unique(source$cluster_unit), "space")) {
    stop(
      "Primary AME inference source must use cluster_unit = space.",
      call. = FALSE
    )
  }
  if (length(unique(source$bootstrap_seed)) != 1 ||
      source$bootstrap_seed[[1]] <= 0 ||
      source$bootstrap_seed[[1]] != floor(source$bootstrap_seed[[1]])) {
    stop(
      "Primary AME inference source must use one positive integer bootstrap_seed value.",
      call. = FALSE
    )
  }
  source$bootstrap_seed <- as.integer(source$bootstrap_seed)
  assert_primary_ame_focal_contrasts(source)
  source
}

primary_ame_inference_table <- function(three_bias_summary,
                                        inference_path = default_primary_ame_inference_path(),
                                        ame_tolerance = 0.005) {
  required_summary <- c("Bias contrast", "Single preference")
  missing_summary <- setdiff(required_summary, names(three_bias_summary))
  if (length(missing_summary) > 0) {
    stop(
      "Three-bias summary missing column(s): ",
      paste(missing_summary, collapse = ", "),
      call. = FALSE
    )
  }
  source <- read_primary_ame_inference_source(inference_path)
  summary_ames <- data.frame(
    three_bias_contrast = as.character(three_bias_summary[["Bias contrast"]]),
    current_ame_pp = as.numeric(three_bias_summary[["Single preference"]]),
    stringsAsFactors = FALSE
  )
  match_idx <- match(source$three_bias_contrast, summary_ames$three_bias_contrast)
  if (any(is.na(match_idx))) {
    stop(
      "Primary AME inference source contrast(s) missing from three-bias summary: ",
      paste(source$three_bias_contrast[is.na(match_idx)], collapse = ", "),
      call. = FALSE
    )
  }

  current_ames <- summary_ames$current_ame_pp[match_idx]
  stale_rows <- abs(source$ame_pp - current_ames) > ame_tolerance
  if (any(stale_rows)) {
    stale <- source[stale_rows, , drop = FALSE]
    stop(
      "Primary AME inference source does not match the current expanded single-preference AME for: ",
      paste(stale$three_bias_contrast, collapse = ", "),
      call. = FALSE
    )
  }

  out <- data.frame(
    Contrast = source$contrast,
    `AME (pp)` = current_ames,
    `Ordinary 95% bootstrap CI` = format_ci_pp(
      source$ordinary_ci_low_pp,
      source$ordinary_ci_high_pp
    ),
    `Bootstrap p` = source$bootstrap_p,
    `Holm/Bonferroni p` = stats::p.adjust(source$bootstrap_p, method = "holm"),
    `BH/FDR q` = stats::p.adjust(source$bootstrap_p, method = "BH"),
    `Bonferroni 95% CI` = format_ci_pp(
      source$bonferroni_ci_low_pp,
      source$bonferroni_ci_high_pp
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "bootstrap_replicates") <- unique(source$bootstrap_replicates)
  attr(out, "cluster_unit") <- unique(source$cluster_unit)
  attr(out, "bootstrap_seed") <- unique(source$bootstrap_seed)
  attr(out, "inference_path") <- normalizePath(inference_path, winslash = "/", mustWork = FALSE)
  out
}

format_primary_ame_inference_table <- function(df) {
  out <- df
  out[["AME (pp)"]] <- format_num(out[["AME (pp)"]], 2)
  out[["Bootstrap p"]] <- format_num(out[["Bootstrap p"]], 3)
  out[["Holm/Bonferroni p"]] <- format_num(out[["Holm/Bonferroni p"]], 3)
  out[["BH/FDR q"]] <- format_num(out[["BH/FDR q"]], 3)
  out
}

primary_ame_inference_align <- function() {
  paste0(
    "@{}>{\\RaggedRight\\arraybackslash}p{0.16\\linewidth}",
    "r",
    ">{\\RaggedRight\\arraybackslash}p{0.18\\linewidth}",
    "rrr",
    ">{\\RaggedRight\\arraybackslash}p{0.18\\linewidth}@{}"
  )
}

primary_ame_inference_header <- function() {
  paste(
    c(
      "Contrast",
      "\\makecell[r]{AME\\\\(pp)}",
      "\\makecell[l]{Ordinary 95\\%\\\\bootstrap CI}",
      "\\makecell[r]{Bootstrap\\\\p}",
      "\\makecell[r]{Holm/\\\\Bonferroni p}",
      "\\makecell[r]{BH/FDR\\\\q}",
      "\\makecell[l]{Bonferroni\\\\95\\% CI}"
    ),
    collapse = " & "
  )
}

primary_ame_inference_notes <- function(df = NULL) {
  bootstrap_replicates <- if (!is.null(df)) {
    attr(df, "bootstrap_replicates", exact = TRUE)
  } else {
    NULL
  }
  cluster_unit <- if (!is.null(df)) attr(df, "cluster_unit", exact = TRUE) else NULL
  bootstrap_seed <- if (!is.null(df)) attr(df, "bootstrap_seed", exact = TRUE) else NULL
  cluster_label <- if (length(cluster_unit) == 1 && identical(cluster_unit, "space")) {
    "DAO-space"
  } else if (length(cluster_unit) == 1 && nzchar(cluster_unit)) {
    cluster_unit
  } else {
    "clustered"
  }

  bootstrap_note <- if (length(bootstrap_replicates) == 1 &&
      !is.na(bootstrap_replicates) &&
      bootstrap_replicates > 0) {
    paste0(
      "Bootstrap intervals and p-values use a ",
      cluster_label,
      " clustered bootstrap with B = ",
      format_int(bootstrap_replicates),
      "."
    )
  } else {
    "Bootstrap intervals and p-values use the versioned clustered-bootstrap inference source."
  }
  p_floor_note <- if (length(bootstrap_replicates) == 1 &&
      !is.na(bootstrap_replicates) &&
      bootstrap_replicates > 0) {
    paste0(
      "The bootstrap p-value is at the smallest possible two-sided value with B = ",
      format_int(bootstrap_replicates),
      ", 2/(",
      format_int(bootstrap_replicates),
      " + 1) = ",
      format_num(2 / (bootstrap_replicates + 1), 3),
      "."
    )
  } else {
    "Multiplicity columns are derived from the ordinary bootstrap p-values."
  }

  c(
    "The family is the three focal contrasts in the expanded primary single-preference model.",
    bootstrap_note,
    if (length(bootstrap_seed) == 1 && !is.na(bootstrap_seed)) {
      paste0(
        "The frozen bootstrap artifact uses seed = ",
        format(as.integer(bootstrap_seed), scientific = FALSE, trim = TRUE),
        "."
      )
    },
    p_floor_note,
    "Bonferroni intervals are sourced from the same inference artifact and approximate simultaneous 95% intervals for the same three-contrast family."
  )
}
