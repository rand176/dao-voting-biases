################################################################################
## Frozen clustered-bootstrap inference source for the three-bias forest plot
################################################################################

Sys.setenv(ROBUSTNESS_BIAS_MECHANISMS_SKIP_MAIN = "1")
script_dir <- file.path(PACKAGE_ROOT,"src/r")
source(file.path(PACKAGE_ROOT, "src/r/models/workflow.R"),local=TRUE)

bootstrap_replicates <- as.integer(Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_REPLICATES", unset = "199"))
bootstrap_seed <- as.integer(Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_SEED", unset = "20260518"))
bootstrap_glm_iter <- as.integer(Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_GLM_ITER", unset = "100"))
bootstrap_cores <- as.integer(Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_CORES", unset = "1"))
if (is.na(bootstrap_replicates) || bootstrap_replicates < 2) {
  stop("THREE_BIAS_FOREST_BOOTSTRAP_REPLICATES must be an integer greater than one.", call. = FALSE)
}
if (is.na(bootstrap_seed) || bootstrap_seed <= 0) {
  stop("THREE_BIAS_FOREST_BOOTSTRAP_SEED must be a positive integer.", call. = FALSE)
}
if (is.na(bootstrap_glm_iter) || bootstrap_glm_iter <= 0) {
  stop("THREE_BIAS_FOREST_BOOTSTRAP_GLM_ITER must be a positive integer.", call. = FALSE)
}
if (is.na(bootstrap_cores) || bootstrap_cores <= 0) {
  stop("THREE_BIAS_FOREST_BOOTSTRAP_CORES must be a positive integer.", call. = FALSE)
}

three_bias_forest_bootstrap_output_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_OUTPUT_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  default_three_bias_forest_inference_path(project_root)
}

resample_space_clusters_for_three_bias <- function(model_dt, sampled_spaces, space_rows) {
  require_packages("data.table")
  row_idx <- unlist(space_rows[sampled_spaces], use.names = FALSE)
  data.table::copy(model_dt[row_idx])
}

three_bias_grid_key <- function(grid) {
  paste(grid$specification, grid$bias_contrast, sep = "\r")
}

fit_three_bias_shared_outputs <- function(model_df, frozen_sets, glm_iter = bootstrap_glm_iter) {
  models <- fit_shared_bias_summary_model_set(
    model_df,
    excluded_spaces = frozen_sets$excluded_spaces,
    top_tvl_spaces = frozen_sets$top_tvl_spaces,
    top_tvl_sample_spaces = frozen_sets$top_tvl_sample_spaces,
    glm_iter = glm_iter
  )
  summary <- build_three_bias_summary(models)
  list(
    models = models,
    summary = summary,
    grid = build_three_bias_summary_long_grid(summary, models)
  )
}

fit_three_bias_shared_grid <- function(model_df, frozen_sets, glm_iter = bootstrap_glm_iter) {
  fit_three_bias_shared_outputs(model_df, frozen_sets, glm_iter)$grid
}

three_bias_vote_count_specification <- function() {
  "Vote-count share, expanded single-preference sample"
}

fit_three_bias_vote_count_shared_grid <- function(model_df, frozen_sets, glm_iter = bootstrap_glm_iter) {
  model_df <- apply_shared_specification_sets(
    model_df,
    excluded_spaces = frozen_sets$excluded_spaces,
    top_tvl_spaces = frozen_sets$top_tvl_spaces,
    top_tvl_sample_spaces = frozen_sets$top_tvl_sample_spaces
  )
  spec <- shared_bias_summary_specifications()
  spec <- spec[spec$specification == three_bias_vote_count_specification(), , drop = FALSE]
  if (nrow(spec) != 1L) {
    stop("Shared specifications must contain exactly one vote-count row.", call. = FALSE)
  }
  model <- fit_fractional_logit(
    model_df, "vote_count_share", "vote_count_outcome_row", "any", "space",
    three_bias_vote_count_specification(),
    position_spec = "rank_quadratic",
    glm_iter = glm_iter
  )
  contrasts <- three_bias_contrast_specifications()
  data.frame(
    specification = spec$specification,
    specification_order = spec$specification_order,
    bias_contrast = contrasts$bias_contrast,
    contrast_order = contrasts$contrast_order,
    fixed_effect = spec$fixed_effect,
    model_nobs = as.integer(stats::nobs(model)),
    ame_pp = vapply(seq_len(nrow(contrasts)), function(contrast_idx) {
      three_bias_contrast_pp(model, as.list(contrasts[contrast_idx, ]))
    }, numeric(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

run_three_bias_bootstrap_replicates <- function(
    sampled_space_sets,
    point_grid,
    model_dt,
    space_rows,
    frozen_sets,
    glm_iter = bootstrap_glm_iter,
    cores = bootstrap_cores,
    fit_grid_fn = fit_three_bias_shared_grid) {
  if (is.na(cores) || cores <= 0) {
    stop("Bootstrap cores must be a positive integer.", call. = FALSE)
  }
  cores <- as.integer(cores)
  b <- length(sampled_space_sets)
  point_key <- three_bias_grid_key(point_grid)
  draws <- matrix(
    NA_real_,
    nrow = b,
    ncol = nrow(point_grid),
    dimnames = list(NULL, point_key)
  )

  run_one <- function(replicate_idx) {
    if (cores > 1 &&
        requireNamespace("fixest", quietly = TRUE) &&
        exists("setFixest_nthreads", asNamespace("fixest"))) {
      fixest::setFixest_nthreads(1)
    }
    boot_df <- resample_space_clusters_for_three_bias(
      model_dt,
      sampled_space_sets[[replicate_idx]],
      space_rows
    )
    boot_grid <- fit_grid_fn(
      boot_df,
      frozen_sets,
      glm_iter = glm_iter
    )
    if (!identical(three_bias_grid_key(boot_grid), point_key)) {
      stop("Bootstrap replicate ", replicate_idx, " returned an unexpected three-bias grid.", call. = FALSE)
    }
    if (any(!is.finite(boot_grid$ame_pp))) {
      bad <- boot_grid[!is.finite(boot_grid$ame_pp), , drop = FALSE]
      stop(
        "Bootstrap replicate ", replicate_idx, " produced non-finite AME(s): ",
        paste(paste(bad$specification, bad$bias_contrast, sep = " / "), collapse = ", "),
        call. = FALSE
      )
    }
    as.numeric(boot_grid$ame_pp)
  }

  if (cores <= 1 || b <= 1) {
    for (replicate_idx in seq_len(b)) {
      draws[replicate_idx, ] <- run_one(replicate_idx)
      if (replicate_idx %% 10 == 0 || replicate_idx == b) {
        message("Completed bootstrap replicate ", replicate_idx, "/", b)
      }
    }
    return(draws)
  }

  if (.Platform$OS.type == "windows") {
    warning("Parallel bootstrap workers require fork support; falling back to one core.", call. = FALSE)
    for (replicate_idx in seq_len(b)) {
      draws[replicate_idx, ] <- run_one(replicate_idx)
      if (replicate_idx %% 10 == 0 || replicate_idx == b) {
        message("Completed bootstrap replicate ", replicate_idx, "/", b)
      }
    }
    return(draws)
  }

  cores <- min(cores, b)
  message("Using ", cores, " parallel bootstrap worker(s).")
  replicate_indices <- seq_len(b)
  chunk_size <- max(1L, cores * 2L)
  chunks <- split(replicate_indices, ceiling(seq_along(replicate_indices) / chunk_size))
  for (chunk in chunks) {
    chunk_draws <- parallel::mclapply(
      chunk,
      run_one,
      mc.cores = cores,
      mc.preschedule = FALSE
    )
    for (idx in seq_along(chunk)) {
      result <- chunk_draws[[idx]]
      if (inherits(result, "try-error") || inherits(result, "error") ||
          !is.numeric(result)) {
        warning(
          "Bootstrap replicate ", chunk[[idx]], " failed: ",
          if (inherits(result, "try-error")) conditionMessage(attr(result, "condition")) else class(result)[[1]],
          call. = FALSE
        )
      } else {
        draws[chunk[[idx]], ] <- result
      }
    }
    message("Completed bootstrap replicate ", max(chunk), "/", b)
  }
  draws
}

hash_if_present <- function(path) {
  if (is.na(path) || !nzchar(path)) return(NA_character_)
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(normalized)) return(NA_character_)
  unname(tools::md5sum(normalized))
}

single_metadata_value <- function(source, column) {
  values <- unique(source[[column]])
  if (length(values) != 1) {
    stop("Legacy three-bias forest source must use one ", column, " value.", call. = FALSE)
  }
  values[[1]]
}

validate_legacy_three_bias_forest_source <- function(source) {
  source <- as_three_bias_forest_source(source)
  required <- unique(c(
    three_bias_forest_required_columns(),
    "fwer_alpha",
    "fwer_z",
    "analysis_data_release",
    "input_path",
    "successful_replicates"
  ))
  missing <- setdiff(required, names(source))
  if (length(missing) > 0) {
    stop(
      "Legacy three-bias forest source missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  numeric_cols <- c(
    "specification_order",
    "model_nobs",
    "contrast_order",
    "ame_pp",
    "bootstrap_se_pp",
    "fwer_ci_low_pp",
    "fwer_ci_high_pp",
    "bootstrap_replicates",
    "bootstrap_seed",
    "glm_iter",
    "fwer_family_size",
    "fwer_alpha",
    "fwer_z",
    "successful_replicates"
  )
  for (col in numeric_cols) {
    source[[col]] <- suppressWarnings(as.numeric(source[[col]]))
    if (any(!is.finite(source[[col]]))) {
      stop("Legacy three-bias forest source has non-finite values in ", col, ".", call. = FALSE)
    }
  }
  for (col in c("specification", "fixed_effect", "bias_contrast", "cluster_unit", "ci_method", "analysis_data_release", "input_path")) {
    source[[col]] <- trimws(as.character(source[[col]]))
  }

  if (anyDuplicated(source[c("specification", "bias_contrast")])) {
    stop("Legacy three-bias forest source has duplicated specification/contrast rows.", call. = FALSE)
  }
  if (nrow(source) != 33L) {
    stop("Legacy three-bias forest source must contain exactly 33 rows.", call. = FALSE)
  }
  if (any(source$specification == three_bias_vote_count_specification())) {
    stop("Legacy three-bias forest source must not contain vote-count rows.", call. = FALSE)
  }
  if (!all(as.integer(source$fwer_family_size) == 33L)) {
    stop("Legacy three-bias forest source fwer_family_size must be 33.", call. = FALSE)
  }
  if (!all(source$cluster_unit == "space")) {
    stop("Legacy three-bias forest source cluster_unit must be space.", call. = FALSE)
  }
  if (!all(source$ci_method == "bootstrap_normal_bonferroni")) {
    stop("Legacy three-bias forest source ci_method must be bootstrap_normal_bonferroni.", call. = FALSE)
  }
  for (col in c(
    "bootstrap_replicates",
    "bootstrap_seed",
    "glm_iter",
    "fwer_alpha",
    "fwer_z",
    "analysis_data_release",
    "input_path",
    "successful_replicates"
  )) {
    single_metadata_value(source, col)
  }
  if (single_metadata_value(source, "bootstrap_replicates") <= 0 ||
      single_metadata_value(source, "bootstrap_replicates") != floor(single_metadata_value(source, "bootstrap_replicates"))) {
    stop("Legacy three-bias forest source bootstrap_replicates must be a positive integer.", call. = FALSE)
  }
  if (single_metadata_value(source, "bootstrap_seed") <= 0 ||
      single_metadata_value(source, "bootstrap_seed") != floor(single_metadata_value(source, "bootstrap_seed"))) {
    stop("Legacy three-bias forest source bootstrap_seed must be a positive integer.", call. = FALSE)
  }
  if (single_metadata_value(source, "glm_iter") <= 0 ||
      single_metadata_value(source, "glm_iter") != floor(single_metadata_value(source, "glm_iter"))) {
    stop("Legacy three-bias forest source glm_iter must be a positive integer.", call. = FALSE)
  }
  if (single_metadata_value(source, "successful_replicates") !=
      single_metadata_value(source, "bootstrap_replicates")) {
    stop("Legacy three-bias forest source successful_replicates must equal bootstrap_replicates.", call. = FALSE)
  }

  source <- source[order(source$specification_order, source$contrast_order), ]
  row.names(source) <- NULL
  source
}

build_incremental_vote_count_forest_source <- function(old_source,
                                                       point_grid,
                                                       vote_count_bootstrap_draws,
                                                       input_path = NULL,
                                                       ame_tolerance = 0.005) {
  old_source <- validate_legacy_three_bias_forest_source(old_source)
  point_grid <- data.frame(point_grid, check.names = FALSE, stringsAsFactors = FALSE)
  required_grid <- c(
    "specification",
    "specification_order",
    "bias_contrast",
    "contrast_order",
    "fixed_effect",
    "model_nobs",
    "ame_pp"
  )
  missing_grid <- setdiff(required_grid, names(point_grid))
  if (length(missing_grid) > 0) {
    stop(
      "Current three-bias point grid missing column(s): ",
      paste(missing_grid, collapse = ", "),
      call. = FALSE
    )
  }
  for (col in c("specification", "bias_contrast", "fixed_effect")) {
    point_grid[[col]] <- trimws(as.character(point_grid[[col]]))
  }
  for (col in c("specification_order", "contrast_order", "model_nobs", "ame_pp")) {
    point_grid[[col]] <- suppressWarnings(as.numeric(point_grid[[col]]))
    if (any(!is.finite(point_grid[[col]]))) {
      stop("Current three-bias point grid has non-finite values in ", col, ".", call. = FALSE)
    }
  }
  point_grid <- point_grid[order(point_grid$specification_order, point_grid$contrast_order), ]
  row.names(point_grid) <- NULL
  if (nrow(point_grid) != 36L) {
    stop("Current three-bias point grid must contain exactly 36 rows.", call. = FALSE)
  }
  if (anyDuplicated(point_grid[c("specification", "bias_contrast")])) {
    stop("Current three-bias point grid has duplicated specification/contrast rows.", call. = FALSE)
  }
  vote_count_rows <- point_grid[
    point_grid$specification == three_bias_vote_count_specification(),
    ,
    drop = FALSE
  ]
  if (nrow(vote_count_rows) != nrow(three_bias_contrast_specifications())) {
    stop("Current three-bias point grid must contain exactly three vote-count rows.", call. = FALSE)
  }

  old_key <- three_bias_grid_key(old_source)
  point_key <- three_bias_grid_key(point_grid)
  old_match <- match(old_key, point_key)
  if (any(is.na(old_match))) {
    stop("Current three-bias point grid is missing old specification/contrast rows.", call. = FALSE)
  }
  old_ame_delta <- abs(old_source$ame_pp - point_grid$ame_pp[old_match])
  if (any(old_ame_delta > ame_tolerance)) {
    stale <- paste(
      paste(old_source$specification[old_ame_delta > ame_tolerance],
            old_source$bias_contrast[old_ame_delta > ame_tolerance],
            sep = " / "),
      collapse = ", "
    )
    stop("Current observed grid does not match old AME(s) for: ", stale, call. = FALSE)
  }

  vote_count_bootstrap_draws <- as.matrix(vote_count_bootstrap_draws)
  storage.mode(vote_count_bootstrap_draws) <- "double"
  vote_key <- three_bias_grid_key(vote_count_rows)
  if (is.null(colnames(vote_count_bootstrap_draws))) {
    if (ncol(vote_count_bootstrap_draws) != length(vote_key)) {
      stop("Vote-count bootstrap draws must contain exactly three contrast columns.", call. = FALSE)
    }
    colnames(vote_count_bootstrap_draws) <- vote_key
  }
  missing_draw_cols <- setdiff(vote_key, colnames(vote_count_bootstrap_draws))
  extra_draw_cols <- setdiff(colnames(vote_count_bootstrap_draws), vote_key)
  if (length(missing_draw_cols) > 0 || length(extra_draw_cols) > 0) {
    stop("Vote-count bootstrap draw columns must match the vote-count contrast rows.", call. = FALSE)
  }
  vote_count_bootstrap_draws <- vote_count_bootstrap_draws[, vote_key, drop = FALSE]
  if (any(!is.finite(vote_count_bootstrap_draws))) {
    stop("Vote-count bootstrap draws contain non-finite values.", call. = FALSE)
  }
  bootstrap_replicates <- as.integer(single_metadata_value(old_source, "bootstrap_replicates"))
  if (nrow(vote_count_bootstrap_draws) != bootstrap_replicates) {
    stop("Vote-count bootstrap draw replicate count must equal stored bootstrap_replicates.", call. = FALSE)
  }
  vote_count_se <- apply(vote_count_bootstrap_draws, 2, stats::sd, na.rm = FALSE)
  if (any(!is.finite(vote_count_se))) {
    stop("Vote-count bootstrap produced non-finite SE values.", call. = FALSE)
  }

  out <- point_grid
  out$bootstrap_se_pp <- NA_real_
  old_lookup <- match(point_key, old_key)
  old_rows <- !is.na(old_lookup)
  out$bootstrap_se_pp[old_rows] <- old_source$bootstrap_se_pp[old_lookup[old_rows]]
  vote_lookup <- match(point_key, vote_key)
  vote_rows <- !is.na(vote_lookup)
  out$bootstrap_se_pp[vote_rows] <- unname(vote_count_se[vote_lookup[vote_rows]])
  if (any(!is.finite(out$bootstrap_se_pp))) {
    stop("Incremental three-bias forest source has missing bootstrap SE values.", call. = FALSE)
  }

  fwer_alpha <- as.numeric(single_metadata_value(old_source, "fwer_alpha"))
  fwer_family_size <- nrow(out)
  fwer_z <- stats::qnorm(1 - fwer_alpha / (2 * fwer_family_size))
  out$fwer_ci_low_pp <- out$ame_pp - fwer_z * out$bootstrap_se_pp
  out$fwer_ci_high_pp <- out$ame_pp + fwer_z * out$bootstrap_se_pp
  out$bootstrap_replicates <- bootstrap_replicates
  out$cluster_unit <- single_metadata_value(old_source, "cluster_unit")
  out$bootstrap_seed <- as.integer(single_metadata_value(old_source, "bootstrap_seed"))
  out$glm_iter <- as.integer(single_metadata_value(old_source, "glm_iter"))
  out$ci_method <- single_metadata_value(old_source, "ci_method")
  out$fwer_family_size <- fwer_family_size
  out$fwer_alpha <- fwer_alpha
  out$fwer_z <- fwer_z
  out$analysis_data_release <- single_metadata_value(old_source, "analysis_data_release")
  out$input_path <- if (!is.null(input_path)) {
    normalizePath(input_path, winslash = "/", mustWork = FALSE)
  } else {
    single_metadata_value(old_source, "input_path")
  }
  out$successful_replicates <- bootstrap_replicates
  out$specification_order <- as.integer(out$specification_order)
  out$contrast_order <- as.integer(out$contrast_order)
  out$model_nobs <- as.integer(out$model_nobs)
  out
}

assert_explicit_bootstrap_env_matches <- function(env_var, stored_value) {
  raw_value <- Sys.getenv(env_var, unset = NA_character_)
  if (is.na(raw_value)) return(invisible(TRUE))
  parsed <- suppressWarnings(as.integer(raw_value))
  if (is.na(parsed) || parsed != as.integer(stored_value)) {
    stop(
      env_var, " is explicitly set to ", raw_value,
      " but the legacy forest source stores ", as.integer(stored_value), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_incremental_input_matches <- function(old_source, input_path) {
  old_input_path <- single_metadata_value(old_source, "input_path")
  old_normalized <- normalizePath(old_input_path, winslash = "/", mustWork = FALSE)
  current_normalized <- normalizePath(input_path, winslash = "/", mustWork = FALSE)
  old_hash <- hash_if_present(old_normalized)
  current_hash <- hash_if_present(current_normalized)
  if (!is.na(old_hash) && !is.na(current_hash)) {
    if (!identical(old_hash, current_hash)) {
      stop("Legacy forest source input hash does not match current analysis input.", call. = FALSE)
    }
  } else if (!identical(old_normalized, current_normalized)) {
    stop("Legacy forest source input path does not match current analysis input.", call. = FALSE)
  }
  invisible(TRUE)
}

run_three_bias_forest_bootstrap <- function(
    input_path = default_input_path(),
    output_path = three_bias_forest_bootstrap_output_path(),
    b = bootstrap_replicates,
    seed = bootstrap_seed,
    cores = bootstrap_cores) {
  require_packages()
  if (requireNamespace("fixest", quietly = TRUE) &&
      exists("setFixest_notes", asNamespace("fixest"))) {
    fixest::setFixest_notes(FALSE)
  }

  message("Loading active choice-level data: ", input_path)
  raw_df <- load_choice_data(input_path)
  model_df <- prepare_model_dataframe(raw_df)
  documented_stance_proposals <- load_documented_stance_proposals()
  model_df <- normalize_undocumented_one_sided_stance(model_df, documented_stance_proposals)
  invisible(check_expected_counts(model_df))

  message("Freezing shared specification sets from observed normalized data")
  frozen_sets <- shared_specification_sets(model_df, diagnostics_path = NA_character_)

  message("Fitting observed shared specifications")
  point_outputs <- fit_three_bias_shared_outputs(model_df, frozen_sets, glm_iter = bootstrap_glm_iter)
  point_grid <- point_outputs$grid
  expected_n <- nrow(point_grid)
  if (expected_n == 0 || any(!is.finite(point_grid$ame_pp))) {
    stop(
      "Observed three-bias grid must contain finite AMEs; got ",
      expected_n, " rows with ",
      sum(!is.finite(point_grid$ame_pp)), " non-finite.",
      call. = FALSE
    )
  }

  require_packages("data.table")
  model_dt <- data.table::copy(data.table::as.data.table(model_df))
  space_rows <- split(seq_len(nrow(model_dt)), model_dt$space)
  spaces <- sort(names(space_rows))
  if (length(spaces) == 0) {
    stop("No DAO spaces available for three-bias forest bootstrap.", call. = FALSE)
  }

  set.seed(seed)
  sampled_space_sets <- replicate(
    b,
    sample(spaces, length(spaces), replace = TRUE),
    simplify = FALSE
  )

  point_key <- three_bias_grid_key(point_grid)
  draws <- matrix(
    NA_real_,
    nrow = b,
    ncol = nrow(point_grid),
    dimnames = list(NULL, point_key)
  )

  message(
    "Running ", b, " DAO-space clustered bootstrap replicate(s) with seed = ",
    seed,
    " and glm.iter = ",
    bootstrap_glm_iter
  )
  draws <- run_three_bias_bootstrap_replicates(
    sampled_space_sets,
    point_grid,
    model_dt,
    space_rows,
    frozen_sets,
    glm_iter = bootstrap_glm_iter,
    cores = cores
  )

  successful_replicates <- sum(apply(draws, 1, function(r) all(is.finite(r))))
  if (successful_replicates < b) {
    warning(
      successful_replicates, " of ", b, " bootstrap replicates succeeded; ",
      b - successful_replicates, " failed replicate(s) excluded from SE computation.",
      call. = FALSE
    )
  }
  bootstrap_se <- apply(draws, 2, stats::sd, na.rm = TRUE)
  if (any(!is.finite(bootstrap_se))) {
    bad_cols <- names(bootstrap_se)[!is.finite(bootstrap_se)]
    stop(
      "Bootstrap produced non-finite SE(s): ",
      paste(bad_cols, collapse = ", "),
      call. = FALSE
    )
  }

  fwer_alpha <- 0.05
  fwer_family_size <- nrow(point_grid)
  fwer_z <- stats::qnorm(1 - fwer_alpha / (2 * fwer_family_size))
  out <- data.frame(
    point_grid,
    bootstrap_se_pp = unname(bootstrap_se),
    fwer_ci_low_pp = point_grid$ame_pp - fwer_z * unname(bootstrap_se),
    fwer_ci_high_pp = point_grid$ame_pp + fwer_z * unname(bootstrap_se),
    bootstrap_replicates = b,
    cluster_unit = "space",
    bootstrap_seed = seed,
    glm_iter = bootstrap_glm_iter,
    ci_method = "bootstrap_normal_bonferroni",
    fwer_family_size = fwer_family_size,
    fwer_alpha = fwer_alpha,
    fwer_z = fwer_z,
    analysis_data_release = "current",
    input_path = normalizePath(input_path, winslash = "/", mustWork = FALSE),
    successful_replicates = successful_replicates,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  validate_three_bias_forest_inference_source(
    out,
    point_outputs$summary,
    models = point_outputs$models
  )

  output_path <- generated_output_path(output_path)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, output_path, row.names = FALSE)
  message("Wrote ", output_path)
  invisible(out)
}

run_three_bias_forest_incremental_vote_count <- function(
    input_path = default_input_path(),
    output_path = three_bias_forest_bootstrap_output_path(),
    cores = bootstrap_cores) {
  require_packages()
  if (requireNamespace("fixest", quietly = TRUE) &&
      exists("setFixest_notes", asNamespace("fixest"))) {
    fixest::setFixest_notes(FALSE)
  }

  message("Loading legacy three-bias forest source: ", output_path)
  old_source <- validate_legacy_three_bias_forest_source(output_path)
  old_b <- as.integer(single_metadata_value(old_source, "bootstrap_replicates"))
  old_seed <- as.integer(single_metadata_value(old_source, "bootstrap_seed"))
  old_glm_iter <- as.integer(single_metadata_value(old_source, "glm_iter"))
  assert_explicit_bootstrap_env_matches("THREE_BIAS_FOREST_BOOTSTRAP_REPLICATES", old_b)
  assert_explicit_bootstrap_env_matches("THREE_BIAS_FOREST_BOOTSTRAP_SEED", old_seed)
  assert_explicit_bootstrap_env_matches("THREE_BIAS_FOREST_BOOTSTRAP_GLM_ITER", old_glm_iter)
  assert_incremental_input_matches(old_source, input_path)

  message("Loading active choice-level data: ", input_path)
  raw_df <- load_choice_data(input_path)
  model_df <- prepare_model_dataframe(raw_df)
  documented_stance_proposals <- load_documented_stance_proposals()
  model_df <- normalize_undocumented_one_sided_stance(model_df, documented_stance_proposals)
  invisible(check_expected_counts(model_df))

  message("Freezing shared specification sets from observed normalized data")
  frozen_sets <- shared_specification_sets(model_df, diagnostics_path = NA_character_)

  message("Fitting current observed shared specifications")
  point_outputs <- fit_three_bias_shared_outputs(model_df, frozen_sets, glm_iter = old_glm_iter)
  point_grid <- point_outputs$grid
  if (nrow(point_grid) != 36L) {
    stop("Current observed shared grid must contain 36 rows for incremental vote-count mode.", call. = FALSE)
  }
  vote_count_rows <- point_grid[
    point_grid$specification == three_bias_vote_count_specification(),
    ,
    drop = FALSE
  ]
  if (nrow(vote_count_rows) != nrow(three_bias_contrast_specifications())) {
    stop("Current observed shared grid must contain exactly three vote-count rows.", call. = FALSE)
  }

  require_packages("data.table")
  model_dt <- data.table::copy(data.table::as.data.table(model_df))
  space_rows <- split(seq_len(nrow(model_dt)), model_dt$space)
  spaces <- sort(names(space_rows))
  if (length(spaces) == 0) {
    stop("No DAO spaces available for incremental three-bias forest bootstrap.", call. = FALSE)
  }

  set.seed(old_seed)
  sampled_space_sets <- replicate(
    old_b,
    sample(spaces, length(spaces), replace = TRUE),
    simplify = FALSE
  )

  message(
    "Running ", old_b, " DAO-space clustered bootstrap replicate(s) for vote-count rows with seed = ",
    old_seed,
    " and glm.iter = ",
    old_glm_iter
  )
  vote_count_draws <- run_three_bias_bootstrap_replicates(
    sampled_space_sets,
    vote_count_rows,
    model_dt,
    space_rows,
    frozen_sets,
    glm_iter = old_glm_iter,
    cores = cores,
    fit_grid_fn = fit_three_bias_vote_count_shared_grid
  )
  if (any(!is.finite(vote_count_draws))) {
    stop("Incremental vote-count bootstrap produced non-finite replicate draw(s).", call. = FALSE)
  }

  out <- build_incremental_vote_count_forest_source(
    old_source,
    point_grid,
    vote_count_bootstrap_draws = vote_count_draws,
    input_path = input_path
  )
  validate_three_bias_forest_inference_source(
    out,
    point_outputs$summary,
    models = point_outputs$models
  )

  output_path <- generated_output_path(output_path)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, output_path, row.names = FALSE)
  message("Wrote ", output_path)
  invisible(out)
}

run_bootstrap_command <- function() {
  mode <- Sys.getenv("THREE_BIAS_FOREST_BOOTSTRAP_MODE", unset = "full")
  if (identical(mode, "incremental_vote_count")) {
    run_three_bias_forest_incremental_vote_count()
  } else if (identical(mode, "full")) {
    run_three_bias_forest_bootstrap()
  } else {
    stop("Unknown THREE_BIAS_FOREST_BOOTSTRAP_MODE: ", mode, call. = FALSE)
  }
}
