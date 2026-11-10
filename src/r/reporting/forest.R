three_bias_forest_required_columns <- function() {
  c(
    "specification",
    "specification_order",
    "fixed_effect",
    "model_nobs",
    "bias_contrast",
    "contrast_order",
    "ame_pp",
    "bootstrap_se_pp",
    "fwer_ci_low_pp",
    "fwer_ci_high_pp",
    "bootstrap_replicates",
    "cluster_unit",
    "bootstrap_seed",
    "glm_iter",
    "ci_method",
    "fwer_family_size"
  )
}

as_three_bias_forest_source <- function(source) {
  if (is.character(source) && length(source) == 1) {
    if (!file.exists(source)) {
      stop("Three-bias forest inference source is missing: ", source, call. = FALSE)
    }
    return(utils::read.csv(source, stringsAsFactors = FALSE))
  }
  data.frame(source, check.names = FALSE, stringsAsFactors = FALSE)
}

validate_three_bias_forest_inference_source <- function(source,
                                                        three_bias_summary = NULL,
                                                        models = NULL,
                                                        ame_tolerance = 0.005,
                                                        ci_tolerance = 1e-8) {
  source <- as_three_bias_forest_source(source)
  required <- three_bias_forest_required_columns()
  missing <- setdiff(required, names(source))
  if (length(missing) > 0) {
    stop(
      "Three-bias forest inference source missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  optional_numeric <- c("fwer_alpha", "fwer_z", "successful_replicates")
  numeric_cols <- intersect(
    c(
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
      optional_numeric
    ),
    names(source)
  )
  for (col in numeric_cols) {
    source[[col]] <- suppressWarnings(as.numeric(source[[col]]))
    if (any(!is.finite(source[[col]]))) {
      stop("Three-bias forest inference source has non-finite values in ", col, ".", call. = FALSE)
    }
  }
  source$specification <- trimws(as.character(source$specification))
  source$fixed_effect <- trimws(as.character(source$fixed_effect))
  source$bias_contrast <- trimws(as.character(source$bias_contrast))
  source$cluster_unit <- trimws(as.character(source$cluster_unit))
  source$ci_method <- trimws(as.character(source$ci_method))

  specs <- shared_bias_summary_specifications()
  contrasts <- three_bias_contrast_specifications()
  expected <- merge(
    specs[, c("specification", "specification_order", "fixed_effect"), drop = FALSE],
    contrasts[, c("bias_contrast", "contrast_order"), drop = FALSE],
    all = TRUE
  )
  expected <- expected[order(expected$specification_order, expected$contrast_order), ]
  source <- source[order(source$specification_order, source$contrast_order), ]
  row.names(source) <- NULL

  if (nrow(source) != nrow(expected)) {
    stop(
      "Three-bias forest inference source must contain exactly ",
      nrow(expected),
      " rows.",
      call. = FALSE
    )
  }
  if (anyDuplicated(source[c("specification", "bias_contrast")])) {
    stop("Three-bias forest inference source has duplicated specification/contrast rows.", call. = FALSE)
  }
  if (!identical(source$specification, expected$specification) ||
      !identical(as.integer(source$specification_order), as.integer(expected$specification_order)) ||
      !identical(source$bias_contrast, expected$bias_contrast) ||
      !identical(as.integer(source$contrast_order), as.integer(expected$contrast_order))) {
    stop("Three-bias forest inference source rows do not match the shared specification/contrast order.", call. = FALSE)
  }
  if (!identical(source$fixed_effect, expected$fixed_effect)) {
    stop("Three-bias forest inference source fixed-effect labels do not match the shared specifications.", call. = FALSE)
  }
  if (!all(source$ci_method == "bootstrap_normal_bonferroni")) {
    stop("Three-bias forest inference source must use ci_method = bootstrap_normal_bonferroni.", call. = FALSE)
  }
  if (!all(source$fwer_family_size == nrow(expected))) {
    stop("Three-bias forest inference source must use fwer_family_size = ", nrow(expected), ".", call. = FALSE)
  }
  if (!all(source$cluster_unit == "space")) {
    stop("Three-bias forest inference source must use cluster_unit = space.", call. = FALSE)
  }
  if (length(unique(source$bootstrap_replicates)) != 1 ||
      unique(source$bootstrap_replicates) <= 0) {
    stop("Three-bias forest inference source must use one positive bootstrap_replicates value.", call. = FALSE)
  }
  if (length(unique(source$bootstrap_seed)) != 1 ||
      unique(source$bootstrap_seed) <= 0 ||
      unique(source$bootstrap_seed) != floor(unique(source$bootstrap_seed))) {
    stop("Three-bias forest inference source must use one positive integer bootstrap_seed value.", call. = FALSE)
  }
  if (length(unique(source$glm_iter)) != 1 ||
      unique(source$glm_iter) <= 0 ||
      unique(source$glm_iter) != floor(unique(source$glm_iter))) {
    stop("Three-bias forest inference source must use one positive integer glm_iter value.", call. = FALSE)
  }
  if ("successful_replicates" %in% names(source) &&
      any(source$successful_replicates != source$bootstrap_replicates)) {
    stop("Three-bias forest inference source successful_replicates must equal bootstrap_replicates.", call. = FALSE)
  }

  if (!is.null(three_bias_summary)) {
    current_grid <- build_three_bias_summary_long_grid(three_bias_summary, models = models)
    current_grid <- current_grid[order(current_grid$specification_order, current_grid$contrast_order), ]
    stale <- abs(source$ame_pp - current_grid$ame_pp) > ame_tolerance
    if (any(stale)) {
      stale_rows <- paste(
        paste(current_grid$specification[stale], current_grid$bias_contrast[stale], sep = " / "),
        collapse = ", "
      )
      stop(
        "Three-bias forest inference source does not match the current three-bias AME for: ",
        stale_rows,
        call. = FALSE
      )
    }
    if (!all(is.na(current_grid$model_nobs)) &&
        any(as.integer(source$model_nobs) != as.integer(current_grid$model_nobs))) {
      stop("Three-bias forest inference source model_nobs does not match the current models.", call. = FALSE)
    }
  }

  if (any(source$bootstrap_se_pp < 0)) {
    stop("Three-bias forest inference source has negative bootstrap_se_pp values.", call. = FALSE)
  }
  if (any(source$fwer_ci_low_pp > source$ame_pp | source$fwer_ci_high_pp < source$ame_pp)) {
    stop("Three-bias forest inference source CI does not contain ame_pp.", call. = FALSE)
  }

  fwer_z <- if ("fwer_z" %in% names(source)) {
    source$fwer_z
  } else {
    stats::qnorm(1 - 0.05 / (2 * nrow(expected)))
  }
  expected_low <- source$ame_pp - fwer_z * source$bootstrap_se_pp
  expected_high <- source$ame_pp + fwer_z * source$bootstrap_se_pp
  if (any(abs(source$fwer_ci_low_pp - expected_low) > ci_tolerance) ||
      any(abs(source$fwer_ci_high_pp - expected_high) > ci_tolerance)) {
    stop("Three-bias forest inference source CI arithmetic does not match ame_pp +/- fwer_z * bootstrap_se_pp.", call. = FALSE)
  }

  source$specification_order <- as.integer(source$specification_order)
  source$contrast_order <- as.integer(source$contrast_order)
  source$model_nobs <- as.integer(source$model_nobs)
  source$bootstrap_replicates <- as.integer(source$bootstrap_replicates)
  source$bootstrap_seed <- as.integer(source$bootstrap_seed)
  source$glm_iter <- as.integer(source$glm_iter)
  source$fwer_family_size <- as.integer(source$fwer_family_size)
  if ("successful_replicates" %in% names(source)) {
    source$successful_replicates <- as.integer(source$successful_replicates)
  }
  source
}

build_three_bias_forest_plot_grid <- function(source,
                                              three_bias_summary = NULL,
                                              models = NULL,
                                              omit_specifications = character()) {
  grid <- validate_three_bias_forest_inference_source(source, three_bias_summary, models)
  omit_specifications <- trimws(as.character(omit_specifications))
  omit_specifications <- omit_specifications[nzchar(omit_specifications)]
  if (length(omit_specifications) > 0) {
    missing_omit <- setdiff(omit_specifications, unique(grid$specification))
    if (length(missing_omit) > 0) {
      stop(
        "Cannot omit unknown three-bias forest specification(s): ",
        paste(missing_omit, collapse = ", "),
        call. = FALSE
      )
    }
    grid <- grid[!grid$specification %in% omit_specifications, , drop = FALSE]
    if (nrow(grid) == 0) {
      stop("Three-bias forest plot grid cannot be empty after omitting specifications.", call. = FALSE)
    }
  }
  author_rows <- grid[grid$bias_contrast == "Author choice", ]
  author_rows <- author_rows[order(-author_rows$ame_pp, author_rows$specification_order), ]
  grid$plot_specification_order <- match(grid$specification, author_rows$specification)
  grid <- grid[order(grid$plot_specification_order, grid$contrast_order), ]
  row.names(grid) <- NULL
  grid
}

three_bias_forest_top100_omitted_specifications <- function() {
  c(
    "Top 25 Ethereum-TVL DAO subset",
    "Top 50 Ethereum-TVL DAO subset"
  )
}

three_bias_forest_top100_caption_note <- function() {
  "TVL-25 and TVL-50 rows omitted for readability; estimates are comparable to TVL-100."
}

three_bias_forest_latex_include <- function() {
  family_size <- nrow(shared_bias_summary_specifications()) *
    nrow(three_bias_contrast_specifications())
  paste(
    c(
      "\\begin{figure}[htbp]",
      "    \\centering",
      "    \\includegraphics[width=\\linewidth]{figures/2026/robustness/figure_three_bias_forest.pdf}",
      sprintf(
        "    \\caption{Three-bias average marginal effects across shared robustness specifications. Points show response-scale AMEs in percentage points; horizontal intervals are Bonferroni FWER-adjusted across the %d displayed AMEs.}",
        family_size
      ),
      "    \\label{fig:three-bias-forest}",
      "\\end{figure}"
    ),
    collapse = "\n"
  )
}

three_bias_forest_top100_latex_include <- function(label = "fig:three-bias-forest") {
  family_size <- nrow(shared_bias_summary_specifications()) *
    nrow(three_bias_contrast_specifications())
  paste(
    c(
      "\\begin{figure}[htbp]",
      "    \\centering",
      "    \\includegraphics[width=\\linewidth]{figures/2026/robustness/figure_three_bias_forest_top100.pdf}",
      sprintf(
        paste0(
          "    \\caption{Three-bias average marginal effects across shared robustness specifications. ",
          "Points show response-scale AMEs in percentage points; horizontal intervals are Bonferroni FWER-adjusted across the %d shared AMEs. ",
          "%s}"
        ),
        family_size,
        three_bias_forest_top100_caption_note()
      ),
      paste0("    \\label{", label, "}"),
      "\\end{figure}"
    ),
    collapse = "\n"
  )
}

three_bias_forest_plot_caption <- function(plot_grid, caption_note = NULL) {
  family_size <- unique(plot_grid$fwer_family_size)
  if (length(family_size) != 1 || !is.finite(family_size)) {
    family_size <- nrow(plot_grid)
  }
  family_label <- if (as.integer(family_size) == nrow(plot_grid)) "displayed" else "shared"
  caption_lines <- c(
    sprintf(
      "Intervals are Bonferroni FWER-adjusted across the %d %s AMEs.",
      as.integer(family_size),
      family_label
    )
  )
  if (!is.null(caption_note) && nzchar(caption_note)) {
    caption_lines <- c(caption_lines, caption_note)
  }
  paste(caption_lines, collapse = "\n")
}

draw_three_bias_forest_plot <- function(plot_grid, caption_note = NULL) {
  contrast_levels <- three_bias_contrast_specifications()$bias_contrast
  colors <- c(
    "Author choice" = "#0072B2",
    "First vs second choice" = "#D55E00",
    "Approve stance" = "#009E73"
  )

  apply_display_label <- function(x) {
    if (exists("robustness_short_name", mode = "function", inherits = TRUE)) {
      x <- robustness_short_name(x)
    }
    x <- ifelse(x == "Compat full", "Expanded", x)
    x <- ifelse(x == "All systems", "Full", x)
    x
  }

  spec_levels <- unique(plot_grid$specification[order(plot_grid$plot_specification_order)])
  display_levels <- apply_display_label(spec_levels)

  pd <- plot_grid
  pd$specification_display <- factor(
    apply_display_label(pd$specification),
    levels = rev(display_levels)
  )
  pd$bias_contrast <- factor(pd$bias_contrast, levels = contrast_levels)

  ggplot2::ggplot(
    pd,
    ggplot2::aes(
      x = ame_pp,
      y = specification_display,
      colour = bias_contrast,
      xmin = fwer_ci_low_pp,
      xmax = fwer_ci_high_pp
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.4) +
    ggplot2::geom_pointrange(
      position = ggplot2::position_dodge(width = 0.6),
      size = 0.6,
      linewidth = 0.7
    ) +
    ggplot2::scale_colour_manual(values = colors, breaks = contrast_levels) +
    ggplot2::scale_x_continuous(breaks = seq(0, 70, by = 10)) +
    ggplot2::labs(
      x = "Average marginal effect (percentage points)",
      y = NULL,
      colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1.0)),
      legend.key.width = grid::unit(1.4, "lines"),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.05), colour = "grey15"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.0), colour = "grey15"),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 8),
        size = ggplot2::rel(1.0)
      ),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.4),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 6, r = 14, b = 6, l = 6)
    )
}

write_three_bias_forest_plot <- function(source = default_three_bias_forest_inference_path(),
                                         three_bias_summary = NULL,
                                         models = NULL,
                                         figure_dir = default_robustness_figure_dir(),
                                         file_stem = "figure_three_bias_forest",
                                         width = 9.0,
                                         height = 7.0,
                                         omit_specifications = character(),
                                         caption_note = NULL) {
  plot_grid <- build_three_bias_forest_plot_grid(
    source,
    three_bias_summary,
    models,
    omit_specifications = omit_specifications
  )
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    svg = file.path(figure_dir, paste0(file_stem, ".svg")),
    pdf = file.path(figure_dir, paste0(file_stem, ".pdf"))
  )

  p <- draw_three_bias_forest_plot(plot_grid, caption_note = caption_note)
  ggplot2::ggsave(
    paths[["svg"]],
    plot = p,
    width = width,
    height = height,
    units = "in",
    device = "svg"
  )
  ggplot2::ggsave(
    paths[["pdf"]],
    plot = p,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf
  )

  message("Wrote ", paths[["svg"]])
  message("Wrote ", paths[["pdf"]])
  invisible(paths)
}

write_three_bias_forest_top100_plot <- function(source = default_three_bias_forest_inference_path(),
                                                three_bias_summary = NULL,
                                                models = NULL,
                                                figure_dir = default_robustness_figure_dir(),
                                                file_stem = "figure_three_bias_forest_top100",
                                                width = 9.0,
                                                height = 6.2) {
  write_three_bias_forest_plot(
    source = source,
    three_bias_summary = three_bias_summary,
    models = models,
    figure_dir = figure_dir,
    file_stem = file_stem,
    width = width,
    height = height,
    omit_specifications = three_bias_forest_top100_omitted_specifications(),
    caption_note = three_bias_forest_top100_caption_note()
  )
}
