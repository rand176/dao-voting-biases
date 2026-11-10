figure_output_directory <- function(context, release, choices_file, save=TRUE) {
  root <- Sys.getenv("DAO_BIAS_FIGURE_ROOT",unset=file.path(context$OUTPUT_ROOT,"figures"))
  scope <- gsub("[^A-Za-z0-9_.-]+","_",paste(release,tools::file_path_sans_ext(basename(choices_file)),sep="__"))
  path <- paste0(file.path(root,scope),"/")
  if (save && !dir.exists(path)) dir.create(path,recursive=TRUE,showWarnings=FALSE)
  path
}

configure_figures <- function() {
  evalq({
    stance_colors =  c(
        "#4E9F3D",  # muted green
        "#C0392B",  # muted red
        "#2980B9",  # muted blue
        "#8E44AD"   # muted purple
      )

    stance_colors_mixed <- c(stance_colors,
                             "gray50"  # muted orange / amber
    )

    stance_colors_two <- c(stance_colors[1],
                           "salmon"  # muted orange / amber
    )

    #################################
    ## Note: must use choice_share_2.
    #################################

    ## Full analytical sample and explicit low-vote sensitivity sample.
    df_choices_full <- df_choices
    df_choices_gt1vote <- df_choices[df_choices$tot_votes_2 > 1, ]

    ## Derived helpers from is_author_choice (the canonical column in the current
    ## release). Built locally because process_df_choices() gates them on the
    ## legacy is_author_choice2 column, which no longer ships with the release.
    addAuthorChoiceHelpers <- function(df) {
      df$is_author_choice_str2 <- factor(
        ifelse(
          is.na(df$is_author_choice),
          "No Auth.\nVote",
          ifelse(df$is_author_choice == 1, "Author", "Others")
        ),
        levels = c("Author", "Others", "No Auth.\nVote")
      )
      if ("choice_won" %in% colnames(df)) {
        df$won_author_choice <- ifelse(
          !is.na(df$is_author_choice) & df$is_author_choice == 1 & df$choice_won == 1,
          1, 0
        )
      }
      df$n_votes_cut <- cut(
        df$n_votes,
        breaks = c(-Inf, 1, 10, 100, Inf),
        labels = c("1", "2-10", "11-100", "100+")
      )
      df
    }

    addTotalVotesHelpers <- function(df) {
      df$tot_votes_cut <- cut(
        df$tot_votes_2,
        breaks = c(-Inf, 1, 10, 100, Inf),
        labels = c("1", "2-10", "11-100", "100+")
      )
      df$tot_votes_bin <- as.integer(df$tot_votes_cut)
      df
    }

    df_choices <- addTotalVotesHelpers(addAuthorChoiceHelpers(df_choices))
    df_choices_full <- addTotalVotesHelpers(addAuthorChoiceHelpers(df_choices_full))
    df_choices_gt1vote <- addTotalVotesHelpers(addAuthorChoiceHelpers(df_choices_gt1vote))

    cat("df_choices_full rows:", nrow(df_choices_full), "\n")
    cat("df_choices_gt1vote rows:", nrow(df_choices_gt1vote), "\n")

    ## Build long-format stance comparisons only when model stance columns are present.
    stance_model_cols <- setdiff(grep("^stance", colnames(df_choices_full), value = TRUE), "stance")

    if (length(stance_model_cols) > 0) {
      df_choices_long <- df_choices_full %>%
        dplyr::select(-dplyr::any_of("stance")) %>%
        pivot_longer(
          cols = all_of(stance_model_cols),
          names_to = "model",
          values_to = "stance"
        )

      df_choices_long$stance_num <- ifelse(
        df_choices_long$stance == "APPROVE",
        2,
        ifelse(df_choices_long$stance == "OTHER", 1, ifelse(df_choices_long$stance == "ABSTAIN", 0, -1))
      )

      df_choices_long$model <- stage5PlotModelLabels(df_choices_long$model)
    }

    ################################################################################

    figureOutputFlag <- function(name, default = TRUE) {
      value <- Sys.getenv(name, unset = NA_character_)
      if (is.na(value) || !nzchar(value)) {
        return(default)
      }
      tolower(value) %in% c("1", "true", "t", "yes", "y")
    }

    sanitizeFigurePathPart <- function(value) {
      gsub("[^A-Za-z0-9_.-]+", "_", value)
    }

    ## Keep regenerated figures separate from historical hand-edited exports.
    ## Override DAO_BIAS_FIGURE_ROOT to write elsewhere, or SAVEIMG=false to preview only.
    IMGDIR <- figure_output_directory(list(PACKAGE_ROOT=PACKAGE_ROOT,OUTPUT_ROOT=OUTPUT_ROOT),
      analysis_data_provenance$release_name[[1]],analysis_data_provenance$choices_file[[1]],
      save=figureOutputFlag("SAVEIMG",default=TRUE))
    FIGURE_OUTPUT_ROOT <- IMGDIR
    SAVEIMG <- figureOutputFlag("SAVEIMG",default=TRUE)

  }, parent.frame())
}
