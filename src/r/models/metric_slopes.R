run_metric_slopes <- function(context) {
 envir <- new_analysis_environment(context)
 evalq(loadAnalysisData(version=ANALYSIS_DATA_VERSION),envir)
 evalq({
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(fixest)
})

build_metric_test <- function(df, bucket_label, bucket_filter_expr) {
  df %>%
    dplyr::filter(N_choices > 1) %>%
    dplyr::mutate(bucket = ifelse({{ bucket_filter_expr }}, bucket_label, "Others")) %>%
    dplyr::group_by(proposal, space, type, N_choices, N_choices5, bucket) %>%
    dplyr::summarise(
      share_sum  = sum(choice_share_2, na.rm = TRUE),
      excess_sum = sum(excess_share, na.rm = TRUE),
      K          = dplyr::n(),
      .groups    = "drop"
    ) %>%
    dplyr::filter(bucket == bucket_label) %>%
    dplyr::mutate(mult_excess = (N_choices / K) * share_sum - 1) %>%
    tidyr::pivot_longer(
      cols      = c(excess_sum, mult_excess),
      names_to  = "metric_type",
      values_to = "metric_value"
    ) %>%
    dplyr::mutate(
      metric_type = factor(
        metric_type,
        levels = c("excess_sum", "mult_excess"),
        labels = c("Additive excess", "Multiplicative excess")
      )
    )
}

df_pos_metric_test <- build_metric_test(
  df_choices_full,
  bucket_label        = "First",
  bucket_filter_expr  = choice_idx == 1
)

df_author_metric_test <- df_choices_full %>%
  dplyr::filter(!is.na(is_author_choice)) %>%
  build_metric_test(
    bucket_label       = "Author",
    bucket_filter_expr = is_author_choice == 1
  )

df_approve_metric_test <- df_choices_full %>%
  dplyr::filter(N_choices > 1) %>%
  dplyr::group_by(proposal) %>%
  dplyr::filter(any(stance == "APPROVE", na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  build_metric_test(
    bucket_label       = "Approve",
    bucket_filter_expr = !is.na(stance) & stance == "APPROVE"
  )

message("Row counts (per-proposal observations, biased bucket only):")
message("  Position: ", nrow(df_pos_metric_test) / 2)
message("  Author:   ", nrow(df_author_metric_test) / 2)
message("  Approve:  ", nrow(df_approve_metric_test) / 2)

slope_fit <- function(df, metric_label) {
  fixest::feols(
    metric_value ~ type + log(N_choices) | space,
    data    = subset(df, metric_type == metric_label),
    cluster = ~ space
  )
}

pos_add  <- slope_fit(df_pos_metric_test,     "Additive excess")
pos_mult <- slope_fit(df_pos_metric_test,     "Multiplicative excess")
aut_add  <- slope_fit(df_author_metric_test,  "Additive excess")
aut_mult <- slope_fit(df_author_metric_test,  "Multiplicative excess")
app_add  <- slope_fit(df_approve_metric_test, "Additive excess")
app_mult <- slope_fit(df_approve_metric_test, "Multiplicative excess")

cat("\n", strrep("=", 78), "\n", sep = "")
cat("OLS slopes: metric_value ~ type + log(N_choices) | space, clustered on space\n")
cat(strrep("=", 78), "\n\n", sep = "")

print(fixest::etable(
  list(
    "Position (add)"  = pos_add,
    "Position (mult)" = pos_mult,
    "Author (add)"    = aut_add,
    "Author (mult)"   = aut_mult,
    "Approve (add)"   = app_add,
    "Approve (mult)"  = app_mult
  ),
  cluster = ~ space,
  digits  = 3
))

extract_slope <- function(fit) {
  cf <- summary(fit)$coeftable["log(N_choices)", , drop = TRUE]
  data.frame(
    slope   = unname(cf["Estimate"]),
    se      = unname(cf["Std. Error"]),
    t_stat  = unname(cf["t value"]),
    p_value = unname(cf[grep("Pr", names(cf))])
  )
}

slope_table <- dplyr::bind_rows(
  cbind(bias = "Position", metric = "Additive",       extract_slope(pos_add)),
  cbind(bias = "Position", metric = "Multiplicative", extract_slope(pos_mult)),
  cbind(bias = "Author",   metric = "Additive",       extract_slope(aut_add)),
  cbind(bias = "Author",   metric = "Multiplicative", extract_slope(aut_mult)),
  cbind(bias = "Approve",  metric = "Additive",       extract_slope(app_add)),
  cbind(bias = "Approve",  metric = "Multiplicative", extract_slope(app_mult))
)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Slope summary (log(N_choices) coefficient)\n")
cat(strrep("=", 78), "\n\n", sep = "")
print(slope_table, row.names = FALSE, digits = 4)

cat("\n", strrep("=", 78), "\n", sep = "")
cat("Sanity check (Position only): slope(mult) - slope(add) should equal 1\n")
cat(strrep("=", 78), "\n", sep = "")
pos_diff <- slope_table$slope[2] - slope_table$slope[1]
cat(sprintf("  slope(mult) - slope(add) = %.4f  (expected: 1.0000)\n", pos_diff))

invisible(slope_table)

 },envir)
 invisible(envir)
}
