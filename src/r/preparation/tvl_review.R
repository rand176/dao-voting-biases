source(file.path(PACKAGE_ROOT, "src/r/data/model_paths.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/preparation/tvl.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/data/model_data.R"), local = TRUE)
source(file.path(PACKAGE_ROOT, "src/r/models/samples.R"), local = TRUE)

run_top100_snapshot_matching_review <- function(input_path = default_input_path()) {
  require_packages("data.table")
  message("Loading current choice-level release for matching counts: ", input_path)
  raw_df <- load_choice_data(input_path)
  model_df <- prepare_model_dataframe(raw_df)
  write_top100_matching_review_artifacts(
    model_df,
    sample_memberships_path = NA_character_
  )
}
