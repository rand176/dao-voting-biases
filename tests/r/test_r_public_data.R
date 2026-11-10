source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
source(file.path(PACKAGE_ROOT, "src/r/data/model_paths.R"))

local({
  root <- tempfile("public-data-paths-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  relative <- file.path("metadata", "analysis_data_provenance", "three_bias_forest_bootstrap_inference.csv")
  private <- file.path(root, relative)
  public <- file.path(root, "public_data", relative)
  nested_public <- file.path(root, "analysis", "public_data", relative)
  dir.create(dirname(private), recursive = TRUE)
  dir.create(dirname(public), recursive = TRUE)
  dir.create(dirname(nested_public), recursive = TRUE)
  writeLines("private", private)
  writeLines("public", public)
  writeLines("unrelated nested tree", nested_public)
  previous <- Sys.getenv("ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE") else
    Sys.setenv(ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE = previous), add = TRUE)
  Sys.unsetenv("ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE")
  stopifnot(default_three_bias_forest_inference_path(root) == private)
  Sys.setenv(ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE = private)
  stopifnot(default_three_bias_forest_inference_path(root, public_data = TRUE) == public)
  unlink(public)
  error <- tryCatch(default_three_bias_forest_inference_path(root, public_data = TRUE), error = identity)
  stopifnot(inherits(error, "error"), grepl("Public input is unavailable", conditionMessage(error)))
  for (public_mode in c(FALSE, TRUE)) {
    missing <- tryCatch(resolve_data_input("missing.csv", root, public_data = public_mode), error = identity)
    stopifnot(inherits(missing, "error"))
    message <- conditionMessage(missing)
    if (public_mode) {
      stopifnot(grepl("no private fallback", message), !grepl("Private mode requires", message))
    } else {
      stopifnot(grepl("Private mode requires original analysis inputs", message),
                !grepl("Public data cannot run", message))
    }
  }
})
cat("Public R input selection passed.\n")
