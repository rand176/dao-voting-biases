################################################################################
## Robustness and mechanism checks for author-choice bias
################################################################################

required_packages <- c("data.table", "fixest", "ggplot2", "marginaleffects", "modelsummary")

require_packages <- function(packages = required_packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing required R package(s): ",
      paste(missing, collapse = ", "),
      ". Install them before running this robustness script.",
      call. = FALSE
    )
  }
}

################################################################################
## Paths
################################################################################

find_project_root <- function() {
  root <- Sys.getenv("ROBUSTNESS_PROJECT_ROOT", unset=Sys.getenv("DAO_BIAS_DATA_ROOT",unset=PACKAGE_ROOT))
  normalizePath(root,winslash="/",mustWork=TRUE)
}

release_path_from_registry <- function(release_name, project_root = find_project_root()) {
  registry_candidates <- c(
    file.path(project_root, "analysis", "metadata", "analysis_data_releases.csv"),
    file.path(project_root, "metadata", "analysis_data_releases.csv")
  )
  generated_registry <- file.path(Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=tempdir()),"metadata/analysis_data_releases.csv")
  registry_candidates <- c(generated_registry,registry_candidates)
  existing_registries <- registry_candidates[file.exists(registry_candidates)]
  if (length(existing_registries) == 0) {
    stop(
      "Release registry is missing. Expected one of: ",
      paste(registry_candidates, collapse = " ; "),
      call. = FALSE
    )
  }
  registry_path <- existing_registries[[1]]

  registry <- utils::read.csv(registry_path, stringsAsFactors = FALSE)
  if (!"release_name" %in% colnames(registry)) {
    stop("Release registry is missing release_name: ", registry_path, call. = FALSE)
  }

  release_rows <- registry[
    !is.na(registry$release_name) & registry$release_name == release_name,
    ,
    drop = FALSE
  ]
  if (nrow(release_rows) != 1) {
    stop(
      "Release registry must contain exactly one ", release_name, " row: ",
      registry_path,
      call. = FALSE
    )
  }

  relative_path <- if ("relative_path" %in% colnames(release_rows)) {
    release_rows$relative_path[[1]]
  } else {
    NA_character_
  }
  if (!is.na(relative_path) && nzchar(relative_path)) {
    return(relative_path)
  }

  path <- if ("path" %in% colnames(release_rows)) {
    release_rows$path[[1]]
  } else {
    NA_character_
  }
  if (!is.na(path) && nzchar(path)) {
    return(path)
  }

  stop(
    "Release registry row is missing relative_path/path for ",
    release_name,
    ": ",
    registry_path,
    call. = FALSE
  )
}

current_release_path_from_registry <- function(project_root = find_project_root()) {
  release_path_from_registry("current", project_root)
}

candidate_release_path_from_registry <- function(project_root = find_project_root()) {
  release_path_from_registry("candidate", project_root)
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

default_input_path <- function(project_root = find_project_root()) {
  env_input <- Sys.getenv("ROBUSTNESS_INPUT_FILE", unset = "")
  if (nzchar(env_input)) {
    return(normalizePath(env_input, winslash = "/", mustWork = TRUE))
  }

  release_path <- release_path_from_registry(Sys.getenv("DAO_BIAS_RELEASE",unset="current"),project_root)
  candidates <- if (is_absolute_path(release_path)) {
    release_path
  } else {
    c(
      file.path(project_root, "analysis", release_path),
      file.path(project_root, release_path)
    )
  }
  generated <- file.path(Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=tempdir()),release_path)
  if(!is_absolute_path(release_path)) candidates <- c(generated,candidates)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Selected release input is missing. Expected one of: ",
      paste(candidates, collapse = " ; "),
      call. = FALSE
    )
  }
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

default_output_dir <- function(project_root = find_project_root()) {
  env_output <- Sys.getenv("ROBUSTNESS_OUTPUT_DIR", unset = "")
  if (nzchar(env_output)) {
    return(normalizePath(env_output, winslash = "/", mustWork = FALSE))
  }
  file.path(Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=file.path(PACKAGE_ROOT,"outputs")),"models")
}

default_stance_overrides_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv(
    "ROBUSTNESS_STANCE_ONE_SIDED_EXCEPTIONS_FILE",
    unset = Sys.getenv("ROBUSTNESS_STANCE_OVERRIDES_FILE", unset = "")
  )
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(project_root, "analysis", "metadata", "exceptions", "stance_one_sided_exceptions.csv"),
    file.path(project_root, "metadata", "exceptions", "stance_one_sided_exceptions.csv")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  existing[[1]]
}

default_candidate_normalization_audit_path <- function(project_root = find_project_root()) {
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "stage5_candidate_release_normalization_audit.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "stage5_candidate_release_normalization_audit.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  existing[[1]]
}

default_top_tvl_spaces_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP_TVL_SPACES_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "defillama_protocol_ethereum_tvl_top100.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "defillama_protocol_ethereum_tvl_top100.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

default_top_tvl_overrides_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP_TVL_OVERRIDES_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "defillama_snapshot_overrides.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "defillama_snapshot_overrides.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

default_snapshot_spaces_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_SNAPSHOT_SPACES_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(project_root, "analysis", "data", "2024", "raw", "spaces_0.csv.gz"),
    file.path(project_root, "data", "2024", "raw", "spaces_0.csv.gz")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

default_top_tvl_match_diagnostics_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "defillama_top_tvl_snapshot_match_diagnostics.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "defillama_top_tvl_snapshot_match_diagnostics.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

default_top100_match_summary_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP100_MATCH_SUMMARY_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  file.path(
    project_root,
    "metadata",
    "analysis_data_provenance",
    "defillama_top100_match_summary.csv"
  )
}

default_top100_overrides_draft_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP100_OVERRIDES_DRAFT_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  file.path(
    project_root,
    "metadata",
    "analysis_data_provenance",
    "defillama_snapshot_overrides_draft.csv"
  )
}

default_top100_match_review_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP100_MATCH_REVIEW_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  file.path(
    project_root,
    "metadata",
    "analysis_data_provenance",
    "defillama_snapshot_match_review.md"
  )
}

default_top_tvl_sample_memberships_path <- function(
    project_root = find_project_root(),
    diagnostics_path = default_top_tvl_match_diagnostics_path(project_root)) {
  env_path <- Sys.getenv("ROBUSTNESS_TOP_TVL_SAMPLE_MEMBERSHIPS_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  if (length(diagnostics_path) == 0 || is.na(diagnostics_path) || !nzchar(diagnostics_path)) {
    return(NA_character_)
  }
  diagnostics_env_path <- Sys.getenv("ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE", unset = "")
  if (nzchar(diagnostics_env_path)) {
    warning(
      "ROBUSTNESS_TOP_TVL_MATCH_DIAGNOSTICS_FILE is set; the top-TVL sample memberships path ",
      "is being derived from its directory. Set ROBUSTNESS_TOP_TVL_SAMPLE_MEMBERSHIPS_FILE ",
      "explicitly to avoid this implicit coupling.",
      call. = FALSE
    )
  }
  file.path(dirname(diagnostics_path), "defillama_top_tvl_snapshot_sample_memberships.csv")
}

default_top_tvl_protocol_match_path <- function(project_root = find_project_root()) {
  default_top_tvl_overrides_path(project_root)
}

default_primary_ame_inference_path <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_PRIMARY_AME_INFERENCE_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "primary_ame_bootstrap_inference.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "primary_ame_bootstrap_inference.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

resolve_data_input <- function(relative_path, project_root = find_project_root(), public_data = FALSE) {
  if (is_absolute_path(relative_path) || any(strsplit(relative_path, "[/\\\\]")[[1]] == "..")) {
    stop("Input paths must be relative to the analysis repository.", call. = FALSE)
  }
  roots <- if (isTRUE(public_data)) {
    file.path(project_root, "public_data")
  } else {
    c(file.path(project_root, "analysis"), project_root)
  }
  candidates <- file.path(roots, relative_path)
  existing <- candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (!length(existing)) {
    stop(if (public_data) "Public input is unavailable: " else "Private input is unavailable: ",
         paste(candidates, collapse = " ; "),
         if (isTRUE(public_data)) ". Restore the published input; no private fallback is attempted."
         else ". Private mode requires original analysis inputs.", call. = FALSE)
  }
  existing[[1]]
}

default_three_bias_forest_inference_path <- function(project_root = find_project_root(), public_data = FALSE) {
  if (isTRUE(public_data)) {
    return(resolve_data_input(
      "metadata/analysis_data_provenance/three_bias_forest_bootstrap_inference.csv",
      project_root, public_data = TRUE
    ))
  }
  env_path <- Sys.getenv("ROBUSTNESS_THREE_BIAS_FOREST_INFERENCE_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  candidates <- c(
    file.path(
      project_root,
      "analysis",
      "metadata",
      "analysis_data_provenance",
      "three_bias_forest_bootstrap_inference.csv"
    ),
    file.path(
      project_root,
      "metadata",
      "analysis_data_provenance",
      "three_bias_forest_bootstrap_inference.csv"
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(candidates[[1]])
  existing[[1]]
}

default_robustness_figure_dir <- function(project_root = find_project_root()) {
  env_path <- Sys.getenv("ROBUSTNESS_FIGURE_DIR", unset = "")
  if (nzchar(env_path)) {
    return(normalizePath(env_path, winslash = "/", mustWork = FALSE))
  }
  file.path(Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=file.path(PACKAGE_ROOT,"outputs")),"figures/robustness")
}

safe_md5 <- function(path) {
  if (length(path) == 0 || is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  unname(as.character(tools::md5sum(path)))
}

resolve_release_path <- function(release_path, project_root = find_project_root()) {
  candidates <- if (is_absolute_path(release_path)) {
    release_path
  } else {
    c(
      file.path(project_root, "analysis", release_path),
      file.path(project_root, release_path)
    )
  }
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) return(NA_character_)
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

load_promoted_candidate_normalization_audit <- function(
    input_path = default_input_path(),
    project_root = find_project_root()) {
  candidate_input_path <- resolve_release_path(
    candidate_release_path_from_registry(project_root),
    project_root
  )
  audit_path <- default_candidate_normalization_audit_path(project_root)
  if (is.na(candidate_input_path) || is.na(audit_path)) {
    return(NULL)
  }
  if (!identical(safe_md5(input_path), safe_md5(candidate_input_path))) {
    return(NULL)
  }
  audit <- utils::read.csv(audit_path, stringsAsFactors = FALSE)
  required <- c("proposal", "original_stance_state", "action", "rows")
  if (!all(required %in% colnames(audit))) {
    stop(
      "Candidate normalization audit is missing required columns: ",
      paste(setdiff(required, colnames(audit)), collapse = ", "),
      call. = FALSE
    )
  }
  audit <- audit[
    !is.na(audit$proposal) & nzchar(audit$proposal) & audit$action == "normalized_to_other",
    ,
    drop = FALSE
  ]
  if (nrow(audit) == 0) {
    return(NULL)
  }
  audit$proposal <- as.character(audit$proposal)
  audit$original_stance_state <- trimws(tolower(as.character(audit$original_stance_state)))
  audit$rows <- as.integer(audit$rows)
  unique(audit[, required, drop = FALSE])
}

# Route artifacts into the selected output tree; keep unrelated explicit paths.
generated_output_path <- function(path) {
  output <- Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset="")
  if(!nzchar(output) || is.na(path) || !nzchar(path)) return(path)
  input <- find_project_root()
  input <- sub("/$","",normalizePath(input,winslash="/",mustWork=FALSE))
  prefix <- paste0(input,"/")
  absolute <- normalizePath(path,winslash="/",mustWork=FALSE)
  if(!startsWith(absolute,prefix)) return(path)
  relative <- substring(absolute,nchar(prefix)+1L)
  relative <- sub("^analysis/","",relative)
  file.path(output,relative)
}
