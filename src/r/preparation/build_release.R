run_release_build <- function(context) {
 envir <- new_analysis_environment(context)
 sys.source(file.path(context$PACKAGE_ROOT,"src/r/preparation/author_power.R"),envir)
 sys.source(file.path(context$PACKAGE_ROOT,"src/r/preparation/release.R"),envir)
 evalq({
build_result <- buildStage5CandidateRelease()

message("Raw release artifact: ", build_result$raw_output_path)
message("Candidate release: ", build_result$candidate_output_path)
message("Normalization audit: ", build_result$normalization_audit_path)
message("Candidate release rows: ", build_result$candidate_rows)

 },envir)
 invisible(envir)
}
