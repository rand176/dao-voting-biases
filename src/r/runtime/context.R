analysis_context <- function(package_root = Sys.getenv("DAO_BIAS_ROOT", unset=""),
                             data_root = Sys.getenv("DAO_BIAS_DATA_ROOT", unset=""),
                             output_root = Sys.getenv("DAO_BIAS_OUTPUT_ROOT", unset="")) {
  if (!nzchar(package_root)) stop("DAO_BIAS_ROOT must name the installed package")
  package_root <- normalizePath(package_root,winslash="/",mustWork=TRUE)
  if (!nzchar(data_root)) data_root <- package_root
  if (!nzchar(output_root)) output_root <- file.path(package_root,"outputs")
  data_root <- normalizePath(data_root,winslash="/",mustWork=TRUE)
  output_root <- normalizePath(output_root,winslash="/",mustWork=FALSE)
  generated <- file.path(output_root,"data/2024")
  list(PACKAGE_ROOT=package_root, MAIN_DIR=data_root, R_DIR=file.path(package_root,"src/r"),
    INPUT_ROOT=data_root, OUTPUT_ROOT=output_root, DATA_DIR=file.path(data_root,"data/2024"),
    OUTPUT_DATA_DIR=generated, RAW_DATA_DIR=file.path(data_root,"data/2024/raw"),
    DERIVED_DIR=file.path(generated,"derived"), DERIVED_ACTIVE_DIR=file.path(generated,"derived/active"),
    DERIVED_LEGACY_DIR=file.path(generated,"derived/legacy"), DERIVED_DEBUG_DIR=file.path(generated,"derived/debug"),
    DERIVED_ARCHIVE_LARGE_DIR=file.path(generated,"derived/archive_large"),
    PROPOSAL_FLAGS_DIR=file.path(generated,"clean_proposal_flags"),
    EXCEPTIONS_DIR=file.path(data_root,"metadata/exceptions"),
    LLM_DATA_DIR=Sys.getenv("LLM_DATA_DIR",unset=file.path(data_root,"llms")),
    analysis_data_provenance=NULL,df_proposals=NULL,df_choices=NULL,df_choices_full=NULL,
    df_choices_gt1vote=NULL,df_choices_multichoice=NULL,df_choices_multichoice_gt1vote=NULL)
}

new_analysis_environment <- function(context) {
  envir <- new.env(parent=globalenv())
  initialize_analysis(context,envir)
  for (path in c(context$DERIVED_ACTIVE_DIR,context$DERIVED_DEBUG_DIR,
                 file.path(context$OUTPUT_ROOT,"provenance")))
    dir.create(path,recursive=TRUE,showWarnings=FALSE)
  envir$ANALYSIS_DATA_VERSION <- if (is.null(context$release)) "current" else context$release
  envir$ANALYSIS_NOTEBOOK_NAME <- "analysis"
  for (module in c("preparation/choice_helpers.R","preparation/author_votes.R","preparation/labels.R",
                   "preparation/stance_patterns.R","data/load.R","reporting/setup.R"))
    sys.source(file.path(context$PACKAGE_ROOT,"src/r",module),envir=envir)
  envir
}

# Preserve notebook-style evaluation of visible expressions in script workflows.
evaluate_analysis <- function(body,envir,report=NULL) {
  if(!is.null(report)) {
    dir.create(dirname(report),recursive=TRUE,showWarnings=FALSE)
    connection <- file(report,open="wt")
    sink(connection)
    on.exit({sink();close(connection)},add=TRUE)
  }
  expressions <- if(is.call(body) && identical(body[[1]],as.name("{"))) as.list(body)[-1] else list(body)
  for(expression in expressions) {
    value <- withVisible(eval(expression,envir=envir))
    if(value$visible && !is.null(value$value)) print(value$value)
  }
  invisible(envir)
}
