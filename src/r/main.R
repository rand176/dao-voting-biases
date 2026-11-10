#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
file_arg <- grep("^--file=",commandArgs(FALSE),value=TRUE)[[1]]
PACKAGE_ROOT <- Sys.getenv("DAO_BIAS_ROOT",unset=dirname(dirname(dirname(normalizePath(sub("^--file=","",file_arg))))))
source(file.path(PACKAGE_ROOT,"src/r/runtime/context.R"))
source(file.path(PACKAGE_ROOT,"src/r/runtime/init.R"))
context <- analysis_context(PACKAGE_ROOT)
options(device=function(...) {
  path <- file.path(context$OUTPUT_ROOT,"figures/session.pdf")
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  grDevices::pdf(path,...)
})
context$release <- Sys.getenv("DAO_BIAS_RELEASE",unset="current")
if (!length(args)) stop("Use python run.py --help")
command <- args[[1]]; rest <- args[-1]
if (length(rest) %% 2L) stop("Expected --option value pairs")
options <- list()
if (length(rest)) for (i in seq.int(1L,length(rest),2L)) options[[sub("^--","",rest[[i]])]] <- rest[[i+1L]]
module <- function(path,envir=globalenv()) sys.source(file.path(PACKAGE_ROOT,"src/r",path),envir=envir)
if (command=="author-timing") {
  module("models/author_timing.R")
  a <- c("--input-root",context$INPUT_ROOT,"--output-dir",context$OUTPUT_ROOT,
         "--workers",if(is.null(options$workers)) "4" else options$workers)
  if(!is.null(options[["timing-file"]])) a <- c(a,"--timing-file",options[["timing-file"]])
  author_timing_main(a)
} else if (command %in% c("models","bootstrap","tvl-review")) {
  env <- list2env(context,parent=globalenv())
  module("models/workflow.R",env)
  if(command=="bootstrap") {
    if(!is.null(options$workers)) Sys.setenv(THREE_BIAS_FOREST_BOOTSTRAP_CORES=options$workers)
    if(!is.null(options$replicates)) Sys.setenv(THREE_BIAS_FOREST_BOOTSTRAP_REPLICATES=options$replicates)
    if(!is.null(options$seed)) Sys.setenv(THREE_BIAS_FOREST_BOOTSTRAP_SEED=options$seed)
    module("inference/bootstrap.R",env);env$run_bootstrap_command()
  } else if(command=="tvl-review") {
    module("preparation/tvl_review.R",env);env$run_top100_snapshot_matching_review()
  } else if (identical(options$kind,"additional")) {
    module("models/additional.R");run_additional_models(context)
  } else if (is.null(options$kind) || options$kind=="main") env$run_robustness_bias_mechanisms()
  else stop("Unknown model kind")
} else if (command=="tvl-memberships") {
  module("preparation/tvl_memberships.R");regenerate_tvl_memberships(context)
} else if(command=="tables" && identical(options$kind,"heuristics")) {
  module("reporting/heuristic_tables.R")
  write_stance_heuristic_tables(output_dir=file.path(context$OUTPUT_ROOT,"tables"))
} else if(command %in% c("descriptive","metric-slopes","stance-comparison","prepare","figures")) {
  if(command=="prepare") {
    stages <- list(dataset=c("preparation/dataset.R","prepare_dataset"),stance=c("preparation/stance.R","prepare_stance"),
      "merge-votes"=c("preparation/merge_votes.R","prepare_vote_merge"),features=c("preparation/features.R","prepare_features"),
      labels=c("preparation/integrate_labels.R","prepare_labels"),candidate=c("preparation/build_release.R","run_release_build"))
    selected <- stages[[options$stage]];if(is.null(selected)) stop("Unknown R preparation stage")
    module(selected[[1]]);get(selected[[2]])(context)
  } else if(command=="figures") {
    files <- list.files(file.path(PACKAGE_ROOT,"src/r/reporting/figures"),pattern="[.]R$",full.names=TRUE)
    if(!is.null(options$kind) && options$kind!="all") files <- files[tools::file_path_sans_ext(basename(files))==options$kind]
    if(!length(files)) stop("Unknown figure kind")
    for(f in files) {sys.source(f,globalenv());get(paste0("figure_",tools::file_path_sans_ext(basename(f))))(context)}
  } else if(command=="descriptive") {
    module("reporting/descriptive.R");run_descriptive_analysis(context)
  } else if(command=="metric-slopes") {
    module("models/metric_slopes.R");run_metric_slopes(context)
  } else {
    env <- new_analysis_environment(context);module("models/stance_comparison.R",env)
    env$write_stance_heuristic_comparison(output_dir=file.path(context$OUTPUT_ROOT,"stance-comparison"))
  }
} else stop("Unknown command: ",command)
