source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
# Runtime path regressions: no private data and no writes outside the fixture.
suppressPackageStartupMessages(library(data.table))
test_publication_paths <- function() {
  package <- normalizePath(".",winslash="/",mustWork=TRUE)
  workspace <- tempfile("publication-paths-");dir.create(workspace)
  on.exit(unlink(workspace,recursive=TRUE),add=TRUE)
  previous_wd <- getwd();on.exit(setwd(previous_wd),add=TRUE)
  keys <- unique(c(grep("^(ROBUSTNESS_|DAO_BIAS_)",names(Sys.getenv()),value=TRUE),
    "DAO_BIAS_ROOT","DAO_BIAS_DATA_ROOT","DAO_BIAS_OUTPUT_ROOT",
    "ROBUSTNESS_PROJECT_ROOT","ROBUSTNESS_TOP_TVL_SAMPLE_MEMBERSHIPS_FILE",
    "DAO_BIAS_FIGURE_ROOT","SAVEIMG"))
  previous <- Sys.getenv(keys,unset=NA_character_)
  on.exit({
    Sys.unsetenv(keys)
    present <- !is.na(previous)
    if (any(present)) do.call(Sys.setenv,as.list(previous[present]))
  },add=TRUE)
  Sys.unsetenv(keys)
  installed <- file.path(workspace,"installed");dir.create(installed)
  files <- c("src/r/preparation/tvl_memberships.R",
    "src/r/data/model_paths.R","src/r/preparation/tvl.R")
  for (name in files) {
    target <- file.path(installed,name)
    dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
    file.copy(file.path(package,name),target)
  }
  # Prevent a regressed entrypoint from sourcing any code outside the test fixture.
  script <- file.path(installed,files[[1]])
  guard <- c(
    ".real_source <- base::source",
    "source <- function(file, local=FALSE, ...) {",
    paste0("  allowed <- ",encodeString(paste0(normalizePath(workspace),"/"),quote='"')),
    "  resolved <- normalizePath(file, winslash='/', mustWork=FALSE)",
    "  if (!startsWith(resolved,allowed)) stop('Unexpected external code path: ',resolved)",
    "  .real_source(file, local=if(identical(local,TRUE)) parent.frame() else local, ...)","}")
  writeLines(c(guard,readLines(script,warn=FALSE)),script)
  fixture <- function(root,space) {
    meta <- file.path(root,"metadata/analysis_data_provenance")
    dir.create(meta,recursive=TRUE)
    fwrite(data.table(protocol_rank=1L,protocol_slug="alpha",
      matched_spaces=space,reviewed_snapshot_space=paste0(space,"-review")),
      file.path(meta,"defillama_top100_match_summary.csv"))
    fwrite(data.table(protocol_slug="alpha",protocol_name="Alpha",category="DEX",
      median_ethereum_tvl_usd=100),file.path(meta,"defillama_protocol_ethereum_tvl_top100.csv"))
    fwrite(data.table(candidate_snapshot_space=space,proposals=2L,choices=4L),
      file.path(meta,"defillama_top_tvl_snapshot_match_diagnostics.csv"))
    dir.create(file.path(root,"data/2024/raw"),recursive=TRUE)
    con <- gzfile(file.path(root,"data/2024/raw/spaces_0.csv.gz"),"wt")
    write.csv(data.frame(id=c(space,paste0(space,"-review")),flagged=0,deleted=0),con,row.names=FALSE)
    close(con)
    fwrite(data.table(space=paste0(space,"-review"),proposal="p-review"),file.path(root,"release.csv"))
    fwrite(data.table(release_name="current",relative_path="release.csv"),
      file.path(root,"metadata/analysis_data_releases.csv"))
    file.path(meta,"defillama_top_tvl_snapshot_sample_memberships.csv")
  }
  default_output <- fixture(installed,"default.eth")
  elsewhere <- file.path(workspace,"elsewhere");dir.create(elsewhere);setwd(elsewhere)
  run <- function() {
    log <- file.path(workspace,"regeneration.log")
    status <- system2("Rscript",shQuote(script),stdout=log,stderr=log)
    if (status!=0) stop(paste(readLines(log,warn=FALSE),collapse="\n"))
  }
  run()
  result <- fread(default_output)
  stopifnot(nrow(result)==6L,
    setequal(result$candidate_snapshot_space,c("default.eth","default.eth-review")),
    all(result[candidate_snapshot_space=="default.eth-review",proposals]==1L),
    all(result[candidate_snapshot_space=="default.eth-review",choices]==1L))
  default_hash <- tools::md5sum(default_output)
  override_root <- file.path(workspace,"explicit-inputs")
  unused_default <- fixture(override_root,"override.eth")
  custom_output <- file.path(workspace,"custom-output.csv")
  Sys.setenv(ROBUSTNESS_PROJECT_ROOT=override_root,
    ROBUSTNESS_TOP_TVL_SAMPLE_MEMBERSHIPS_FILE=custom_output)
  run();result <- fread(custom_output)
  stopifnot(setequal(result$candidate_snapshot_space,c("override.eth","override.eth-review")),
    !file.exists(unused_default),identical(tools::md5sum(default_output),default_hash))
  Sys.unsetenv(c("ROBUSTNESS_PROJECT_ROOT","ROBUSTNESS_TOP_TVL_SAMPLE_MEMBERSHIPS_FILE"))

  source(file.path(package,"src/r/reporting/setup.R"))
  plot_package <- file.path(workspace,"plot-package")
  context <- list(PACKAGE_ROOT=plot_package,OUTPUT_ROOT=file.path(plot_package,"outputs"))
  for(override in c("",file.path(workspace,"explicit-figures"))) {
    if(nzchar(override)) Sys.setenv(DAO_BIAS_FIGURE_ROOT=override) else Sys.unsetenv("DAO_BIAS_FIGURE_ROOT")
    out <- figure_output_directory(context,"current","fixture.csv")
    expected <- file.path(if(nzchar(override)) override else file.path(context$OUTPUT_ROOT,"figures"),"current__fixture")
    stopifnot(dir.exists(expected),identical(out,paste0(expected,"/")))
    grDevices::pdf(file.path(out,"probe.pdf"));plot(1:2);grDevices::dev.off()
    stopifnot(file.exists(file.path(expected,"probe.pdf")))
  }
  # Generated archive paths agree with Python; supplied archives remain readable.
  archive_input <- file.path(workspace,"archive-inputs");dir.create(archive_input)
  archive_output <- file.path(workspace,"archive-output")
  archive_context <- analysis_context(package,archive_input,archive_output)
  archive_env <- list2env(archive_context,parent=globalenv())
  sys.source(file.path(package,"src/r/data/paths.R"),archive_env)
  supplied <- file.path(archive_input,"data/2024/derived/archive_large/choices.csv")
  dir.create(dirname(supplied),recursive=TRUE)
  writeLines("proposal,choice_idx",supplied)
  generated <- archive_env$archiveLargeDataPath("choices.csv")
  stopifnot(identical(generated,file.path(archive_output,"data/2024/derived/archive_large/choices.csv")))
  stopifnot(identical(archive_env$requireExistingInputPath(generated),supplied))
  dir.create(dirname(generated),recursive=TRUE)
  writeLines("proposal,choice_idx",generated)
  stopifnot(identical(archive_env$requireExistingInputPath(generated),generated))

  message("Publication roots, unrelated cwd, input fallback, and explicit path overrides passed")
}
test_publication_paths()
