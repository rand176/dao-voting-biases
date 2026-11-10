PACKAGE_ROOT <- normalizePath(Sys.getenv("DAO_BIAS_ROOT",unset="."),winslash="/",mustWork=TRUE)
source(file.path(PACKAGE_ROOT,"src/r/runtime/context.R"))
source(file.path(PACKAGE_ROOT,"src/r/runtime/init.R"))
initialize_test_analysis <- function(envir=parent.frame()) {
  context <- analysis_context(PACKAGE_ROOT,output_root=Sys.getenv("DAO_BIAS_OUTPUT_ROOT",unset=tempfile("dao-test-output-")))
  for (n in intersect(names(context),ls(envir,all.names=TRUE))) context[[n]] <- get(n,envir=envir)
  initialize_analysis(context,envir)
}
