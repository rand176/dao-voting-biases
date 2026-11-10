source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
test_files <- sort(list.files(
  path = file.path(PACKAGE_ROOT,"tests/r"),
  pattern = "^test_.*\\.R$",
  full.names = TRUE
))

if (!length(test_files)) stop("No R tests found; run from the package root")

Sys.setenv(DAO_BIAS_ROOT=PACKAGE_ROOT)
for (test_file in test_files) {
  message("Running ", test_file)
  output <- tempfile("dao-r-test-output-")
  Sys.setenv(DAO_BIAS_OUTPUT_ROOT=output)
  status <- system2("Rscript", shQuote(test_file))
  unlink(output,recursive=TRUE)
  if (!identical(status, 0L)) {
    quit(status = status)
  }
}

message("R tests passed: ", length(test_files))
