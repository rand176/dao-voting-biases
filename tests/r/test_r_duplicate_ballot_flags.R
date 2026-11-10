source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
PROPOSAL_FLAGS_DIR <- tempdir()
initialize_test_analysis()

flags <- data.frame(
  id = c("prop-1", "prop-2"),
  flag_duplicate_ballots = c(0, 1),
  duplicate_pairs = c(0, 2),
  duplicate_ballot_rows = c(0, 5),
  duplicate_wallets = c(0, 2),
  flag_category = c("not_flagged", "duplicate_ballots"),
  flag_reason = c("No duplicate ballot signal", "Contains repeated proposal+voter ballot histories in raw Snapshot votes"),
  stringsAsFactors = FALSE
)

write.csv(
  flags,
  gzfile(file.path(PROPOSAL_FLAGS_DIR, "proposal_duplicate_ballot_flags.csv.gz")),
  row.names = FALSE
)

loaded <- loadProposalDuplicateBallotFlags()
stopifnot(nrow(loaded) == 2)
stopifnot(identical(getDuplicateBallotProposalIds(), "prop-2"))

df <- data.frame(
  proposal = c("prop-1", "prop-2", "prop-3"),
  value = c(10, 20, 30),
  stringsAsFactors = FALSE
)

filtered <- applyDuplicateBallotExclusions(df)
stopifnot(identical(filtered$proposal, c("prop-1", "prop-3")))
