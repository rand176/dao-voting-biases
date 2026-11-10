source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
library(stringr)

source("src/r/preparation/stance_patterns.R")

classify_choice <- function(choice_text) {
  classifyStanceChoiceInstrumented(data.frame(
    choice_text = choice_text,
    body = "Governance proposal body",
    stringsAsFactors = FALSE
  ))
}

expect_stance <- function(choice_text, expected_stance, expected_source = NULL) {
  match <- classify_choice(choice_text)
  stopifnot(identical(match$resolved_stance, expected_stance))
  if (!is.null(expected_source)) {
    stopifnot(identical(match$match_source, expected_source))
  }
  invisible(match)
}

expect_stance(" s ", "APPROVE", "approve_exact")
expect_stance(" y ", "APPROVE", "approve_exact")
expect_stance("Adam S", "OTHER", "none")
expect_stance("Jeep Wrangler Unlimited S", "OTHER", "none")
expect_stance("Option S", "OTHER", "none")

expect_stance("No change", "REJECT")
expect_stance("do not support", "REJECT")
expect_stance("We do not support X", "REJECT", "reject_contains")
expect_stance("I don't approve", "REJECT", "reject_exact")
expect_stance("We do not approve X", "REJECT", "reject_contains")
expect_stance("dissent", "REJECT", "reject_exact")
expect_stance("We dissent here", "REJECT", "reject_contains")
expect_stance("disapporve", "REJECT", "reject_exact")
expect_stance("dissaprove", "REJECT", "reject_exact")
expect_stance("againist", "REJECT", "reject_exact")
expect_stance("aganist", "REJECT", "reject_exact")

support_match <- expect_stance("support", "APPROVE", "approve_exact")
stopifnot(!support_match$has_conflicting_matches)

customer_support_match <- expect_stance(
  "customer support",
  "OTHER",
  "approve_low_confidence_contains"
)
stopifnot(grepl(
  "approve_low_confidence_contains",
  customer_support_match$all_match_sources,
  fixed = TRUE
))

expect_stance("支持", "APPROVE", "approve_exact")
expect_stance("不支持", "REJECT", "reject_exact")
expect_stance("会", "APPROVE", "approve_exact")
expect_stance("不会", "REJECT", "reject_exact")
expect_stance("可以", "APPROVE", "approve_exact")
expect_stance("不可以", "REJECT", "reject_exact")
expect_stance("有", "APPROVE", "approve_exact")
expect_stance("这个提案有问题", "OTHER", "none")
