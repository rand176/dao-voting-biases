source(file.path(Sys.getenv("DAO_BIAS_ROOT", unset="."),"tests/r/helpers.R"))
# CLI boundary regression: reconstructing the paper sample from synthetic ballots.
suppressPackageStartupMessages(library(data.table))
project_root <- normalizePath(".", winslash="/", mustWork=TRUE)
workspace <- tempfile("author-timing-test-")
dir.create(workspace)
input_root <- file.path(workspace,"inputs")
dir.create(input_root)
write_input <- function(relative, value) {
  path <- file.path(input_root,relative)
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  # Base R gzip writing avoids adding a compression-package dependency.
  connection <- if (grepl("[.]gz$",path)) gzfile(path,"wt") else file(path,"wt")
  write.csv(value,connection,row.names=FALSE,na="NA")
  close(connection)
}
proposals <- c("first","tied","mismatch","flagged","single","issue",
  "duplicate","noauthor","badtime","release_noauthor")
raw <- rbindlist(lapply(proposals,function(p) {
  n <- if (p=="first") 20L else 10L
  times <- seq_len(n)+1000
  times[1] <- if (p=="tied") 1002 else 1000
  if (p=="badtime") times[2] <- NA_real_
  voters <- paste0(p,"-",seq_len(n))
  voters[1] <- if (p=="noauthor") "someone-else" else paste0(p,"-author")
  if (p=="duplicate") voters[3] <- voters[2]
  data.table(id=paste0(p,"-vote-",seq_len(n)),proposal=toupper(p),
    voter=toupper(voters),created=times,choice="1",vp=1,vp_state="final")
}))
# Give the tied author one strictly earlier ballot and one same-second ballot.
raw[proposal=="TIED" & id=="tied-vote-2",created:=1001]
raw[proposal=="TIED" & id=="tied-vote-3",created:=1002]
props <- data.table(id=toupper(proposals),author=toupper(paste0(proposals,"-author")),
  type="single-choice",choices='["Yes","No"]',created=900,start=900,end=2000,
  votes=10,privacy="",scores_state="final",flagged=proposals=="flagged")
choices <- rbindlist(lapply(proposals,function(p) {
  n <- if (p=="single") 1L else 2L
  data.table(proposal=toupper(p),choice_idx=seq_len(n),N_choices=n,flagged=FALSE,
    is_author_choice=if (p %in% c("noauthor","release_noauthor")) 0L else as.integer(seq_len(n)==1),
    tot_votes_2=if (p=="first") 20+5e-7 else if (p=="mismatch") 9 else 10,
    author_pos=if (p=="tied") 3 else 1,author_rank="0.1")
}))
write_input("data/2024/derived/active/df_choices_28_stance_normalized_no_author_vp_llama33.csv",choices)
write_input("data/2024/raw/proposals_0.csv.gz",props)
write_input("data/2024/raw/votes/shard.csv.gz",raw)
write_input("data/2024/clean_proposal_flags/proposal_vote_issue_flags.csv.gz",
  data.table(id="ISSUE",flag_any_vote_issues=1))
# Single-choice-count and Snapshot-flag exclusion leave exactly eight proposals.
write_input("metadata/analysis_data_provenance/latex_counts_current.csv",
  data.table(macro=c("DaoBiasFinalChoiceN","DaoBiasFinalProposalN"),value=c(16,8)))
inputs <- list.files(input_root,recursive=TRUE,full.names=TRUE)
before <- tools::md5sum(inputs)
script <- file.path(project_root, "src/r/models/author_timing.R")
run <- function(args) {
  logs <- file.path(workspace,"run.log")
  status <- system2("python",c(shQuote(file.path(PACKAGE_ROOT,"run.py")),"author-timing",vapply(args,shQuote,character(1))),
    stdout=logs,stderr=logs)
  list(status=status,log=paste(readLines(logs,warn=FALSE),collapse="\n"))
}
missing_root <- file.path(workspace,"missing-inputs");dir.create(missing_root)
missing_out <- file.path(workspace,"missing-output")
missing <- run(c("--input-root",missing_root,"--output-dir",missing_out))
stopifnot(missing$status!=0,!dir.exists(missing_out))
missing_replay <- run(c("--timing-file",file.path(workspace,"missing-timing.csv"),"--output-dir",missing_out))
stopifnot(missing_replay$status!=0,!dir.exists(missing_out))

out <- file.path(workspace,"result")
args <- c("--input-root",input_root,"--output-dir",out,"--workers","1")
result <- run(args)
if (result$status!=0) stop(result$log)
selected <- fread(file.path(out,"main_proposal_timing.csv"))
stopifnot(identical(selected$proposal,c("first","tied")),
  identical(selected$prior_voters,c(0L,1L)),identical(selected$tied_voters,c(0L,1L)),
  identical(selected$rank_lower,c(1L,2L)),identical(selected$rank_upper,c(1L,3L)),
  isTRUE(all.equal(selected$percentile_lower,c(5,20))),
  isTRUE(all.equal(selected$percentile_upper,c(5,30))))
summary <- fread(file.path(out,"main_timing_summary.csv"))
stopifnot(summary[min_voters==10,proposals]==2,
  summary[min_voters==10,first_pct]==50,
  summary[min_voters==10,first10_pct]==50,
  summary[min_voters==10,first_unique_pct]==50,
  summary[min_voters==10,median_percentile]==12.5,
  summary[min_voters==20,proposals]==1)
pdf <- file.path(out,"author_position_distribution_main.pdf")
png <- file.path(out,"author_position_distribution_main.png")
stopifnot(rawToChar(readBin(pdf,"raw",4))=="%PDF",
  identical(readBin(png,"raw",8),as.raw(c(137,80,78,71,13,10,26,10))),
  identical(unname(tools::md5sum(inputs)),unname(before)))
# The CLI must refuse reuse of an output directory before writing anything.
outputs <- list.files(out,full.names=TRUE)
saved <- tools::md5sum(outputs)
result <- run(args)
stopifnot(result$status!=0,grepl("already exists",result$log),
  identical(unname(tools::md5sum(outputs)),unname(saved)))
# Cached rendering is explicit; a retained rank outside the tie interval fails.
invalid <- copy(selected);invalid[proposal=="tied",release_pos:=4]
invalid_file <- file.path(workspace,"invalid.csv");fwrite(invalid,invalid_file)
failed_out <- file.path(workspace,"invalid-output")
result <- run(c("--timing-file",invalid_file,"--output-dir",failed_out))
stopifnot(result$status!=0,!dir.exists(failed_out))
unlink(workspace,recursive=TRUE)
message("Author timing CLI sample, ties, accepted counts, output protection, and source-preservation checks passed")
