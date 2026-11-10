#!/usr/bin/env Rscript
# Replication-package extraction of the manuscript's author-position figure.
# Source reconstruction and tie rules: analyze.R; accepted-count selection and
# summary: report.py; histogram style: render_main.R in the source diagnostic.
# No diagnostic reports, raw data, or saved timing datasets are bundled.

suppressPackageStartupMessages(library(data.table))

author_timing_reconstruct <- function(input_root, package_root, workers = 4L) {
  guards <- new.env(parent = globalenv())
  sys.source(file.path(package_root, "src/r/preparation/choice_helpers.R"), guards)
  setDTthreads(workers)
  release <- file.path(input_root,
    "data/2024/derived/active/df_choices_28_stance_normalized_no_author_vp_llama33.csv")
  choices <- fread(release, select=c("proposal","choice_idx","N_choices","flagged","is_author_choice","tot_votes_2","author_pos","author_rank"), colClasses=c(proposal="character",author_rank="character"))
  choices[, proposal := trimws(tolower(proposal))]
  # Direct selected-column read avoids loader provenance/registry side effects.
  # Use the actual shared guard rather than running any pipeline notebook.
  choices <- as.data.table(guards$dropSingleOptionProposals(as.data.frame(choices)))
  props <- fread(file.path(input_root, "data/2024/raw/proposals_0.csv.gz"), select=c("id","author","type","choices","created","start","end","votes","privacy","scores_state","flagged"), colClasses=c(id="character",author="character"))
  props[, `:=`(id=trimws(tolower(id)), author=trimws(tolower(author)))]
  stopifnot(!anyDuplicated(props$id))
  flagged_ids <- props[toupper(trimws(as.character(flagged))) %in% c("1","TRUE","T","YES","Y"), id]
  choices <- choices[!proposal %in% flagged_ids]
  sample <- choices[, .(release_author_voted=any(is_author_choice == 1,na.rm=TRUE), release_n=first(tot_votes_2), release_pos=first(author_pos)), by=proposal]
  paper_counts <- fread(file.path(input_root, "metadata/analysis_data_provenance/latex_counts_current.csv"))
  stopifnot(!any(choices$proposal %in% flagged_ids),
            nrow(choices)==as.numeric(paper_counts[macro=="DaoBiasFinalChoiceN",value]),
            nrow(sample)==as.numeric(paper_counts[macro=="DaoBiasFinalProposalN",value]))
  # Include all guarded proposals, so raw author participation is independently verified.
  props <- props[id %in% sample$proposal]
  issues <- fread(file.path(input_root, "data/2024/clean_proposal_flags/proposal_vote_issue_flags.csv.gz"), colClasses=c(id="character"))
  issues[, id := tolower(id)]
  files <- sort(list.files(file.path(input_root, "data/2024/raw/votes"),pattern="[.]csv[.]gz$",full.names=TRUE))
  if (!length(files)) stop("No raw vote shards found under input-root/data/2024/raw/votes")
  chunks <- parallel::mclapply(seq_along(files), function(i) {
    setDTthreads(1)
    d <- fread(files[i], select=c("id","proposal","voter","created","choice","vp","vp_state"), colClasses=c(id="character",proposal="character",voter="character",choice="character"), showProgress=FALSE)
    d[, `:=`(proposal=trimws(tolower(proposal)),voter=trimws(tolower(voter)))]
    d <- d[proposal %in% props$id]
    message(sprintf("Shard %d/%d: kept %d ballots", i, length(files), nrow(d)))
    d
  }, mc.cores=workers)
  stopifnot(all(vapply(chunks,is.list,logical(1))))
  v <- rbindlist(chunks)
  rm(chunks); gc()
  v <- merge(v,props[, .(proposal=id,author,type,privacy,start,end,proposal_created=created)],by="proposal",sort=FALSE)
  v[, is_author := !is.na(voter) & voter == author]
  quality <- v[, .(n=.N, unique_voters=uniqueN(voter), duplicate_vote_ids=.N-uniqueN(id), author_ballots=sum(is_author),
                   missing_time=sum(!is.finite(created)), noninteger_time=sum(is.finite(created)&created!=floor(created)),
                   invalid_vp=sum(!is.finite(vp)|vp<0), missing_voter=sum(is.na(voter)|voter==""),
                   outside_window=sum(is.finite(created)&(created<start|created>end)),
                   not_final_vp=sum(is.na(vp_state)|vp_state!="final")),by=proposal]
  quality <- merge(quality,sample,by="proposal",all.y=TRUE)
  quality[, issue_flag := proposal %in% issues[flag_any_vote_issues==1,id]]
  quality[, eligible := !is.na(n) & n==unique_voters & duplicate_vote_ids==0 & missing_time==0 & noninteger_time==0 & invalid_vp==0 & missing_voter==0 & !issue_flag]
  v <- v[proposal %in% quality[eligible==TRUE & author_ballots==1,proposal]]
  v[, author_time := created[is_author][1],by=proposal]
  timing <- v[, {
    other <- !is_author; earlier <- other & created<author_time[1]; tied <- other & created==author_time[1]
    n <- .N; k <- sum(earlier); t <- sum(tied); den <- sum(vp[other])
    list(n_voters=n,author_time=author_time[1],prior_voters=k,tied_voters=t,rank_lower=k+1L,rank_upper=k+t+1L,
         percentile_lower=100*(k+1)/n,percentile_upper=100*(k+t+1)/n,
         prior_other_pct=if(n>1)100*k/(n-1) else NA_real_,prior_other_pct_upper=if(n>1)100*(k+t)/(n-1) else NA_real_,
         prior_vp_pct=if(den>0)100*sum(vp[earlier])/den else NA_real_,prior_vp_pct_upper=if(den>0)100*sum(vp[earlier|tied])/den else NA_real_,
         nonauthor_vp=den,type=type[1],privacy=privacy[1])
  },by=proposal]
  timing <- merge(timing,sample,by="proposal")

  timing
}

author_timing_select <- function(timing) {
  required <- c("proposal", "n_voters", "prior_voters", "tied_voters", "rank_lower",
    "rank_upper", "percentile_lower", "percentile_upper", "prior_other_pct",
    "prior_vp_pct", "release_author_voted", "release_n", "release_pos")
  missing <- setdiff(required, names(timing))
  if (length(missing)) stop("Missing timing columns: ", paste(missing, collapse = ", "))
  if (anyDuplicated(timing$proposal)) stop("Duplicate proposal identifiers in timing input")
  # Same finite-number predicate as report.py math.isclose(rel_tol=1e-10, abs_tol=1e-6).
  matches <- is.finite(timing$n_voters) & is.finite(timing$release_n) &
    abs(timing$n_voters - timing$release_n) <=
      pmax(1e-10 * pmax(abs(timing$n_voters), abs(timing$release_n)), 1e-6)
  keep <- !is.na(timing$release_author_voted) &
    as.character(timing$release_author_voted) == "TRUE" & matches
  selected <- timing[which(keep), ]
  if (!nrow(selected)) stop("No proposals pass author-voted and accepted-ballot-count selection")
  stopifnot(all(selected$rank_lower >= 1),
    all(selected$rank_lower <= selected$rank_upper),
    all(selected$rank_upper <= selected$n_voters),
    all(selected$rank_lower <= selected$release_pos & selected$release_pos <= selected$rank_upper),
    all(abs(selected$percentile_lower - 100 * selected$rank_lower / selected$n_voters) < 1e-10))
  selected
}

author_timing_summary <- function(timing) {
  rbindlist(lapply(c(1, 2, 5, 10, 20), function(threshold) {
    d <- timing[n_voters >= threshold]
    others <- d[n_voters > 1]
    vp <- d$prior_vp_pct[!is.na(d$prior_vp_pct)]
    data.table(min_voters=threshold, proposals=nrow(d),
      first_pct=100*mean(d$prior_voters==0),
      first_unique_pct=100*mean(d$prior_voters==0 & d$tied_voters==0),
      first10_pct=100*mean(d$percentile_lower<=10),
      first10_conservative_pct=100*mean(d$percentile_upper<=10),
      before25_pct=100*mean(others$prior_other_pct<25),
      before50_pct=100*mean(others$prior_other_pct<50),
      other_share_defined=nrow(others),
      median_percentile=median(d$percentile_lower), mean_percentile=mean(d$percentile_lower),
      median_prior_vp_pct=median(vp), mean_prior_vp_pct=mean(vp),
      vp_share_defined=length(vp), tied_author_proposals=sum(d$tied_voters>0))
  }))
}

author_timing_render <- function(timing, output_dir) {
  d <- timing[n_voters >= 10]
  if (!nrow(d)) stop("No retained proposals have at least 10 voters")
  draw <- function() {
    par(mar=c(5.5,4.5,1,1))
    hist(d$percentile_lower, breaks=seq(0,100,5), col="#3b6b88", border="white",
      main="", xlab="Author position in voting sequence (percentile)",
      ylab="Number of proposals", sub=sprintf("At least 10 voters; N = %s",
        format(nrow(d), big.mark=",")))
  }
  pdf(file.path(output_dir, "author_position_distribution_main.pdf"), width=7, height=4.4)
  tryCatch(draw(), finally=dev.off())
  png(file.path(output_dir, "author_position_distribution_main.png"), width=1400, height=880, res=200)
  tryCatch(draw(), finally=dev.off())
}

author_timing_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  usage <- paste(
    "Usage: python run.py author-timing [--input-root PATH] [--output-dir NEW_PATH]",
    "       [--workers N] [--timing-file EXISTING_RECONSTRUCTED_TIMING_CSV]",
    "Without --timing-file, reconstructs from the omitted Snapshot inputs and paper counts.",
    "With --timing-file, explicitly replays a supplied reconstructed timing table;",
    "raw sample guards cannot be rechecked from that table alone.",
    "Output directory must not exist. No automatic fallback to cached or public data.", sep="\n")
  if ("--help" %in% args) { cat(usage, "\n"); return(invisible(NULL)) }
  file_arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
  if (!length(file_arg)) stop("Run this entrypoint with Rscript")
  package_root <- PACKAGE_ROOT
  options <- list("--input-root"=package_root,
    "--output-dir"=file.path(package_root,"outputs","author_timing"), "--workers"="4")
  if (length(args) %% 2L) stop(usage)
  if (length(args)) for (i in seq.int(1L, length(args), by=2L)) {
    key <- args[[i]]
    if (!key %in% c(names(options), "--timing-file")) stop("Unknown option: ", key, "\n", usage)
    options[[key]] <- args[[i+1L]]
  }
  workers <- suppressWarnings(as.integer(options[["--workers"]]))
  if (is.na(workers) || workers < 1L) stop("--workers must be a positive integer")
  out <- options[["--output-dir"]]
  if (file.exists(out) || dir.exists(out)) stop("Output directory already exists: ", out)
  timing <- if (is.null(options[["--timing-file"]])) {
    author_timing_reconstruct(normalizePath(options[["--input-root"]], mustWork=TRUE),
      package_root, workers)
  } else {
    fread(options[["--timing-file"]], colClasses=c(proposal="character"))
  }
  main <- author_timing_select(timing)
  if (!any(main$n_voters >= 10)) stop("No retained proposals have at least 10 voters")
  summary <- author_timing_summary(main)
  if (!dir.create(out, recursive=TRUE)) stop("Cannot create output directory: ", out)
  fwrite(main, file.path(out,"main_proposal_timing.csv"))
  fwrite(summary, file.path(out,"main_timing_summary.csv"))
  author_timing_render(main, out)
  message("Wrote selected timing data, summary, and manuscript PDF/PNG to ", out)
  invisible(NULL)
}
