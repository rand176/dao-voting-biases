regenerate_tvl_memberships <- function(context) {
suppressWarnings(suppressMessages(library(data.table)))

PACKAGE_ROOT <- context$PACKAGE_ROOT
source(file.path(PACKAGE_ROOT,"src/r/data/model_paths.R"),local=TRUE)
source(file.path(PACKAGE_ROOT,"src/r/preparation/tvl.R"),local=TRUE)
root_override <- Sys.getenv("ROBUSTNESS_PROJECT_ROOT",unset=context$INPUT_ROOT)
root <- normalizePath(root_override,winslash="/",mustWork=TRUE)
input_root <- if (dir.exists(file.path(root,"analysis/metadata"))) file.path(root,"analysis") else root
# Minimal local copy (avoids sourcing the heavy 01_data_load.R just for this).
as_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x) || is.integer(x)) {
    out <- rep(NA_integer_, length(x))
    out[!is.na(x)] <- as.integer(x[!is.na(x)] != 0)
    return(out)
  }
  lower <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(lower))
  out[lower %in% c("1", "true", "t", "yes", "y")] <- 1L
  out[lower %in% c("0", "false", "f", "no", "n", "")] <- 0L
  out
}

norm <- normalize_top_tvl_space
split_spaces <- function(value) {
  if (is.na(value)) return(character())
  parts <- unlist(strsplit(as.character(value), "[|;,]"))
  parts <- norm(parts)
  unique(parts[nzchar(parts)])
}

## 1. Candidate spaces from BOTH summary columns ----------------------------
summ <- fread(
  default_top100_match_summary_path(input_root),
  showProgress = FALSE
)
summ[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]

cand <- rbindlist(lapply(seq_len(nrow(summ)), function(i) {
  spaces <- unique(c(
    split_spaces(summ$matched_spaces[i]),
    split_spaces(summ$reviewed_snapshot_space[i])
  ))
  if (length(spaces) == 0) return(NULL)
  data.table(
    protocol_rank = as.integer(summ$protocol_rank[i]),
    protocol_slug = summ$protocol_slug[i],
    candidate_snapshot_space = spaces
  )
}))

## 2. Protocol metadata (category, median TVL) ------------------------------
prot <- fread(
  default_top_tvl_spaces_path(input_root),
  showProgress = FALSE
)
prot[, protocol_slug := trimws(tolower(as.character(protocol_slug)))]
cand <- merge(
  cand,
  prot[, .(protocol_slug, protocol_name, category, median_ethereum_tvl_usd)],
  by = "protocol_slug", all.x = TRUE, sort = FALSE
)

## 3. Per-space release counts ----------------------------------------------
# Prefer the authoritative modeling-frame counts already cached in the match
# diagnostics (release aggregates are independent of the matching, so these stay
# valid). For spaces that never appeared in diagnostics (the newly added review
# matches), fall back to a direct read of the release input.
diag <- fread(
  default_top_tvl_match_diagnostics_path(input_root),
  showProgress = FALSE
)
diag[, candidate_snapshot_space := norm(candidate_snapshot_space)]
diag_counts <- unique(diag[, .(
  space = candidate_snapshot_space,
  proposals = as.integer(proposals),
  choices = as.integer(choices)
)])
diag_counts <- diag_counts[!is.na(space) & nzchar(space)]
max_int <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_integer_)
  as.integer(max(x))
}
diag_counts <- diag_counts[, .(
  proposals = max_int(proposals),
  choices = max_int(choices)
), by = space]

new_spaces <- setdiff(unique(cand$candidate_snapshot_space), diag_counts$space)
if (length(new_spaces) > 0) {
  rel <- fread(default_input_path(input_root), select = c("space", "proposal"), showProgress = FALSE)
  raw_counts <- current_release_space_counts(rel)
  raw_counts[, space := norm(space)]
  raw_counts <- raw_counts[space %in% new_spaces, .(space, proposals = as.integer(proposals), choices = as.integer(choices))]
  counts <- rbindlist(list(diag_counts, raw_counts), use.names = TRUE, fill = TRUE)
} else {
  counts <- diag_counts
}

## 4. Flagged / deleted status from raw Snapshot spaces ---------------------
sp_meta <- load_snapshot_spaces(default_snapshot_spaces_path(input_root))

cand <- merge(cand, counts, by.x = "candidate_snapshot_space", by.y = "space", all.x = TRUE, sort = FALSE)
cand <- merge(cand, sp_meta, by.x = "candidate_snapshot_space", by.y = "space", all.x = TRUE, sort = FALSE)
cand[is.na(proposals), proposals := NA_integer_]
cand[is.na(choices), choices := NA_integer_]
cand[is.na(raw_snapshot_flagged), raw_snapshot_flagged := FALSE]
cand[is.na(raw_snapshot_deleted), raw_snapshot_deleted := FALSE]

## 5. Eligibility + ranking (mirrors annotate_top_tvl_sample_memberships) ---
cand[, in_analysis_release := !is.na(choices) & choices > 0]
cand[, subset_candidate := in_analysis_release & !raw_snapshot_flagged & !raw_snapshot_deleted]
setorder(cand, protocol_rank, candidate_snapshot_space)

eligible_spaces <- unique(cand[subset_candidate == TRUE, candidate_snapshot_space])
rank_dt <- data.table(
  candidate_snapshot_space = eligible_spaces,
  top_tvl_sample_rank = seq_along(eligible_spaces)
)
cand <- merge(cand, rank_dt, by = "candidate_snapshot_space", all.x = TRUE, sort = FALSE)

sample_sizes <- top_tvl_sample_sizes()  # top25=25, top50=50, top100=100
for (sample_name in names(sample_sizes)) {
  n <- sample_sizes[[sample_name]]
  cand[, (top_tvl_sample_column(sample_name)) :=
    subset_candidate & !is.na(top_tvl_sample_rank) & top_tvl_sample_rank <= n]
}

## 6. Emit one row per (protocol x space) per sample ------------------------
membership <- rbindlist(lapply(names(sample_sizes), function(sample_name) {
  col <- top_tvl_sample_column(sample_name)
  d <- cand[get(col) == TRUE]
  if (nrow(d) == 0) return(NULL)
  setorder(d, top_tvl_sample_rank, protocol_rank, candidate_snapshot_space)
  d[, .(
    sample = sample_name,
    top_tvl_sample_rank,
    candidate_snapshot_space,
    protocol_rank,
    protocol_slug,
    protocol_name,
    category,
    median_ethereum_tvl_usd,
    proposals,
    choices
  )]
}), use.names = TRUE)

out_path <- generated_output_path(default_top_tvl_sample_memberships_path(input_root))
dir.create(dirname(out_path),recursive=TRUE,showWarnings=FALSE)
fwrite(membership, out_path)

## Report -------------------------------------------------------------------
report <- membership[, .(
  spaces = uniqueN(candidate_snapshot_space),
  proposals = sum(unique(data.table(candidate_snapshot_space, proposals))$proposals, na.rm = TRUE),
  choices = sum(unique(data.table(candidate_snapshot_space, choices))$choices, na.rm = TRUE)
), by = sample]
cat("Wrote:", out_path, "\n\n")
print(report)
dropped <- cand[subset_candidate == FALSE, .N]
cat("\nCandidate (protocol x space) rows dropped as ineligible (not in release / flagged / deleted):", dropped, "\n")

 invisible(membership)
}

if (sys.nframe()==0L) {
 script <- sub("^--file=","",grep("^--file=",commandArgs(FALSE),value=TRUE)[[1]])
 PACKAGE_ROOT <- Sys.getenv("DAO_BIAS_ROOT",unset=dirname(dirname(dirname(dirname(normalizePath(script))))))
 regenerate_tvl_memberships(list(PACKAGE_ROOT=PACKAGE_ROOT,INPUT_ROOT=Sys.getenv("DAO_BIAS_DATA_ROOT",unset=PACKAGE_ROOT)))
}
