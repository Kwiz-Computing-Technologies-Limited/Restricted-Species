#!/usr/bin/env Rscript
# refresh_iucn_assessment_urls.R
# Surgically replace dead IUCN /species/<taxon_id> URLs with the working
# /species/<taxon_id>/<assessment_id> form.
#
# Why a separate script? The existing src/upgrade_iucn_urls.R produces the
# single-ID URL form, which IUCN's site does not honour for many taxa
# (returns 404). This script does the opposite direction — it reads the
# taxon ID straight out of the existing dead URL, calls the IUCN public
# API for the current assessment ID, and rewrites the URL in place.
#
# Surgical guarantee: only the URL substring inside each cell is changed.
# Every other character of every cell, every other column, and every other
# row is left exactly as written. Manual edits to the CSVs are preserved.
#
# Usage:
#   IUCN_TOKEN=<your-token> Rscript src/refresh_iucn_assessment_urls.R
#
#   (Optionally limit which file is processed:)
#   IUCN_TOKEN=<your-token> Rscript src/refresh_iucn_assessment_urls.R \
#       output/tables/kenya_regulated_plants_gbif.csv
#
# Free IUCN API token (30 seconds to register):
#   https://apiv3.iucnredlist.org/api/v3/token
#
# Idempotent: rerunning is a no-op for any URL that already carries the
# /species/<taxon>/<assessment> form. URLs whose assessment lookup fails
# are left untouched (no data loss; no false rewrites).

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
})

token <- Sys.getenv("IUCN_TOKEN", unset = NA_character_)
if (is.na(token) || nchar(token) == 0) {
  stop("IUCN_TOKEN env var not set.\n",
       "  Register a free token at https://apiv3.iucnredlist.org/api/v3/token\n",
       "  Then run:  IUCN_TOKEN=<your-token> Rscript src/refresh_iucn_assessment_urls.R")
}

# ── Targets ───────────────────────────────────────────────────────────────
# Each entry: (file path, name of the column holding URLs).
DEFAULT_TARGETS <- list(
  list(path = "output/tables/kenya_regulated_plants_gbif.csv", col = "Note"),
  list(path = "data/raw/species_raw.csv",                       col = "notes")
)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) > 0) {
  targets <- map(cli_args, function(p) {
    col <- if (basename(p) == "species_raw.csv") "notes" else "Note"
    list(path = p, col = col)
  })
} else {
  targets <- DEFAULT_TARGETS
}

# ── Regex for the dead URL form ───────────────────────────────────────────
# Matches https://www.iucnredlist.org/species/<digits> when NOT followed by
# /<digit> (i.e. the working two-ID form is excluded).
DEAD_PAT <- "https://www\\.iucnredlist\\.org/species/([0-9]+)(?!/[0-9])"

# ── IUCN API: taxon_id -> assessment_id ───────────────────────────────────
UA <- user_agent("KenyaRestrictedSpecies/1.0 refresh-iucn-assessment-urls")
asmt_cache <- new.env(parent = emptyenv())

fetch_assessment_id <- function(taxon_id) {
  if (exists(taxon_id, envir = asmt_cache, inherits = FALSE)) {
    return(get(taxon_id, envir = asmt_cache))
  }
  url <- sprintf("https://apiv3.iucnredlist.org/api/v3/species/id/%s?token=%s",
                 taxon_id, token)
  resp <- tryCatch(
    RETRY("GET", url, UA, times = 3, pause_base = 2, terminate_on = c(401, 403)),
    error = function(e) NULL
  )
  asmt <- NA_character_
  if (!is.null(resp) && status_code(resp) == 200) {
    payload <- tryCatch(
      fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE),
      error = function(e) NULL
    )
    res <- payload$result
    if (!is.null(res) && length(res) > 0 && !is.null(res$assessment_id)) {
      asmt <- as.character(res$assessment_id[1])
    }
  }
  assign(taxon_id, asmt, envir = asmt_cache)
  asmt
}

# ── Replace dead URLs inside one cell, leaving the rest of the cell alone ──
fix_cell <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(text)
  matches <- regmatches(text, gregexpr(DEAD_PAT, text, perl = TRUE))[[1]]
  if (length(matches) == 0) return(text)

  taxa <- sub(DEAD_PAT, "\\1", matches, perl = TRUE)
  out <- text
  for (i in seq_along(matches)) {
    Sys.sleep(0.6)  # polite to IUCN API (well under 10k/day budget)
    asmt <- fetch_assessment_id(taxa[i])
    if (is.na(asmt)) next  # leave dead URL as-is; no data loss
    new_url <- sprintf("https://www.iucnredlist.org/species/%s/%s", taxa[i], asmt)
    # sub() always replaces the first remaining occurrence; matched URLs
    # post-replacement no longer fit DEAD_PAT (negative lookahead fails),
    # so each iteration safely targets the next still-dead URL.
    out <- sub(DEAD_PAT, new_url, out, perl = TRUE)
  }
  out
}

# ── Process each target ────────────────────────────────────────────────────
for (t in targets) {
  if (!file.exists(t$path)) {
    message(sprintf("[skip] %s — file not found", t$path)); next
  }
  d <- read_csv(t$path, col_types = cols(.default = col_character()))
  if (!t$col %in% names(d)) {
    message(sprintf("[skip] %s — column '%s' not present", t$path, t$col)); next
  }
  before <- sum(grepl(DEAD_PAT, d[[t$col]] %||% "", perl = TRUE))
  if (before == 0) {
    message(sprintf("[skip] %s — no dead URLs in '%s'", t$path, t$col)); next
  }
  message(sprintf("[run]  %s — %d dead URL(s) in '%s'", t$path, before, t$col))
  d[[t$col]] <- vapply(d[[t$col]], fix_cell, character(1))
  after <- sum(grepl(DEAD_PAT, d[[t$col]] %||% "", perl = TRUE))
  write_csv(d, t$path, na = "")
  message(sprintf("[done] %s — dead URLs: %d -> %d (refreshed %d)",
                  t$path, before, after, before - after))
}

message(sprintf("\n[refresh] %d unique taxon IDs queried; cached for the run.",
                length(ls(asmt_cache))))
