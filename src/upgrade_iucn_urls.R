#!/usr/bin/env Rscript
# upgrade_iucn_urls.R
# Replace IUCN Red List search URLs with direct species-page URLs in the
# format /species/<taxon_id>/<assessment_id>. Single-ID URLs (e.g.
# /species/48154088) 404 for many taxa, so both IDs are required for a
# stable link.
#
# Lookup strategy:
#   1. Fetch IUCN taxon ID (P627) from Wikidata SPARQL.
#   2. Fetch the *current* assessment ID from the IUCN public API
#      (apiv3.iucnredlist.org). Requires a free token registered at
#      https://apiv3.iucnredlist.org/api/v3/token.
#
# Usage:
#   IUCN_TOKEN=<your-token> Rscript src/upgrade_iucn_urls.R
#
# Re-runnable: rows that already carry a /species/<taxon>/<assessment> URL
# are skipped.
#
# Touches:
#   data/raw/species_raw.csv
#   output/tables/kenya_regulated_plants_gbif.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
})

token <- Sys.getenv("IUCN_TOKEN", unset = NA_character_)
if (is.na(token) || nchar(token) == 0) {
  stop("Set IUCN_TOKEN to your IUCN Red List API token.\n",
       "Register a free token at: https://apiv3.iucnredlist.org/api/v3/token")
}

raw_csv  <- "data/raw/species_raw.csv"
gbif_csv <- "output/tables/kenya_regulated_plants_gbif.csv"
stopifnot(file.exists(raw_csv), file.exists(gbif_csv))

UA <- user_agent("KenyaRestrictedSpecies/1.0 (https://github.com/Kwiz-Computing-Technologies-Limited/restricted-species)")

# ── 1. Wikidata: binomial -> IUCN taxon ID ───────────────────────────────
lookup_taxon_ids <- function(binomials, batch_size = 25) {
  results <- tibble(binomial = character(), iucn_taxon_id = character())
  for (chunk in split(binomials, ceiling(seq_along(binomials) / batch_size))) {
    values <- paste0('"', chunk, '"', collapse = " ")
    query <- sprintf('
      SELECT ?binomial ?iucnId WHERE {
        VALUES ?binomial { %s }
        ?item wdt:P225 ?binomial ;
              wdt:P627 ?iucnId .
      }', values)

    resp <- RETRY("POST",
      url = "https://query.wikidata.org/sparql",
      body = list(query = query, format = "json"),
      encode = "form",
      add_headers(Accept = "application/sparql-results+json"),
      UA, times = 3, pause_base = 2)
    if (status_code(resp) != 200) next

    payload <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
    rows <- payload$results$bindings
    if (is.null(rows) || length(rows) == 0) next
    results <- bind_rows(results, tibble(
      binomial      = rows$binomial$value,
      iucn_taxon_id = rows$iucnId$value
    ))
    Sys.sleep(1)
  }
  results |> group_by(binomial) |> slice(1) |> ungroup()
}

# ── 2. IUCN API: taxon ID -> latest assessment ID ────────────────────────
lookup_assessment_id <- function(taxon_id) {
  url <- sprintf("https://apiv3.iucnredlist.org/api/v3/species/id/%s?token=%s",
                 taxon_id, token)
  resp <- RETRY("GET", url, UA, times = 3, pause_base = 2)
  if (status_code(resp) != 200) return(NA_character_)
  payload <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  if (is.null(payload$result) || length(payload$result) == 0) return(NA_character_)
  # The API returns assessment_id under different keys in different versions
  asmt <- payload$result$assessment_id %||%
          payload$result$published_year %||%   # fallback signal
          NA_character_
  if (is.null(asmt) || length(asmt) == 0 || all(is.na(asmt))) return(NA_character_)
  as.character(asmt[1])
}

# ── 3. Read both CSVs and gather binomials needing lookup ────────────────
gbif <- read_csv(gbif_csv, col_types = cols(.default = col_character()))
raw  <- read_csv(raw_csv,  col_types = cols(.default = col_character()))

# Anything currently citing an IUCN URL but NOT in the /<taxon>/<asmt> form
need_pattern_old   <- "iucnredlist\\.org/(?:search|species/[0-9]+(?!/[0-9]))"
need_pattern_done  <- "iucnredlist\\.org/species/[0-9]+/[0-9]+"

needs_upgrade <- function(text) {
  has_old  <- grepl(need_pattern_old,  text %||% "", perl = TRUE)
  has_done <- grepl(need_pattern_done, text %||% "")
  has_old & !has_done
}

binomials <- unique(c(
  gbif$prefName[needs_upgrade(gbif$Note)],
  raw$scientific_name[needs_upgrade(raw$notes)]
)) |> discard(is.na)

message(sprintf("[iucn] Binomials needing upgrade: %d", length(binomials)))

# ── 4. Resolve taxon IDs then assessment IDs ─────────────────────────────
ids <- lookup_taxon_ids(binomials)
message(sprintf("[iucn] Wikidata taxon IDs: %d / %d", nrow(ids), length(binomials)))

ids$assessment_id <- vapply(ids$iucn_taxon_id, function(x) {
  Sys.sleep(0.6)  # polite to IUCN API (limit ~10k/day; 1 req/sec safe)
  lookup_assessment_id(x)
}, character(1))
n_complete <- sum(!is.na(ids$assessment_id))
message(sprintf("[iucn] Assessment IDs from IUCN API: %d / %d",
                n_complete, nrow(ids)))

direct_url_for <- function(binomial) {
  row <- ids[ids$binomial == binomial, , drop = FALSE]
  if (nrow(row) == 0 || is.na(row$assessment_id)) return(NA_character_)
  sprintf("https://www.iucnredlist.org/species/%s/%s",
          row$iucn_taxon_id, row$assessment_id)
}
search_url_for <- function(binomial) {
  sprintf("https://www.iucnredlist.org/search?query=%s&searchType=species",
          URLencode(binomial, reserved = TRUE))
}

# ── 5. URL rewriter ──────────────────────────────────────────────────────
rewrite <- function(text, binomial) {
  if (is.na(text) || nchar(text) == 0) return(text)
  if (is.na(binomial) || nchar(binomial) == 0) return(text)
  direct <- direct_url_for(binomial)
  if (is.na(direct)) return(text)               # leave untouched if no asmt id
  search <- search_url_for(binomial)
  bad_single <- "https://www\\.iucnredlist\\.org/species/[0-9]+(?!/[0-9])"
  text <- gsub(bad_single, direct, text, perl = TRUE)
  text <- gsub(search, direct, text, fixed = TRUE)
  text
}

gbif_new <- gbif |> mutate(Note  = mapply(rewrite, Note,  prefName))
raw_new  <- raw  |> mutate(notes = mapply(rewrite, notes, scientific_name))

write_csv(gbif_new, gbif_csv, na = "")
write_csv(raw_new,  raw_csv,  na = "")

message(sprintf("[iucn] DONE. Rewrote %s and %s",
                basename(gbif_csv), basename(raw_csv)))
message("[iucn] Binomials still on search URLs (no assessment ID resolved):")
missed <- ids$binomial[is.na(ids$assessment_id)] |>
  c(setdiff(binomials, ids$binomial))
if (length(missed) > 0) message("        ", paste(missed, collapse = ", "))
