#!/usr/bin/env Rscript
# upgrade_iucn_urls.R
# One-shot migration: replace the IUCN Red List search URLs (added in commit
# a9d8bed for the 105 FCMA+EMCA rows) with direct species-page URLs keyed by
# the IUCN Red List taxon ID, and record the numeric ID as a new `iucn_id`
# column on the downstream CSV.
#
# Sources:
#   Wikidata SPARQL endpoint: https://query.wikidata.org/sparql
#   IUCN ID property:         wdt:P627
#   Taxon name property:      wdt:P225
#
# Run from the repository root:
#   Rscript src/upgrade_iucn_urls.R
#
# Requires network access to query.wikidata.org. Safe to re-run; any row
# that already has a direct IUCN species-page URL is skipped.
#
# Touches:
#   data/raw/species_raw.csv
#   output/tables/kenya_regulated_plants_gbif.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(jsonlite)
})

raw_csv  <- "data/raw/species_raw.csv"
gbif_csv <- "output/tables/kenya_regulated_plants_gbif.csv"

stopifnot(file.exists(raw_csv), file.exists(gbif_csv))

# ── 1. Determine which binomials still need lookup ────────────────────────
gbif <- read_csv(gbif_csv, col_types = cols(.default = col_character()))

fcma_mask <- str_detect(gbif$governing_law %||% "", fixed("FCMA 2016")) &
             str_detect(gbif$governing_law %||% "", fixed("EMCA 1999"))

need_lookup <- gbif |>
  filter(fcma_mask) |>
  filter(!str_detect(coalesce(Note, ""),
                     "iucnredlist\\.org/species/[0-9]+")) |>
  pull(prefName) |>
  unique()

message(sprintf("[iucn] FCMA+EMCA rows: %d | needing IUCN ID lookup: %d",
                sum(fcma_mask), length(need_lookup)))

# ── 2. SPARQL: fetch IUCN IDs from Wikidata ────────────────────────────────
lookup_wikidata_iucn <- function(binomials, batch_size = 25) {
  results <- tibble(binomial = character(), iucn_id = character())

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
      user_agent("KenyaRestrictedSpecies/1.0 (https://github.com/Kwiz-Computing-Technologies-Limited/restricted-species)"),
      times = 3, pause_base = 2
    )

    if (status_code(resp) != 200) {
      warning(sprintf("Wikidata returned %d for batch of %d", status_code(resp), length(chunk)))
      next
    }

    payload <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
    rows <- payload$results$bindings
    if (is.null(rows) || length(rows) == 0) next

    chunk_df <- tibble(
      binomial = rows$binomial$value,
      iucn_id  = rows$iucnId$value
    )
    results <- bind_rows(results, chunk_df)
    Sys.sleep(1)  # polite
  }

  results |>
    group_by(binomial) |>
    slice(1) |>                     # take first ID if Wikidata has duplicates
    ungroup()
}

if (length(need_lookup) > 0) {
  ids <- lookup_wikidata_iucn(need_lookup)
  message(sprintf("[iucn] Wikidata returned IDs for %d / %d binomials",
                  nrow(ids), length(need_lookup)))
} else {
  ids <- tibble(binomial = character(), iucn_id = character())
}

missing <- setdiff(need_lookup, ids$binomial)
if (length(missing) > 0) {
  message("[iucn] No Wikidata IUCN ID for ", length(missing),
          " species (their URLs will keep the search fallback):")
  message("       ", paste(missing, collapse = ", "))
}

# ── 3. URL rewriter ───────────────────────────────────────────────────────
# Replace any occurrence of
#   https://www.iucnredlist.org/search?query=<binomial>&searchType=species
# with
#   https://www.iucnredlist.org/species/<iucn_id>
# (without assessment suffix; IUCN redirects to the latest assessment).

rewrite_urls <- function(text, binomial, id_map) {
  if (is.na(text) || nchar(text) == 0) return(text)
  id <- id_map[[binomial]]
  if (is.null(id) || is.na(id)) return(text)

  enc <- URLencode(binomial, reserved = TRUE)
  search_url <- sprintf("https://www.iucnredlist.org/search?query=%s&searchType=species", enc)
  direct_url <- sprintf("https://www.iucnredlist.org/species/%s", id)
  str_replace_all(text, fixed(search_url), direct_url)
}

id_map <- setNames(as.list(ids$iucn_id), ids$binomial)

# ── 4. Update output/tables/kenya_regulated_plants_gbif.csv ───────────────
gbif_new <- gbif |>
  mutate(
    Note = mapply(rewrite_urls, Note, prefName,
                  MoreArgs = list(id_map = id_map))
  )

# Add iucn_id column after GBIFusageKey if not already present
if (!"iucn_id" %in% names(gbif_new)) {
  gbif_new <- gbif_new |>
    mutate(iucn_id = map_chr(prefName, ~id_map[[.x]] %||% "")) |>
    relocate(iucn_id, .after = GBIFusageKey)
} else {
  gbif_new <- gbif_new |>
    mutate(iucn_id = coalesce(iucn_id, unlist(id_map[prefName])))
}

write_csv(gbif_new, gbif_csv, na = "")
message(sprintf("[iucn] Rewrote %s (cols: %d -> %d)",
                basename(gbif_csv), ncol(gbif), ncol(gbif_new)))

# ── 5. Update data/raw/species_raw.csv ────────────────────────────────────
raw <- read_csv(raw_csv, col_types = cols(.default = col_character()))
raw_new <- raw |>
  mutate(
    notes = mapply(rewrite_urls, notes, scientific_name,
                   MoreArgs = list(id_map = id_map))
  )

if (!"iucn_id" %in% names(raw_new)) {
  raw_new <- raw_new |>
    mutate(iucn_id = map_chr(scientific_name, ~id_map[[.x]] %||% "")) |>
    relocate(iucn_id, .after = scientific_name)
}

write_csv(raw_new, raw_csv, na = "")
message(sprintf("[iucn] Rewrote %s (cols: %d -> %d)",
                basename(raw_csv), ncol(raw), ncol(raw_new)))

message("\n[iucn] DONE. Commit both CSVs when satisfied.")
