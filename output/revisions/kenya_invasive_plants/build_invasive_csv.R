# =============================================================================
# build_invasive_csv.R
# Source-verified Kenya invasive / prohibited plant species — 18-column CSV
#
# Produces: kenya_invasive_plants_verified.csv
#
# This script:
#   1. Extracts 6 already-resolved species from the existing GBIF CSV
#   2. Resolves 3 unresolved species (Tagetes, Ipomoea, Eucalyptus) via rgbif
#   3. Corrects legal columns with verified source citations
#   4. Outputs 18-column CSV matching regulated_plants_v9.7.csv schema
#
# Verified against:
#   - FAO/KEPHIS Ch.26: https://www.fao.org/4/y5968e/y5968e10.htm
#   - GISD (iucngisd.org)
#   - CABI Invasive Species Compendium
#   - Kenya Law (kenyalaw.org): Cap 325, Cap 324, EMCA 1999, Agriculture Act
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rgbif)
  library(glue)
})

proj_root  <- here::here()
output_dir <- file.path(proj_root, "output", "revisions", "kenya_invasive_plants")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load existing GBIF-resolved plants ────────────────────────────────────

existing_csv <- file.path(proj_root, "output", "tables", "kenya_regulated_plants_gbif.csv")
gbif_all     <- read_csv(existing_csv, col_types = cols(.default = col_character()))

message(glue("[build] Loaded {nrow(gbif_all)} species from existing GBIF CSV."))

# ── 2. Extract 6 already-resolved invasive species ──────────────────────────

keep_keys <- c(
  "2765940",  # Pontederia crassipes (Water hyacinth)
  "5274863",  # Salvinia molesta
  "2925303",  # Lantana camara
  "7792960",  # Allium vineale
  "5358460",  # Prosopis juliflora
  "5384075"   # Opuntia stricta
)

existing_inv <- gbif_all |> filter(GBIFusageKey %in% keep_keys)
message(glue("[build] Extracted {nrow(existing_inv)} already-resolved species."))

# ── 3. Resolve 3 new species via rgbif ──────────────────────────────────────

new_species <- tribble(
  ~query_name,      ~common_name,
  "Tagetes minuta",  "Mexican marigold",
  "Ipomoea",         "Morning glory",
  "Eucalyptus",      "Eucalyptus"
)

message("[build] Resolving 3 new species via GBIF backbone ...")

resolve_one <- function(name) {
  tryCatch({
    res <- name_backbone(name = name, kingdom = "Plantae")
    as_tibble(res)
  }, error = function(e) {
    tibble(usageKey = NA_integer_, scientificName = name,
           canonicalName = name, rank = NA_character_,
           matchType = "NONE", family = NA_character_,
           genus = NA_character_, species = NA_character_)
  })
}

backbone_new <- map_dfr(new_species$query_name, resolve_one)

# ── 4. Fetch vernacular names and synonyms ──────────────────────────────────

get_vernacular <- function(key) {
  if (is.na(key)) return(NA_character_)
  tryCatch({
    vn <- name_usage(key = as.numeric(key), data = "vernacularNames", limit = 100)
    if (is.null(vn$data) || nrow(vn$data) == 0) return(NA_character_)
    en <- vn$data |>
      filter(language %in% c("eng", "en")) |>
      pull(vernacularName) |>
      unique()
    if (length(en) == 0) return(NA_character_)
    paste(en, collapse = ", ")
  }, error = function(e) NA_character_)
}

get_synonyms <- function(key) {
  if (is.na(key)) return(NA_character_)
  tryCatch({
    syn <- name_usage(key = as.numeric(key), data = "synonyms", limit = 100)
    if (is.null(syn$data) || nrow(syn$data) == 0) return(NA_character_)
    syn_names <- syn$data |>
      pull(canonicalName) |>
      unique() |>
      na.omit()
    if (length(syn_names) == 0) return(NA_character_)
    paste(syn_names, collapse = ", ")
  }, error = function(e) NA_character_)
}

new_keys <- backbone_new$usageKey
message(glue("[build] Fetching vernacular/synonyms for {length(new_keys)} new keys ..."))

vern_new <- map_chr(new_keys, get_vernacular)
syn_new  <- map_chr(new_keys, get_synonyms)

# Build rows for new species
new_rows <- tibble(
  GBIFusageKey      = as.character(backbone_new$usageKey),
  country           = "Kenya",
  region            = "",
  jurisdiction      = "NATIONAL",
  jurisdiction_group = "",
  prefName          = coalesce(backbone_new$species, backbone_new$canonicalName),
  classification    = "Restricted",
  taxonLevel        = tolower(coalesce(backbone_new$rank, "species")),
  family            = backbone_new$family,
  englishName       = vern_new,
  synonyms          = syn_new,
  Note              = NA_character_,
  genus_I           = backbone_new$genus,
  is_aquatic        = "no",
  aquatic_type_I    = NA_character_,
  legal_status_ke   = NA_character_,
  restriction_type  = NA_character_,
  governing_law     = NA_character_
)

# Patch common names where GBIF vernacular may be sparse
new_rows <- new_rows |>
  mutate(
    englishName = case_when(
      str_detect(prefName, "Tagetes")    ~ paste0("Mexican marigold, ", coalesce(englishName, "")),
      str_detect(prefName, "Ipomoea")    ~ paste0("Morning glory, ", coalesce(englishName, "")),
      str_detect(prefName, "Eucalyptus") ~ paste0("Eucalyptus, ", coalesce(englishName, "")),
      TRUE ~ englishName
    ),
    englishName = str_remove(englishName, ",\\s*$")  # trim trailing comma if GBIF was NA
  )

# ── 5. Combine all 9 species ────────────────────────────────────────────────

all_inv <- bind_rows(existing_inv, new_rows)
message(glue("[build] Combined {nrow(all_inv)} invasive species."))

# ── 6. Overwrite legal columns with verified sources ────────────────────────
#
# Sources verified 2026-04-16 against:
#   FAO/KEPHIS: https://www.fao.org/4/y5968e/y5968e10.htm
#   GISD: iucngisd.org
#   Kenya Law: kenyalaw.org
#   CABI: cabi.org/isc
#   Africa Check, WWF-Kenya, Standard Media Kenya

verified <- tribble(
  ~GBIFusageKey, ~classification, ~legal_status_ke, ~restriction_type, ~governing_law, ~Note,

  # ── Water hyacinth (Pontederia crassipes / Eichhornia crassipes) ──
  "2765940",
  "Prohibited",
  "Declared noxious weed; introduction prohibited; eradication active",
  "Prohibited introduction; active control",
  "Suppression of Noxious Weeds Act Cap 325; EMCA 1999 s.42(d)",
  paste0(
    "Arrived Kenya ~1989. Declared noxious weed under Cap 325 Schedule. ",
    "EMCA 1999 s.42(d) prohibits any person from introducing 'any part of a plant specimen, whether alien or indigenous, dead or alive, in any river, lake or wetland' without Director-General approval. ",
    "FAO/KEPHIS Table 1: serious ecosystem & livelihood impact. ",
    "Biocontrol (Neochetina weevils) deployed on Lake Victoria. ",
    "Sources: GISD https://www.iucngisd.org/gisd/species.php?sc=70 | ",
    "FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
    "Cap 325 http://kenyalaw.org/kl/fileadmin/pdfdownloads/Acts/SuppressionofNoxiousWeedsAct_Cap325.pdf | ",
    "EMCA 1999 https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31"
  ),

  # ── Water fern (Salvinia molesta) ──
  "5274863",
  "Prohibited",
  "Introduction prohibited; invasive aquatic fern",
  "Prohibited introduction",
  "Suppression of Noxious Weeds Act Cap 325 (as S. auriculata); EMCA 1999 s.42(d)",
  paste0(
    "Arrived Kenya 1984. FAO/KEPHIS Table 1: serious impact. ",
    "Cap 325 Schedule lists S. auriculata (taxonomically close but distinct). ",
    "EMCA 1999 s.42(d) prohibits any person from introducing 'any part of a plant specimen, whether alien or indigenous, dead or alive, in any river, lake or wetland' without Director-General approval. ",
    "Dominant alien in Lake Naivasha floating mats (Harper et al. 1995). ",
    "Sources: GISD https://www.iucngisd.org/gisd/species.php?sc=569 | ",
    "FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
    "EPPO https://gd.eppo.int/taxon/SAVMO/datasheet | ",
    "Harper et al. 1995 https://link.springer.com/article/10.1007/BF00177693 | ",
    "EMCA 1999 https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31"
  ),

  # ── Lantana (Lantana camara) ──
  "2925303",
  "Restricted",
  "Ecologically invasive; NOT formally declared noxious weed",
  "Invasive alien species (ecological, not legally declared)",
  "No species-specific legal declaration found; EMCA 1999 s.42(d) for riparian contexts; Plant Protection Act Cap 324 (framework)",
  paste0(
    "Present since 1950s. FAO/KEPHIS: out-competes vegetation, poisonous to livestock, tsetse habitat. ",
    "CABI explicitly states NOT listed as noxious weed in Kenya (as of survey). ",
    "Standard Media Kenya confirms: 'Lantana is yet to be listed as a noxious weed in East Africa.' ",
    "Sources: GISD https://www.iucngisd.org/gisd/species.php?sc=56 | ",
    "FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
    "Standard Media https://www.standardmedia.co.ke/smart-harvest/article/2000203235"
  ),

  # ── Prosopis juliflora (Mesquite) ──
  "5358460",
  "Restricted",
  "Introduction restricted; eradication in sensitive areas",
  "Restricted; invasive alien species",
  "EMCA 1999 s.42(d) for riparian contexts; Plant Protection Act Cap 324 (framework)",
  paste0(
    "Introduced 1983 for arid-zone revegetation. Now invasive in ASALs. ",
    "FAO/KEPHIS Table 1: serious ecosystem & livelihood impact. ",
    "Declared invasive by Kenya government; eradication in Baringo & northern Kenya. ",
    "Sources: GISD https://www.iucngisd.org/gisd/species.php?sc=77 | ",
    "FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
    "CABI https://www.cabi.org/isc/datasheet/43897"
  ),

  # ── Opuntia stricta (Prickly pear) ──
  "5384075",
  "Restricted",
  "Introduction restricted; eradication at Laikipia and Samburu",
  "Restricted; invasive alien species",
  "EMCA 1999 s.42(d) for riparian contexts; Plant Protection Act Cap 324 (framework)",
  paste0(
    "Major invasive in dry savannas. Threatens Grevy's zebra habitat in Laikipia. ",
    "FAO/KEPHIS Table 1: serious ecosystem impact. ",
    "Active biocontrol and mechanical removal programmes. ",
    "Sources: CABI https://www.cabi.org/isc/datasheet/37714 | ",
    "FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm"
  ),

  # ── Wild garlic (Allium vineale) ──
  "7792960",
  "Prohibited",
  "Prohibited introduction; invasive weed",
  "Prohibited introduction; invasive alien species",
  "Plant Protection Act Cap 324 (framework; no public species schedule)",
  paste0(
    "FAO/KEPHIS Table 1: serious economic impact to horticultural farmers. ",
    "Contaminates wheat grain and taints dairy products. ",
    "No publicly accessible KEPHIS document names this species, but FAO/KEPHIS chapter confirms invasive status. ",
    "Sources: FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
    "CABI https://www.cabi.org/isc/datasheet/4255"
  )
)

# Merge verified data over existing columns
all_inv <- all_inv |>
  rows_update(verified, by = "GBIFusageKey", unmatched = "ignore")

# Now patch the 3 newly-resolved species that weren't in `verified` by GBIFusageKey
# (their keys were just resolved — we need to match by prefName instead)

all_inv <- all_inv |>
  mutate(
    # Tagetes minuta
    classification   = if_else(str_detect(prefName, "Tagetes"), "Restricted", classification),
    legal_status_ke  = if_else(str_detect(prefName, "Tagetes"),
      "Invasive weed; no species-specific legal declaration found", legal_status_ke),
    restriction_type = if_else(str_detect(prefName, "Tagetes"),
      "Invasive alien species (ecological)", restriction_type),
    governing_law    = if_else(str_detect(prefName, "Tagetes"),
      "Plant Protection Act Cap 324 (framework)", governing_law),
    Note = if_else(str_detect(prefName, "Tagetes"),
      paste0(
        "FAO/KEPHIS Table 1: one of 9 invasive plants in Kenya. ",
        "Allelopathic annual weed widespread in highland Kenya. ",
        "Weed of maize, beans, wheat. ",
        "Sources: FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
        "CABI https://www.cabidigitallibrary.org/doi/abs/10.1079/cabicompendium.52642 | ",
        "Cropnuts Kenya https://cropnuts.com/wp-content/uploads/2020/08/Arable-Weeds-Guide.pdf"
      ), Note),

    # Ipomoea spp.
    classification   = if_else(str_detect(prefName, "Ipomoea"), "Restricted", classification),
    legal_status_ke  = if_else(str_detect(prefName, "Ipomoea"),
      "Invasive weed; county-level disaster declarations", legal_status_ke),
    restriction_type = if_else(str_detect(prefName, "Ipomoea"),
      "Invasive alien species; county-level control", restriction_type),
    governing_law    = if_else(str_detect(prefName, "Ipomoea"),
      "EMCA 1999 s.42(d) for riparian contexts; Kajiado County disaster declaration", governing_law),
    Note = if_else(str_detect(prefName, "Ipomoea"),
      paste0(
        "FAO/KEPHIS Table 1: one of 9 invasive plants in Kenya. ",
        "I. hildebrandtii dominant species in rangelands. ",
        "3 million acres affected in Kajiado County; declared county disaster. ",
        "Sources: FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
        "WWF-Kenya https://wwf-kenya.medium.com/combatting-the-invasive-ipomoea-scourge-in-kajiado-county-2b3c7605ea68 | ",
        "Kenya News Agency https://www.kenyanews.go.ke/invasive-weed-proves-a-menace-to-kajiado-livestock-farmers/"
      ), Note),

    # Eucalyptus spp.
    classification   = if_else(str_detect(prefName, "Eucalyptus"), "Restricted", classification),
    legal_status_ke  = if_else(str_detect(prefName, "Eucalyptus"),
      "Regulated; banned in riparian zones (30m buffer)", legal_status_ke),
    restriction_type = if_else(str_detect(prefName, "Eucalyptus"),
      "Regulated introduction; riparian planting prohibited", restriction_type),
    governing_law    = if_else(str_detect(prefName, "Eucalyptus"),
      "Agriculture Act (farm forestry rules); Water Act; NEMA guidelines under EMCA 1999", governing_law),
    Note = if_else(str_detect(prefName, "Eucalyptus"),
      paste0(
        "FAO/KEPHIS Table 1: one of 9 invasive plants in Kenya. ",
        "High water consumption and allelopathy in riparian zones. ",
        "NEMA enforces 30m buffer from water sources. Court orders for removal. ",
        "Sources: FAO/KEPHIS https://www.fao.org/4/y5968e/y5968e10.htm | ",
        "Africa Check https://africacheck.org/fact-checks/blog/analysis-thirsty-species-science-behind-eucalyptus-tree-ban-kenyas-wetlands | ",
        "Daily Nation https://nation.africa/kenya/news/uproot-all-eucalyptus-trees-within-30m-of-water-sources-5262368"
      ), Note)
  )

# ── 7. Final column order (18 columns) ─────────────────────────────────────

output <- all_inv |>
  select(
    GBIFusageKey, country, region, jurisdiction, jurisdiction_group,
    prefName, classification, taxonLevel, family, englishName,
    synonyms, Note, genus_I, is_aquatic, aquatic_type_I,
    legal_status_ke, restriction_type, governing_law
  )

# ── 8. Save ─────────────────────────────────────────────────────────────────

csv_out <- file.path(output_dir, "kenya_invasive_plants_verified.csv")
write_csv(output, csv_out)

message(glue("\n[build] DONE. Saved {nrow(output)} species x {ncol(output)} columns"))
message(glue("        -> {csv_out}"))
message("\n[build] Species included:")
output |>
  select(GBIFusageKey, prefName, classification) |>
  mutate(row = row_number()) |>
  relocate(row) |>
  print(n = 20)
