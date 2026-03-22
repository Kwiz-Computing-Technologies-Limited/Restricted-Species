# =============================================================================
# 01_scrape_laws.R
# Catalogue all relevant Kenyan laws and regulations on restricted/prohibited species
#
# Purpose: Load, clean, and enrich the pre-compiled laws dataset.
#          This script also provides the framework for live web scraping from
#          Kenya Law (kenyalaw.org) and CITES databases when network access
#          is available.
#
# Outputs:
#   data/processed/laws_clean.rds
#   data/processed/laws_clean.csv
#
# Sources:
#   - Kenya Law Reform Commission: https://new.kenyalaw.org
#   - CITES: https://cites.org/eng/parties/country-profiles/ke
#   - KWS: https://www.kws.go.ke
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(glue)
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────

proj_root  <- here::here()   # requires 'here' package; falls back to getwd()
data_raw   <- file.path(proj_root, "data", "raw")
data_proc  <- file.path(proj_root, "data", "processed")

# Create processed directory if it does not exist
dir.create(data_proc, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load pre-compiled laws dataset ─────────────────────────────────────────

message("[01] Loading raw laws data ...")

laws_raw <- read_csv(
  file.path(data_raw, "laws.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

message(glue("    Loaded {nrow(laws_raw)} laws/regulations."))

# ── 2. Clean and type-cast ────────────────────────────────────────────────────

laws_clean <- laws_raw |>
  clean_names() |>
  mutate(
    year_enacted      = as.integer(year_enacted),
    year_last_amended = as.integer(year_last_amended),
    # Derive age and recency flags
    years_since_enactment = as.integer(format(Sys.Date(), "%Y")) - year_enacted,
    recently_amended      = year_last_amended >= 2020,
    # Tidy restriction_type into a factor with sensible ordering
    restriction_type = str_squish(restriction_type),
    # Split multi-institution enforcement into list column (character vector)
    enforcement_list = str_split(enforcement_institution, ";\\s*")
  )

# ── 3. Derive restriction category ───────────────────────────────────────────
#
#  We bucket each law into a high-level restriction category for cross-referencing
#  with species records.
#

restriction_categories <- tribble(
  ~law_id, ~restriction_category,
  "LAW01", "Wildlife",
  "LAW02", "Biodiversity/Environment",
  "LAW03", "Fisheries/Marine",
  "LAW04", "Forestry",
  "LAW05", "Plant/Seed",
  "LAW06", "Biosafety/GMO",
  "LAW07", "Wildlife",           # CITES is implemented through WCMA
  "LAW08", "Plant/Seed",
  "LAW09", "Marine/Maritime",
  "LAW10", "Heritage",
  "LAW11", "Criminal",
  "LAW12", "Criminal"
)

laws_clean <- laws_clean |>
  left_join(restriction_categories, by = "law_id")

# ── 4. Summary diagnostics ────────────────────────────────────────────────────

message("\n[01] Laws summary:")
message(glue("    Total laws/regulations: {nrow(laws_clean)}"))

laws_clean |>
  count(restriction_category, sort = TRUE) |>
  mutate(label = glue("    {restriction_category}: {n}")) |>
  pull(label) |>
  walk(message)

message(glue("\n    Oldest law: {min(laws_clean$year_enacted, na.rm = TRUE)}"))
message(glue("    Newest law: {max(laws_clean$year_enacted, na.rm = TRUE)}"))
message(glue("    Laws amended since 2020: {sum(laws_clean$recently_amended, na.rm = TRUE)}"))

# ── 5. Optional: Live scraping from Kenya Law ─────────────────────────────────
#
#  The block below scrapes metadata from Kenya Law's Akoma Ntoso API.
#  Set SCRAPE_LIVE = TRUE to enable — requires internet access and the
#  rvest and httr packages.
#
#  NOTE: Be respectful of the server — do not hammer with rapid requests.
#        Add Sys.sleep() between calls in production.
#
# SCRAPE_LIVE <- Sys.getenv("SCRAPE_LIVE", unset = "FALSE") == "TRUE"
#
# if (SCRAPE_LIVE) {
#   library(rvest)
#   library(httr)
#
#   kenya_law_base <- "https://new.kenyalaw.org"
#
#   # Example: scrape the WCMA index page
#   wcma_url  <- glue("{kenya_law_base}/akn/ke/act/2013/47/eng@2025-11-04")
#   wcma_page <- read_html(wcma_url)
#
#   # Extract section headings
#   wcma_sections <- wcma_page |>
#     html_elements(".akn-section") |>
#     html_attr("id")
#
#   message(glue("    WCMA: found {length(wcma_sections)} sections via live scrape."))
# }

# ── 6. Save outputs ───────────────────────────────────────────────────────────

saveRDS(laws_clean, file.path(data_proc, "laws_clean.rds"))

laws_clean |>
  select(-enforcement_list) |>      # drop list column for CSV export
  write_csv(file.path(data_proc, "laws_clean.csv"))

message("\n[01] Done. Saved laws_clean.rds and laws_clean.csv to data/processed/")
