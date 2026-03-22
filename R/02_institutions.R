# =============================================================================
# 02_institutions.R
# Map institutions and industries associated with each Kenyan species law
#
# Purpose: Load, clean, and link institutions to laws.
#          Build a cross-reference table (law × institution) and a
#          jurisdiction/role summary for use in reporting.
#
# Outputs:
#   data/processed/institutions_clean.rds
#   data/processed/institutions_clean.csv
#   data/processed/law_institution_xref.rds   (long-format cross-reference)
#   data/processed/law_institution_xref.csv
#
# Sources:
#   - Kenya Wildlife Service: https://www.kws.go.ke
#   - Kenya Forest Service: https://www.kenyaforestservice.org
#   - NEMA: https://www.nema.go.ke
#   - KEPHIS: https://www.kephis.go.ke
#   - National Biosafety Authority: https://www.biosafetykenya.go.ke
#   - Kenya Fisheries Service: https://www.agriculture.go.ke/fisheries
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(glue)
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────

proj_root  <- here::here()
data_raw   <- file.path(proj_root, "data", "raw")
data_proc  <- file.path(proj_root, "data", "processed")

dir.create(data_proc, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load data ──────────────────────────────────────────────────────────────

message("[02] Loading institutions and laws data ...")

institutions_raw <- read_csv(
  file.path(data_raw, "institutions.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

laws_clean <- readRDS(file.path(data_proc, "laws_clean.rds"))

message(glue("    Loaded {nrow(institutions_raw)} institutions."))

# ── 2. Clean institutions ─────────────────────────────────────────────────────

institutions_clean <- institutions_raw |>
  clean_names() |>
  mutate(
    establishment_year = as.integer(establishment_year),
    # Split comma-separated industries into list column
    industries_list = str_split(industries_regulated, ";\\s*"),
    # Derive statutory vs non-statutory
    statutory = !str_detect(legal_basis, regex("non-statutory|not statutory|registered under Societies", ignore_case = TRUE))
  )

# ── 3. Build long-format institution × law cross-reference ───────────────────
#
#  Each institution enforces one or more laws.  We build a tidy cross-reference
#  by parsing the laws_enforced field.
#

law_institution_xref <- institutions_clean |>
  select(inst_id, institution_name, abbreviation, laws_enforced) |>
  mutate(
    # Normalise separators and split
    laws_list = str_split(laws_enforced, ";\\s*")
  ) |>
  unnest(laws_list) |>
  rename(law_reference = laws_list) |>
  select(inst_id, institution_name, abbreviation, law_reference) |>
  # Try to match back to a law_id via the law short name or act number in our laws table
  mutate(
    # Crude cross-match: does the law reference contain any keyword from our law names?
    matched_law_id = map_chr(law_reference, function(ref) {
      # Check against our laws table
      match_idx <- str_detect(
        laws_clean$law_name,
        regex(
          str_extract(ref, "^[A-Za-z ]+") |> str_squish(),
          ignore_case = TRUE
        )
      )
      if (any(match_idx, na.rm = TRUE)) {
        laws_clean$law_id[which(match_idx)[1]]
      } else {
        NA_character_
      }
    })
  )

# ── 4. Summary diagnostics ────────────────────────────────────────────────────

message("\n[02] Institutions summary:")
message(glue("    Total institutions: {nrow(institutions_clean)}"))
message(glue("    Statutory bodies: {sum(institutions_clean$statutory, na.rm = TRUE)}"))

message("\n    Parent ministries:")
institutions_clean |>
  count(parent_ministry, sort = TRUE) |>
  mutate(label = glue("      {parent_ministry}: {n} institution(s)")) |>
  pull(label) |>
  walk(message)

message("\n    Institutions per law (top 5 most institutionally governed):")
law_institution_xref |>
  count(law_reference, sort = TRUE) |>
  slice_head(n = 5) |>
  mutate(label = glue("      {law_reference}: {n} institution(s)")) |>
  pull(label) |>
  walk(message)

# ── 5. Jurisdiction matrix ───────────────────────────────────────────────────
#
#  Build a wide matrix: rows = restriction categories, columns = institutions,
#  values = TRUE/FALSE (whether the institution has jurisdiction in that category).
#

# First: join restriction_category from laws_clean to cross-reference
law_inst_with_category <- law_institution_xref |>
  left_join(
    laws_clean |> select(law_id, restriction_category),
    by = c("matched_law_id" = "law_id")
  ) |>
  filter(!is.na(restriction_category))

jurisdiction_matrix <- law_inst_with_category |>
  distinct(abbreviation, restriction_category) |>
  mutate(has_jurisdiction = TRUE) |>
  pivot_wider(
    names_from  = restriction_category,
    values_from = has_jurisdiction,
    values_fill = FALSE
  )

message("\n    Jurisdiction matrix (institutions × restriction categories):")
print(jurisdiction_matrix)

# ── 6. Save outputs ───────────────────────────────────────────────────────────

saveRDS(institutions_clean, file.path(data_proc, "institutions_clean.rds"))
saveRDS(law_institution_xref, file.path(data_proc, "law_institution_xref.rds"))
saveRDS(jurisdiction_matrix, file.path(data_proc, "jurisdiction_matrix.rds"))

institutions_clean |>
  select(-industries_list) |>
  write_csv(file.path(data_proc, "institutions_clean.csv"))

law_institution_xref |>
  write_csv(file.path(data_proc, "law_institution_xref.csv"))

jurisdiction_matrix |>
  write_csv(file.path(data_proc, "jurisdiction_matrix.csv"))

message("\n[02] Done. Saved to data/processed/")
