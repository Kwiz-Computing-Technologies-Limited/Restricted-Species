# =============================================================================
# 04_merge_and_export.R
# Merge all datasets into final outputs: CSV master table and formatted tables
#
# Purpose: Join the species master, institutions, and laws datasets into a
#          single, comprehensive, publication-ready listing.
#          Export to CSV and produce formatted HTML/Excel tables.
#
# Outputs:
#   output/tables/kenya_restricted_species_master.csv
#   output/tables/kenya_laws_summary.csv
#   output/tables/kenya_institutions_summary.csv
#   output/tables/species_by_cites_appendix.csv
#   output/tables/species_by_iucn_status.csv
#
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(glue)
  library(kableExtra)   # for formatted HTML tables
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────

proj_root   <- here::here()
data_proc   <- file.path(proj_root, "data", "processed")
output_dir  <- file.path(proj_root, "output", "tables")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load processed data ────────────────────────────────────────────────────

message("[04] Loading processed datasets ...")

species     <- readRDS(file.path(data_proc, "species_master.rds"))
laws        <- readRDS(file.path(data_proc, "laws_clean.rds"))
institutions <- readRDS(file.path(data_proc, "institutions_clean.rds"))
species_by_law <- readRDS(file.path(data_proc, "species_by_law.rds"))

# ── 2. Comprehensive merged species × law × institution table ─────────────────

message("[04] Building comprehensive master table ...")

# Aggregate governing laws per species (semicolon-separated string for CSV)
species_laws_agg <- species_by_law |>
  filter(!is.na(law_id)) |>
  group_by(species_id) |>
  summarise(
    governing_law_ids    = paste(unique(law_id), collapse = "; "),
    governing_law_names  = paste(
      unique(str_extract(law_reference, "^[^(]+")),
      collapse = "; "
    ),
    restriction_categories = paste(unique(restriction_category), collapse = "; "),
    .groups = "drop"
  )

# Build master table
species_master_full <- species |>
  select(
    species_id, common_name, scientific_name, taxonomic_group, taxonomic_order,
    kingdom, organism_type, iucn_status, cites_appendix, legal_status_kenya,
    restriction_type, restriction_severity, managing_institutions, notes, source_url
  ) |>
  left_join(species_laws_agg, by = "species_id") |>
  arrange(restriction_severity, taxonomic_group, common_name)

message(glue("    Master table: {nrow(species_master_full)} rows × {ncol(species_master_full)} columns"))

# ── 3. Thematic sub-tables ────────────────────────────────────────────────────

# 3a. Species by CITES Appendix
species_cites <- species_master_full |>
  filter(cites_appendix != "Not listed") |>
  select(
    common_name, scientific_name, taxonomic_group,
    cites_appendix, iucn_status, legal_status_kenya, managing_institutions
  ) |>
  arrange(cites_appendix, taxonomic_group, common_name)

# 3b. Species by IUCN threat category (Critically Endangered and Endangered only)
species_threatened <- species_master_full |>
  filter(as.character(iucn_status) %in% c("Critically Endangered", "Endangered")) |>
  select(
    common_name, scientific_name, taxonomic_group,
    iucn_status, cites_appendix, legal_status_kenya, governing_law_names
  ) |>
  arrange(iucn_status, taxonomic_group, common_name)

# 3c. Invasive/prohibited species
species_invasive <- species_master_full |>
  filter(
    str_detect(restriction_type, regex("prohibited introduction|invasive", ignore_case = TRUE)) |
    str_detect(legal_status_kenya, regex("invasive|introduction prohibited", ignore_case = TRUE))
  ) |>
  select(
    common_name, scientific_name, taxonomic_group,
    legal_status_kenya, restriction_type, governing_law_names, notes
  )

# 3d. Laws summary
laws_summary <- laws |>
  select(
    law_id, law_name, short_name, act_number, year_enacted, year_last_amended,
    restriction_category, species_categories, enforcement_institution, url_kenya_law
  ) |>
  arrange(restriction_category, year_enacted)

# 3e. Institutions summary
institutions_summary <- institutions |>
  select(
    inst_id, institution_name, abbreviation, parent_ministry, establishment_year,
    statutory, mandate_summary, laws_enforced, jurisdiction_scope, url
  ) |>
  arrange(parent_ministry, institution_name)

# ── 4. Diagnostics ────────────────────────────────────────────────────────────

message("\n[04] Final statistics:")
message(glue("    Total species in master: {nrow(species_master_full)}"))
message(glue("    CITES-listed species: {nrow(species_cites)}"))
message(glue(
  "    CITES Appendix I: {sum(species_master_full$cites_appendix == 'Appendix I', na.rm = TRUE)}"
))
message(glue(
  "    CITES Appendix II: {sum(species_master_full$cites_appendix == 'Appendix II', na.rm = TRUE)}"
))
message(glue("    Critically Endangered + Endangered: {nrow(species_threatened)}"))
message(glue("    Invasive / prohibited species: {nrow(species_invasive)}"))
message(glue("    Total laws catalogued: {nrow(laws_summary)}"))
message(glue("    Total institutions: {nrow(institutions_summary)}"))

# ── 5. Export CSVs ────────────────────────────────────────────────────────────

write_csv(species_master_full,
          file.path(output_dir, "kenya_restricted_species_master.csv"))

write_csv(species_cites,
          file.path(output_dir, "species_by_cites_appendix.csv"))

write_csv(species_threatened,
          file.path(output_dir, "species_by_iucn_status.csv"))

write_csv(species_invasive,
          file.path(output_dir, "species_invasive_prohibited.csv"))

write_csv(laws_summary,
          file.path(output_dir, "kenya_laws_summary.csv"))

write_csv(institutions_summary,
          file.path(output_dir, "kenya_institutions_summary.csv"))

message(glue("\n[04] CSVs saved to {output_dir}/"))

# ── 6. Formatted HTML tables (kableExtra) ─────────────────────────────────────

message("[04] Generating formatted HTML tables ...")

make_html_table <- function(df, caption, scroll_height = "400px") {
  df |>
    kbl(caption = caption, escape = FALSE) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width        = TRUE,
      font_size         = 12
    ) |>
    scroll_box(height = scroll_height)
}

# Master species table HTML
html_master <- species_master_full |>
  select(
    `Common Name`     = common_name,
    `Scientific Name` = scientific_name,
    `Group`           = taxonomic_group,
    `IUCN`            = iucn_status,
    `CITES`           = cites_appendix,
    `Kenya Status`    = legal_status_kenya,
    `Governing Laws`  = governing_law_names
  ) |>
  make_html_table(
    caption       = "Table 1. Kenya Restricted Species Master Listing",
    scroll_height = "600px"
  )

save_kable(
  html_master,
  file = file.path(output_dir, "table_species_master.html")
)

# Laws HTML table
html_laws <- laws_summary |>
  select(
    `ID`       = law_id,
    `Law`      = law_name,
    `Year`     = year_enacted,
    `Amended`  = year_last_amended,
    `Category` = restriction_category,
    `Enforced by` = enforcement_institution
  ) |>
  make_html_table(
    caption = "Table 2. Kenyan Laws Governing Restricted/Prohibited Species"
  )

save_kable(
  html_laws,
  file = file.path(output_dir, "table_laws.html")
)

# Institutions HTML table
html_inst <- institutions_summary |>
  select(
    `Abbreviation` = abbreviation,
    `Institution`  = institution_name,
    `Ministry`     = parent_ministry,
    `Est.`         = establishment_year,
    `Mandate`      = mandate_summary
  ) |>
  make_html_table(
    caption = "Table 3. Kenyan Institutions with Species Regulatory Mandate"
  )

save_kable(
  html_inst,
  file = file.path(output_dir, "table_institutions.html")
)

message(glue("[04] HTML tables saved to {output_dir}/"))
message("\n[04] All done. Run src/restricted_species_kenya.Rmd to generate the report.")
