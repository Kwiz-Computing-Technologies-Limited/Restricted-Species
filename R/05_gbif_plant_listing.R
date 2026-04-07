# =============================================================================
# 05_gbif_plant_listing.R
# Query GBIF Backbone Taxonomy for Kenya restricted plant species using rgbif
#
# Purpose: Extract the 125 plant species from species_raw.csv, resolve each
#          name via rgbif::name_backbone(), fetch vernacular names and synonyms,
#          and produce output matching the regulated_plants CSV template format.
#
# Outputs:
#   output/tables/kenya_regulated_plants_gbif.csv
#   output/reports/kenya_regulated_plants_gbif.html
#
# Requires: rgbif, tidyverse, glue, kableExtra, jsonlite
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(rgbif)
  library(glue)
  library(kableExtra)
  library(jsonlite)
})

# ── 0. Paths ────────────────────────────────────────────────────────────────

proj_root  <- here::here()
data_raw   <- file.path(proj_root, "data", "raw")
output_tbl <- file.path(proj_root, "output", "tables")
output_rpt <- file.path(proj_root, "output", "reports")

dir.create(output_tbl, showWarnings = FALSE, recursive = TRUE)
dir.create(output_rpt, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load raw species and filter to plants ────────────────────────────────

message("[05] Loading species data and filtering to plants ...")

species_raw <- read_csv(
  file.path(data_raw, "species_raw.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

plant_groups <- c("Magnoliopsida", "Monocots", "Cycadopsida", "Polypodiopsida")

plants <- species_raw |>
  filter(taxonomic_group %in% plant_groups)

message(glue("    Found {nrow(plants)} plant species records."))

# ── 2. Helper: clean scientific names for GBIF matching ─────────────────────

clean_sci_name <- function(name) {
  name |>
    str_remove_all("\\s+spp\\.?$") |>
    str_remove_all("\\s+sensu\\s+lato$") |>
    str_remove_all("\\(.*?\\)") |>
    str_squish()
}

# ── 3. Resolve names via rgbif::name_backbone() ───────────────────────────

message("[05] Resolving names via rgbif::name_backbone() ...")

# Prepare clean names vector
plants <- plants |>
  mutate(clean_name = clean_sci_name(scientific_name))

# Use name_backbone_checklist for batch lookup (more efficient)
# Build a dataframe for the checklist approach
checklist_df <- plants |>
  select(name = clean_name) |>
  mutate(kingdom = "Plantae")

# name_backbone_checklist sends names in batch — much faster
backbone_results <- tryCatch({
  name_backbone_checklist(checklist_df)
}, error = function(e) {
  message("    Batch lookup failed, falling back to individual queries ...")
  # Fallback: query one at a time
  results <- map_dfr(plants$clean_name, function(nm) {
    tryCatch({
      res <- name_backbone(name = nm, kingdom = "Plantae")
      as_tibble(res)
    }, error = function(e2) {
      tibble(
        usageKey = NA_integer_, scientificName = nm,
        canonicalName = nm, rank = NA_character_,
        status = NA_character_, matchType = "NONE",
        kingdom = "Plantae", phylum = NA_character_,
        class = NA_character_, order = NA_character_,
        family = NA_character_, genus = NA_character_,
        species = NA_character_, confidence = NA_integer_,
        verbatim_name = nm
      )
    })
  })
  results
})

message(glue("    Backbone returned {nrow(backbone_results)} results."))

# Standardise column names (rgbif uses camelCase)
backbone_clean <- backbone_results |>
  as_tibble() |>
  mutate(
    GBIFusageKey = as.character(usageKey),
    prefName     = coalesce(species, canonicalName, verbatim_name),
    taxonLevel   = tolower(if_else(!is.na(rank), rank, "species")),
    gbif_family  = family,
    gbif_genus   = genus,
    gbif_order   = order,
    gbif_class   = class,
    gbif_phylum  = phylum,
    gbif_kingdom = kingdom,
    gbif_status  = status,
    gbif_matchType = matchType,
    gbif_confidence = confidence
  )

# Match backbone results back to our plants by row position
# (name_backbone_checklist preserves input order)
plants_matched <- plants |>
  bind_cols(
    backbone_clean |>
      select(GBIFusageKey, prefName, taxonLevel,
             gbif_family, gbif_genus, gbif_order, gbif_class,
             gbif_phylum, gbif_kingdom, gbif_status,
             gbif_matchType, gbif_confidence)
  )

n_matched <- sum(!is.na(plants_matched$GBIFusageKey))
message(glue("    Matched: {n_matched} / {nrow(plants_matched)}"))

# ── 4. Fetch vernacular names and synonyms for matched species ──────────────

message("[05] Fetching vernacular names and synonyms from GBIF ...")

# Helper: get English vernacular names via rgbif
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

# Helper: get synonyms via rgbif
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

# Query vernacular names and synonyms for each matched species
unique_keys <- plants_matched |>
  filter(!is.na(GBIFusageKey)) |>
  distinct(GBIFusageKey) |>
  pull(GBIFusageKey)

message(glue("    Querying {length(unique_keys)} unique GBIF keys ..."))

vern_list <- list()
syn_list  <- list()

for (i in seq_along(unique_keys)) {
  key <- unique_keys[i]
  if (i %% 10 == 0 || i == 1) {
    message(glue("    {i}/{length(unique_keys)} ..."))
  }
  vern_list[[key]] <- get_vernacular(key)
  syn_list[[key]]  <- get_synonyms(key)
  if (i %% 5 == 0) Sys.sleep(0.3)
}

vern_df <- tibble(
  GBIFusageKey   = names(vern_list),
  gbif_english   = unlist(vern_list),
  gbif_synonyms  = unlist(syn_list[names(vern_list)])
)

plants_enriched <- plants_matched |>
  left_join(vern_df, by = "GBIFusageKey") |>
  mutate(
    # Use GBIF English names if available, otherwise fall back to local common_name
    englishName = coalesce(gbif_english, common_name),
    synonyms    = gbif_synonyms,
    # Use GBIF family if available
    family      = coalesce(gbif_family, NA_character_),
    genus       = coalesce(gbif_genus, NA_character_)
  )

# ── 5. Determine aquatic status ────────────────────────────────────────────

plants_enriched <- plants_enriched |>
  mutate(
    is_aquatic = case_when(
      str_detect(common_name, regex("water|aquatic|marine|mangrove", ignore_case = TRUE)) ~ "yes",
      str_detect(scientific_name, regex("Eichhornia|Salvinia|Pistia|Nymphaea", ignore_case = TRUE)) ~ "yes",
      TRUE ~ "no"
    ),
    aquatic_type_I = case_when(
      is_aquatic == "yes" & str_detect(common_name, regex("marine|mangrove", ignore_case = TRUE)) ~ "marine",
      is_aquatic == "yes" ~ "freshwater",
      TRUE ~ NA_character_
    )
  )

# ── 6. Build template-format CSV output ────────────────────────────────────

message("[05] Building regulated_plants template format ...")

template_output <- plants_enriched |>
  transmute(
    GBIFusageKey     = GBIFusageKey,
    country          = "Kenya",
    region           = "",                # National level — no region (empty, not NA)
    jurisdiction     = "NATIONAL",
    jurisdiction_group = "",
    prefName         = prefName,
    classification   = case_when(
      str_detect(restriction_type, regex("prohibited", ignore_case = TRUE)) ~ "Prohibited",
      str_detect(restriction_type, regex("restricted", ignore_case = TRUE)) ~ "Restricted",
      str_detect(restriction_type, regex("regulated|protected", ignore_case = TRUE)) ~ "Protected / Regulated",
      TRUE ~ legal_status_kenya
    ),
    taxonLevel       = taxonLevel,
    family           = family,
    englishName      = englishName,
    synonyms         = synonyms,
    Note             = notes,
    genus_I          = genus,
    is_aquatic       = is_aquatic,
    aquatic_type_I   = aquatic_type_I,
    legal_status_ke  = legal_status_kenya,
    restriction_type = restriction_type,
    governing_law    = governing_laws
  )

csv_path <- file.path(output_tbl, "kenya_regulated_plants_gbif.csv")
write_csv(template_output, csv_path)
message(glue("[05] CSV saved: {csv_path}"))
message(glue("    {nrow(template_output)} rows x {ncol(template_output)} columns"))

# ── 7. Build HTML report with table and download button ────────────────────

message("[05] Generating HTML report ...")

plants_for_html <- plants_enriched |>
  mutate(
    # Use the ORIGINAL scientific name for IUCN (not GBIF-resolved), because
    # IUCN may index older names (e.g. Eichhornia crassipes, not Pontederia crassipes)
    iucn_search_name = scientific_name |>
      str_remove_all("\\(.*?\\)") |>
      str_squish() |>
      URLencode(reserved = TRUE),
    iucn_url = paste0("https://www.iucnredlist.org/search?query=", iucn_search_name),

    # For CITES: reduce to binomial (genus + species), use original name,
    # and only generate link for CITES-listed species
    cites_binomial_url = scientific_name |>
      str_remove("\\s*\\(.*\\)$") |>
      str_squish() |>
      str_extract("^\\S+\\s+\\S+") |>
      URLencode(reserved = TRUE),
    cites_url = if_else(
      !is.na(cites_appendix) & cites_appendix != "Not listed",
      paste0(
        "https://checklist.cites.org/#/en/search/output_layout=alphabetical",
        "&scientific_name=", cites_binomial_url
      ),
      NA_character_
    ),

    # GBIF species page link
    gbif_url = if_else(
      !is.na(GBIFusageKey),
      glue("https://www.gbif.org/species/{GBIFusageKey}"),
      NA_character_
    )
  )

html_table_data <- plants_for_html |>
  transmute(
    `#`               = row_number(),
    `Scientific Name` = if_else(
      !is.na(gbif_url),
      glue("<a href='{gbif_url}' target='_blank'><em>{prefName}</em></a>"),
      glue("<em>{prefName}</em>")
    ),
    `Common Name`     = englishName,
    `Family`          = family,
    `GBIF Key`        = GBIFusageKey,
    `IUCN Status`     = iucn_status,
    `CITES`           = cites_appendix,
    `Kenya Class.`    = case_when(
      str_detect(restriction_type, regex("prohibited", ignore_case = TRUE)) ~ "Prohibited",
      str_detect(restriction_type, regex("restricted", ignore_case = TRUE)) ~ "Restricted",
      str_detect(restriction_type, regex("regulated|protected", ignore_case = TRUE)) ~
        "Protected / Regulated",
      TRUE ~ legal_status_kenya
    ),
    `IUCN`            = glue("<a href='{iucn_url}' target='_blank'>IUCN</a>"),
    `CITES Check`     = if_else(
      !is.na(cites_url),
      glue("<a href='{cites_url}' target='_blank'>CITES</a>"),
      ""
    ),
    `Synonyms`        = if_else(
      !is.na(synonyms) & synonyms != "",
      str_trunc(synonyms, 80),
      ""
    )
  )

species_table_html <- html_table_data |>
  kbl(
    caption = "Kenya Regulated Plant Species \u2014 GBIF Backbone Resolved",
    escape  = FALSE,
    format  = "html"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = TRUE,
    font_size         = 12
  ) |>
  column_spec(2, width = "180px") |>
  column_spec(3, width = "150px") |>
  column_spec(11, width = "200px") |>
  scroll_box(height = "700px")

# Read project CSS
css_file <- file.path(proj_root, "docs", "style.css")
project_css <- if (file.exists(css_file)) {
  paste(readLines(css_file), collapse = "\n")
} else {
  ""
}

# Embed CSV data as JSON for the download button
csv_json <- template_output |>
  mutate(across(everything(), ~ replace_na(., ""))) |>
  toJSON()

# Stats for banner
n_species   <- nrow(template_output)
n_resolved  <- sum(!is.na(template_output$GBIFusageKey))
n_families  <- n_distinct(template_output$family, na.rm = TRUE)
gen_date    <- Sys.Date()

html_page <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Kenya Regulated Plant Species \u2014 GBIF Backbone</title>
  <link rel="stylesheet"
        href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css">
  <style>
    ', project_css, '

    body {
      font-family: "Source Sans Pro", "Helvetica Neue", Arial, sans-serif;
      font-size: 15px; color: #333; line-height: 1.65;
      padding: 30px 50px; max-width: 1400px; margin: 0 auto;
    }
    h1.title {
      font-family: Georgia, serif; font-size: 2.2em; font-weight: bold;
      border-bottom: 3px solid #2e7d32; padding-bottom: 10px;
    }
    .subtitle { font-size: 1.3em; color: #555; font-style: italic; }
    .stats-banner {
      background: #f0f7f0; border-left: 5px solid #2e7d32;
      padding: 15px 20px; margin: 20px 0; border-radius: 4px;
    }
    .stats-banner strong { color: #2e7d32; }
    .download-section { margin: 25px 0; display: flex; gap: 12px; flex-wrap: wrap; }
    .btn-download {
      display: inline-block; padding: 10px 24px; background-color: #2e7d32;
      color: white; text-decoration: none; border-radius: 6px;
      font-weight: 600; font-size: 14px; border: none; cursor: pointer;
      transition: background 0.2s;
    }
    .btn-download:hover { background-color: #1b5e20; color: white; }
    .method-note {
      background: #e8f4f8; border-left: 5px solid #1a8cb5;
      padding: 12px 16px; margin: 20px 0; border-radius: 4px; font-size: 13px;
    }
    .method-note a { color: #2e7d32; }
    .kqb-report-footer {
      margin-top: 40px; padding: 20px 30px; background: #f5f5f5;
      border-top: 3px solid #c9a84c; text-align: center;
      font-size: 11px; color: #666666;
    }
    table.table { font-size: 12px; }
    table.table th {
      font-weight: 600; position: sticky; top: 0; background: white; z-index: 2;
    }
    table.table-striped > tbody > tr:nth-of-type(odd) { background-color: #f0f7f0; }
  </style>
</head>
<body>
  <h1 class="title">Kenya Regulated Plant Species</h1>
  <p class="subtitle">GBIF Backbone Taxonomy \u2014 National-Level Restrictions</p>

  <div class="stats-banner">
    <strong>', n_species, '</strong> plant species |
    <strong>', n_resolved, '</strong> resolved via GBIF Backbone |
    <strong>', n_families, '</strong> families |
    Jurisdiction: <strong>NATIONAL</strong> (Kenya)
  </div>

  <div class="method-note">
    <strong>Methodology:</strong> Scientific names resolved via the
    <a href="https://www.gbif.org/developer/species" target="_blank">GBIF Backbone
    Taxonomy API</a> using the <code>rgbif</code> R package.
    English names and synonyms fetched from GBIF vernacular-names and synonyms endpoints.
    Classification reflects Kenya legal status under WCMA 2013, EMCA 1999, FCMA 2016,
    CITES (L.N. 241/2017), Plant Protection Act Cap. 324, and Biosafety Act 2009.
    <br><em>Generated: ', gen_date, '</em>
  </div>

  <div class="download-section">
    <button class="btn-download" onclick="downloadCSV()">
      &#11015; Download CSV (template format)
    </button>
    <button class="btn-download" onclick="downloadTableCSV()">
      &#11015; Download display table as CSV
    </button>
  </div>

  ', species_table_html, '

  <div class="kqb-report-footer">
    <p><strong>Kwiz Computing</strong> \u2014 Biodiversity Data Services</p>
    <p>Data sources: GBIF Backbone Taxonomy, IUCN Red List, CITES Checklist, Kenya Law</p>
    <p>Generated ', gen_date, ' | For official regulatory use, verify with KWS / KEPHIS / NEMA</p>
  </div>

  <script>
  const csvData = ', csv_json, ';

  function downloadCSV() {
    if (!csvData || csvData.length === 0) return;
    const headers = Object.keys(csvData[0]);
    let csv = headers.join(",") + "\\n";
    csvData.forEach(function(row) {
      csv += headers.map(function(h) {
        var val = String(row[h] || "");
        if (val.indexOf(",") >= 0 || val.indexOf(\'"\') >= 0 || val.indexOf("\\n") >= 0) {
          val = \'"\' + val.replace(/"/g, \'""\'  ) + \'"\';
        }
        return val;
      }).join(",") + "\\n";
    });
    var blob = new Blob([csv], {type: "text/csv;charset=utf-8;"});
    var link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = "kenya_regulated_plants_gbif.csv";
    link.click();
  }

  function downloadTableCSV() {
    var table = document.querySelector("table");
    if (!table) return;
    var csv = [];
    var rows = table.querySelectorAll("tr");
    rows.forEach(function(row) {
      var cols = row.querySelectorAll("td, th");
      var rowData = [];
      cols.forEach(function(col) {
        var text = col.innerText.replace(/"/g, \'""\'  );
        if (text.indexOf(",") >= 0 || text.indexOf(\'"\') >= 0 || text.indexOf("\\n") >= 0) {
          text = \'"\' + text + \'"\';
        }
        rowData.push(text);
      });
      csv.push(rowData.join(","));
    });
    var blob = new Blob([csv.join("\\n")], {type: "text/csv;charset=utf-8;"});
    var link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = "kenya_regulated_plants_display.csv";
    link.click();
  }
  </script>
</body>
</html>')

html_path <- file.path(proj_root, "docs", "kenya_regulated_plants_gbif.html")
writeLines(html_page, html_path)
message(glue("[05] HTML report saved: {html_path}"))

# ── 8. Summary diagnostics ─────────────────────────────────────────────────

# Report unmatched species
unmatched <- plants_enriched |>
  filter(is.na(GBIFusageKey)) |>
  pull(scientific_name)

message("\n[05] ══════════ SUMMARY ══════════")
message(glue("    Total plant species:    {n_species}"))
message(glue("    GBIF-resolved:          {n_resolved}"))
message(glue("    Unresolved:             {length(unmatched)}"))
message(glue("    Unique families:        {n_families}"))
message(glue("    Aquatic species:        {sum(template_output$is_aquatic == 'yes')}"))

if (length(unmatched) > 0) {
  message("    Unmatched names:")
  walk(unmatched, ~ message("      - ", .x))
}

message(glue("    CSV:  {csv_path}"))
message(glue("    HTML: {html_path}"))
message("[05] Done.")
