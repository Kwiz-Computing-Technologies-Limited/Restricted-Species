# _run_pipeline.R  — install packages, run pipeline, render report

# ── 0. Install missing packages ───────────────────────────────────────────────
pkgs_needed <- c(
  "tidyverse", "janitor", "lubridate", "glue", "here",
  "knitr", "rmarkdown", "kableExtra", "rvest", "httr", "writexl",
  "rgbif", "jsonlite"
)

missing_pkgs <- pkgs_needed[!pkgs_needed %in% rownames(installed.packages())]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(
    missing_pkgs,
    repos   = "https://cloud.r-project.org",
    quiet   = FALSE,
    Ncpus   = parallel::detectCores()
  )
} else {
  message("All required packages already installed.")
}

# ── 1. Set working directory to project root ──────────────────────────────────
proj_root <- "/Users/kwizera.jvk/Desktop/Kwiz Computing Comes to Life/Restricted Species"
setwd(proj_root)

# ── 2. Run scripts in order ───────────────────────────────────────────────────
# Force re-run of all scripts to pick up expanded species data
source(file.path(proj_root, "R", "01_scrape_laws.R"))
source(file.path(proj_root, "R", "02_institutions.R"))

message("\n===== 03_species_listing.R =====")
source(file.path(proj_root, "R", "03_species_listing.R"))

message("\n===== 04_merge_and_export.R =====")
source(file.path(proj_root, "R", "04_merge_and_export.R"))

message("\n===== 05_gbif_plant_listing.R =====")
source(file.path(proj_root, "R", "05_gbif_plant_listing.R"))

# ── 3. Render Rmd ─────────────────────────────────────────────────────────────
message("\n===== Rendering Rmd =====")
output_dir <- file.path(proj_root, "output", "reports")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

rmarkdown::render(
  input      = file.path(proj_root, "docs", "restricted_species_kenya.Rmd"),
  output_dir = output_dir,
  quiet      = FALSE
)

out_file <- file.path(output_dir, "restricted_species_kenya.html")
message("\nMain report: ", out_file)

# ── 4. Render Plant Species Report ───────────────────────────────────────────
message("\n===== Rendering Plant Species Report =====")
rmarkdown::render(
  input      = file.path(proj_root, "docs", "plant_species_kenya.Rmd"),
  output_dir = output_dir,
  quiet      = FALSE
)

out_plant <- file.path(output_dir, "plant_species_kenya.html")
message("\n===== DONE =====")
message("Main report:  ", out_file)
message("Plant report: ", out_plant)
