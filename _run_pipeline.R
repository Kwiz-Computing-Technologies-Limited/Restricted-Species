# _run_pipeline.R  — install packages, run pipeline, render report

# ── 0. Install missing packages ───────────────────────────────────────────────
pkgs_needed <- c(
  "tidyverse", "janitor", "lubridate", "glue", "here",
  "knitr", "rmarkdown", "kableExtra", "rvest", "httr", "writexl"
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
# 01 and 02 outputs are unchanged; only 03 and 04 need re-running for this update.
# Uncomment below to force a full re-run:
# source(file.path(proj_root, "R", "01_scrape_laws.R"))
# source(file.path(proj_root, "R", "02_institutions.R"))

message("\n===== 03_species_listing.R =====")
source(file.path(proj_root, "R", "03_species_listing.R"))

message("\n===== 04_merge_and_export.R =====")
source(file.path(proj_root, "R", "04_merge_and_export.R"))

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
message("\n===== DONE =====")
message("HTML report: ", out_file)
