# =============================================================================
# 03_species_listing.R
# Compile and merge species lists from all Kenyan wildlife laws
#
# Purpose: Load the raw species dataset, clean and enrich it, assign
#          taxonomic hierarchy, merge CITES status, and produce the
#          comprehensive master species listing.
#
# Outputs:
#   data/processed/species_master.rds
#   data/processed/species_master.csv
#   data/processed/species_by_taxon.rds    (nested by taxonomic group)
#   data/processed/species_by_law.rds      (long format: one row per species × law)
#
# Key sources:
#   - CITES Species+ / Checklist: https://checklist.cites.org
#   - IUCN Red List: https://www.iucnredlist.org
#   - BirdLife International DataZone: https://www.birdlife.org/species
#   - Kenya Wildlife Service: https://www.kws.go.ke
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(glue)
})

# ── 0. Paths ──────────────────────────────────────────────────────────────────

proj_root <- here::here()
data_raw  <- file.path(proj_root, "data", "raw")
data_proc <- file.path(proj_root, "data", "processed")

dir.create(data_proc, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load data ──────────────────────────────────────────────────────────────

message("[03] Loading raw species data ...")

species_raw <- read_csv(
  file.path(data_raw, "species_raw.csv"),
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

laws_clean <- readRDS(file.path(data_proc, "laws_clean.rds"))

message(glue("    Loaded {nrow(species_raw)} species records."))

# ── 2. Clean and standardise ──────────────────────────────────────────────────

species_clean <- species_raw |>
  clean_names() |>
  # Remove duplicate SP101 (same species as SP020, different common name)
  filter(species_id != "SP101") |>
  mutate(
    # Standardise IUCN status as ordered factor
    iucn_status = factor(
      iucn_status,
      levels = c(
        "Extinct (EX)",
        "Extinct in the Wild (EW)",
        "Critically Endangered",
        "Endangered",
        "Vulnerable",
        "Near Threatened",
        "Least Concern",
        "Data Deficient",
        "Not assessed (invasive)",
        "Not assessed"
      )
    ),
    # Standardise CITES appendix (check III and II before I to avoid substring match)
    cites_appendix = case_when(
      str_detect(cites_appendix, "Appendix III") ~ "Appendix III",
      str_detect(cites_appendix, "Appendix II")  ~ "Appendix II",
      str_detect(cites_appendix, "Appendix I")   ~ "Appendix I",
      TRUE                                        ~ "Not listed"
    ),
    cites_appendix = factor(
      cites_appendix,
      levels = c("Appendix I", "Appendix II", "Appendix III", "Not listed")
    ),
    # Derive broad taxonomic kingdom
    kingdom = case_when(
      taxonomic_group %in% c("Mammalia", "Aves", "Reptilia", "Amphibia",
                             "Actinopterygii", "Chondrichthyes", "Bivalvia",
                             "Gastropoda")           ~ "Animalia",
      taxonomic_group %in% c("Magnoliopsida",
                             "Monocots", "Cycadopsida",
                             "Polypodiopsida")        ~ "Plantae",
      TRUE                                            ~ "Other"
    ),
    # Derive vertebrate / invertebrate / plant flag
    organism_type = case_when(
      taxonomic_group %in% c("Mammalia", "Aves", "Reptilia",
                             "Amphibia", "Actinopterygii",
                             "Chondrichthyes")       ~ "Vertebrate",
      taxonomic_group %in% c("Bivalvia", "Gastropoda") ~ "Invertebrate",
      kingdom == "Plantae"                           ~ "Plant",
      TRUE                                           ~ "Other"
    ),
    # Restriction severity level (1 = highest threat / most restricted)
    restriction_severity = case_when(
      str_detect(restriction_type, regex("fully protected|prohibited commercial|directed fishing prohibited|prohibited.*wild", ignore_case = TRUE)) ~ 1L,
      str_detect(restriction_type, regex("fully protected|prohibited|banned", ignore_case = TRUE)) ~ 2L,
      str_detect(restriction_type, regex("regulated|restricted", ignore_case = TRUE)) ~ 3L,
      TRUE ~ 4L
    ),
    # Clean scientific name (strip italics markdown if present)
    scientific_name = str_remove_all(scientific_name, "\\*")
  )

# ── 3. Split multi-value columns into list columns ────────────────────────────

species_clean <- species_clean |>
  mutate(
    governing_laws_list   = str_split(governing_laws, ";\\s*"),
    managing_inst_list    = str_split(managing_institutions, ";\\s*")
  )

# ── 4. Build per-species source URLs ─────────────────────────────────────────
#
#  Three link types are constructed:
#    (a) legal_url  — most specific available link to the Kenyan legal instrument
#                     (Kenya Law Act page or CITES Appendices)
#    (b) iucn_url   — IUCN Red List: direct taxon page where taxon ID is known,
#                     otherwise a pre-filled search URL
#    (c) cites_url  — CITES Species+ Checklist search (only for CITES-listed spp.)
#
#  References for IUCN taxon IDs:
#    IUCN Red List API: https://api.iucnredlist.org
#    IUCN species pages follow the pattern:
#    https://www.iucnredlist.org/species/{taxon_id}

# Verified IUCN Red List taxon IDs (source: iucnredlist.org species pages)
iucn_id_lookup <- tribble(
  ~species_id, ~iucn_taxon_id,
  # ── Mammals ──────────────────────────────────────────────────────────────────
  "SP001", "12392",      # Loxodonta africana — African elephant
  "SP002", "6557",       # Diceros bicornis — Black rhinoceros
  "SP003", "4185",       # Ceratotherium simum — White rhinoceros
  "SP004", "220",        # Acinonyx jubatus — Cheetah
  "SP005", "15954",      # Panthera pardus — Leopard
  "SP006", "15951",      # Panthera leo — Lion
  "SP007", "12436",      # Lycaon pictus — African wild dog
  "SP008", "5674",       # Crocuta crocuta — Spotted hyena
  "SP009", "10103",      # Hippopotamus amphibius — Hippopotamus
  "SP010", "88420717",   # Giraffa reticulata — Reticulated giraffe
  "SP011", "88420729",   # Giraffa tippelskirchi — Masai giraffe
  "SP012", "7950",       # Equus grevyi — Grevy's zebra
  "SP013", "2654",       # Beatragus hunteri — Hirola
  "SP014", "84161717",   # Tragelaphus eurycerus isaaci — Mountain bongo
  "SP015", "10167",      # Hippotragus equinus — Roan antelope
  "SP016", "4200",       # Cercocebus galeritus — Tana River mangabey
  "SP017", "18248",      # Piliocolobus rufomitratus — Tana River red colobus
  "SP018", "55067",      # Pan troglodytes schweinfurthii — Eastern chimpanzee
  "SP019", "5144",       # Colobus guereza — Black-and-white colobus
  "SP020", "12765",      # Smutsia temminckii — Ground pangolin
  "SP021", "12767",      # Phataginus tricuspis — Tree pangolin
  "SP022", "12764",      # Smutsia gigantea — Giant pangolin
  "SP023", "6909",       # Dugong dugon — Dugong
  "SP024", "13006",      # Megaptera novaeangliae — Humpback whale
  "SP025", "41755",      # Physeter macrocephalus — Sperm whale
  "SP026", "41714",      # Tursiops aduncus — Indo-Pacific bottlenose dolphin
  "SP027", "82031696",   # Sousa plumbea — Indo-Pacific humpback dolphin
  "SP028", "21251",      # Syncerus caffer — African buffalo
  # ── Birds ────────────────────────────────────────────────────────────────────
  "SP029", "22692046",   # Balearica regulorum — Grey crowned crane
  "SP030", "22692047",   # Balearica pavonina — Black crowned crane
  "SP031", "22713481",   # Apalis fuscigularis — Taita Apalis
  "SP032", "22708824",   # Turdus helleri — Taita thrush
  "SP033", "22718516",   # Macronyx sharpei — Sharpe's Longclaw
  "SP034", "22718501",   # Anthus sokokensis — Sokoke Pipit
  "SP035", "22719085",   # Ploceus golandi — Clarke's Weaver
  "SP036", "22714249",   # Zosterops silvanus — Taita White-eye
  "SP037", "22696235",   # Sagittarius serpentarius — Secretarybird
  "SP038", "22695189",   # Gyps africanus — White-backed vulture
  "SP039", "22695199",   # Gyps rueppelli — Rüppell's vulture
  "SP040", "22695197",   # Trigonoceps occipitalis — White-headed vulture
  "SP041", "22695183",   # Necrosyrtes monachus — Hooded vulture
  "SP042", "22695190",   # Torgos tracheliotos — Lappet-faced vulture
  "SP043", "22695180",   # Neophron percnopterus — Egyptian vulture
  "SP044", "22724813",   # Psittacus erithacus — Grey parrot
  "SP045", "22696269",   # Falco cherrug — Saker falcon
  "SP046", "22688685",   # Otus ireneae — Sokoke scops-owl
  "SP047", "22716574",   # Turdoides hindei — Hinde's pied-babbler
  "SP048", "22714800",   # Calamonastides gracilirostris — Papyrus yellow warbler
  "SP049", "22712204",   # Hirundo atrocaerulea — Blue swallow
  "SP050", "22682516",   # Bucorvus leadbeateri — Southern ground-hornbill
  "SP091", "22712235",   # Tyto capensis — African grass owl
  "SP092", "22696172",   # Polemaetus bellicosus — Martial eagle
  "SP093", "22696105",   # Lophaetus occipitalis — Long-crested eagle
  # ── Reptiles ─────────────────────────────────────────────────────────────────
  "SP051", "45433",      # Crocodylus niloticus — Nile crocodile
  "SP052", "87980",      # Chelonia mydas — Green sea turtle
  "SP053", "8005",       # Eretmochelys imbricata — Hawksbill sea turtle
  "SP054", "3897",       # Caretta caretta — Loggerhead sea turtle
  "SP055", "11534",      # Lepidochelys olivacea — Olive ridley sea turtle
  "SP056", "10523",      # Dermochelys coriacea — Leatherback sea turtle
  "SP057", "21657",      # Trionyx triunguis — Nile softshell turtle
  "SP058", "12625",      # Malacochersus tornieri — Pancake tortoise
  "SP059", "62247",      # Varanus niloticus — Nile monitor
  "SP060", "62275",      # Varanus albigularis — Rock monitor
  "SP061", "176086",     # Eryx colubrinus — Kenya sand boa
  "SP094", "176227",     # Rhampholeon kerstenii — Montane dwarf chameleon
  "SP095", "176117",     # Trioceros jacksonii — Jackson's chameleon
  "SP096", "176098",     # Kinyongia fischeri — Fischer's chameleon
  "SP097", "177562",     # Python regius — Ball python
  "SP098", "177562",     # Python sebae — African rock python (same genus)
  # ── Amphibians ───────────────────────────────────────────────────────────────
  "SP083", "56262",      # Hyperolius cystocandicans — Tigoni reed frog
  # ── Fish & Marine ────────────────────────────────────────────────────────────
  "SP062", "63438",      # Oreochromis esculentus — Ngege tilapia
  "SP063", "63440",      # Oreochromis variabilis — Victoria tilapia
  "SP065", "63468",      # Labeo victorianus — Labeo
  "SP066", "19488",      # Rhincodon typus — Whale shark
  "SP067", "198921",     # Mobula birostris — Oceanic manta ray
  "SP086", "41006",      # Hippocampus kuda — Common seahorse
  "SP087", "4592",       # Cheilinus undulatus — Napoleon wrasse
  # ── Plants ───────────────────────────────────────────────────────────────────
  "SP076", "38622",      # Osyris lanceolata — East African sandalwood
  "SP077", "34632",      # Prunus africana — African cherry
  "SP081", "41907"       # Encephalartos hildebrandtii — Coastal cycad
)

# More specific legal source URLs keyed by species_id
# (overrides the generic source_url from the CSV where we can be more precise)
legal_url_overrides <- tribble(
  ~species_id, ~legal_url, ~legal_label,
  # WCMA + L.N. 242/2017 (Endangered/Threatened Species Regulations)
  "SP001", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP002", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP003", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP004", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP005", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP006", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP007", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP008", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP009", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP010", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP011", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP012", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP013", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP014", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP015", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP016", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP017", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP018", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP019", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP020", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP021", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP022", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Sea turtles / marine mammals — FMDA 2016 s.46 + WCMA 2013
  "SP023", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP024", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP025", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP026", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP027", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP028", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Birds — WCMA Sixth Schedule + L.N. 242/2017
  "SP029", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP030", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP031", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP032", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP033", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP034", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP035", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP036", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP037", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP038", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP039", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP040", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP041", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP042", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP043", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP044", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP045", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP046", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP047", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP048", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP049", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP050", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP091", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP092", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP093", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Reptiles
  "SP051", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP052", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP053", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP054", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP055", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP056", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.46 (Kenya Law)",
  "SP057", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP058", "https://new.kenyalaw.org/akn/ke/act/ln/2017/242/eng@2017-09-22","L.N. 242/2017 (Kenya Law)",
  "SP059", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP060", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP061", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP094", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP095", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP096", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP097", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP098", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Amphibians
  "SP083", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP084", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP085", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Fish & Marine
  "SP062", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.45 (Kenya Law)",
  "SP063", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.45 (Kenya Law)",
  "SP064", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP065", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 s.45 (Kenya Law)",
  "SP066", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP067", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP068", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP086", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP087", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP088", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP089", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP099", "https://new.kenyalaw.org/akn/ke/act/2016/35/eng@2022-12-31",    "FMDA 2016 (Kenya Law)",
  "SP100", "https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31",     "EMCA 1999 s.55 (Kenya Law)",
  # Plants
  "SP076", "https://new.kenyalaw.org/akn/ke/act/2016/34/eng@2022-12-31",    "FCMA 2016 s.40 (Kenya Law)",
  "SP077", "https://new.kenyalaw.org/akn/ke/act/2016/34/eng@2022-12-31",    "FCMA 2016 s.40 (Kenya Law)",
  "SP078", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP079", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP080", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP081", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  "SP082", "https://new.kenyalaw.org/akn/ke/act/2013/47/eng@2025-11-04",    "WCMA 2013 (Kenya Law)",
  # Invasive species — EMCA 1999 + Plant Protection Act Cap.324
  "SP069", "https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31",     "EMCA 1999 s.55 (Kenya Law)",
  "SP070", "https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31",     "EMCA 1999 s.55 (Kenya Law)",
  "SP071", "https://new.kenyalaw.org/akn/ke/act/1937/24/eng@2022-12-31",    "Plant Protection Act Cap.324",
  "SP072", "https://new.kenyalaw.org/akn/ke/act/1937/24/eng@2022-12-31",    "Plant Protection Act Cap.324",
  "SP073", "https://new.kenyalaw.org/akn/ke/act/1999/8/eng@2022-12-31",     "EMCA 1999 (Kenya Law)",
  "SP074", "https://new.kenyalaw.org/akn/ke/act/1937/24/eng@2022-12-31",    "Plant Protection Act Cap.324",
  "SP075", "https://new.kenyalaw.org/akn/ke/act/1937/24/eng@2022-12-31",    "Plant Protection Act Cap.324"
)

# Merge IUCN IDs and legal URL overrides into species_clean
species_clean <- species_clean |>
  left_join(iucn_id_lookup,     by = "species_id") |>
  left_join(legal_url_overrides, by = "species_id") |>
  mutate(
    # Clean scientific name for URL encoding (strip subspecies notes in parens)
    sci_name_url = str_remove(scientific_name, "\\s*\\(.*\\)$") |>
      str_squish() |>
      URLencode(reserved = TRUE),

    # For CITES links: reduce trinomials (subspecies) to binomial (genus + species),
    # since CITES lists at the species level. Uses the first two whitespace-separated
    # words of the cleaned name — purely programmatic, no guessing.
    cites_binomial_url = str_remove(scientific_name, "\\s*\\(.*\\)$") |>
      str_squish() |>
      str_extract("^\\S+\\s+\\S+") |>   # take only genus + species epithet
      URLencode(reserved = TRUE),

    # IUCN Red List URL: direct taxon page if ID known, search otherwise
    iucn_url = if_else(
      !is.na(iucn_taxon_id),
      paste0("https://www.iucnredlist.org/species/", iucn_taxon_id),
      paste0("https://www.iucnredlist.org/search?query=", sci_name_url)
    ),

    # CITES Species+ Checklist URL (only for CITES-listed species).
    # Uses the binomial so subspecies like "Diceros bicornis michaeli" resolve
    # correctly as "Diceros bicornis" on the CITES checklist.
    cites_checklist_url = if_else(
      as.character(cites_appendix) != "Not listed",
      paste0(
        "https://checklist.cites.org/#/en/search/output_layout=alphabetical",
        "&scientific_name=", cites_binomial_url
      ),
      NA_character_
    ),

    # Use the specific legal_url override where available; else fall back to source_url
    legal_url_final   = coalesce(legal_url, source_url),
    legal_label_final = coalesce(legal_label, "Legal source"),

    # Compose the HTML sources column
    # Format: "Kenya Law | IUCN | CITES" with each word hyperlinked
    sources_html = pmap_chr(
      list(legal_url_final, legal_label_final, iucn_url, cites_checklist_url),
      function(lurl, llabel, iurl, curl) {
        parts <- sprintf(
          '<a href="%s" target="_blank" title="%s">%s</a>',
          lurl, llabel, llabel
        )
        parts <- c(parts, sprintf(
          '<a href="%s" target="_blank" title="IUCN Red List">IUCN</a>',
          iurl
        ))
        if (!is.na(curl)) {
          parts <- c(parts, sprintf(
            '<a href="%s" target="_blank" title="CITES Species+ Checklist">CITES</a>',
            curl
          ))
        }
        paste(parts, collapse = " | ")
      }
    )
  )

message(glue("    URL columns added. Sources HTML built for all {nrow(species_clean)} species."))

# ── 5. Long-format species × law table ───────────────────────────────────────

species_by_law <- species_clean |>
  select(species_id, common_name, scientific_name, taxonomic_group, governing_laws_list) |>
  unnest(governing_laws_list) |>
  rename(law_reference = governing_laws_list) |>
  mutate(law_reference = str_squish(law_reference)) |>
  # Map to law_id using fuzzy matching on short name keywords
  mutate(
    law_id = case_when(
      str_detect(law_reference, regex("WCMA|Wildlife Conservation", ignore_case = TRUE)) ~ "LAW01",
      str_detect(law_reference, regex("EMCA|Environmental Management", ignore_case = TRUE)) ~ "LAW02",
      str_detect(law_reference, regex("FMDA|Fisheries", ignore_case = TRUE)) ~ "LAW03",
      str_detect(law_reference, regex("FCMA|Forest Conservation", ignore_case = TRUE)) ~ "LAW04",
      str_detect(law_reference, regex("Seeds|SPVA|Cap.*326", ignore_case = TRUE)) ~ "LAW05",
      str_detect(law_reference, regex("Biosafety|BSA", ignore_case = TRUE)) ~ "LAW06",
      str_detect(law_reference, regex("CITES", ignore_case = TRUE)) ~ "LAW07",
      str_detect(law_reference, regex("Plant Protection|Cap.*324", ignore_case = TRUE)) ~ "LAW08",
      str_detect(law_reference, regex("Maritime", ignore_case = TRUE)) ~ "LAW09",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(
    laws_clean |> select(law_id, short_name = short_name, restriction_category),
    by = "law_id"
  )

# ── 5. Summary diagnostics ────────────────────────────────────────────────────

message("\n[03] Species master listing summary:")
message(glue("    Total species: {nrow(species_clean)}"))
message(glue("    Duplicates removed: 1 (SP101 = SP020, different common name)"))

message("\n    By taxonomic group:")
species_clean |>
  count(taxonomic_group, sort = TRUE) |>
  mutate(label = glue("      {taxonomic_group}: {n}")) |>
  pull(label) |>
  walk(message)

message("\n    By IUCN status:")
species_clean |>
  count(iucn_status, sort = TRUE) |>
  mutate(label = glue("      {iucn_status}: {n}")) |>
  pull(label) |>
  walk(message)

message("\n    By CITES Appendix:")
species_clean |>
  count(cites_appendix, sort = TRUE) |>
  mutate(label = glue("      {cites_appendix}: {n}")) |>
  pull(label) |>
  walk(message)

message("\n    By organism type:")
species_clean |>
  count(organism_type, sort = TRUE) |>
  mutate(label = glue("      {organism_type}: {n}")) |>
  pull(label) |>
  walk(message)

# ── 6. Derived lookup: species governed by multiple laws ──────────────────────

multi_law_species <- species_by_law |>
  filter(!is.na(law_id)) |>
  distinct(species_id, common_name, law_id) |>
  count(species_id, common_name, name = "n_laws") |>
  filter(n_laws > 1) |>
  arrange(desc(n_laws))

message(glue("\n    Species governed by more than one law: {nrow(multi_law_species)}"))
message("    Top 5 by number of governing laws:")
multi_law_species |>
  slice_head(n = 5) |>
  mutate(label = glue("      {common_name}: {n_laws} laws")) |>
  pull(label) |>
  walk(message)

# ── 7. Save outputs ───────────────────────────────────────────────────────────

# Master listing (without list columns for CSV compatibility)
species_clean |>
  select(-governing_laws_list, -managing_inst_list) |>
  write_csv(file.path(data_proc, "species_master.csv"))

saveRDS(species_clean, file.path(data_proc, "species_master.rds"))

# Species by taxonomic group (nested)
species_by_taxon <- species_clean |>
  nest(.by = taxonomic_group) |>
  arrange(taxonomic_group)

saveRDS(species_by_taxon, file.path(data_proc, "species_by_taxon.rds"))

# Species × law cross-reference
saveRDS(species_by_law, file.path(data_proc, "species_by_law.rds"))
write_csv(species_by_law, file.path(data_proc, "species_by_law.csv"))

message("\n[03] Done. Saved to data/processed/")
