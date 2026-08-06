# archive/compare_thermal_niches/generate_thermal_niches_survey_literature.R
#
# Purpose: Generate a comprehensive table of candidate thermal niches for ALL managed 
#          species (NEFMC, MAFMC, JOINT, ASMFC) to test sensitivity.
#          Also exports the preferred empirical niche (Survey 10-90th) to the main
#          data-raw folder for the production pipeline.
#
# Outputs: 1. archive/compare_thermal_niches/candidate_thermal_niches.rds
#          2. data-raw/thermal_niche.rds (Clean 10-90th percentile for main workflow)
#          3. data-raw/final_thermal_niche_values.csv

library(tidyverse)
library(here)

# -------------------------------------------------------------------
# 1. Output Setup
# -------------------------------------------------------------------

dir_out_archive <- here::here("archive/compare_thermal_niches")
dir_out_main    <- here::here("data-raw")

if (!dir.exists(dir_out_archive)) dir.create(dir_out_archive, recursive = TRUE)
if (!dir.exists(dir_out_main))    dir.create(dir_out_main, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Setup Managed Species & Load Raw Survey Data
# -------------------------------------------------------------------
message("Loading survey data and defining managed species list...")

survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/delete_brandon/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/delete_brandon/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

# Correct Windowpane
species <- species |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

# Define ASMFC managed species
asmfc_species <- c(
  "STRIPED BASS", "ATLANTIC MENHADEN", "TAUTOG", "WEAKFISH",
  "ATLANTIC CROAKER", "SPOT", "AMERICAN EEL", "ATLANTIC STURGEON",
  "HORSESHOE CRAB", "AMERICAN SHAD", "ALEWIFE", "BLUEBACK HERRING"
)

managed_species <- species |>
  dplyr::mutate(State.Managed = ifelse(COMNAME %in% asmfc_species, "ASMFC", NA_character_)) |>
  dplyr::filter(Fed.Managed %in% c("NEFMC", "MAFMC", "JOINT") | State.Managed == "ASMFC") |>
  dplyr::distinct(SVSPP, .keep_all = TRUE) |>
  dplyr::select(SVSPP, COMNAME, SCINAME, Fed.Managed, State.Managed)

# Filter raw survey data for valid bottom temperatures and positive catches
survdat_mgmt <- survdat |>
  dplyr::inner_join(managed_species, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(BOTTEMP)) |>
  dplyr::mutate(BOTTEMP = as.numeric(BOTTEMP))


# -------------------------------------------------------------------
# 3. Survey Data: 10-90th Percentile (Preferred Niche)
# -------------------------------------------------------------------
message("Calculating Survey 10-90th percentiles...")

survey_1090 <- survdat_mgmt |>
  dplyr::group_by(COMNAME) |>
  dplyr::summarize(
    tmin = quantile(BOTTEMP, probs = 0.10, na.rm = TRUE),
    tmax = quantile(BOTTEMP, probs = 0.90, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::mutate(source = "Survey_10_90")


# -------------------------------------------------------------------
# 4. Survey Data: 0-100th Percentile (Full Range)
# -------------------------------------------------------------------
message("Calculating Survey 0-100th percentiles (Full Range)...")

survey_0_100 <- survdat_mgmt |>
  dplyr::group_by(COMNAME) |>
  dplyr::summarize(
    tmin = min(BOTTEMP, na.rm = TRUE),
    tmax = max(BOTTEMP, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::mutate(source = "Survey_0_100")


# -------------------------------------------------------------------
# 5. Literature Data (Adults & Northeast US)
# -------------------------------------------------------------------
message("Processing Literature candidate niches...")

lit <- read_csv(here::here("data", "MS_screening_analysis_cleaned.csv"), show_col_types = FALSE) |> 
  dplyr::mutate(
    COMNAME = stringr::str_to_upper(common.name),
    # Ensure all temperature columns are numeric. 
    # suppressWarnings() safely silences the "NAs introduced by coercion" warning 
    # caused by text entries (like "NA" or "-") in the literature spreadsheet.
    dplyr::across(
      c(opt.temp.min, opt.temp.max, 
        stress.temp.min, stress.temp.max, 
        lethal.temp.min, lethal.temp.max), 
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# Base filter for Adults, Northeast US, and only our managed species list
lit_filtered <- lit |> 
  dplyr::filter(
    age.group == "Adult",
    stringr::str_detect(location, "(?i)Northeast US"),
    COMNAME %in% managed_species$COMNAME
  )

# 5a. Optimal Limits
lit_opt <- lit_filtered |> 
  dplyr::select(COMNAME, tmin = opt.temp.min, tmax = opt.temp.max) |> 
  dplyr::filter(!is.na(tmin) & !is.na(tmax)) |> 
  dplyr::distinct() |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(source = paste0("Lit_Opt_", dplyr::row_number())) |> 
  dplyr::ungroup()

# 5b. Stress Limits
lit_stress <- lit_filtered |> 
  dplyr::select(COMNAME, tmin = stress.temp.min, tmax = stress.temp.max) |> 
  dplyr::filter(!is.na(tmin) & !is.na(tmax)) |> 
  dplyr::distinct() |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(source = paste0("Lit_Stress_", dplyr::row_number())) |> 
  dplyr::ungroup()

# 5c. Lethal Limits
lit_lethal <- lit_filtered |> 
  dplyr::select(COMNAME, tmin = lethal.temp.min, tmax = lethal.temp.max) |> 
  dplyr::filter(!is.na(tmin) & !is.na(tmax)) |> 
  dplyr::distinct() |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(source = paste0("Lit_Lethal_", dplyr::row_number())) |> 
  dplyr::ungroup()

# Combine all literature candidates
lit_candidates <- dplyr::bind_rows(lit_opt, lit_stress, lit_lethal)


# -------------------------------------------------------------------
# 6. Combine and Finalize Outputs
# -------------------------------------------------------------------
message("Combining and saving outputs...")

# --- Output A: The Comprehensive Archive Comparison Table ---
candidate_niches <- dplyr::bind_rows(
  survey_1090,
  survey_0_100,
  lit_candidates
) |> 
  # Create a unique scenario ID (e.g., "ATLANTIC COD_Survey_10_90")
  dplyr::mutate(
    scenario_id = paste(COMNAME, source, sep = "_")
  ) |> 
  dplyr::arrange(COMNAME, source)

out_rds_archive <- file.path(dir_out_archive, "candidate_thermal_niches.rds")
out_csv_archive <- file.path(dir_out_archive, "candidate_thermal_niches.csv")

saveRDS(candidate_niches, out_rds_archive)
write_csv(candidate_niches, out_csv_archive)

message("Generated ", nrow(candidate_niches), " candidate niches for the comparison workflow.")


# --- Output B: The Clean Preferred Niche for Main Production Pipeline ---
# This ensures data-raw always has the updated 10-90th values for ALL managed species
preferred_niche <- survey_1090 |>
  dplyr::select(COMNAME, tmin, tmax, source)

out_rds_main <- file.path(dir_out_main, "thermal_niche.rds")
out_csv_main <- file.path(dir_out_main, "final_thermal_niche_values.csv")

saveRDS(preferred_niche, out_rds_main)
write_csv(preferred_niche, out_csv_main)

message("Exported clean Survey 10-90th percentiles to data-raw/ for the main workflow.")