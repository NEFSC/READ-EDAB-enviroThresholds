# archive/compare_thermal_niches/generate_thermal_niches_survey_literature.R
#
# Purpose: Generate a comprehensive table of candidate thermal niches for each species 
#          to test the sensitivity of the downstream suitable habitat indicator.
#          Includes:
#            1. Survey 10-90th Percentile
#            2. Survey 0-100th Percentile (Full Observed Range)
#            3. Literature values (Adults & Northeast US) for Optimal, Stress, and Lethal limits.

library(tidyverse)
library(here)

# -------------------------------------------------------------------
# 1. Output Setup
# -------------------------------------------------------------------

dir_out <- here::here("archive/compare_thermal_niches")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Survey Data: 10-90th Percentile
# -------------------------------------------------------------------

message("Processing Survey 10-90th percentiles...")

survey_1090 <- readRDS(here::here("data-raw", "survey_thermal_niche.rds")) |> 
  dplyr::rename(COMNAME = COMNAME) |> 
  dplyr::mutate(
    tmin = as.numeric(tmin),
    tmax = as.numeric(tmax),
    source = "Survey_10_90"
  ) |> 
  dplyr::select(COMNAME, tmin, tmax, source)


# -------------------------------------------------------------------
# 3. Survey Data: 0-100th Percentile (Full Range)
# -------------------------------------------------------------------

message("Processing Survey 0-100th percentiles (Full Range)...")

survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species_list <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed)) |>
  dplyr::filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  dplyr::distinct(SVSPP, .keep_all = TRUE) |>
  dplyr::select(SVSPP, COMNAME)

survey_0_100 <- survdat |>
  dplyr::inner_join(species_list, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(BOTTEMP)) |>
  dplyr::group_by(COMNAME) |>
  dplyr::summarize(
    tmin = min(as.numeric(BOTTEMP), na.rm = TRUE),
    tmax = max(as.numeric(BOTTEMP), na.rm = TRUE),
    .groups = "drop"
  ) |> 
  dplyr::mutate(source = "Survey_0_100")


# -------------------------------------------------------------------
# 4. Literature Data (Adults & Northeast US)
# -------------------------------------------------------------------

message("Processing Literature candidate niches...")

lit <- read_csv(here::here("data", "MS_screening_analysis_cleaned.csv"), show_col_types = FALSE) |> 
  dplyr::mutate(
    COMNAME = stringr::str_to_upper(common.name),
    # Ensure all temperature columns are numeric
    dplyr::across(
      c(opt.temp.min, opt.temp.max, 
        stress.temp.min, stress.temp.max, 
        lethal.temp.min, lethal.temp.max), 
      ~ as.numeric(.x)
    )
  )

# Base filter for Adults and Northeast US
lit_filtered <- lit |> 
  dplyr::filter(
    age.group == "Adult",
    stringr::str_detect(location, "(?i)Northeast US")
  )

# 4a. Optimal Limits
lit_opt <- lit_filtered |> 
  dplyr::select(COMNAME, tmin = opt.temp.min, tmax = opt.temp.max) |> 
  dplyr::filter(!is.na(tmin) & !is.na(tmax)) |> 
  dplyr::distinct() |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(source = paste0("Lit_Opt_", dplyr::row_number())) |> 
  dplyr::ungroup()

# 4b. Stress Limits
lit_stress <- lit_filtered |> 
  dplyr::select(COMNAME, tmin = stress.temp.min, tmax = stress.temp.max) |> 
  dplyr::filter(!is.na(tmin) & !is.na(tmax)) |> 
  dplyr::distinct() |> 
  dplyr::group_by(COMNAME) |> 
  dplyr::mutate(source = paste0("Lit_Stress_", dplyr::row_number())) |> 
  dplyr::ungroup()

# 4c. Lethal Limits
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
# 5. Combine and Finalize
# -------------------------------------------------------------------

message("Combining all candidate niches...")

# Bind them all together
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

# Save the final tables
out_rds <- file.path(dir_out, "candidate_thermal_niches.rds")
out_csv <- file.path(dir_out, "candidate_thermal_niches.csv")

saveRDS(candidate_niches, out_rds)
write_csv(candidate_niches, out_csv)

message("Generated ", nrow(candidate_niches), " total candidate niches.")
message("Saved to: ", out_rds)