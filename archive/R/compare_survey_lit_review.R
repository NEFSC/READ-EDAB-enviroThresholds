# archive/R/compare_survey_lit_review.R
#
# Purpose: Compare thermal niches generated from survey data (middle 80th percentile)
#          to those from the literature review FOR ALL MANAGED SPECIES (NEFMC, MAFMC, JOINT, ASMFC).
#          Filtered to only evaluate Adult literature values. Generates histograms 
#          of raw observed bottom temperatures to visually evaluate the impact of 
#          using empirical percentiles vs. the spread of available literature values.

library(tidyverse)
library(here)

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_plots <- here::here("images/thermal_niche_explorations")
if (!dir.exists(dir_plots)) dir.create(dir_plots, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Raw Survey Data & Define Managed Species
# -------------------------------------------------------------------
message("Loading raw survey data and defining managed species list...")

survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species_raw <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

asmfc_species <- c(
  "STRIPED BASS", "ATLANTIC MENHADEN", "TAUTOG", "WEAKFISH",
  "ATLANTIC CROAKER", "SPOT", "AMERICAN EEL", "ATLANTIC STURGEON",
  "HORSESHOE CRAB", "AMERICAN SHAD", "ALEWIFE", "BLUEBACK HERRING"
)

managed_species <- species_raw |>
  dplyr::mutate(State.Managed = ifelse(COMNAME %in% asmfc_species, "ASMFC", NA_character_)) |>
  dplyr::filter(Fed.Managed %in% c("NEFMC", "MAFMC", "JOINT") | State.Managed == "ASMFC") |>
  dplyr::distinct(SVSPP, .keep_all = TRUE) |>
  dplyr::select(SVSPP, COMNAME)

# Filter raw data for positive catches with valid bottom temperatures
survdat_mgmt <- survdat |>
  dplyr::inner_join(managed_species, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(BOTTEMP)) |>
  dplyr::mutate(BOTTEMP = as.numeric(BOTTEMP))


# -------------------------------------------------------------------
# 3. Calculate Empirical Thermal Niches (Survey 10-90th)
# -------------------------------------------------------------------
message("Calculating Survey 10-90th percentiles for all managed species...")

survey <- survdat_mgmt |>
  dplyr::group_by(COMNAME) |>
  dplyr::summarize(
    tmin = quantile(BOTTEMP, probs = 0.10, na.rm = TRUE),
    tmax = quantile(BOTTEMP, probs = 0.90, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(common.name = COMNAME)


# -------------------------------------------------------------------
# 4. Load and prep Literature Data
# -------------------------------------------------------------------
message("Loading Literature data...")

lit <- read_csv(here::here('data', 'MS_screening_analysis_cleaned.csv'), show_col_types = FALSE) |> 
  dplyr::mutate(
    common.name = stringr::str_to_upper(common.name),
    # Ensure all temperature columns are numeric, suppressing coercion warnings for text like "NA"
    dplyr::across(
      c(opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max),
      ~ suppressWarnings(as.numeric(.x))
    )
  )

# Filter for Adult literature values
lit_adults <- lit |> 
  dplyr::filter(age.group == "Adult")


# -------------------------------------------------------------------
# 5. Create Comparison Table (Survey vs. Adult Literature)
# -------------------------------------------------------------------
message("Building comparison table...")

comparison_tbl <- survey |> 
  dplyr::inner_join(
    lit_adults |> 
      dplyr::select(
        common.name, age.group, effect.type,
        opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max
      ),
    by = "common.name"
  ) |> 
  dplyr::mutate(
    # Optimal temperature differences
    diff_opt_min_from_survey_min  = opt.temp.min  - tmin,
    diff_opt_max_from_survey_max  = opt.temp.max  - tmax,
    
    # Stress temperature differences
    diff_stress_min_from_survey_min = stress.temp.min - tmin,
    diff_stress_max_from_survey_max = stress.temp.max - tmax,
    
    # Lethal temperature differences
    diff_lethal_min_from_survey_min = lethal.temp.min - tmin,
    diff_lethal_max_from_survey_max = lethal.temp.max - tmax  
  ) |> 
  dplyr::select(
    common.name, age.group, effect.type,
    tmin, tmax,
    opt.temp.min, opt.temp.mean, opt.temp.max,
    stress.temp.min, stress.temp.max,
    lethal.temp.min, lethal.temp.max,
    starts_with("diff_")
  )

# Save comparison table as CSV for GitHub viewing
write_csv(
  comparison_tbl,
  here::here("data-raw", "survey_vs_lit_adult_thermal_niche_comparison.csv")
)


# -------------------------------------------------------------------
# 6. Generate Histograms
# -------------------------------------------------------------------

message("Generating histograms for ", length(unique(comparison_tbl$common.name)), " species...")

purrr::walk(unique(comparison_tbl$common.name), function(sp) {
  
  # Filter raw data for the specific species
  df_sp <- survdat_mgmt |> 
    dplyr::filter(COMNAME == sp) 
  
  if(nrow(df_sp) == 0) return(NULL)
  
  # Extract ALL comparison limits for this species (could be multiple rows)
  sp_limits <- comparison_tbl |> dplyr::filter(common.name == sp)
  
  # Survey limits are identical across rows, so we only need to plot them once
  survey_bounds <- sp_limits |> dplyr::slice(1)
  
  p <- ggplot(df_sp, aes(x = BOTTEMP)) +
    geom_histogram(binwidth = 0.5, fill = "grey75", color = "grey30", alpha = 0.8) +
    
    # 10-90th Percentile (Survey Empirical) - Plotted once
    geom_vline(data = survey_bounds, aes(xintercept = tmin, color = "Survey 10-90th"), linewidth = 1.2, linetype = "solid") +
    geom_vline(data = survey_bounds, aes(xintercept = tmax, color = "Survey 10-90th"), linewidth = 1.2, linetype = "solid") +
    
    # Literature Optimal Limits - Maps all available estimates for the species
    geom_vline(data = sp_limits, aes(xintercept = opt.temp.min, color = "Lit Opt Low"), linewidth = 1, linetype = "dashed", na.rm = TRUE) +
    geom_vline(data = sp_limits, aes(xintercept = opt.temp.max, color = "Lit Opt High"), linewidth = 1, linetype = "dashed", na.rm = TRUE) +
    
    # Literature Lethal Limits - Maps all available estimates for the species
    geom_vline(data = sp_limits, aes(xintercept = lethal.temp.min, color = "Lit Lethal Low"), linewidth = 1, linetype = "dotted", na.rm = TRUE) +
    geom_vline(data = sp_limits, aes(xintercept = lethal.temp.max, color = "Lit Lethal High"), linewidth = 1, linetype = "dotted", na.rm = TRUE) +
    
    scale_color_manual(
      name = "Niche Boundaries",
      values = c(
        "Survey 10-90th"  = "#0072B2",  # Dark Blue
        "Lit Opt Low"     = "#56B4E9",  # Sky Blue
        "Lit Opt High"    = "#E69F00",  # Orange
        "Lit Lethal Low"  = "#CC79A7",  # Pink
        "Lit Lethal High" = "#D55E00"   # Vermillion
      )
    ) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Observed Bottom Temperatures"),
      subtitle = paste0("Total positive tows: ", nrow(df_sp), "\nLiterature estimates plotted: ", nrow(sp_limits)),
      x = "Bottom Temperature (\u00B0C)",
      y = "Frequency (Number of Tows)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  # Save the plot
  safe_name <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_plots, paste0(safe_name, "_thermal_histogram.png"))
  ggsave(file_name, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
})

message("Histograms saved to: ", dir_plots)


# -------------------------------------------------------------------
# 7. Create Final Thermal Niche Table for the Pipeline
# -------------------------------------------------------------------
message("Saving final thermal niche values...")

# Exclusively use survey 10-90th percentiles for all managed species
final_thermal_niche <- survey |> 
  dplyr::mutate(source = "Survey") |> 
  dplyr::select(
    COMNAME = common.name,
    tmin,
    tmax,
    source
  )

# Save as CSV so it can be easily viewed on GitHub
write_csv(
  final_thermal_niche,
  here::here("data-raw", "final_thermal_niche_values.csv")
)

# Also save as RDS to ensure 01_get_perc_suitable_thermal_habitat_seasonally.R 
# can load it exactly as it expects
saveRDS(
  final_thermal_niche,
  here::here("data-raw", "thermal_niche.rds")
)

message("Script complete.")