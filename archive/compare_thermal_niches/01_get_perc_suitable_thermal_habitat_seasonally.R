# archive/compare_thermal_niches/01_get_perc_suitable_thermal_habitat_seasonally.R
#
# Purpose: Calculate the `perc_within_hist` seasonal indicators for NEFMC-managed species
#          ACROSS MULTIPLE THERMAL NICHE SCENARIOS (Survey vs Literature).
#          This script takes the seasonal historic habitat envelopes (V6) and calculates 
#          the percentage of available "habitat-days" within that footprint that fell 
#          within the candidate thermal limits.
#
# Output:
#   RDS : archive/compare_thermal_niches/indicators/perc_suitable_thermal_habitat_seasonally.rds
#
# Dependencies: tidyverse, sf, terra, exactextractr, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(exactextractr) 
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_output <- here::here("archive/compare_thermal_niches/indicators")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Inputs
# -------------------------------------------------------------------

# Load V6 Seasonal Historic Habitat
habitat_v6_seasonal <- readRDS(here::here("data/historic_habitat_V6_seasonal/historic_habitat_V6_seasonal.rds"))

# Load the comprehensive candidate thermal niches
candidate_niches <- readRDS(here::here("archive/compare_thermal_niches/candidate_thermal_niches.rds"))

# Define Data Paths
nc_extended_file <- here::here("data-raw/duPontavice_bottom_temp.nc")
nc_daily_dir     <- here::here("data-raw")


# -------------------------------------------------------------------
# 3. Core Extraction Function
# -------------------------------------------------------------------

process_year_suitability_seasonally <- function(bt_daily, current_year) {
  
  # Extract dates and months from the daily raster layers
  layer_dates <- terra::time(bt_daily)
  layer_months <- as.numeric(format(layer_dates, "%m"))
  
  # Define season month groupings to strictly match the NEFSC survey timing
  seasons <- list(
    SPRING = c(3, 4, 5),     # March, April, May
    FALL   = c(9, 10, 11)    # September, October, November
  )
  
  # Iterate over every scenario in the candidate niches table
  map_dfr(1:nrow(candidate_niches), function(i) {
    
    row_scenario <- candidate_niches[i, ]
    sp           <- row_scenario$COMNAME
    tmin         <- row_scenario$tmin
    tmax         <- row_scenario$tmax
    source_name  <- row_scenario$source
    scenario_id  <- row_scenario$scenario_id
    
    map_dfr(names(seasons), function(season_name) {
      
      # The habitat_season strictly matches the season_name
      habitat_season <- season_name 
      poly_key <- paste0(sp, "_", habitat_season)
      
      poly <- habitat_v6_seasonal[[poly_key]]
      if (is.null(poly)) return(NULL)
      
      if (sf::st_crs(poly)$epsg != 4326) {
        poly <- sf::st_transform(poly, 4326)
      }
      
      # Subset the raster to just the days falling in these specific survey months
      season_months <- seasons[[season_name]]
      season_indices <- which(layer_months %in% season_months)
      
      if (length(season_indices) == 0) return(NULL)
      
      bt_season <- terra::subset(bt_daily, season_indices)
      bt_crop <- terra::crop(bt_season, terra::ext(poly))
      
      extraction <- exactextractr::exact_extract(
        x = bt_crop, 
        y = poly, 
        progress = FALSE
      )[[1]]
      
      df_long <- extraction |>
        tidyr::pivot_longer(
          cols = -coverage_fraction,
          names_to = "day_layer",
          values_to = "temp"
        ) |>
        dplyr::filter(!is.na(temp)) |>
        dplyr::mutate(
          is_suitable   = temp >= tmin & temp <= tmax,
          weighted_area = is_suitable * coverage_fraction
        )
      
      total_coverage <- sum(df_long$coverage_fraction)
      
      if (total_coverage == 0) {
        perc_within_hist <- NA_real_
      } else {
        perc_within_hist <- (sum(df_long$weighted_area) / total_coverage) * 100
      }
      
      tibble(
        species          = sp,
        scenario_id      = scenario_id,
        source           = source_name,
        year             = current_year,
        season           = season_name,
        habitat_used     = habitat_season, 
        perc_within_hist = perc_within_hist,
        tmin_used        = tmin,
        tmax_used        = tmax
      )
    })
  })
}


# -------------------------------------------------------------------
# 4. Process Historical Data (1959 - 2021)
# -------------------------------------------------------------------

message("--- Processing Historical ERDDAP Data (1959-2021) ---")

if (!file.exists(nc_extended_file)) {
  stop("Extended time series NetCDF not found.")
}

bt_all <- terra::rast(nc_extended_file)
layer_times <- terra::time(bt_all)
layer_years <- as.numeric(format(layer_times, "%Y"))
unique_years <- sort(unique(layer_years))

results_extended <- list()

for (year in unique_years) {
  # Stop processing at 2021 to prevent overlap with newer data
  if (year > 2021) next 
  
  message("  Processing year: ", year)
  year_indices <- which(layer_years == year)
  bt_daily <- terra::subset(bt_all, year_indices)
  
  results_extended[[as.character(year)]] <- process_year_suitability_seasonally(bt_daily, year)
  
  rm(bt_daily, year_indices)
  gc()
}


# -------------------------------------------------------------------
# 5. Process Recent Data (2022+)
# -------------------------------------------------------------------

message("\n--- Processing Recent GLORYS Data (2022+) ---")

nc_files_recent <- list.files(nc_daily_dir, pattern = "GLORYS_daily_BottomTemp_\\d{4}\\.nc$", full.names = TRUE)
results_recent <- list()

for (f in nc_files_recent) {
  year <- as.numeric(stringr::str_extract(basename(f), "\\d{4}"))
  
  # Only process files from 2022 onward
  if (year < 2022) next 
  
  message("  Processing year: ", year, " (", basename(f), ")")
  
  bt_daily <- terra::rast(f)
  results_recent[[as.character(year)]] <- process_year_suitability_seasonally(bt_daily, year)
  
  rm(bt_daily)
  gc()
}


# -------------------------------------------------------------------
# 6. Finalize and Save
# -------------------------------------------------------------------

# Concatenate and bind vertically
all_results <- c(results_extended, results_recent)
indicator_results_df <- bind_rows(all_results) |>
  arrange(species, scenario_id, year, season)

message("\nSuccessfully calculated seasonal historic thermal habitat percentage for ", 
        length(unique(indicator_results_df$scenario_id)), " candidate scenarios across ",
        length(unique(indicator_results_df$species)), " species.")
message("Time series span: ", min(indicator_results_df$year), " - ", max(indicator_results_df$year))

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat_seasonally.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)

# -------------------------------------------------------------------
# 7. Visualize Time-Series Comparisons
# -------------------------------------------------------------------

message("\n--- Generating comparison time-series plots ---")

dir_plots <- here::here("archive/compare_thermal_niches/images")
if (!dir.exists(dir_plots)) dir.create(dir_plots, recursive = TRUE)

# Get list of unique species in the results
species_list_results <- unique(indicator_results_df$species)

purrr::walk(species_list_results, function(sp) {
  
  # Filter data for the specific species
  df_sp <- indicator_results_df |>
    dplyr::filter(species == sp, !is.na(perc_within_hist))
  
  if (nrow(df_sp) == 0) return(NULL)
  
  p <- ggplot(df_sp, aes(x = year, y = perc_within_hist, color = source, group = scenario_id)) +
    # Make the Survey 10-90th line thicker to stand out against the literature scenarios
    geom_line(aes(linewidth = source == "Survey_10_90"), alpha = 0.8) +
    scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.5), guide = "none") +
    
    facet_wrap(~ season, ncol = 1) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Thermal Habitat Suitability"),
      subtitle = "Sensitivity of suitability index to candidate thermal niche thresholds",
      x = "Year",
      y = "Percent of Historic Habitat Suitable (%)",
      color = "Thermal Niche Source"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11)
    )
  
  # Save the plot
  safe_name <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_plots, paste0(safe_name, "_suitability_comparison.png"))
  
  ggsave(file_name, plot = p, width = 10, height = 7, dpi = 300, bg = "white")
})

message("All comparison time-series plots saved to: ", dir_plots)