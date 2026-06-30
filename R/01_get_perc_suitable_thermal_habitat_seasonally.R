# get_perc_suitable_thermal_habitat_seasonally.R
#
# Purpose: Calculate the `perc_within_hist` seasonal indicators for NEFMC-managed species.
#          This script takes the seasonal historic habitat envelopes (V6) and calculates 
#          the percentage of available "habitat-days" within that footprint that fell 
#          within the species' thermal niche.
#
# Logic:   - SPRING and WINTER indicators use the SPRING historic habitat footprint.
#          - FALL and SUMMER indicators use the FALL historic habitat footprint.
#          - Processes historical data (1959-2021) and recent GLORYS data (2022+).
#
# Output:
#   RDS : data/indicators/perc_suitable_thermal_habitat_seasonally.rds
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

dir_output <- here::here("data/indicators")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Inputs
# -------------------------------------------------------------------

# Load V6 Seasonal Historic Habitat
habitat_v6_seasonal <- readRDS(here::here("data/historic_habitat_V6_seasonal/historic_habitat_V6_seasonal.rds"))

# Load Thermal Niche Definitions
thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

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
  
  # Define season month groupings (Standard Quarters)
  seasons <- list(
    WINTER = c(1, 2, 3),   # Jan, Feb, Mar
    SPRING = c(4, 5, 6),   # Apr, May, Jun
    SUMMER = c(7, 8, 9),   # Jul, Aug, Sep
    FALL   = c(10, 11, 12) # Oct, Nov, Dec
  )
  
  # Extract unique species names from the seasonal habitat keys (e.g., "ATLANTIC COD_SPRING")
  all_keys <- names(habitat_v6_seasonal)
  species_list <- unique(sub("_(SPRING|FALL)$", "", all_keys))
  
  map_dfr(species_list, function(sp) {
    
    th <- thermal_niche |> dplyr::filter(COMNAME == sp)
    if (nrow(th) == 0) return(NULL)
    
    tmin <- th$tmin[1]
    tmax <- th$tmax[1]
    
    map_dfr(names(seasons), function(season_name) {
      
      # Map the current season to the correct historic habitat footprint
      habitat_season <- if (season_name %in% c("SPRING", "WINTER")) "SPRING" else "FALL"
      poly_key <- paste0(sp, "_", habitat_season)
      
      poly <- habitat_v6_seasonal[[poly_key]]
      if (is.null(poly)) return(NULL)
      
      if (sf::st_crs(poly)$epsg != 4326) {
        poly <- sf::st_transform(poly, 4326)
      }
      
      # Subset the raster to just the days falling in this season
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
        year             = current_year,
        season           = season_name,
        habitat_used     = habitat_season, # Track which footprint was used
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
  arrange(species, year, season)

message("\nSuccessfully calculated seasonal historic thermal habitat percentage for ", 
        length(unique(indicator_results_df$species)), " species.")
message("Time series span: ", min(indicator_results_df$year), " - ", max(indicator_results_df$year))

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat_seasonally.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)