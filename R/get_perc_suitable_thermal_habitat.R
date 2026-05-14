# get_perc_suitable_thermal_habitat.R
#
# Purpose: Calculate the `perc_within_hist` indicator for NEFMC-managed species.
#          This script takes the fixed historic habitat envelopes defined in V6 
#          and calculates the percentage of available "habitat-days" within that  
#          footprint that fell within the species' thermal niche.
#
# Logic:   1. Process historical data (1959-2021) from the duPontavice NetCDF.
#          2. Process recent data (2022+) from individual GLORYS daily NetCDFs.
#          3. Bind the data into a single continuous time series.
#
# Output:
#   RDS : data/indicators/perc_suitable_thermal_habitat.rds
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

# Load V6 Historic Habitat
habitat_v6 <- readRDS(here::here("data/historic_habitat_V6/historic_habitat_V6.rds"))

# Load Thermal Niche Definitions
thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

# Define Data Paths
nc_extended_file <- here::here("data-raw/duPontavice_bottom_temp.nc")
nc_daily_dir     <- here::here("data-raw")


# -------------------------------------------------------------------
# 3. Core Extraction Function
# -------------------------------------------------------------------
# Wraps the spatial intersection logic so it can be applied identically 
# to both data sources.

process_year_suitability <- function(bt_daily, current_year) {
  
  map_dfr(names(habitat_v6), function(sp) {
    
    poly <- habitat_v6[[sp]]
    th <- thermal_niche |> dplyr::filter(COMNAME == sp)
    
    if (nrow(th) == 0 || is.null(poly)) return(NULL)
    
    tmin <- th$tmin[1]
    tmax <- th$tmax[1]
    
    if (sf::st_crs(poly)$epsg != 4326) {
      poly <- sf::st_transform(poly, 4326)
    }
    
    bt_crop <- terra::crop(bt_daily, terra::ext(poly))
    
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
      perc_within_hist = perc_within_hist,
      tmin_used        = tmin,
      tmax_used        = tmax
    )
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
  
  results_extended[[as.character(year)]] <- process_year_suitability(bt_daily, year)
  
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
  results_recent[[as.character(year)]] <- process_year_suitability(bt_daily, year)
  
  rm(bt_daily)
  gc()
}


# -------------------------------------------------------------------
# 6. Finalize and Save
# -------------------------------------------------------------------

# Concatenate the two lists of dataframes into one single list first
all_results <- c(results_extended, results_recent)

# Now bind them vertically into one neat dataframe
indicator_results_df <- bind_rows(all_results) |>
  arrange(species, year)

message("\nSuccessfully calculated daily historic thermal habitat percentage for ", 
        length(unique(indicator_results_df$species)), " species.")
message("Time series span: ", min(indicator_results_df$year), " - ", max(indicator_results_df$year))

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)
