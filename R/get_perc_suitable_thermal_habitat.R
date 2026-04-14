# get_perc_suitable_thermal_habitat.R
#
# Purpose: Calculate the `perc_within_hist` indicator for NEFMC-managed species.
#          This script takes the fixed historic habitat envelopes defined in V6 
#          (based on all-time survey strata presence) and calculates what 
#          percentage of the available "habitat-days" within that fixed 
#          historic footprint fell within the species' thermal niche in a given year.
#
# Logic:   1. Loads daily GLORYS bottom temperature NetCDF files (multi-layer).
#          2. Extracts all daily pixel values within the fixed V6 historic polygon.
#          3. Evaluates every pixel on every day against tmin and tmax.
#          4. Calculates the overall percentage of suitable pixel-days for the year.
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
library(exactextractr) # Highly optimized raster-polygon extractions
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_output <- here::here("data/indicators")
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Inputs
# -------------------------------------------------------------------

# Load V6 Historic Habitat (List of sf polygons)
# This represents the fixed geographic baseline for the perc_within_hist metric
habitat_v6 <- readRDS(here::here("data/historic_habitat_V6/historic_habitat_V6.rds"))

# Load Thermal Niche Definitions
thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

# Define GLORYS files
nc_path <- "~/EDAB_Datasets/GLORYS/GLORYS_daily"
nc_files <- list.files(nc_path, pattern = "GLORYS_daily_BottomTemp_\\d{4}\\.nc$", full.names = TRUE)

if (length(nc_files) == 0) {
  stop("No GLORYS .nc files found in the specified directory.")
}


# -------------------------------------------------------------------
# 3. Core Processing Loop
# -------------------------------------------------------------------

results_list <- list()

for (f in nc_files) {
  
  # Extract year from the filename
  year <- as.numeric(stringr::str_extract(basename(f), "\\d{4}"))
  message("Processing year: ", year, " (", basename(f), ")")
  
  # Load daily data (365 or 366 layers)
  bt_daily <- terra::rast(f)
  
  # Loop over all species with a V6 habitat polygon
  year_results <- map_dfr(names(habitat_v6), function(sp) {
    
    poly <- habitat_v6[[sp]]
    th <- thermal_niche |> dplyr::filter(COMNAME == sp)
    
    # Skip if missing polygon or thermal niche data
    if (nrow(th) == 0 || is.null(poly)) {
      return(NULL)
    }
    
    tmin <- th$tmin[1]
    tmax <- th$tmax[1]
    
    # Ensure CRS matches GLORYS (EPSG:4326)
    if (sf::st_crs(poly)$epsg != 4326) {
      poly <- sf::st_transform(poly, 4326)
    }
    
    # Crop the multi-layer daily raster to the historic habitat bounding box
    bt_crop <- terra::crop(bt_daily, terra::ext(poly))
    
    # Extract pixel values for ALL days
    # Returns a dataframe with 'coverage_fraction' and one column per day (layer)
    extraction <- exactextractr::exact_extract(
      x = bt_crop, 
      y = poly, 
      progress = FALSE
    )[[1]]
    
    # Reshape the data to evaluate suitability per pixel, per day
    df_long <- extraction |>
      tidyr::pivot_longer(
        cols = -coverage_fraction,
        names_to = "day_layer",
        values_to = "temp"
      ) |>
      # Drop NA temperatures (e.g., pixels that fall on land)
      dplyr::filter(!is.na(temp)) |>
      dplyr::mutate(
        is_suitable   = temp >= tmin & temp <= tmax,
        weighted_area = is_suitable * coverage_fraction
      )
    
    # total_coverage is now the sum of (fractional pixels * days)
    total_coverage <- sum(df_long$coverage_fraction)
    
    if (total_coverage == 0) {
      perc_within_hist <- NA_real_
    } else {
      perc_within_hist <- (sum(df_long$weighted_area) / total_coverage) * 100
    }
    
    # Return as a tidy row
    tibble(
      species          = sp,
      year             = year,
      perc_within_hist = perc_within_hist,
      tmin_used        = tmin,
      tmax_used        = tmax
    )
  })
  
  # Store the dataframe for this year
  results_list[[as.character(year)]] <- year_results
  
  # Free up memory before the next NetCDF file loads
  rm(bt_daily, year_results)
  gc()
}

# -------------------------------------------------------------------
# 4. Finalize and Save
# -------------------------------------------------------------------

indicator_results_df <- bind_rows(results_list)

message("Successfully calculated daily historic thermal habitat percentage for ", 
        length(unique(indicator_results_df$species)), " species across ", 
        length(results_list), " years.")

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)
