# get_perc_suitable_thermal_habitat.R
#
# Purpose: Calculate the `perc_within_hist` indicator for NEFMC-managed species.
#          This script takes the fixed historic habitat envelopes defined in V6 
#          (based on all-time survey strata presence) and calculates what 
#          percentage of the available "habitat-days" within that fixed 
#          historic footprint fell within the species' thermal niche in a given year.
#
# Logic:   1. Loads the extended (1959-2021) GLORYS/ROMS bottom temp NetCDF.
#          2. Dynamically subsets the 60+ year raster into single-year chunks.
#          3. Extracts all daily pixel values within the fixed V6 historic polygon.
#          4. Evaluates every pixel on every day against tmin and tmax.
#          5. Calculates the overall percentage of suitable pixel-days for the year.
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
habitat_v6 <- readRDS(here::here("data/historic_habitat_V6/historic_habitat_V6.rds"))

# Load Thermal Niche Definitions
thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

# Define Extended GLORYS/ROMS NetCDF file
# Note: Download this from ERDDAP as a .nc file, not a CSV/HTML table!
nc_file <- here::here("data-raw/duPontavice_bottom_temp.nc")

if (!file.exists(nc_file)) {
  stop("Extended time series NetCDF not found at specified path.")
}

# Load the multi-decade raster
# terra handles this lazily, so it won't crash your RAM
bt_all <- terra::rast(nc_file)

# Extract time attributes from the raster layers
layer_times <- terra::time(bt_all)
if (any(is.na(layer_times))) {
  stop("Raster time dimension is missing or malformed.")
}

# Identify all unique years in the dataset
layer_years <- as.numeric(format(layer_times, "%Y"))
unique_years <- sort(unique(layer_years))


# -------------------------------------------------------------------
# 3. Core Processing Loop
# -------------------------------------------------------------------

results_list <- list()

# Loop through each year sequentially to manage memory
for (year in unique_years) {
  
  message("Processing year: ", year)
  
  # Identify which layers belong to the current year
  year_indices <- which(layer_years == year)
  
  # Subset the massive raster down to just the 365/366 layers for this year
  bt_daily <- terra::subset(bt_all, year_indices)
  
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
    
    # Ensure CRS matches the NetCDF (typically EPSG:4326 for ERDDAP)
    if (sf::st_crs(poly)$epsg != 4326) {
      poly <- sf::st_transform(poly, 4326)
    }
    
    # Crop the daily raster to the historic habitat bounding box
    bt_crop <- terra::crop(bt_daily, terra::ext(poly))
    
    # Extract pixel values for ALL days in the year subset
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
  
  # Free up memory before subsetting the next year
  rm(bt_daily, year_indices, year_results)
  gc()
}


# -------------------------------------------------------------------
# 4. Finalize and Save
# -------------------------------------------------------------------

indicator_results_df <- bind_rows(results_list)

message("Successfully calculated daily historic thermal habitat percentage for ", 
        length(unique(indicator_results_df$species)), " species across ", 
        length(unique_years), " years.")

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)
