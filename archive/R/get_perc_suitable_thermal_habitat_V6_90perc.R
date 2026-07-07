# get_perc_suitable_thermal_habitat_V6_90perc.R
#
# Purpose: Calculate the `perc_within_hist` indicator for NEFMC-managed species.
#          Evaluates the V6_90perc historic habitat envelopes (which dynamically 
#          retain 90% of historical observations) against daily GLORYS bottom 
#          temperatures.
#
# Output:
#   RDS : data/indicators/perc_suitable_thermal_habitat_V6_90perc.rds
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

# Load V6 90% Historic Habitat 
habitat_v6_90 <- readRDS(here::here("data/historic_habitat_V6_90perc/historic_habitat_V6_90perc.rds"))

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
  
  year <- as.numeric(stringr::str_extract(basename(f), "\\d{4}"))
  message("Processing year: ", year, " (", basename(f), ")")
  
  bt_daily <- terra::rast(f)
  
  year_results <- map_dfr(names(habitat_v6_90), function(sp) {
    
    poly <- habitat_v6_90[[sp]]
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
      year             = year,
      perc_within_hist = perc_within_hist,
      tmin_used        = tmin,
      tmax_used        = tmax
    )
  })
  
  results_list[[as.character(year)]] <- year_results
  
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

out_file <- file.path(dir_output, "perc_suitable_thermal_habitat_V6_90perc.rds")
saveRDS(indicator_results_df, out_file)
message("Results saved to: ", out_file)