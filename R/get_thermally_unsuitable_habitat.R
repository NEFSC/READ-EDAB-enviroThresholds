# get_thermally_unsuitable_habitat.R
#
# Purpose: Calculate the percentage of historic habitat excluded for being 
#          "too hot" (> tmax) and "too cold" (< tmin) for NEFMC-managed species.
#
# Logic:   1. Process historical data (1959-2021) from the duPontavice NetCDF.
#          2. Process recent data (2022+) from individual GLORYS daily NetCDFs.
#          3. Bind the data into a continuous time series.
#          4. Export tabular CSV and generate time-series trend plots.
#
# Output:
#   RDS : data/indicators/unsuitable_thermal_habitat.rds
#   CSV : data/indicators/unsuitable_thermal_habitat.csv
#   PDF : data/indicators/unsuitable_thermal_habitat_plots.pdf
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

process_year_unsuitability <- function(bt_daily, current_year) {
  
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
        is_too_hot    = temp > tmax,
        is_too_cold   = temp < tmin,
        weighted_hot  = is_too_hot * coverage_fraction,
        weighted_cold = is_too_cold * coverage_fraction
      )
    
    total_coverage <- sum(df_long$coverage_fraction)
    
    if (total_coverage == 0) {
      perc_too_hot  <- NA_real_
      perc_too_cold <- NA_real_
    } else {
      perc_too_hot  <- (sum(df_long$weighted_hot) / total_coverage) * 100
      perc_too_cold <- (sum(df_long$weighted_cold) / total_coverage) * 100
    }
    
    tibble(
      species       = sp,
      year          = current_year,
      perc_too_hot  = perc_too_hot,
      perc_too_cold = perc_too_cold,
      tmin_used     = tmin,
      tmax_used     = tmax
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
  if (year > 2021) next 
  
  message("  Processing year: ", year)
  year_indices <- which(layer_years == year)
  bt_daily <- terra::subset(bt_all, year_indices)
  
  results_extended[[as.character(year)]] <- process_year_unsuitability(bt_daily, year)
  
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
  
  if (year < 2022) next 
  
  message("  Processing year: ", year, " (", basename(f), ")")
  
  bt_daily <- terra::rast(f)
  results_recent[[as.character(year)]] <- process_year_unsuitability(bt_daily, year)
  
  rm(bt_daily)
  gc()
}


# -------------------------------------------------------------------
# 6. Finalize and Save Data
# -------------------------------------------------------------------

all_results <- c(results_extended, results_recent)

indicator_results_df <- bind_rows(all_results) |>
  arrange(species, year) |>
  mutate(across(c(perc_too_hot, perc_too_cold), ~round(., 2)))

message("\nSuccessfully calculated thermal exclusion percentages for ", 
        length(unique(indicator_results_df$species)), " species.")

# Save RDS
rds_out <- file.path(dir_output, "unsuitable_thermal_habitat.rds")
saveRDS(indicator_results_df, rds_out)

# Save CSV (Tabular Format)
csv_out <- file.path(dir_output, "unsuitable_thermal_habitat.csv")
write_csv(indicator_results_df, csv_out)
message("Tabular data saved to: ", csv_out)


# -------------------------------------------------------------------
# 7. Generate Visualizations (with Significant Trend Lines)
# -------------------------------------------------------------------

message("Evaluating statistical significance of trends and generating plots...")

# Convert to long format for easier ggplot processing
plot_data_long <- indicator_results_df |>
  pivot_longer(
    cols = c(perc_too_hot, perc_too_cold),
    names_to = "Exclusion_Type",
    values_to = "Percent_Excluded"
  ) |>
  mutate(
    Exclusion_Type = case_when(
      Exclusion_Type == "perc_too_hot"  ~ "Too Hot (> tmax)",
      Exclusion_Type == "perc_too_cold" ~ "Too Cold (< tmin)"
    )
  )

# Calculate linear models and extract p-values to find significant trends (p < 0.05)
significant_trends <- plot_data_long |>
  group_by(species, Exclusion_Type) |>
  # Ensure there is enough variance/data to run a model safely
  filter(sum(!is.na(Percent_Excluded)) > 2) |>
  summarise(
    p_val = tryCatch({
      mod <- lm(Percent_Excluded ~ year)
      summary(mod)$coefficients[2,4] # Extract the p-value for the 'year' coefficient
    }, error = function(e) NA_real_),
    .groups = "drop"
  ) |>
  filter(!is.na(p_val) & p_val < 0.05)

# Join the significant flag back to the data so we can selectively draw lines
plot_data_significant <- plot_data_long |>
  inner_join(significant_trends, by = c("species", "Exclusion_Type"))

pdf_path <- file.path(dir_output, "unsuitable_thermal_habitat_plots.pdf")
pdf(pdf_path, width = 10, height = 8)

species_list <- unique(indicator_results_df$species)
chunk_size <- 6

for (i in seq(1, length(species_list), by = chunk_size)) {
  target_spp <- species_list[i:min((i + chunk_size - 1), length(species_list))]
  
  # Base dataset for the chunk
  chunk_data <- plot_data_long |> filter(species %in% target_spp)
  
  # Significant data only for the trend lines in this chunk
  chunk_sig_data <- plot_data_significant |> filter(species %in% target_spp)
  
  p <- ggplot() +
    # The actual data lines and points
    geom_line(data = chunk_data, aes(x = year, y = Percent_Excluded, color = Exclusion_Type), linewidth = 1) +
    geom_point(data = chunk_data, aes(x = year, y = Percent_Excluded, color = Exclusion_Type), size = 1.5, alpha = 0.7) +
    
    # Overlay the linear trend line ONLY for species/types with p < 0.05
    geom_smooth(data = chunk_sig_data, aes(x = year, y = Percent_Excluded, color = Exclusion_Type),
                method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
    
    scale_color_manual(values = c("Too Hot (> tmax)" = "#d73027", "Too Cold (< tmin)" = "#4575b4")) +
    facet_wrap(~species, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(
      title = "Thermal Habitat Exclusion Trends",
      subtitle = "Dashed lines represent statistically significant linear trends (p < 0.05)",
      x = "Year",
      y = "% Habitat Excluded",
      color = "Exclusion Type"
    ) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10)
    )
  
  print(p)
}

dev.off()
message("Plots saved to: ", pdf_path)