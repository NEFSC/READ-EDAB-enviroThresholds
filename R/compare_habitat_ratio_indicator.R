# compare_habitat_ratio_indicator.R
#
# Purpose: Ground-truth the thermal suitability indicator by comparing it to the 
#          empirical survey observation ratio (Annual Realized Habitat Area / 
#          Total Historic Habitat Area) for Issue #7.
#
# Logic:   1. Calculate total V6 historic area per species.
#          2. Calculate annual realized area (strata with >= 3 obs/year).
#          3. Calculate empirical ratio = Annual Area / Total Historic Area.
#          4. Merge with perc_suitable_thermal_habitat.
#          5. Calculate correlation stats and generate a 1:1 scatterplot (0-100 axes).
#          6. Generate a dual-line time series plot for temporal comparison (0-100 y-axis).
#
# Output:
#   CSV   : data/validation/habitat_vs_temperature_correlation.csv
#   Plots : images/validation/habitat_ratio_vs_thermal_scatterplot.png
#   Plots : images/validation/habitat_ratio_vs_thermal_timeseries.png
#
# Dependencies: tidyverse, sf, here, broom

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)
library(broom)

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_val_data <- here::here("data/validation")
dir_val_img  <- here::here("images/validation")

if (!dir.exists(dir_val_data)) dir.create(dir_val_data, recursive = TRUE)
if (!dir.exists(dir_val_img))  dir.create(dir_val_img, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

message("Loading spatial, survey, and indicator data...")

# --- Survey Strata ---
strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
strata_sf <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>       
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  mutate(
    strata_uid = row_number(),
    # Pre-calculate area for each stratum in km2
    strata_area_km2 = as.numeric(sf::st_area(geometry)) / 1e6
  )
Sys.unsetenv("SHAPE_RESTORE_SHX")

# --- Historic V6 Envelopes ---
historic_v6 <- readRDS(here::here("data/historic_habitat_V6/historic_habitat_V6.rds"))

# --- Thermal Suitability Indicator ---
thermal_ind <- readRDS(here::here("data/indicators/perc_suitable_thermal_habitat.rds")) |>
  select(species, year, perc_within_hist)

# --- Survey Observations ---
survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat, inshore, by = join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species_list <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed)) |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME)

obs_clean <- survdat |>
  inner_join(species_list, by = "SVSPP") |>
  filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
  mutate(
    LAT  = as.numeric(LAT), 
    LON  = as.numeric(LON),
    year = as.numeric(YEAR)
  ) |>
  distinct(COMNAME, year, CRUISE6, STATION, LAT, LON) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)


# -------------------------------------------------------------------
# 3. Calculate Spatial Ratios
# -------------------------------------------------------------------

message("Calculating spatial habitat ratios...")

# A. Calculate total historic area per species
total_areas <- map_dfr(names(historic_v6), function(sp) {
  poly <- historic_v6[[sp]]
  tibble(
    species = sp,
    total_v6_area_km2 = as.numeric(sum(sf::st_area(poly))) / 1e6
  )
})

# B. Calculate annual empirical area per species
obs_with_strata <- sf::st_join(obs_clean, strata_sf)

annual_areas <- obs_with_strata |>
  sf::st_drop_geometry() |>
  filter(!is.na(strata_uid)) |>
  group_by(COMNAME, year, strata_uid, strata_area_km2) |>
  # Apply the V6 threshold: >= 3 obs in a single year
  summarise(n_stations = n_distinct(paste(CRUISE6, STATION)), .groups = "drop") |>
  filter(n_stations >= 3) |>
  # Sum the area of qualifying strata per species, per year
  group_by(COMNAME, year) |>
  summarise(annual_area_km2 = sum(strata_area_km2), .groups = "drop") |>
  rename(species = COMNAME)

# C. Combine and compute ratio
habitat_ratios <- total_areas |>
  # left_join ensures we keep species even if some years had 0 qualifying strata
  left_join(annual_areas, by = "species", relationship = "many-to-many") |>
  mutate(
    annual_area_km2 = replace_na(annual_area_km2, 0),
    empirical_habitat_ratio = (annual_area_km2 / total_v6_area_km2) * 100
  )


# -------------------------------------------------------------------
# 4. Merge with Thermal Indicator & Calculate Correlation
# -------------------------------------------------------------------

message("Merging with thermal indicator and calculating statistics...")

comparison_df <- habitat_ratios |>
  inner_join(thermal_ind, by = c("species", "year")) |>
  # Drop NAs to ensure clean math
  drop_na(empirical_habitat_ratio, perc_within_hist)

# Calculate Pearson correlation per species
correlation_stats <- comparison_df |>
  group_by(species) |>
  # Filter out species with too few data points to run a correlation
  filter(n() >= 5) |>
  summarise(
    correlation = cor(perc_within_hist, empirical_habitat_ratio, method = "pearson"),
    n_years = n(),
    .groups = "drop"
  ) |>
  arrange(desc(correlation))

# Save the stats table
write_csv(correlation_stats, file.path(dir_val_data, "habitat_vs_temperature_correlation.csv"))
message("Correlation statistics saved.")

# Join stats back to main df for plot annotations
comparison_df <- comparison_df |>
  left_join(correlation_stats, by = "species") |>
  mutate(
    label = paste0("r = ", round(correlation, 2))
  )

# -------------------------------------------------------------------
# 5. Visualization: Scatterplot (1:1 Comparison)
# -------------------------------------------------------------------

message("Generating 1:1 comparison scatterplot...")

p_scatter <- ggplot(comparison_df, aes(x = perc_within_hist, y = empirical_habitat_ratio)) +
  geom_point(alpha = 0.5, color = "darkblue", size = 1.5) +
  geom_smooth(method = "lm", color = "firebrick", fill = "grey80", linewidth = 0.8) +
  geom_text(
    aes(label = label), 
    x = -Inf, y = Inf, 
    hjust = -0.2, vjust = 1.5, 
    size = 3.5, fontface = "bold", color = "black",
    check_overlap = TRUE
  ) +
  facet_wrap(~species, ncol = 6) +
  scale_x_continuous(limits = c(0, 100)) + 
  scale_y_continuous(limits = c(0, 100)) + 
  labs(
    title = "Validation: Thermal Suitability vs. Empirical Habitat Area",
    subtitle = "Assessing if bottom temperature (% suitable habitat) correlates with realized spatial distribution (% historic area occupied).",
    x = "Thermal Indicator (% Suitable Habitat-Days)",
    y = "Empirical Survey Ratio (% Historic Area Occupied)",
    caption = "Data: NEFSC Bottom Trawl Survey & GLORYS/ROMS | r = Pearson correlation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 9),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.grid.minor = element_blank()
  )

file_scatter_plot <- file.path(dir_val_img, "habitat_ratio_vs_thermal_scatterplot.png")
ggsave(file_scatter_plot, plot = p_scatter, width = 16, height = 12, dpi = 300, bg = "white")

# -------------------------------------------------------------------
# 6. Visualization: Time Series Overlay
# -------------------------------------------------------------------

message("Generating time series comparison overlay...")

# Reshape the data so ggplot can easily draw two lines with a legend
ts_df <- comparison_df |>
  select(species, year, perc_within_hist, empirical_habitat_ratio) |>
  pivot_longer(
    cols = c(perc_within_hist, empirical_habitat_ratio),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = if_else(
      metric == "perc_within_hist", 
      "Thermal Suitability (%)", 
      "Empirical Habitat Ratio (%)"
    )
  )

p_timeseries <- ggplot(ts_df, aes(x = year, y = value, color = metric)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 0.8, alpha = 0.6) + 
  facet_wrap(~species, ncol = 6) + # Removed scales = "free_y"
  scale_y_continuous(limits = c(0, 100)) + # Standardized y-axis to strict 0-100%
  scale_color_manual(
    values = c(
      "Thermal Suitability (%)" = "firebrick",
      "Empirical Habitat Ratio (%)" = "steelblue"
    )
  ) +
  labs(
    title = "Validation: Thermal Suitability vs. Empirical Habitat Over Time",
    subtitle = "Comparing the temporal trends of the thermal indicator against the realized survey footprint.",
    x = "Year",
    y = "Percentage (%)",
    color = "Metric",
    caption = "Data: NEFSC Bottom Trawl Survey & GLORYS/ROMS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 15)),
    strip.text = element_text(face = "bold", size = 9),
    panel.border = element_rect(color = "grey70", fill = NA),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

file_ts_plot <- file.path(dir_val_img, "habitat_ratio_vs_thermal_timeseries.png")
ggsave(file_ts_plot, plot = p_timeseries, width = 16, height = 12, dpi = 300, bg = "white")

message("Time series plot saved to: ", file_ts_plot)
message("Validation script completed successfully.")