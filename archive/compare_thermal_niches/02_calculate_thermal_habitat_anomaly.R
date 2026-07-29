# archive/compare_thermal_niches/02_calculate_thermal_habitat_anomaly.R
#
# Purpose: Calculate and visualize the Thermal Habitat Anomaly for each NEFMC species
#          across multiple thermal niche scenarios.
#
# Methodology: 1. Isolates the Spring and Fall `perc_within_hist` indicators.
#              2. Calculates the anomaly for each season and scenario by subtracting 
#                 the value of the first year (baseline) from all subsequent years.
#              3. Sums the Spring and Fall anomalies to create a combined Annual Anomaly.
#                 (A value of 0 means habitat is identical to the start of the time series).
#
# Output:
#   Individual Sp. Plots : archive/compare_thermal_niches/images/thermal_habitat_anomaly/<species>_anomaly.png
#   Faceted Summary      : archive/compare_thermal_niches/images/thermal_habitat_anomaly/ALL_SPECIES_anomaly_summary.png
#   Summary Table        : archive/compare_thermal_niches/indicators/combined_spring_fall_anomaly.csv
#
# Dependencies: tidyverse, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_images <- here::here("archive/compare_thermal_niches/images/thermal_habitat_anomaly")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)

dir_data <- here::here("archive/compare_thermal_niches/indicators")
if (!dir.exists(dir_data)) dir.create(dir_data, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

data_file <- here::here("archive/compare_thermal_niches/indicators/perc_suitable_thermal_habitat_seasonally.rds")

if (!file.exists(data_file)) {
  stop("Indicator data not found. Run 01_get_perc_suitable_thermal_habitat_seasonally.R first.")
}

indicators <- readRDS(data_file) 


# -------------------------------------------------------------------
# 3. Calculate Seasonal Anomalies (Relative to Year 1)
# -------------------------------------------------------------------

message("Calculating seasonal anomalies relative to the first year of the time series...")

seasonal_anomalies <- indicators |>
  # Ensure we only process Spring and Fall, dropping any NAs
  filter(season %in% c("SPRING", "FALL"), !is.na(perc_within_hist)) |>
  # Group by scenario_id and source as well, to keep niches independent
  arrange(species, scenario_id, season, year) |>
  group_by(species, scenario_id, source, season) |>
  mutate(
    # The value of the very first year in the time series acts as the 0-point
    baseline_val = first(perc_within_hist),
    season_anomaly = perc_within_hist - baseline_val
  ) |>
  ungroup()


# -------------------------------------------------------------------
# 4. Calculate Combined Annual Anomaly (Spring + Fall)
# -------------------------------------------------------------------

message("Combining Spring and Fall anomalies into a single annual metric...")

annual_anomalies <- seasonal_anomalies |>
  group_by(species, scenario_id, source, year) |>
  summarise(
    n_seasons = n(),
    # Sum the anomalies of the two transitional seasons
    annual_anomaly = sum(season_anomaly, na.rm = TRUE),
    # Keep metadata for plotting
    tmin_used = first(tmin_used),
    tmax_used = first(tmax_used),
    .groups = "drop"
  ) |>
  # Ensure we only calculate the anomaly for years where both Spring and Fall data exist
  filter(n_seasons == 2) |>
  arrange(species, scenario_id, year)

# Save this dataset for downstream evaluation
data_out_file <- file.path(dir_data, "combined_spring_fall_anomaly.csv")
write_csv(annual_anomalies, data_out_file)
message("Saved combined anomaly data to: ", data_out_file)


# -------------------------------------------------------------------
# 5. Generate Individual Plots (Scenario Comparisons)
# -------------------------------------------------------------------

species_list <- unique(annual_anomalies$species)
message("Generating individual anomaly comparison plots for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  df_annual <- annual_anomalies |> filter(species == sp) |> arrange(year)
  
  # Determine SPECIES-SPECIFIC y-axis limits to keep 0 centered
  max_anom <- max(abs(df_annual$annual_anomaly), na.rm = TRUE)
  # Fallback just in case max_anom is 0 or NA
  if (is.na(max_anom) || max_anom == 0) max_anom <- 1 
  y_limits <- c(-max_anom * 1.1, max_anom * 1.1)
  
  # Ensure Survey_10_90 is plotted on top by making it a factor
  df_annual <- df_annual |>
    mutate(source = forcats::fct_relevel(source, "Survey_10_90"))
  
  p_single <- ggplot(df_annual, aes(x = year, y = annual_anomaly, color = source, group = scenario_id)) +
    # Draw a strong horizontal line at 0 (the baseline)
    geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    
    # Plot the combined annual anomaly for all scenarios
    geom_line(aes(linewidth = source == "Survey_10_90", alpha = source == "Survey_10_90")) +
    scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.5), guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.6), guide = "none") +
    
    # Now this uses the dynamic, species-specific limits!
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = seq(min(df_annual$year, na.rm=TRUE), max(df_annual$year, na.rm=TRUE), by = 10)) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Thermal Habitat Anomaly"),
      subtitle = "Comparing the Combined Annual Anomaly across candidate thermal niches",
      x = "Year",
      y = "Combined Habitat Anomaly (%)",
      color = "Thermal Niche Source"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_images, paste0(safe_name, "_anomaly.png"))
  ggsave(file_name, plot = p_single, width = 9, height = 6, dpi = 300, bg = "white")
})

message("Individual plots saved to: ", dir_images)


# -------------------------------------------------------------------
# 6. Generate Faceted Summary Plot
# -------------------------------------------------------------------

message("Generating all-species faceted summary plot...")

# Reorder factor for plotting priority
annual_anomalies <- annual_anomalies |>
  mutate(source = forcats::fct_relevel(source, "Survey_10_90"))

p_facet <- ggplot(annual_anomalies, aes(x = year, y = annual_anomaly, color = source, group = scenario_id)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") +
  
  geom_line(aes(linewidth = source == "Survey_10_90", alpha = source == "Survey_10_90")) +
  scale_linewidth_manual(values = c("TRUE" = 0.8, "FALSE" = 0.3), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.5), guide = "none") +
  
  scale_y_continuous(limits = y_limits) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  
  facet_wrap(~species, ncol = 5) +
  
  labs(
    title = "Combined Annual Thermal Habitat Anomaly by Species",
    subtitle = "Zero indicates no change from Year 1. Bold line represents the Survey 10-90th empirical method.",
    x = "Year",
    y = "Combined Habitat Anomaly (%)",
    color = "Thermal Niche Source"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.3),
    legend.position = "bottom"
  )

facet_file <- file.path(dir_images, "ALL_SPECIES_anomaly_summary.png")
ggsave(facet_file, plot = p_facet, width = 16, height = 12, dpi = 300, bg = "white")

message("Faceted summary plot saved to: ", facet_file)
message("Script complete.")