# archive/compare_thermal_niches/02_calculate_thermal_habitat_anomaly.R
#
# Purpose: Calculate and visualize the Thermal Habitat Anomaly for all managed species
#          ACROSS MULTIPLE THERMAL NICHE SCENARIOS.
#
# Methodology: 1. Isolates the Spring and Fall `perc_within_hist` indicators.
#              2. Calculates the anomaly for each season and scenario by subtracting 
#                 the value of the first year (baseline) from all subsequent years.
#              3. Sums the Spring and Fall anomalies to create a combined Annual Anomaly.
#                 (A value of 0 means habitat is identical to the start of the time series).
#
# Outputs: 
#   Archive (Comparison) : indicators/combined_spring_fall_anomaly.csv & images (Scenario comparisons)
#   Main (Production)    : data/indicators/combined_spring_fall_anomaly.csv & images (Seasonal contributions)
#
# Dependencies: tidyverse, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(here)


# -------------------------------------------------------------------
# 1. Output directories (Dual Routing)
# -------------------------------------------------------------------

# Archive Directories
dir_archive_data   <- here::here("archive/compare_thermal_niches/indicators")
dir_archive_images <- here::here("archive/compare_thermal_niches/images/thermal_habitat_anomaly")
if (!dir.exists(dir_archive_data))   dir.create(dir_archive_data, recursive = TRUE)
if (!dir.exists(dir_archive_images)) dir.create(dir_archive_images, recursive = TRUE)

# Main Production Directories
dir_main_data   <- here::here("data/indicators")
dir_main_images <- here::here("images/indicators/thermal_habitat_anomaly")
if (!dir.exists(dir_main_data))   dir.create(dir_main_data, recursive = TRUE)
if (!dir.exists(dir_main_images)) dir.create(dir_main_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Data (From Archive Output)
# -------------------------------------------------------------------

data_file <- file.path(dir_archive_data, "perc_suitable_thermal_habitat_seasonally.rds")

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


# -------------------------------------------------------------------
# 5. Save Dual Outputs (Data)
# -------------------------------------------------------------------

# Output A: Archive (All Scenarios)
data_out_archive <- file.path(dir_archive_data, "combined_spring_fall_anomaly.csv")
write_csv(annual_anomalies, data_out_archive)
message("Saved full comparison anomaly dataset to: ", data_out_archive)

# Output B: Main Production (Survey 10-90th ONLY)
annual_anomalies_main <- annual_anomalies |>
  filter(source == "Survey_10_90") |>
  select(species, year, n_seasons, annual_anomaly, tmin_used, tmax_used)

data_out_main <- file.path(dir_main_data, "combined_spring_fall_anomaly.csv")
write_csv(annual_anomalies_main, data_out_main)
message("Saved clean production anomaly dataset to: ", data_out_main)


# -------------------------------------------------------------------
# 6. Generate Archive Plots (Scenario Comparisons)
# -------------------------------------------------------------------

species_list <- unique(annual_anomalies$species)
message("\n--- Generating ARCHIVE comparison plots ---")

walk(species_list, function(sp) {
  
  df_annual <- annual_anomalies |> filter(species == sp) |> arrange(year)
  
  # Dynamic y-axis for the specific species
  max_anom <- max(abs(df_annual$annual_anomaly), na.rm = TRUE)
  if (is.na(max_anom) || max_anom == 0) max_anom <- 1 
  y_limits <- c(-max_anom * 1.1, max_anom * 1.1)
  
  # Ensure Survey_10_90 is plotted on top by making it a factor
  df_annual <- df_annual |>
    mutate(source = forcats::fct_relevel(source, "Survey_10_90"))
  
  p_single <- ggplot(df_annual, aes(x = year, y = annual_anomaly, color = source, group = scenario_id)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    geom_line(aes(linewidth = source == "Survey_10_90", alpha = source == "Survey_10_90")) +
    scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.5), guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.6), guide = "none") +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = seq(min(df_annual$year, na.rm=TRUE), max(df_annual$year, na.rm=TRUE), by = 10)) +
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Thermal Habitat Anomaly"),
      subtitle = "Comparing the Combined Annual Anomaly across candidate thermal niches",
      x = "Year", y = "Combined Habitat Anomaly (%)", color = "Thermal Niche Source"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom", legend.box = "horizontal",
      panel.grid.minor = element_blank(), panel.border = element_rect(color = "grey80", fill = NA)
    )
  
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_archive_images, paste0(safe_name, "_anomaly_comparison.png"))
  ggsave(file_name, plot = p_single, width = 9, height = 6, dpi = 300, bg = "white")
})


# -------------------------------------------------------------------
# 7. Generate Archive Faceted Summary Plot
# -------------------------------------------------------------------

df_facet_archive <- annual_anomalies |> mutate(source = forcats::fct_relevel(source, "Survey_10_90"))
max_global_archive <- max(abs(df_facet_archive$annual_anomaly), na.rm = TRUE)

p_facet_archive <- ggplot(df_facet_archive, aes(x = year, y = annual_anomaly, color = source, group = scenario_id)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(linewidth = source == "Survey_10_90", alpha = source == "Survey_10_90")) +
  scale_linewidth_manual(values = c("TRUE" = 0.8, "FALSE" = 0.3), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.5), guide = "none") +
  scale_y_continuous(limits = c(-max_global_archive * 1.1, max_global_archive * 1.1)) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  facet_wrap(~species, ncol = 5) +
  labs(
    title = "Combined Annual Thermal Habitat Anomaly by Species",
    subtitle = "Zero indicates no change from Year 1. Bold line represents the Survey 10-90th empirical method.",
    x = "Year", y = "Combined Habitat Anomaly (%)", color = "Thermal Niche Source"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8), panel.border = element_rect(color = "grey80", fill = NA), legend.position = "bottom")

ggsave(file.path(dir_archive_images, "ALL_SPECIES_anomaly_comparison_summary.png"), plot = p_facet_archive, width = 16, height = 12, dpi = 300, bg = "white")


# -------------------------------------------------------------------
# 8. Generate Main Production Plots (Seasonal Contributions)
# -------------------------------------------------------------------

message("\n--- Generating MAIN production plots (Survey_10_90 only) ---")

seasonal_anomalies_main <- seasonal_anomalies |> filter(source == "Survey_10_90")

walk(species_list, function(sp) {
  
  df_annual   <- annual_anomalies_main |> filter(species == sp) |> arrange(year)
  df_seasonal <- seasonal_anomalies_main |> filter(species == sp) |> arrange(year)
  
  if(nrow(df_annual) == 0) return(NULL)
  
  tmin <- unique(df_annual$tmin_used)[1]
  tmax <- unique(df_annual$tmax_used)[1]
  
  # Dynamic y-axis for the specific species
  max_anom <- max(abs(df_annual$annual_anomaly), na.rm = TRUE)
  if (is.na(max_anom) || max_anom == 0) max_anom <- 1 
  y_limits <- c(-max_anom * 1.1, max_anom * 1.1)
  
  p_single_main <- ggplot() +
    geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    geom_line(data = df_seasonal, aes(x = year, y = season_anomaly, color = season), linewidth = 0.6, alpha = 0.7) +
    geom_line(data = df_annual, aes(x = year, y = annual_anomaly, linetype = "Combined Anomaly"), color = "black", linewidth = 1.2, alpha = 0.9) +
    scale_color_manual(values = c("SPRING" = "#3182bd", "FALL" = "#e6550d"), name = "Seasonal Contribution") +
    scale_linetype_manual(values = c("Combined Anomaly" = "solid"), name = NULL) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = seq(min(df_annual$year, na.rm=TRUE), max(df_annual$year, na.rm=TRUE), by = 10)) +
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Thermal Habitat Anomaly"),
      subtitle = paste0("Spring & Fall Contributions | Thermal Niche: ", round(tmin, 1), "\u00B0C to ", round(tmax, 1), "\u00B0C"),
      x = "Year", y = "Habitat Anomaly (%)",
      caption = "Colored lines represent independent seasonal anomalies. The thick black line is the sum of both seasons."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom", legend.box = "horizontal",
      panel.grid.minor = element_blank(), panel.border = element_rect(color = "grey80", fill = NA)
    )
  
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  ggsave(file.path(dir_main_images, paste0(safe_name, "_anomaly.png")), plot = p_single_main, width = 8, height = 5, dpi = 300, bg = "white")
})


# -------------------------------------------------------------------
# 9. Generate Main Faceted Summary Plot
# -------------------------------------------------------------------

max_global_main <- max(abs(annual_anomalies_main$annual_anomaly), na.rm = TRUE)

p_facet_main <- ggplot(annual_anomalies_main, aes(x = year, y = annual_anomaly)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") +
  geom_line(linewidth = 0.6, color = "black") +
  scale_y_continuous(limits = c(-max_global_main * 1.1, max_global_main * 1.1)) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  facet_wrap(~species, ncol = 5) +
  labs(
    title = "Combined Spring & Fall Thermal Habitat Anomaly by Species",
    subtitle = "Zero indicates no change from the start of the time series.",
    x = "Year", y = "Combined Habitat Anomaly (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8), panel.border = element_rect(color = "grey80", fill = NA))

ggsave(file.path(dir_main_images, "ALL_SPECIES_anomaly_summary.png"), plot = p_facet_main, width = 15, height = 12, dpi = 300, bg = "white")

message("\nScript complete. Dual outputs routed successfully.")