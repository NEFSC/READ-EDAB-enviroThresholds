# 02_calculate_thermal_habitat_anomaly.R
#
# Purpose: Calculate and visualize the Thermal Habitat Anomaly for each NEFMC species.
#
# Methodology: 1. Isolates the Spring and Fall `perc_within_hist` indicators.
#              2. Calculates the anomaly for each season by subtracting the value of  
#                 the first year (baseline) from all subsequent years.
#              3. Sums the Spring and Fall anomalies to create a combined Annual Anomaly.
#                 (A value of 0 means habitat is identical to the start of the time series).
#
# Output:
#   Individual Sp. Plots : images/indicators/thermal_habitat_anomaly/<species>_anomaly.png
#   Faceted Summary      : images/indicators/thermal_habitat_anomaly/ALL_SPECIES_anomaly_summary.png
#   Summary Table        : data/indicators/combined_spring_fall_anomaly.csv
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

dir_images <- here::here("images/indicators/thermal_habitat_anomaly")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)

dir_data <- here::here("data/indicators")
if (!dir.exists(dir_data)) dir.create(dir_data, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

data_file <- here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")

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
  arrange(species, season, year) |>
  group_by(species, season) |>
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
  group_by(species, year) |>
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
  arrange(species, year)

# Save this dataset for the Z-scoring script
data_out_file <- file.path(dir_data, "combined_spring_fall_anomaly.csv")
write_csv(annual_anomalies, data_out_file)
message("Saved combined anomaly data to: ", data_out_file)


# -------------------------------------------------------------------
# 5. Generate Individual Plots (Showing Seasonal + Combined)
# -------------------------------------------------------------------

species_list <- unique(annual_anomalies$species)
message("Generating individual anomaly plots for ", length(species_list), " species...")

# Determine global y-axis limits to keep plots consistent
max_anom <- max(abs(annual_anomalies$annual_anomaly), na.rm = TRUE)
y_limits <- c(-max_anom * 1.1, max_anom * 1.1)

walk(species_list, function(sp) {
  
  df_annual <- annual_anomalies |> filter(species == sp) |> arrange(year)
  df_seasonal <- seasonal_anomalies |> filter(species == sp) |> arrange(year)
  
  tmin <- unique(df_annual$tmin_used)[1]
  tmax <- unique(df_annual$tmax_used)[1]
  
  p_single <- ggplot() +
    # Draw a strong horizontal line at 0 (the baseline)
    geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    
    # 1. Plot the individual seasonal anomalies (thinner, colored lines)
    geom_line(data = df_seasonal, aes(x = year, y = season_anomaly, color = season), 
              linewidth = 0.6, alpha = 0.7) +
    
    # 2. Plot the combined annual anomaly (thick, black line)
    geom_line(data = df_annual, aes(x = year, y = annual_anomaly, linetype = "Combined Anomaly"), 
              color = "black", linewidth = 1.2, alpha = 0.9) +
    
    # Custom scales for legend mapping
    scale_color_manual(values = c("SPRING" = "#3182bd", "FALL" = "#e6550d"), name = "Seasonal Contribution") +
    scale_linetype_manual(values = c("Combined Anomaly" = "solid"), name = NULL) +
    
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(breaks = seq(min(df_annual$year, na.rm=TRUE), max(df_annual$year, na.rm=TRUE), by = 10)) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Thermal Habitat Anomaly"),
      subtitle = paste0("Spring & Fall Contributions | Thermal Niche: ", round(tmin, 1), "\u00B0C to ", round(tmax, 1), "\u00B0C"),
      x = "Year",
      y = "Habitat Anomaly (%)",
      caption = "Colored lines represent independent seasonal anomalies. The thick black line is the sum of both seasons."
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
  ggsave(file_name, plot = p_single, width = 8, height = 5, dpi = 300, bg = "white")
})

message("Individual plots saved to: ", dir_images)


# -------------------------------------------------------------------
# 6. Generate Faceted Summary Plot (Combined Only)
# -------------------------------------------------------------------

message("Generating all-species faceted summary plot...")

p_facet <- ggplot(annual_anomalies, aes(x = year, y = annual_anomaly)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dashed") +
  
  # Just the raw data line, no LOESS smoothing
  geom_line(linewidth = 0.6, color = "black") +
  
  scale_y_continuous(limits = y_limits) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  
  facet_wrap(~species, ncol = 5) +
  
  labs(
    title = "Combined Spring & Fall Thermal Habitat Anomaly by Species",
    subtitle = "Zero indicates no change from the start of the time series.",
    x = "Year",
    y = "Combined Habitat Anomaly (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.3)
  )

facet_file <- file.path(dir_images, "ALL_SPECIES_anomaly_summary.png")
ggsave(facet_file, plot = p_facet, width = 15, height = 12, dpi = 300, bg = "white")

message("Faceted summary plot saved to: ", facet_file)
message("Script complete.")