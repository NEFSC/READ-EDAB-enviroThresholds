# plot_perc_suitable_thermal_habitat_one_season.R
#
# Purpose: Visualize the time series of the `perc_within_hist` indicator 
#          specifically for the season exhibiting the GREATEST CHANGE over the 
#          time series for each NEFMC-managed species.
#
# Methodology: "Greatest change" is defined as the season with the highest 
#              absolute linear trend (slope) in suitable habitat over time.
#
# Output:
#   Individual Sp. Plots : images/indicators/perc_within_hist_greatest_change/<species>_greatest_change_season.png
#   Faceted Summary      : images/indicators/perc_within_hist_greatest_change/ALL_SPECIES_greatest_change_summary.png
#   Summary Table        : data/indicators/greatest_change_seasonal_suitability.csv
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

dir_images <- here::here("images/indicators/perc_within_hist_greatest_change")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)

dir_data <- here::here("data/indicators")
if (!dir.exists(dir_data)) dir.create(dir_data, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

data_file <- here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")

if (!file.exists(data_file)) {
  stop("Indicator data not found. Run get_perc_suitable_thermal_habitat_seasonally.R first.")
}

indicators <- readRDS(data_file) |>
  # Enforce chronological seasonal order
  mutate(season = factor(season, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

# Define a consistent seasonal color palette
season_colors <- c(
  "WINTER" = "#2C7BB6", # Deep Blue
  "SPRING" = "#ABD9E9", # Light Blue
  "SUMMER" = "#D7191C", # Red
  "FALL"   = "#FDAE61"  # Orange
)


# -------------------------------------------------------------------
# 3. Identify Season of Greatest Change per Species
# -------------------------------------------------------------------

message("Calculating the season with the greatest linear trend for each species...")

# Find the season with the maximum absolute slope for each species
greatest_change_seasons <- indicators |>
  filter(!is.na(perc_within_hist)) |>
  group_by(species, season) |>
  summarize(
    # Fit a linear model to find the slope of change over the years
    trend_slope = abs(coef(lm(perc_within_hist ~ year))[2]),
    .groups = "drop"
  ) |>
  group_by(species) |>
  # Select the single season with the highest absolute slope
  slice_max(order_by = trend_slope, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(species, selected_season = season, absolute_trend = trend_slope)

# Filter the main dataset to ONLY include the selected season for each species
indicators_greatest_change <- indicators |>
  inner_join(greatest_change_seasons, by = c("species", "season" = "selected_season"))

# Save this isolated dataset so your scoring script can easily pick it up later
data_out_file <- file.path(dir_data, "greatest_change_seasonal_suitability.csv")
write_csv(indicators_greatest_change, data_out_file)
message("Saved greatest change data to: ", data_out_file)


# -------------------------------------------------------------------
# 4. Generate Individual Plots
# -------------------------------------------------------------------

species_list <- unique(indicators_greatest_change$species)
message("Generating individual plots for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  # Filter data for the current species
  df_sp <- indicators_greatest_change |> 
    filter(species == sp) |> 
    arrange(year)
  
  # Extract metadata for titles
  tmin <- unique(df_sp$tmin_used)[1]
  tmax <- unique(df_sp$tmax_used)[1]
  target_season <- unique(df_sp$season)[1]
  
  p_single <- ggplot(df_sp, aes(x = year, y = perc_within_hist, color = season)) +
    geom_smooth(method = "loess", se = FALSE, alpha = 0.5, linewidth = 1.2, span = 0.3) +
    geom_line(linewidth = 0.8, alpha = 0.8) +
    geom_point(size = 1.5) +
    scale_color_manual(values = season_colors) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_x_continuous(breaks = seq(min(df_sp$year, na.rm=TRUE), max(df_sp$year, na.rm=TRUE), by = 10)) +
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Greatest Change Season (", target_season, ")"),
      subtitle = paste0("Thermal Niche: ", round(tmin, 1), "\u00B0C to ", round(tmax, 1), "\u00B0C"),
      x = "Year",
      y = "Suitable Historic Habitat (%)",
      caption = "Percentage of available habitat-days within the fixed seasonal V6 historic envelopes."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40", size = 10),
      legend.position = "none", # Color implies season, explicitly stated in title
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
    )
  
  # Save Plot
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_images, paste0(safe_name, "_greatest_change_season.png"))
  ggsave(file_name, plot = p_single, width = 8, height = 5, dpi = 300, bg = "white")
})

message("Individual plots saved to: ", dir_images)


# -------------------------------------------------------------------
# 5. Generate Faceted Summary Plot
# -------------------------------------------------------------------

message("Generating all-species faceted summary plot...")

p_facet <- ggplot(indicators_greatest_change, aes(x = year, y = perc_within_hist, color = season)) +
  geom_line(linewidth = 0.3, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1, span = 0.4) +
  
  scale_color_manual(values = season_colors, name = "Selected Season\n(Greatest Change)") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  
  facet_wrap(~species, ncol = 5) +
  
  labs(
    title = "Season of Greatest Thermal Habitat Change by Species",
    subtitle = "Showing only the season with the highest absolute linear trend in habitat suitability.",
    x = "Year",
    y = "Suitable Historic Habitat (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.3)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))

facet_file <- file.path(dir_images, "ALL_SPECIES_greatest_change_summary.png")
ggsave(facet_file, plot = p_facet, width = 15, height = 12, dpi = 300, bg = "white")

message("Faceted summary plot saved to: ", facet_file)
message("Script complete.")