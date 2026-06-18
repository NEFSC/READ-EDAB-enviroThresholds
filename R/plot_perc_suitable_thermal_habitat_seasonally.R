# plot_perc_suitable_thermal_habitat_seasonally.R
#
# Purpose: Visualize the time series of the seasonal `perc_within_hist` indicator 
#          for each NEFMC-managed species. This metric represents the percentage 
#          of a species' seasonal historic habitat that featured suitable bottom 
#          temperatures in a given year, split by Winter, Spring, Summer, and Fall.
#
# Output:
#   Individual Plots : images/indicators/perc_within_hist_seasonally/<species>.png
#   Faceted Summary  : images/indicators/perc_within_hist_seasonally/all_species_summary.png
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

dir_images <- here::here("images/indicators/perc_within_hist_seasonally")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

data_file <- here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")

if (!file.exists(data_file)) {
  stop("Indicator data not found. Run get_perc_suitable_thermal_habitat_seasonally.R first.")
}

indicators <- readRDS(data_file) |>
  # Enforce chronological seasonal order rather than alphabetical
  mutate(season = factor(season, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

# Define a consistent seasonal color palette
season_colors <- c(
  "WINTER" = "#2C7BB6", # Deep Blue
  "SPRING" = "#ABD9E9", # Light Blue
  "SUMMER" = "#D7191C", # Red
  "FALL"   = "#FDAE61"  # Orange
)


# -------------------------------------------------------------------
# 3. Generate Individual Plots
# -------------------------------------------------------------------

species_list <- unique(indicators$species)

message("Generating individual seasonal time series plots for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  # Filter data for the current species
  df_sp <- indicators |> 
    filter(species == sp) |> 
    arrange(season, year)
  
  # Extract thermal limits for the subtitle
  tmin <- unique(df_sp$tmin_used)[1]
  tmax <- unique(df_sp$tmax_used)[1]
  
  p <- ggplot(df_sp, aes(x = year, y = perc_within_hist, color = season)) +
    # Add a subtle trend line to help visually smooth year-to-year noise
    geom_smooth(method = "loess", se = FALSE, alpha = 0.5, linewidth = 1.2, span = 0.3) +
    # Main time series line and points
    geom_line(linewidth = 0.6, alpha = 0.8) +
    geom_point(size = 1.2) +
    
    # Use 2x2 facet to prevent overlap and make trends clear
    facet_wrap(~season, ncol = 2) +
    
    scale_color_manual(values = season_colors) +
    # Lock y-axis to 0-100 since it is a true percentage
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_x_continuous(breaks = seq(min(df_sp$year, na.rm=TRUE), max(df_sp$year, na.rm=TRUE), by = 10)) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " \u2014 Seasonal Thermal Suitability"),
      subtitle = paste0("Thermal Niche: ", round(tmin, 1), "\u00B0C to ", round(tmax, 1), "\u00B0C"),
      x = "Year",
      y = "Suitable Historic Habitat (%)",
      caption = "Percentage of available habitat-days within the fixed seasonal V6 historic envelopes."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40", size = 10),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none", # Legend is redundant due to facet titles
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5)
    )
  
  # Format filename (replace spaces with underscores)
  file_name <- file.path(dir_images, paste0(gsub(" ", "_", sp), "_perc_within_hist_seasonally.png"))
  
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
})

message("Individual seasonal plots saved to: ", dir_images)


# -------------------------------------------------------------------
# 4. Generate Faceted Summary Plot
# -------------------------------------------------------------------

message("Generating all-species faceted summary plot...")

p_facet <- ggplot(indicators, aes(x = year, y = perc_within_hist, color = season)) +
  # Use raw lines lightly in the background, overlaid with bolder LOESS smooths
  geom_line(linewidth = 0.2, alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8, span = 0.4) +
  
  scale_color_manual(values = season_colors, name = "Season") +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  
  facet_wrap(~species, ncol = 5) +
  
  labs(
    title = "Seasonal Thermal Habitat Suitability by Species",
    subtitle = "Solid lines indicate LOESS smoothed trends. Faint lines indicate raw annual data.",
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

# Save large facet plot (adjust width/height depending on number of species)
facet_file <- file.path(dir_images, "ALL_SPECIES_perc_within_hist_seasonally_summary.png")
ggsave(facet_file, plot = p_facet, width = 15, height = 12, dpi = 300)

message("Faceted summary plot saved to: ", facet_file)