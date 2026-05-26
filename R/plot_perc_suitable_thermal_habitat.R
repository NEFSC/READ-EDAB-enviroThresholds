# plot_perc_suitable_thermal_habitat.R
#
# Purpose: Visualize the time series of the `perc_within_hist` indicator for 
#          each NEFMC-managed species. This metric represents the percentage 
#          of a species' fixed historic habitat that featured suitable bottom 
#          temperatures in a given year.
#
# Output:
#   Individual Plots : images/indicators/perc_within_hist/<species>.png
#   Faceted Summary  : images/indicators/perc_within_hist/all_species_summary.png
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

dir_images <- here::here("images/indicators/perc_within_hist")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Data
# -------------------------------------------------------------------

data_file <- here::here("data/indicators/perc_suitable_thermal_habitat.rds")

if (!file.exists(data_file)) {
  stop("Indicator data not found. Run get_perc_suitable_thermal_habitat.R first.")
}

indicators <- readRDS(data_file)


# -------------------------------------------------------------------
# 3. Generate Individual Plots
# -------------------------------------------------------------------

species_list <- unique(indicators$species)

message("Generating individual time series plots for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  # Filter data for the current species
  df_sp <- indicators |> 
    filter(species == sp) |> 
    arrange(year)
  
  # Extract thermal limits for the subtitle
  tmin <- unique(df_sp$tmin_used)
  tmax <- unique(df_sp$tmax_used)
  
  p <- ggplot(df_sp, aes(x = year, y = perc_within_hist)) +
    # Add a subtle trend line to help visually smooth year-to-year noise
    geom_smooth(method = "loess", se = FALSE, color = "grey80", linewidth = 1.5, span = 0.3) +
    # Main time series line and points
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_point(color = "steelblue4", size = 1.5) +
    
    # Lock y-axis to 0-100 since it is a true percentage
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    scale_x_continuous(breaks = seq(min(df_sp$year), max(df_sp$year), by = 5)) +
    
    labs(
      title = tools::toTitleCase(tolower(sp)),
      subtitle = paste0("Thermal Niche: ", round(tmin, 1), "\u00B0C to ", round(tmax, 1), "\u00B0C"),
      x = "Year",
      y = "Suitable Historic Habitat (%)",
      caption = "Percentage of available habitat-days within the fixed V6 historic envelope."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40", size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    )
  
  # Format filename (replace spaces with underscores)
  file_name <- file.path(dir_images, paste0(gsub(" ", "_", sp), "_perc_within_hist.png"))
  
  ggsave(file_name, plot = p, width = 7, height = 5, dpi = 300)
})

message("Individual plots saved to: ", dir_images)


# -------------------------------------------------------------------
# 4. Generate Faceted Summary Plot
# -------------------------------------------------------------------

message("Generating faceted summary plot...")

p_facet <- ggplot(indicators, aes(x = year, y = perc_within_hist)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_point(color = "steelblue4", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "firebrick", linewidth = 0.5, span = 0.5) +
  
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(min(indicators$year), max(indicators$year), by = 10)) +
  
  facet_wrap(~species, ncol = 5) +
  
  labs(
    title = "Thermal Habitat Suitability (% within Historic Envelope)",
    subtitle = "Red line indicates LOESS smoothed trend.",
    x = "Year",
    y = "Suitable Historic Habitat (%)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

# Save large facet plot (adjust width/height depending on number of species)
facet_file <- file.path(dir_images, "ALL_SPECIES_perc_within_hist_summary.png")
ggsave(facet_file, plot = p_facet, width = 14, height = 10, dpi = 300)

message("Faceted summary plot saved to: ", facet_file)
