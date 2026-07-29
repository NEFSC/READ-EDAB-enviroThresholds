# archive/compare_thermal_niches/04_plot_risk_score_heatmaps_V6_anomaly.R
#
# Purpose: Generate individual heatmap visualizations for each species to compare
#          the V6 thermal habitat risk scores across all candidate thermal niches.
#          
# Output:
#   Individual Plots : archive/compare_thermal_niches/images/scoring/<species>_risk_score_heatmap.png
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

dir_images <- here::here("archive/compare_thermal_niches/images/scoring")

if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Hindcast Data (Anomaly Scores)
# -------------------------------------------------------------------

annual_file <- here::here("archive/compare_thermal_niches/data/scoring/annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds")

if (!file.exists(annual_file)) {
  stop("Score data not found. Run 03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R first.")
}

annual_scores <- readRDS(annual_file)


# -------------------------------------------------------------------
# 3. Generate Individual Species Heatmaps
# -------------------------------------------------------------------

species_list <- unique(annual_scores$species)
message("Generating individual risk score heatmaps for ", length(species_list), " species...")

purrr::walk(species_list, function(sp) {
  
  # Filter data for the specific species and drop NAs
  df_sp <- annual_scores |>
    dplyr::filter(species == sp) |>
    tidyr::drop_na(annual_risk_score)
  
  if (nrow(df_sp) == 0) return(NULL)
  
  # Format the species name for the title
  sp_title <- tools::toTitleCase(tolower(sp))
  
  # Reorder the sources so the empirical survey methods appear at the top
  # We use fct_rev because ggplot draws the first factor level at the bottom of the Y-axis
  df_sp <- df_sp |>
    dplyr::mutate(
      source = forcats::fct_relevel(source, "Survey_10_90", "Survey_0_100"),
      source = forcats::fct_rev(source)
    )
  
  p_heatmap <- ggplot(df_sp, aes(x = year, y = source)) +
    geom_tile(aes(fill = annual_risk_score), color = "grey80", linewidth = 0.2) +
    geom_text(aes(label = annual_risk_score), size = 3.5, color = "black") +
    
    scale_fill_gradient2(
      low = "#27ae60",       # Forest/Kelly Green
      mid = "white",         # Neutral 0
      high = "#c0392b",      # Brick Red
      midpoint = 0,
      limits = c(-4, 4),
      breaks = seq(-4, 4, by = 1),
      name = "Risk Score\n(Spring/Fall Anomaly)",
      na.value = "grey90"
    ) +
    scale_x_continuous(
      position = "top", 
      breaks = seq(min(df_sp$year, na.rm=TRUE), max(df_sp$year, na.rm=TRUE), by = 2)
    ) +
    labs(
      title = paste0(sp_title, " \u2014 Thermal Habitat Risk Scores"),
      subtitle = "Sensitivity to Thermal Niche Scenarios (-4 to +4)",
      caption = "Scores are Z-scores derived from the combined Spring & Fall habitat anomaly, relative to a fixed 30-year historical baseline.",
      x = "Year",
      y = "Thermal Niche Source"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x.top = element_text(angle = 45, hjust = 0, size = 10),
      axis.text.y = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 12, margin = margin(b = 10)),
      plot.caption = element_text(color = "grey50", size = 9, hjust = 0, margin = margin(t = 15)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.key.height = unit(1.5, "cm")
    )
  
  # Dynamically set height based on the number of scenarios available for this species
  n_scenarios <- length(unique(df_sp$source))
  plot_height <- max(4, n_scenarios * 0.6 + 2.5)
  
  safe_name <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_images, paste0(safe_name, "_risk_score_heatmap.png"))
  
  ggsave(file_name, plot = p_heatmap, width = 14, height = plot_height, dpi = 300, bg = "white")
})

message("All individual species heatmaps saved to: ", dir_images)
message("Comparison workflow completed successfully.")