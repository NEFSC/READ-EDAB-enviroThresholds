# archive/compare_thermal_niches/04_plot_risk_score_heatmaps_V6_anomaly.R
#
# Purpose: Generate heatmap visualizations of the V6 thermal habitat risk scores.
#          
# Outputs: 
#   Archive (Comparison) : Individual species heatmaps comparing all candidate niches.
#   Main (Production)    : Single all-species heatmap for the preferred Survey 10-90th niche.
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

dir_images_archive <- here::here("archive/compare_thermal_niches/images/scoring")
dir_images_main    <- here::here("images/scoring")

if (!dir.exists(dir_images_archive)) dir.create(dir_images_archive, recursive = TRUE)
if (!dir.exists(dir_images_main))    dir.create(dir_images_main, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Hindcast Data (Archive Output)
# -------------------------------------------------------------------

# We can load the archive file since it contains all scenarios, 
# and filter it for the main production plot later.
annual_file <- here::here("archive/compare_thermal_niches/data/scoring/annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds")

if (!file.exists(annual_file)) {
  stop("Score data not found. Run 03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R first.")
}

annual_scores <- readRDS(annual_file)


# -------------------------------------------------------------------
# 3. Generate ARCHIVE Plots: Individual Species Heatmaps
# -------------------------------------------------------------------

species_list <- unique(annual_scores$species)
message("Generating ARCHIVE individual risk score heatmaps for ", length(species_list), " species...")

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
  file_name <- file.path(dir_images_archive, paste0(safe_name, "_risk_score_heatmap.png"))
  
  ggsave(file_name, plot = p_heatmap, width = 14, height = plot_height, dpi = 300, bg = "white")
})

message("  All archive individual species heatmaps saved to: ", dir_images_archive)


# -------------------------------------------------------------------
# 4. Generate MAIN Production Plot: All-Species Heatmap (Survey 10-90th)
# -------------------------------------------------------------------

message("\nGenerating MAIN production all-species heatmap (Survey_10_90 only)...")

annual_scores_main <- annual_scores |>
  dplyr::filter(source == "Survey_10_90") |>
  tidyr::drop_na(annual_risk_score) |>
  dplyr::mutate(species_label = tools::toTitleCase(tolower(species)))

p_annual_heatmap_main <- ggplot(annual_scores_main, aes(
  x = year, 
  y = fct_rev(as.factor(species_label))
)) +
  geom_tile(aes(fill = annual_risk_score), color = "grey80", linewidth = 0.2) +
  geom_text(aes(label = annual_risk_score), size = 3, color = "black") +
  scale_fill_gradient2(
    low = "#27ae60",       
    mid = "white",         
    high = "#c0392b",      
    midpoint = 0,
    limits = c(-4, 4),
    breaks = seq(-4, 4, by = 1),
    name = "Risk Score\n(Spring/Fall Anomaly)",
    na.value = "grey90"
  ) +
  scale_x_continuous(
    position = "top", 
    breaks = seq(min(annual_scores_main$year, na.rm=TRUE), max(annual_scores_main$year, na.rm=TRUE), by = 2)
  ) +
  labs(
    title = "Thermal Habitat Risk Scores: Spring/Fall Anomaly w/ 30-Year Baseline (V6)",
    subtitle = "Green (Negative) = Less Risk Averse | White (0) = Neutral | Red (Positive) = More Risk Averse",
    caption = "Scores are Z-scores derived from the combined Spring & Fall habitat anomaly (Survey 10-90th), relative to a fixed 30-year baseline.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x.top = element_text(angle = 45, hjust = 0, size = 9),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10, margin = margin(b = 5)),
    plot.caption = element_text(color = "grey50", size = 9, hjust = 0, margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.key.height = unit(1.5, "cm")
  )

# Dynamically size the height of the plot based on the number of species to prevent squishing
n_species <- length(unique(annual_scores_main$species))
plot_height_main <- max(5, n_species * 0.25 + 2) 

file_name_main <- file.path(dir_images_main, "risk_score_heatmap_V6_anomaly_30yr_baseline.png")
ggsave(file_name_main, plot = p_annual_heatmap_main, width = 14, height = plot_height_main, dpi = 300, bg = "white")

message("  Saved: ", file_name_main)
message("\nComparison workflow completed successfully. Production files routed.")