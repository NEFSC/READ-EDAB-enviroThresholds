# 04_plot_risk_score_heatmaps_V6_anomaly.R
#
# Purpose: Generate a heatmap table visualization of the V6 thermal habitat 
#          risk scores using the 30-Year Fixed Baseline method, focusing 
#          on the combined Spring & Fall thermal habitat anomaly.
#          
# Output:
#   Summary Plot : images/scoring/risk_score_heatmap_V6_anomaly_30yr_baseline.png
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

dir_images <- here::here("images/scoring")

if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Hindcast Data (Anomaly Scores)
# -------------------------------------------------------------------

annual_file <- here::here("data/scoring/annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds")

if (!file.exists(annual_file)) {
  stop("Score data not found. Run 03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R first.")
}

annual_scores <- readRDS(annual_file)


# -------------------------------------------------------------------
# 3. All-Species Anomaly Heatmap
# -------------------------------------------------------------------

message("Generating All-Species Anomaly Heatmap...")

p_annual_heatmap <- annual_scores |>
  drop_na(annual_risk_score) |>
  # Format species names for clean y-axis labeling
  mutate(species_label = tools::toTitleCase(tolower(species))) |>
  ggplot(aes(
    x = as.factor(year), 
    y = fct_rev(as.factor(species_label)))
  ) +
  geom_tile(aes(fill = annual_risk_score), color = "grey80", linewidth = 0.2) +
  geom_text(aes(label = annual_risk_score), size = 3, color = "black") +
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
  scale_x_discrete(position = "top") +
  labs(
    title = "Thermal Habitat Risk Scores: Spring/Fall Anomaly w/ 30-Year Baseline (V6)",
    subtitle = "Green (Negative) = Less Risk Averse | White (0) = Neutral | Red (Positive) = More Risk Averse",
    caption = "Scores are Z-scores derived from the combined Spring & Fall habitat anomaly, relative to a fixed 30-year historical baseline.",
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
n_species <- length(unique(annual_scores$species))
plot_height <- max(5, n_species * 0.25 + 2) 

file_name_annual <- file.path(dir_images, "risk_score_heatmap_V6_anomaly_30yr_baseline.png")
ggsave(file_name_annual, plot = p_annual_heatmap, width = 14, height = plot_height, dpi = 300)

message("  Saved: ", file_name_annual)
message("\nScript completed successfully.")