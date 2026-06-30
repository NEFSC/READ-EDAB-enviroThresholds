# plot_risk_score_heatmaps_V6_seasonally.R
#
# Purpose: Generate heatmap table visualizations of the V6 thermal habitat 
#          risk scores using the 30-Year Fixed Baseline method.
#          
#          Part 1: Generates a single all-species heatmap using the averaged 
#                  annual risk scores.
#          Part 2: Generates individual species-specific heatmaps showing the 
#                  breakdown of the 4 underlying seasonal risk scores.
#
# Output:
#   Summary Plot : images/scoring/risk_score_heatmap_V6_seasonally_30yr_baseline.png
#   Sp. Plots    : images/scoring/seasonal_breakdown/<species>_seasonal_heatmap.png
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
dir_breakdown <- file.path(dir_images, "seasonal_breakdown")

if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)
if (!dir.exists(dir_breakdown)) dir.create(dir_breakdown, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load Hindcast Data (Annual & Seasonal)
# -------------------------------------------------------------------

annual_file <- here::here("data/scoring/annual_risk_scores_hindcast_V6_seasonally_30yr_baseline.rds")
seasonal_file <- here::here("data/scoring/seasonal_risk_scores_V6_30yr_baseline.rds")

if (!file.exists(annual_file) || !file.exists(seasonal_file)) {
  stop("Score data not found. Run thermal_suitability_z_scoring_30yr_baseline_V6_seasonally.R first.")
}

annual_scores <- readRDS(annual_file)

seasonal_scores <- readRDS(seasonal_file) |>
  mutate(season = factor(season, levels = c("WINTER", "SPRING", "SUMMER", "FALL")))

# -------------------------------------------------------------------
# 3. Part 1: All-Species Annual Average Heatmap
# -------------------------------------------------------------------

message("Generating All-Species Annual Average Heatmap...")

p_annual_heatmap <- annual_scores |>
  drop_na(annual_risk_score) |>
  ggplot(aes(
    x = as.factor(year), 
    y = fct_rev(as.factor(species)))
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
    name = "Annual Risk Score\n(Seasonal Avg)",
    na.value = "grey90"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title = "Thermal Habitat Risk Scores: Seasonal Average w/ 30-Year Baseline (V6)",
    subtitle = "Green (Negative) = Less Risk Averse | White (0) = Neutral | Red (Positive) = More Risk Averse",
    caption = "Scores are the rounded average of 4 independent seasonal Z-scores, calculated relative to a fixed 30-year historical baseline.",
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

n_species <- length(unique(annual_scores$species))
plot_height <- max(5, n_species * 0.25 + 2) 

file_name_annual <- file.path(dir_images, "risk_score_heatmap_V6_seasonally_30yr_baseline.png")
ggsave(file_name_annual, plot = p_annual_heatmap, width = 14, height = plot_height, dpi = 300)

message("  Saved: ", file_name_annual)

# -------------------------------------------------------------------
# 4. Part 2: Species-Specific Seasonal Breakdown Heatmaps
# -------------------------------------------------------------------

species_list <- unique(seasonal_scores$species)
message("\nGenerating seasonal breakdown heatmaps for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  df_sp <- seasonal_scores |>
    filter(species == sp) |>
    drop_na(seasonal_risk_score)
  
  if(nrow(df_sp) == 0) return(NULL)
  
  p_seasonal <- ggplot(df_sp, aes(
    x = as.factor(year), 
    y = fct_rev(season)
  )) +
    geom_tile(aes(fill = seasonal_risk_score), color = "grey80", linewidth = 0.5) +
    geom_text(aes(label = seasonal_risk_score), size = 3.5, color = "black") +
    scale_fill_gradient2(
      low = "#27ae60",
      mid = "white",
      high = "#c0392b",
      midpoint = 0,
      limits = c(-4, 4),
      breaks = seq(-4, 4, by = 1),
      name = "Risk Score",
      na.value = "grey90"
    ) +
    scale_x_discrete(position = "top") +
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Seasonal Thermal Risk Scores (V6)"),
      subtitle = "Green (Negative) = Less Risk Averse | White (0) = Neutral | Red (Positive) = More Risk Averse",
      caption = "Z-scores calculated relative to a fixed 30-year historical baseline independently for each season.",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x.top = element_text(angle = 45, hjust = 0, size = 9),
      axis.text.y = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40", size = 10, margin = margin(b = 5)),
      plot.caption = element_text(color = "grey50", size = 9, hjust = 0, margin = margin(t = 10)),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.key.height = unit(1.0, "cm")
    )
  
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name_seasonal <- file.path(dir_breakdown, paste0(safe_name, "_seasonal_heatmap.png"))
  
  ggsave(file_name_seasonal, plot = p_seasonal, width = 15, height = 4, dpi = 300, bg = "white")
})

message("  All seasonal breakdown heatmaps saved to: ", dir_breakdown)
message("\nScript completed successfully.")