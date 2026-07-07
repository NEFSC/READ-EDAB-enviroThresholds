# plot_risk_score_heatmap_state_trend_V6.R
#
# Purpose: Generate a heatmap table visualization of the V6 "State + Trend" 
#          risk scores over time. 
#
# Design:  - Years on the top x-axis.
#          - Species on the y-axis.
#          - Cells colored by risk score: 
#              * Green = Negative (favorable habitat, less risk averse)
#              * White = 0 (neutral)
#              * Red   = Positive (stressful habitat, more risk averse)
#
# Output:
#   Plots : images/scoring/risk_score_heatmap_state_trend_V6.png
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
# 2. Load State + Trend Hindcast Data
# -------------------------------------------------------------------

score_file <- here::here("data/scoring/risk_scores_hindcast_state_trend_V6.rds")

if (!file.exists(score_file)) {
  stop("State + Trend score data not found. Run thermal_suitability_scoring_V6.R first.")
}

risk_scores <- readRDS(score_file)

# -------------------------------------------------------------------
# 3. Build the Heatmap Table
# -------------------------------------------------------------------

message("Generating State + Trend risk score heatmap...")

p_heatmap <- risk_scores |>
  drop_na(risk_score) |>
  ggplot(aes(
    x = as.factor(year), 
    y = fct_rev(as.factor(species)))
  ) +
  
  # The colored cells
  geom_tile(aes(fill = risk_score), color = "grey80", linewidth = 0.2) +
  
  # The text numbers inside the cells
  geom_text(aes(label = risk_score), size = 3, color = "black") +
  
  # Diverging color scale: Green (-) -> White (0) -> Red (+)
  scale_fill_gradient2(
    low = "#27ae60",       # Forest Green
    mid = "white",         # Neutral
    high = "#c0392b",      # Red
    midpoint = 0,
    limits = c(-4, 4),
    breaks = seq(-4, 4, by = 1),
    name = "Risk Score",
    na.value = "grey90"
  ) +
  
  # Move the Years to the top
  scale_x_discrete(position = "top") +
  
  labs(
    title = "Thermal Habitat Risk Scores: State + Trend Matrix (V6)",
    subtitle = "Green (Negative) = Less Risk Averse | White (0) = Neutral | Red (Positive) = More Risk Averse",
    x = NULL,
    y = NULL,
    caption = "Score = [Status Score] + [Long-term Trend Score] + [Short-term Trend Score], bounded at +/- 4."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x.top = element_text(angle = 45, hjust = 0, size = 9),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10, margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.key.height = unit(1.5, "cm")
  )

# -------------------------------------------------------------------
# 4. Save Plot
# -------------------------------------------------------------------

n_species <- length(unique(risk_scores$species))
plot_height <- max(5, n_species * 0.25 + 2) 

file_name <- file.path(dir_images, "risk_score_heatmap_state_trend_V6.png")

ggsave(file_name, plot = p_heatmap, width = 14, height = plot_height, dpi = 300)

message("Heatmap saved to: ", file_name)