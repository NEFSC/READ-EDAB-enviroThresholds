# thermal_suitability_z_scoring.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicators using Z-score (Standard Deviation).
#
# Logic:   1. Calculates the long-term mean and SD for each species.
#          2. Calculates the Z-score for each year.
#          3. Maps the Z-score to a -4 to 4 scale.
#             - High Z-score (abundant habitat) = Negative Risk Score (less risk averse)
#             - Low Z-score (shrinking habitat) = Positive Risk Score (more risk averse)
#             - +/- 0.5 SD = 0 (Neutral / no change in management)
#
# Output:
#   RDS   : data/scoring/risk_scores_hindcast_V6.rds
#   RDS   : data/scoring/risk_scores_terminal_V6.rds
#   Plots : images/scoring/score_distribution_check_V6.png
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

dir_scoring <- here::here("data/scoring")
dir_images  <- here::here("images/scoring")

if (!dir.exists(dir_scoring)) dir.create(dir_scoring, recursive = TRUE)
if (!dir.exists(dir_images))  dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load V6 Indicator Data
# -------------------------------------------------------------------

indicator_file <- here::here("data/indicators/perc_suitable_thermal_habitat.rds")

if (!file.exists(indicator_file)) {
  stop("Indicator data not found. Run get_perc_suitable_thermal_habitat.R first.")
}

indicator_df <- readRDS(indicator_file)


# -------------------------------------------------------------------
# 3. Calculate Z-Scores and Map to Risk Scale
# -------------------------------------------------------------------

risk_scores <- indicator_df |>
  # Drop any years with NA to prevent math errors
  drop_na(perc_within_hist) |>
  group_by(species) |>
  mutate(
    # 1. Calculate the Z-score
    # scale() standardizes the data: (value - mean) / sd
    z_score = as.numeric(scale(perc_within_hist)),
    
    # 2. Map to the -4 to +4 Risk Policy framework
    # Remember: High habitat (Positive Z) = Less risk averse (Negative Score)
    risk_score = case_when(
      z_score >=  2.0 ~ -4,  # Exceptional habitat boom
      z_score >=  1.5 ~ -3,
      z_score >=  1.0 ~ -2,
      z_score >=  0.5 ~ -1,
      z_score >  -0.5 ~  0,  # The Neutral "Deadband" (+/- 0.5 SD)
      z_score >  -1.0 ~  1,
      z_score >  -1.5 ~  2,
      z_score >  -2.0 ~  3,
      TRUE            ~  4   # Severe habitat contraction (Z <= -2.0)
    )
  ) |>
  ungroup() |>
  arrange(species, year)


# -------------------------------------------------------------------
# 4. Extract Terminal Year Scores
# -------------------------------------------------------------------

terminal_scores <- risk_scores |>
  group_by(species) |>
  filter(year == max(year)) |>
  ungroup()


# -------------------------------------------------------------------
# 5. Save Outputs
# -------------------------------------------------------------------

saveRDS(risk_scores, file.path(dir_scoring, "risk_scores_hindcast_V6.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "risk_scores_terminal_V6.rds"))

message("Risk scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 6. Skew Check Visualization
# -------------------------------------------------------------------
# This generates a histogram of all assigned scores across all species
# and all years to ensure the framework isn't systematically biased.

p_skew <- ggplot(risk_scores, aes(x = as.factor(risk_score))) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  
  # Add percentage labels on top of the bars
  geom_text(
    stat = "count", 
    aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 1)),
    vjust = -0.5, 
    size = 3.5
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Distribution of Thermal Habitat Risk Scores (V6 - All Species, All Years)",
    subtitle = "Checking for systemic scoring skew. A balanced, normal distribution centered on 0 is ideal.",
    x = "Risk Score (-4 to +4)",
    y = "Frequency (Number of Species-Years)",
    caption = "Negative scores = Less Risk Averse (Favorable Habitat)\nPositive scores = More Risk Averse (Stressful Habitat)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

file_skew_plot <- file.path(dir_images, "score_distribution_check_V6.png")
ggsave(file_skew_plot, plot = p_skew, width = 8, height = 5, dpi = 300)

message("Skew check visualization saved to: ", file_skew_plot)

# Print a quick console summary of the terminal year
message("\n--- Terminal Year Score Distribution ---")
print(table(terminal_scores$risk_score))
