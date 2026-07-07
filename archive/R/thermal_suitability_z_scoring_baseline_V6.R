# thermal_suitability_z_scoring_baseline_V6.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicators using a Fixed Climatological Baseline Z-score.
#
# Logic:   1. Identifies the first N years of the time series for each species.
#          2. Calculates the mean and SD strictly from that baseline period.
#          3. Calculates the Z-score for EVERY year relative to that fixed baseline.
#          4. Maps the Z-score to the -4 to 4 scale.
#
# Output:
#   RDS   : data/scoring/risk_scores_hindcast_V6_fixed_baseline.rds
#   RDS   : data/scoring/risk_scores_terminal_V6_fixed_baseline.rds
#   Plots : images/scoring/score_distribution_check_V6_fixed_baseline.png
#
# Dependencies: tidyverse, here

# -------------------------------------------------------------------
# 0. Packages & Parameters
# -------------------------------------------------------------------

library(tidyverse)
library(here)

# Number of years to use for the historical baseline
# (10 years is a good start, but 20-30 is standard in climate science)
baseline_length <- 10 

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
# 3. Calculate Fixed Baseline Z-Scores
# -------------------------------------------------------------------

message("Calculating fixed baseline (", baseline_length, "-year) Z-scores...")

risk_scores <- indicator_df |>
  drop_na(perc_within_hist) |>
  arrange(species, year) |>
  group_by(species) |>
  mutate(
    # Identify the baseline years dynamically per species 
    # (handles if a species time series starts later than others)
    is_baseline = year < (min(year) + baseline_length),
    
    # Calculate fixed mean and SD only from the baseline years
    baseline_mean = mean(perc_within_hist[is_baseline], na.rm = TRUE),
    baseline_sd   = sd(perc_within_hist[is_baseline], na.rm = TRUE),
    
    # Apply the fixed mean and SD to the entire time series
    z_score = if_else(
      is.na(baseline_sd) | baseline_sd == 0, 
      0, 
      (perc_within_hist - baseline_mean) / baseline_sd
    ),
    
    # Map to the -4 to +4 Risk Policy framework
    risk_score = case_when(
      z_score >=  2.0 ~ -4,
      z_score >=  1.5 ~ -3,
      z_score >=  1.0 ~ -2,
      z_score >=  0.5 ~ -1,
      z_score >  -0.5 ~  0,
      z_score >  -1.0 ~  1,
      z_score >  -1.5 ~  2,
      z_score >  -2.0 ~  3,
      TRUE            ~  4
    )
  ) |>
  ungroup() |>
  # Drop the helper columns to keep the dataframe clean
  select(-is_baseline, -baseline_mean, -baseline_sd) |>
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

saveRDS(risk_scores, file.path(dir_scoring, "risk_scores_hindcast_V6_fixed_baseline.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "risk_scores_terminal_V6_fixed_baseline.rds"))

message("Risk scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 6. Skew Check Visualization
# -------------------------------------------------------------------

p_skew <- ggplot(risk_scores, aes(x = as.factor(risk_score))) +
  geom_bar(fill = "seagreen", color = "black", alpha = 0.8) + # Swapped to green to differentiate from expanding window
  
  geom_text(
    stat = "count", 
    aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 1)),
    vjust = -0.5, 
    size = 3.5
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("Thermal Habitat Risk Scores (V6 - Fixed ", baseline_length, "-Year Baseline)"),
    subtitle = "Z-scores calculated relative to the first decade of data, preventing shifting baseline syndrome.",
    x = "Risk Score (-4 to +4)",
    y = "Frequency (Number of Species-Years)",
    caption = "Negative scores = Less Risk Averse (Favorable Habitat)\nPositive scores = More Risk Averse (Stressful Habitat)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

file_skew_plot <- file.path(dir_images, "score_distribution_check_V6_fixed_baseline.png")
ggsave(file_skew_plot, plot = p_skew, width = 8, height = 5, dpi = 300)

message("Skew check visualization saved to: ", file_skew_plot)

# Print a quick console summary of the terminal year
message("\n--- Terminal Year Score Distribution (Fixed Baseline) ---")
print(table(terminal_scores$risk_score))