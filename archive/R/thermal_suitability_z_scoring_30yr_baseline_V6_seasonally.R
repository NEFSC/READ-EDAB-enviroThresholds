# thermal_suitability_z_scoring_30yr_baseline_V6_seasonally.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the seasonal V6 thermal 
#          suitability indicators using a 30-Year Fixed Climatological Baseline.
#
# Logic:   1. Identifies the first 30 years of the time series for each species/season.
#          2. Calculates the mean and SD strictly from that baseline period.
#          3. Calculates the Z-score for EVERY year relative to that fixed baseline.
#          4. Maps the Z-score to the -4 to 4 scale for each season.
#          5. Averages the 4 seasonal risk scores into a single annual risk score 
#             per species, rounding to the nearest integer to maintain the discrete scale.
#
# Output:
#   RDS   : data/scoring/seasonal_risk_scores_V6_30yr_baseline.rds
#   RDS   : data/scoring/annual_risk_scores_hindcast_V6_seasonally_30yr_baseline.rds
#   RDS   : data/scoring/annual_risk_scores_terminal_V6_seasonally_30yr_baseline.rds
#   Plots : images/scoring/score_distribution_check_V6_seasonally_30yr_baseline.png
#
# Dependencies: tidyverse, here

# -------------------------------------------------------------------
# 0. Packages & Parameters
# -------------------------------------------------------------------

library(tidyverse)
library(here)

# Number of years to use for the historical baseline
# (30 years is the gold standard in climate science for capturing decadal cycles)
baseline_length <- 30 

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_scoring <- here::here("data/scoring")
dir_images  <- here::here("images/scoring")

if (!dir.exists(dir_scoring)) dir.create(dir_scoring, recursive = TRUE)
if (!dir.exists(dir_images))  dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load V6 Seasonal Indicator Data
# -------------------------------------------------------------------

indicator_file <- here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")

if (!file.exists(indicator_file)) {
  stop("Seasonal indicator data not found. Run get_perc_suitable_thermal_habitat_seasonally.R first.")
}

indicator_df <- readRDS(indicator_file)


# -------------------------------------------------------------------
# 3. Calculate Fixed Baseline Z-Scores (Per Season)
# -------------------------------------------------------------------

message("Calculating seasonal fixed baseline (", baseline_length, "-year) Z-scores...")

seasonal_risk_scores <- indicator_df |>
  drop_na(perc_within_hist) |>
  arrange(species, season, year) |>
  # Group by BOTH species and season so each season gets its own baseline mean/sd
  group_by(species, season) |>
  mutate(
    # Identify the baseline years dynamically per species/season 
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
    seasonal_risk_score = case_when(
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
  # Drop helper columns
  select(-is_baseline, -baseline_mean, -baseline_sd) |>
  arrange(species, year, season)


# -------------------------------------------------------------------
# 4. Collapse into Annual Average Risk Scores
# -------------------------------------------------------------------

message("Collapsing seasonal scores into annual average risk scores...")

annual_risk_scores <- seasonal_risk_scores |>
  group_by(species, year) |>
  summarise(
    n_seasons = n(),
    # Calculate raw mean for reference
    raw_mean_score = mean(seasonal_risk_score, na.rm = TRUE),
    # Round to nearest integer to fit standard discrete risk policy matrix
    annual_risk_score = round(mean(seasonal_risk_score, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  # Ensure only years with all 4 seasons are scored to prevent bias
  filter(n_seasons == 4) |>
  arrange(species, year)


# -------------------------------------------------------------------
# 5. Extract Terminal Year Scores
# -------------------------------------------------------------------

terminal_scores <- annual_risk_scores |>
  group_by(species) |>
  filter(year == max(year)) |>
  ungroup()


# -------------------------------------------------------------------
# 6. Save Outputs
# -------------------------------------------------------------------

# Save both the underlying seasonal scores and the averaged annual scores
saveRDS(seasonal_risk_scores, file.path(dir_scoring, "seasonal_risk_scores_V6_30yr_baseline.rds"))
saveRDS(annual_risk_scores, file.path(dir_scoring, "annual_risk_scores_hindcast_V6_seasonally_30yr_baseline.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "annual_risk_scores_terminal_V6_seasonally_30yr_baseline.rds"))

message("Risk scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 7. Skew Check Visualization (Annual Scores)
# -------------------------------------------------------------------

p_skew <- ggplot(annual_risk_scores, aes(x = as.factor(annual_risk_score))) +
  geom_bar(fill = "darkcyan", color = "black", alpha = 0.8) +
  
  geom_text(
    stat = "count", 
    aes(label = scales::percent(after_stat(count) / sum(after_stat(count)), accuracy = 1)),
    vjust = -0.5, 
    size = 3.5
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("Averaged Annual Thermal Risk Scores (V6 - Fixed ", baseline_length, "-Year Baseline)"),
    subtitle = "Scores derived by averaging 4 independently scaled seasonal risk scores.",
    x = "Annual Risk Score (-4 to +4)",
    y = "Frequency (Number of Species-Years)",
    caption = "Negative scores = Less Risk Averse (Favorable Habitat)\nPositive scores = More Risk Averse (Stressful Habitat)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

file_skew_plot <- file.path(dir_images, "score_distribution_check_V6_seasonally_30yr_baseline.png")
ggsave(file_skew_plot, plot = p_skew, width = 8, height = 5, dpi = 300)

message("Skew check visualization saved to: ", file_skew_plot)

# Print a quick console summary of the terminal year
message("\n--- Terminal Year Score Distribution (Averaged Seasonal, 30-Year Baseline) ---")
print(table(terminal_scores$annual_risk_score))