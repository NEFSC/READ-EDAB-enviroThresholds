# 03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicator, specifically scoring the combined Spring/Fall
#          Thermal Habitat Anomaly.
#
# Logic:   1. Loads the combined anomaly data generated in the previous step.
#          2. Identifies the first 30 years of the time series for each species.
#          3. Calculates the mean and SD strictly from that fixed baseline period.
#          4. Calculates the Z-score for EVERY year relative to that baseline.
#          5. Maps the Z-score directly to the -4 to +4 scale.
#
# Output:
#   RDS   : data/scoring/annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds
#   RDS   : data/scoring/annual_risk_scores_terminal_V6_anomaly_30yr_baseline.rds
#   Plots : images/scoring/score_distribution_check_V6_anomaly_30yr_baseline.png
#
# Dependencies: tidyverse, here

# -------------------------------------------------------------------
# 0. Packages & Parameters
# -------------------------------------------------------------------

library(tidyverse)
library(here)

# Number of years to use for the historical baseline
baseline_length <- 30 

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_scoring <- here::here("data/scoring")
dir_images  <- here::here("images/scoring")

if (!dir.exists(dir_scoring)) dir.create(dir_scoring, recursive = TRUE)
if (!dir.exists(dir_images))  dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Anomaly Indicator Data
# -------------------------------------------------------------------

indicator_file <- here::here("data/indicators/combined_spring_fall_anomaly.csv")

if (!file.exists(indicator_file)) {
  stop("Indicator data not found. Run 02_calculate_thermal_habitat_anomaly.R first.")
}

indicator_df <- read_csv(indicator_file, show_col_types = FALSE)


# -------------------------------------------------------------------
# 3. Calculate Fixed Baseline Z-Scores
# -------------------------------------------------------------------

message("Calculating fixed baseline (", baseline_length, "-year) Z-scores for the habitat anomaly...")

annual_risk_scores <- indicator_df |>
  drop_na(annual_anomaly) |>
  arrange(species, year) |>
  group_by(species) |>
  mutate(
    # Identify the baseline years dynamically per species 
    is_baseline = year < (min(year) + baseline_length),
    
    # Calculate fixed mean and SD only from the baseline years
    baseline_mean = mean(annual_anomaly[is_baseline], na.rm = TRUE),
    baseline_sd   = sd(annual_anomaly[is_baseline], na.rm = TRUE),
    
    # Apply the fixed mean and SD to the entire time series
    z_score = if_else(
      is.na(baseline_sd) | baseline_sd == 0, 
      0, 
      (annual_anomaly - baseline_mean) / baseline_sd
    ),
    
    # Map to the -4 to +4 Risk Policy framework
    # Note: Higher anomaly = positive Z-score = negative risk score (Good)
    #       Lower anomaly = negative Z-score = positive risk score (Bad)
    annual_risk_score = case_when(
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
  select(species, year, annual_anomaly, z_score, annual_risk_score) |>
  arrange(species, year)


# -------------------------------------------------------------------
# 4. Extract Terminal Year Scores
# -------------------------------------------------------------------

terminal_scores <- annual_risk_scores |>
  group_by(species) |>
  filter(year == max(year)) |>
  ungroup()


# -------------------------------------------------------------------
# 5. Save Outputs
# -------------------------------------------------------------------

saveRDS(annual_risk_scores, file.path(dir_scoring, "annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "annual_risk_scores_terminal_V6_anomaly_30yr_baseline.rds"))

message("Risk scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 6. Skew Check Visualization (Annual Scores)
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
    title = paste0("Thermal Anomaly Risk Scores (V6 - Fixed ", baseline_length, "-Year Baseline)"),
    subtitle = "Scores derived from the combined Spring & Fall habitat anomaly.",
    x = "Annual Risk Score (-4 to +4)",
    y = "Frequency (Number of Species-Years)",
    caption = "Negative scores = Less Risk Averse (Favorable Habitat)\nPositive scores = More Risk Averse (Stressful Habitat)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

file_skew_plot <- file.path(dir_images, "score_distribution_check_V6_anomaly_30yr_baseline.png")
ggsave(file_skew_plot, plot = p_skew, width = 8, height = 5, dpi = 300)

message("Skew check visualization saved to: ", file_skew_plot)

# Print a quick console summary of the terminal year
message("\n--- Terminal Year Score Distribution (Spring/Fall Anomaly, 30-Year Baseline) ---")
print(table(terminal_scores$annual_risk_score))