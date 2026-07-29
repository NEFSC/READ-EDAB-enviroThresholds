# archive/compare_thermal_niches/03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicator ACROSS MULTIPLE THERMAL NICHE SCENARIOS.
#
# Logic:   1. Loads the combined anomaly data generated in the previous step.
#          2. Identifies the first 30 years of the time series for each scenario.
#          3. Calculates the mean and SD strictly from that fixed baseline period.
#          4. Calculates the Z-score for EVERY year relative to that baseline.
#          5. Maps the Z-score directly to the -4 to +4 scale.
#
# Output:
#   RDS   : archive/compare_thermal_niches/data/scoring/annual_risk_scores_hindcast.rds
#   RDS   : archive/compare_thermal_niches/data/scoring/annual_risk_scores_terminal.rds
#   Plots : archive/compare_thermal_niches/images/scoring/score_distribution_check.png
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

dir_scoring <- here::here("archive/compare_thermal_niches/data/scoring")
dir_images  <- here::here("archive/compare_thermal_niches/images/scoring")

if (!dir.exists(dir_scoring)) dir.create(dir_scoring, recursive = TRUE)
if (!dir.exists(dir_images))  dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Anomaly Indicator Data
# -------------------------------------------------------------------

indicator_file <- here::here("archive/compare_thermal_niches/indicators/combined_spring_fall_anomaly.csv")

if (!file.exists(indicator_file)) {
  stop("Indicator data not found. Run 02_calculate_thermal_habitat_anomaly.R first.")
}

indicator_df <- read_csv(indicator_file, show_col_types = FALSE)


# -------------------------------------------------------------------
# 3. Calculate Fixed Baseline Z-Scores
# -------------------------------------------------------------------

message("Calculating fixed baseline (", baseline_length, "-year) Z-scores for the habitat anomalies...")

annual_risk_scores <- indicator_df |>
  drop_na(annual_anomaly) |>
  arrange(species, scenario_id, year) |>
  # Ensure calculations are completely independent for each thermal niche scenario
  group_by(species, scenario_id, source) |>
  mutate(
    # Identify the baseline years dynamically per species/scenario 
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
  # Keep relevant metadata for downstream plotting
  select(species, scenario_id, source, year, annual_anomaly, z_score, annual_risk_score) |>
  arrange(species, scenario_id, year)


# -------------------------------------------------------------------
# 4. Extract Terminal Year Scores
# -------------------------------------------------------------------

terminal_scores <- annual_risk_scores |>
  group_by(species, scenario_id) |>
  filter(year == max(year)) |>
  ungroup()


# -------------------------------------------------------------------
# 5. Save Outputs
# -------------------------------------------------------------------

saveRDS(annual_risk_scores, file.path(dir_scoring, "annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "annual_risk_scores_terminal_V6_anomaly_30yr_baseline.rds"))

message("Risk scores saved to: ", dir_scoring)


# -------------------------------------------------------------------
# 6. Skew Check Visualization (Faceted by Niche Source)
# -------------------------------------------------------------------

message("Generating score distribution skew check...")

# Reorder factor so Survey_10_90 is the first facet
annual_risk_scores <- annual_risk_scores |>
  mutate(source = forcats::fct_relevel(source, "Survey_10_90"))

p_skew <- ggplot(annual_risk_scores, aes(x = as.factor(annual_risk_score))) +
  geom_bar(aes(fill = source == "Survey_10_90", y = after_stat(prop), group = 1), 
           color = "black", alpha = 0.8) +
  
  scale_fill_manual(values = c("TRUE" = "darkcyan", "FALSE" = "grey50"), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.15))) +
  
  facet_wrap(~source, scales = "free_y") +
  
  labs(
    title = paste0("Thermal Anomaly Risk Scores (V6 - Fixed ", baseline_length, "-Year Baseline)"),
    subtitle = "Comparing score distributions across all candidate thermal niches.",
    x = "Annual Risk Score (-4 to +4)",
    y = "Percentage of Total Species-Years",
    caption = "Negative scores = Less Risk Averse (Favorable)\nPositive scores = More Risk Averse (Stressful)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 10),
    panel.border = element_rect(color = "grey80", fill = NA)
  )

file_skew_plot <- file.path(dir_images, "score_distribution_check_V6_anomaly_30yr_baseline.png")
ggsave(file_skew_plot, plot = p_skew, width = 12, height = 8, dpi = 300, bg = "white")

message("Skew check visualization saved to: ", file_skew_plot)