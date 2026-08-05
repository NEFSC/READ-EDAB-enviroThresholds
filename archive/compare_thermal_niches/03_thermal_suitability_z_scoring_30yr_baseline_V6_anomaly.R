# archive/compare_thermal_niches/03_thermal_suitability_z_scoring_30yr_baseline_V6_anomaly.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicator ACROSS MULTIPLE THERMAL NICHE SCENARIOS.
#
# Outputs: 
#   Archive (Comparison) : data/scoring & images/scoring (All Scenarios & Baseline Stats)
#   Main (Production)    : data/scoring & images/scoring (Survey 10-90th ONLY)
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
# 1. Output directories (Dual Routing)
# -------------------------------------------------------------------

# Archive Directories
dir_scoring_archive <- here::here("archive/compare_thermal_niches/data/scoring")
dir_images_archive  <- here::here("archive/compare_thermal_niches/images/scoring")
if (!dir.exists(dir_scoring_archive)) dir.create(dir_scoring_archive, recursive = TRUE)
if (!dir.exists(dir_images_archive))  dir.create(dir_images_archive, recursive = TRUE)

# Main Production Directories
dir_scoring_main <- here::here("data/scoring")
dir_images_main  <- here::here("images/scoring")
if (!dir.exists(dir_scoring_main)) dir.create(dir_scoring_main, recursive = TRUE)
if (!dir.exists(dir_images_main))  dir.create(dir_images_main, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Anomaly Indicator Data (From Archive)
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
  # Keep relevant metadata AND baseline stats for downstream plotting/saving
  select(species, scenario_id, source, year, annual_anomaly, baseline_mean, baseline_sd, z_score, annual_risk_score) |>
  arrange(species, scenario_id, year)


# -------------------------------------------------------------------
# 4. Extract Terminal Year Scores
# -------------------------------------------------------------------

terminal_scores <- annual_risk_scores |>
  group_by(species, scenario_id) |>
  filter(year == max(year)) |>
  ungroup()


# -------------------------------------------------------------------
# 5. Save Dual Outputs (Data)
# -------------------------------------------------------------------

# --- Output A: Archive (All Scenarios & Baseline Stats) ---
baseline_stats <- annual_risk_scores |>
  dplyr::distinct(species, scenario_id, source, baseline_mean, baseline_sd) |>
  dplyr::arrange(species, source)

file_baseline_stats <- file.path(dir_scoring_archive, "baseline_statistics.csv")
write_csv(baseline_stats, file_baseline_stats)
message("Saved baseline statistics to archive.")

annual_risk_scores_clean <- annual_risk_scores |> dplyr::select(-baseline_mean, -baseline_sd)
terminal_scores_clean    <- terminal_scores |> dplyr::select(-baseline_mean, -baseline_sd)

saveRDS(annual_risk_scores_clean, file.path(dir_scoring_archive, "annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds"))
saveRDS(terminal_scores_clean, file.path(dir_scoring_archive, "annual_risk_scores_terminal_V6_anomaly_30yr_baseline.rds"))
message("Saved full comparison risk scores to archive.")


# --- Output B: Main Production (Survey 10-90th ONLY) ---
# Strip scenario columns so it identically matches the standard pipeline schema
annual_risk_scores_main <- annual_risk_scores_clean |>
  dplyr::filter(source == "Survey_10_90") |>
  dplyr::select(species, year, annual_anomaly, z_score, annual_risk_score)

terminal_scores_main <- terminal_scores_clean |>
  dplyr::filter(source == "Survey_10_90") |>
  dplyr::select(species, year, annual_anomaly, z_score, annual_risk_score)

saveRDS(annual_risk_scores_main, file.path(dir_scoring_main, "annual_risk_scores_hindcast_V6_anomaly_30yr_baseline.rds"))
saveRDS(terminal_scores_main, file.path(dir_scoring_main, "annual_risk_scores_terminal_V6_anomaly_30yr_baseline.rds"))
message("Saved clean production risk scores to main data directory.")


# -------------------------------------------------------------------
# 6. Generate Archive Skew Check (Faceted by Niche Source)
# -------------------------------------------------------------------

message("\nGenerating ARCHIVE score distribution skew check...")

annual_risk_scores_clean <- annual_risk_scores_clean |>
  mutate(source = forcats::fct_relevel(source, "Survey_10_90"))

p_skew_archive <- ggplot(annual_risk_scores_clean, aes(x = as.factor(annual_risk_score))) +
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
  theme(panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold", size = 10), panel.border = element_rect(color = "grey80", fill = NA))

ggsave(file.path(dir_images_archive, "score_distribution_check_V6_anomaly_30yr_baseline.png"), 
       plot = p_skew_archive, width = 12, height = 8, dpi = 300, bg = "white")


# -------------------------------------------------------------------
# 7. Generate Main Production Skew Check (Survey 10-90th ONLY)
# -------------------------------------------------------------------

message("Generating MAIN production score distribution skew check...")

p_skew_main <- ggplot(annual_risk_scores_main, aes(x = as.factor(annual_risk_score))) +
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
    subtitle = "Scores derived from the combined Spring & Fall habitat anomaly (Survey 10-90th).",
    x = "Annual Risk Score (-4 to +4)",
    y = "Frequency (Number of Species-Years)",
    caption = "Negative scores = Less Risk Averse (Favorable Habitat)\nPositive scores = More Risk Averse (Stressful Habitat)"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"))

ggsave(file.path(dir_images_main, "score_distribution_check_V6_anomaly_30yr_baseline.png"), 
       plot = p_skew_main, width = 8, height = 5, dpi = 300, bg = "white")

message("\nScript complete. Dual outputs routed successfully.")