# thermal_suitability_z_scoring.R
#
# Purpose: Generate Risk Policy scores (-4 to +4) from the V6 thermal 
#          suitability indicators using Z-score (Standard Deviation).
#
# Logic:   Calculates a TRUE Expanding Window Hindcast. For any given year, the 
#          mean and standard deviation are calculated using ONLY the historical 
#          data available up to that year. 
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
# 3. Calculate Z-Scores (True Expanding Window Hindcast)
# -------------------------------------------------------------------

message("Calculating expanding window hindcast scores...")

risk_scores <- indicator_df |>
  drop_na(perc_within_hist) |>
  arrange(species, year) |>
  group_by(species) |>
  group_modify(~{
    
    df_sp <- .x
    
    # Loop over every year in the species' time series
    map_dfr(df_sp$year, function(eval_year) {
      
      # Filter to only the data available up to the evaluation year
      df_sub <- df_sp |> filter(year <= eval_year)
      
      current_val <- df_sub$perc_within_hist[nrow(df_sub)]
      
      # We need a minimum number of years (e.g., 5) to calculate a stable 
      # standard deviation. Before that, default to neutral (0).
      if (nrow(df_sub) < 5) {
        return(tibble(
          year             = eval_year,
          perc_within_hist = current_val,
          z_score          = NA_real_,
          risk_score       = 0
        ))
      }
      
      hist_mean <- mean(df_sub$perc_within_hist)
      hist_sd   <- sd(df_sub$perc_within_hist)
      
      # Calculate Z-score safely
      if (is.na(hist_sd) || hist_sd == 0) {
        z <- 0
      } else {
        z <- (current_val - hist_mean) / hist_sd
      }
      
      # Map to -4 to +4 scale
      risk <- case_when(
        z >=  2.0 ~ -4,
        z >=  1.5 ~ -3,
        z >=  1.0 ~ -2,
        z >=  0.5 ~ -1,
        z >  -0.5 ~  0,
        z >  -1.0 ~  1,
        z >  -1.5 ~  2,
        z >  -2.0 ~  3,
        TRUE      ~  4
      )
      
      tibble(
        year             = eval_year,
        perc_within_hist = current_val,
        z_score          = z,
        risk_score       = risk
      )
    })
  }) |>
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

p_skew <- ggplot(risk_scores, aes(x = as.factor(risk_score))) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.8) +
  
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
