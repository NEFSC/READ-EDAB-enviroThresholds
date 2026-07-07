# thermal_suitability_scoring_V6.R
# Generating Risk Scores using the "State + Trend" Matrix approach
# Evaluates the V6 indicator (perc_suitable_thermal_habitat.rds)

library(dplyr)
library(purrr)
library(stringr)
library(here)

# Load helper functions
source(here::here("R/scoring_functions_V6.R"))

#----------------------------------
# Output directories
#----------------------------------

dir_scoring <- here::here("data/scoring")
if (!dir.exists(dir_scoring)) dir.create(dir_scoring, recursive = TRUE)

#----------------------------------
# Load indicator files
#----------------------------------

indicator_file <- here::here("data/indicators/perc_suitable_thermal_habitat.rds")

if (!file.exists(indicator_file)) {
  stop("Indicator data not found. Run get_perc_suitable_thermal_habitat.R first.")
}

indicator_df <- readRDS(indicator_file)

#----------------------------------
# Prepare data
#----------------------------------

indicator_df <- indicator_df %>%
  # V6 is already a 0-100 percentage, no need to multiply by 100
  transmute(
    species,
    year,
    pct_suitable = perc_within_hist 
  ) %>%
  drop_na(pct_suitable)

#----------------------------------
# Hindcast scoring
#----------------------------------

message("Calculating expanding window State + Trend scores...")

risk_scores <- indicator_df %>%
  group_by(species) %>%
  group_modify(~{
    
    df_species <- arrange(.x, year)
    
    map_dfr(df_species$year, function(y) {
      score_species_year(df_species, y)
    })
    
  }) %>%
  ungroup()

#----------------------------------
# Terminal year only
#----------------------------------

terminal_scores <- risk_scores %>%
  group_by(species) %>%
  filter(year == max(year)) %>%
  ungroup()

#----------------------------------
# Save outputs
#----------------------------------

# Added "_state_trend_" to the filename to distinguish it from the Z-score outputs
saveRDS(risk_scores, file.path(dir_scoring, "risk_scores_hindcast_state_trend_V6.rds"))
saveRDS(terminal_scores, file.path(dir_scoring, "risk_scores_terminal_state_trend_V6.rds"))

message("Risk scores saved to: ", dir_scoring)