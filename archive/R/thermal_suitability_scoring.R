# Generating Risk Scores from thermal suitability indicators
# MTG 02/20/2026

library(dplyr)
library(purrr)
library(stringr)

source("R/scoring_functions.R")

#----------------------------------
# Load indicator files
#----------------------------------

threshold_dir <- "thresholds"

indicator_files <- list.files(
  threshold_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

indicator_df <- map_dfr(indicator_files, readRDS)

#----------------------------------
# Prepare data
#----------------------------------

indicator_df <- indicator_df %>%
  transmute(
    species,
    year,
    pct_suitable = perc_within_year * 100,
    heat_stress = stress_index_year
  ) %>%
  group_by(species) %>%
  mutate(
    heat_stress = as.numeric(scale(heat_stress))
  ) %>%
  ungroup()

#----------------------------------
# Hindcast scoring
#----------------------------------

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

saveRDS(risk_scores, "thresholds/risk_scores_hindcast.rds")
saveRDS(terminal_scores, "thresholds/risk_scores_terminal.rds")
