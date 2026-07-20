# archive/R/compare_survey_lit_review.R
#
# Purpose: Compare thermal niches generated from survey data (middle 80th percentile)
#          to those from the literature review. Filtered to only evaluate Adult
#          literature values. The final thermal niche table used in the 
#          suitability indicators now exclusively uses the empirical survey 
#          data to ensure consistency across all species.

library(tidyverse)
library(here)

# -------------------------------------------------------------------
# 1. Load and prep data
# -------------------------------------------------------------------

# Call in data from survey
survey <- readRDS(here::here('data-raw', 'survey_thermal_niche.rds')) |> 
  dplyr::rename(common.name = COMNAME) |> 
  dplyr::mutate(
    tmin = as.numeric(tmin),
    tmax = as.numeric(tmax)
  )

# Call in data from lit review
lit <- read_csv(here::here('data', 'MS_screening_analysis_cleaned.csv'), show_col_types = FALSE) |> 
  dplyr::mutate(
    common.name = stringr::str_to_upper(common.name),
    # Ensure all temperature columns are numeric
    dplyr::across(
      c(opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max),
      ~ as.numeric(.x)
    )
  )

# -------------------------------------------------------------------
# 2. Filter for Adult literature values
# -------------------------------------------------------------------

lit_adults <- lit |> 
  dplyr::filter(age.group == "Adult")


# -------------------------------------------------------------------
# 3. Create Comparison Table (Survey vs. Adult Literature)
# -------------------------------------------------------------------

comparison_tbl <- survey |> 
  dplyr::inner_join(
    lit_adults |> 
      dplyr::select(
        common.name, age.group, effect.type,
        opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max
      ),
    by = "common.name"
  ) |> 
  dplyr::mutate(
    # Optimal temperature differences
    diff_opt_min_from_survey_min  = opt.temp.min  - tmin,
    diff_opt_max_from_survey_max  = opt.temp.max  - tmax,
    
    # Stress temperature differences
    diff_stress_min_from_survey_min = stress.temp.min - tmin,
    diff_stress_max_from_survey_max = stress.temp.max - tmax,
    
    # Lethal temperature differences
    diff_lethal_min_from_survey_min = lethal.temp.min - tmin,
    diff_lethal_max_from_survey_max = lethal.temp.max - tmax  
  ) |> 
  dplyr::select(
    common.name, age.group, effect.type,
    tmin, tmax,
    opt.temp.min, opt.temp.mean, opt.temp.max,
    stress.temp.min, stress.temp.max,
    lethal.temp.min, lethal.temp.max,
    starts_with("diff_")
  )

# Save comparison table as CSV for GitHub viewing
write_csv(
  comparison_tbl,
  here::here("data-raw", "survey_vs_lit_adult_thermal_niche_comparison.csv")
)


# -------------------------------------------------------------------
# 4. Create Final Thermal Niche Table for the Pipeline
# -------------------------------------------------------------------

# Exclusively use survey 10-90th percentiles for all species
final_thermal_niche <- survey |> 
  dplyr::mutate(source = "Survey") |> 
  dplyr::select(
    COMNAME = common.name,
    tmin,
    tmax,
    source
  )

# Save as CSV so it can be easily viewed on GitHub
write_csv(
  final_thermal_niche,
  here::here("data-raw", "final_thermal_niche_values.csv")
)

# Also save as RDS to ensure 01_get_perc_suitable_thermal_habitat_seasonally.R 
# can load it exactly as it expects
saveRDS(
  final_thermal_niche,
  here::here("data-raw", "thermal_niche.rds")
)
