# comparing thermal niche generated in READ-EDAB-enviroThresholds/R/thermal_suitability.R
# to those from the literature review
# survey niche represents the middle 80 percentile of observed bottom temp for species

# call in data from survey
survey <- readRDS(here::here('data-raw','survey_thermal_niche.rds')) |> 
              dplyr::rename(common.name = COMNAME)

# call in data from lit review
lit <- read_csv(here::here('data','MS_screening_analysis_cleaned.csv')) |> 
  dplyr::mutate(common.name = stringr::str_to_upper(common.name))


# comparison table survey thermal niche to lit stress range

library(dplyr)

comparison_tbl <- survey %>%
  select(common.name, tmin, tmax) %>%
  mutate(
    tmin = as.numeric(tmin),
    tmax = as.numeric(tmax)
  ) %>%
  inner_join(
    lit %>%
      select(
        common.name,
        age.group,
        effect.type,
        opt.temp.min,
        opt.temp.mean,
        opt.temp.max,
        stress.temp.min,
        stress.temp.max
      ) %>%
      mutate(
        across(
          c(
            opt.temp.min,
            opt.temp.mean,
            opt.temp.max,
            stress.temp.min,
            stress.temp.max
          ),
          ~ as.numeric(.x)
        )
      ),
    by = "common.name"
  ) %>%
  mutate(
    # Optimal temperature differences
    diff_opt_min_from_survey_min  = opt.temp.min  - tmin,
    diff_opt_max_from_survey_max  = opt.temp.max  - tmax,
    
    # Stress temperature differences
    diff_stress_min_from_survey_min = stress.temp.min - tmin,
    diff_stress_max_from_survey_max = stress.temp.max - tmax
  ) %>%
  select(
    age.group,
    effect.type,
    common.name,
    tmin,
    tmax,
    opt.temp.min,
    opt.temp.mean,
    opt.temp.max,
    stress.temp.min,
    stress.temp.max,
    diff_opt_min_from_survey_min,
    diff_opt_max_from_survey_max,
    diff_stress_min_from_survey_min,
    diff_stress_max_from_survey_max
  )

saveRDS(
  comparison_tbl,
  here::here("data-raw", "comparison_tbl.rds")
)
