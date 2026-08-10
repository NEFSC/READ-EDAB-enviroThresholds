# Process thermal habitat anomaly data
# These data were derived from the environmental thresholds pipeline, calculating
# the Fall percent suitable thermal habitat anomaly for managed species.
# More information about these data will be available at
# https://noaa-edab.github.io/tech-doc/ 

raw.dir <- here::here("data-raw")

thermal_input <- "thermal_habitat_anomaly.rds"

get_thermal_habitat_anomaly <- function(save_clean = F) {
  thermal_habitat_anomaly <- readRDS(file.path(raw.dir, thermal_input))
  
  if (save_clean) {
    usethis::use_data(thermal_habitat_anomaly, overwrite = T)
  } else {
    return(thermal_habitat_anomaly)
  }
}

get_thermal_habitat_anomaly(save_clean = T)