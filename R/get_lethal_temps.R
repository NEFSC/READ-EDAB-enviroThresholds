# considering lethal temps for those that are available in the literature

comparison_tbl <- readRDS(here::here("data-raw/comparison_tbl.rds"))

library(dplyr)

lethal_temps <- comparison_tbl |> 
  select(
    age.group,
    common.name,
    lethal.temp.min,
    lethal.temp.max
  )  |> 
  filter(
    !is.na(lethal.temp.min) | !is.na(lethal.temp.max)
  )

# lethal temps for age.group Adult only
lethal_temps_adult <- lethal_temps |> 
  filter(age.group == "Adult")

# summarize lethal temps by common.name
lethal_temps_adult_summary <- lethal_temps_adult |> 
  group_by(common.name) |> 
  summarise(
    lethal.temp.min.mean = mean(lethal.temp.min, na.rm = TRUE),
    lethal.temp.min.sd = sd(lethal.temp.min, na.rm = TRUE),
    lethal.temp.max.mean = mean(lethal.temp.max, na.rm = TRUE),
    lethal.temp.max.sd = sd(lethal.temp.max, na.rm = TRUE)
  )
