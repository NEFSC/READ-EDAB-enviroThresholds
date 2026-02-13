# setting niche min and max temperatures by species
# after comparing survey and lit review values

thermal_niche <- readRDS(here::here("data-raw/survey_thermal_niche.rds"))

thermal_niche <- thermal_niche |> dplyr::select(COMNAME,tmin,tmax)

# replace survey values with lit values when appropriate

library(dplyr)

thermal_niche <- thermal_niche  |> 
  mutate(
    tmin = case_when(
      COMNAME == "THORNY SKATE" ~ 2,
      COMNAME == "ATLANTIC HERRING" ~ 2.8,
      COMNAME == "ATLANTIC COD" ~ 3,
      COMNAME == "ATLANTIC HALIBUT" ~ 5,
      COMNAME == "WINTER FLOUNDER" ~ 3,
      COMNAME == "SEA SCALLOP" ~ 5,
      COMNAME == "ATLANTIC SALMON" ~ 10.4,
      TRUE ~ tmin
    ),
    tmax = case_when(
      COMNAME == "ATLANTIC HERRING" ~ 19.35,
      COMNAME == "OFFSHORE HAKE" ~ 13,
      COMNAME == "ATLANTIC COD" ~ 11,
      COMNAME == "ATLANTIC HALIBUT" ~ 16,
      COMNAME == "WINTER FLOUNDER" ~ 16,
      COMNAME == "SEA SCALLOP" ~ 10,
      COMNAME == "ATLANTIC SALMON" ~ 20.9,
      TRUE ~ tmax
    )
  )

# Save updated thermal_niche object as an RDS file
saveRDS(
  thermal_niche,
  file = here::here("data-raw", "thermal_niche.rds")
)
