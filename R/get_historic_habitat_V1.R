# get_historic_habitat_V1.R
#
# Purpose: Define the most conservative "V1" historic habitat for NEFMC species.
#          Habitat is defined strictly as the exact unique lat/lon combinations 
#          where a species was historically observed.
#
# Logic:   1. Extract unique observed LAT/LON coordinates for each species.
#          2. Convert to sf POINT geometry.
#          3. Apply a nominal 1km buffer to turn points into tiny polygons. 
#             (This is mathematically required so subsequent validation scripts 
#             can calculate area ratios rather than dividing by zero).
#
# Output:
#   RDS  : data/historic_habitat_V1/historic_habitat_V1.rds
#
# Dependencies: tidyverse, sf, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data <- here::here("data/historic_habitat_V1")
if (!dir.exists(dir_data)) dir.create(dir_data, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load survey data
# -------------------------------------------------------------------

message("Loading survey data...")

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat, inshore, by = join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

# Windowpane is managed by NEFMC
species <- species |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ne_species <- species |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

# -------------------------------------------------------------------
# 3. Core Extraction Function
# -------------------------------------------------------------------

get_v1_habitat <- function(species_name, survdat_mgmt) {
  
  message("Extracting exact observation points for: ", species_name)
  
  # Filter to unique presence locations
  pts_filtered <- survdat_mgmt |>
    filter(
      COMNAME   == species_name,
      ABUNDANCE >  0,
      !is.na(LAT),
      !is.na(LON)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    # Keep only the unique Lat/Lon combos
    distinct(LAT, LON)
  
  if (nrow(pts_filtered) == 0) {
    message("  No presence records — skipping.")
    return(NULL)
  }
  
  # Convert to spatial points, transform to meters (EPSG:5070), 
  # buffer by 1km, and transform back to Lat/Lon (EPSG:4326)
  habitat_marine <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    sf::st_transform(5070) |>
    sf::st_buffer(dist = 1000) |> # 1 km radius to represent trawl footprint
    sf::st_transform(4326) |>
    sf::st_union() |> # Combine overlapping buffers into a single footprint
    sf::st_make_valid()
  
  # Attach metadata
  habitat_marine <- sf::st_as_sf(habitat_marine) |> 
    rename(geometry = x) |>
    mutate(
      COMNAME = species_name,
      n_unique_locations = nrow(pts_filtered)
    )
  
  return(habitat_marine)
}

# -------------------------------------------------------------------
# 4. Build habitat polygons for all NEFMC species
# -------------------------------------------------------------------

message("\nBuilding V1 habitat for all species...")

historic_habitat_v1 <- map(
  unique(ne_species$COMNAME),
  ~get_v1_habitat(
    species_name = .x,
    survdat_mgmt = survdat_mgmt
  )
) |>
  setNames(unique(ne_species$COMNAME))

# Remove any NULLs
historic_habitat_v1 <- Filter(Negate(is.null), historic_habitat_v1)

message("\nSuccessfully built V1 exact observation footprints for ", length(historic_habitat_v1), " species.")

# -------------------------------------------------------------------
# 5. Save RDS
# -------------------------------------------------------------------

out_file <- file.path(dir_data, "historic_habitat_V1.rds")
saveRDS(historic_habitat_v1, out_file)

message("Results saved to: ", out_file)