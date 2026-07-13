# i18-number-of-tows-per-strata-per-year.R
#
# Purpose: Evaluate sampling effort across NEFSC survey strata over time.
#          Specifically identifies strata that frequently have < 3 tows in a 
#          given year, as these strata would be systematically excluded from 
#          the V6 historic habitat methodology regardless of species presence.
#
# Output:
#   CSV : data-raw/i18_tows_per_strata/strata_tows_per_year.csv
#   Map : data-raw/i18_tows_per_strata/strata_tows_evaluation_map.png
#
# Dependencies: tidyverse, sf, rnaturalearth, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_out <- here::here("data-raw/i18_tows_per_strata")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load Survey Data & Strata
# -------------------------------------------------------------------

message("Loading survey data and spatial strata...")

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat, inshore, by = join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

# Extract ALL unique survey stations (ignoring species catch)
all_stations <- survdat |>
  filter(!is.na(LAT), !is.na(LON), !is.na(YEAR)) |>
  mutate(
    LAT = as.numeric(LAT), 
    LON = as.numeric(LON), 
    YEAR = as.numeric(YEAR)
  ) |>
  distinct(YEAR, CRUISE6, STATION, LAT, LON) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

# Load strata
strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
strata <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>        
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  mutate(strata_uid = row_number())
Sys.unsetenv("SHAPE_RESTORE_SHX")

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)


# -------------------------------------------------------------------
# 3. Calculate Tows per Strata per Year
# -------------------------------------------------------------------

message("Spatially joining stations to strata and tallying tows...")

stations_with_strata <- sf::st_join(all_stations, strata)

# Count tows per year, forcing 0 for years where a stratum wasn't sampled
strata_yearly_counts <- stations_with_strata |>
  sf::st_drop_geometry() |>
  filter(!is.na(strata_uid)) |>
  group_by(strata_uid, YEAR) |>
  summarise(n_tows = n_distinct(paste(CRUISE6, STATION)), .groups = "drop") |>
  # complete() ensures that if a stratum had 0 tows in a year, it gets a 0 instead of NA
  complete(strata_uid, YEAR, fill = list(n_tows = 0))


# -------------------------------------------------------------------
# 4. Generate Tabular Output
# -------------------------------------------------------------------

message("Generating wide-format table of tows per year...")

# Format for easy reading: Strata as rows, Years as columns
tows_table <- strata_yearly_counts |>
  pivot_wider(
    names_from = YEAR,
    values_from = n_tows,
    names_sort = TRUE
  ) |>
  arrange(strata_uid)

csv_file <- file.path(dir_out, "strata_tows_per_year.csv")
write_csv(tows_table, csv_file)
message("  Saved table to: ", csv_file)


# -------------------------------------------------------------------
# 5. Summarize for Map Visualization
# -------------------------------------------------------------------

message("Preparing diagnostic map...")

# Calculate how many years each stratum failed to meet the >= 3 threshold
strata_summary <- strata_yearly_counts |>
  group_by(strata_uid) |>
  summarise(
    total_years = n(),
    years_below_3 = sum(n_tows < 3),
    .groups = "drop"
  ) |>
  mutate(
    # Create the color categories requested
    reliability_category = case_when(
      years_below_3 == 0 ~ "0 Years (Always \u2265 3 Tows)",
      years_below_3 <= 5  ~ "1 - 5 Years < 3 Tows",
      years_below_3 <= 15 ~ "6 - 15 Years < 3 Tows",
      years_below_3 <= 30 ~ "16 - 30 Years < 3 Tows",
      TRUE                ~ "> 30 Years < 3 Tows"
    ),
    reliability_category = factor(reliability_category, levels = c(
      "0 Years (Always \u2265 3 Tows)",
      "1 - 5 Years < 3 Tows",
      "6 - 15 Years < 3 Tows",
      "16 - 30 Years < 3 Tows",
      "> 30 Years < 3 Tows"
    ))
  )

# Join back to spatial sf object
strata_map_data <- strata |>
  left_join(strata_summary, by = "strata_uid")

# Define the custom color palette (Color-blind friendly Okabe-Ito palette)
custom_colors <- c(
  "0 Years (Always \u2265 3 Tows)" = "#0072B2", # Dark Blue (Most Reliable)
  "1 - 5 Years < 3 Tows"           = "#56B4E9", # Sky Blue
  "6 - 15 Years < 3 Tows"          = "#F0E442", # Yellow
  "16 - 30 Years < 3 Tows"         = "#E69F00", # Orange
  "> 30 Years < 3 Tows"            = "#D55E00"  # Vermillion (Least Reliable)
)

# -------------------------------------------------------------------
# 6. Plot the Map
# -------------------------------------------------------------------

bbox <- sf::st_bbox(strata)

p_map <- ggplot() +
  geom_sf(data = strata_map_data, aes(fill = reliability_category), color = "grey30", linewidth = 0.1) +
  geom_sf(data = land, fill = "grey80", color = NA) +
  
  scale_fill_manual(
    values = custom_colors,
    name = "Strata Reliability\n(Threshold = 3 Tows)",
    na.translate = FALSE
  ) +
  
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]), 
    ylim = c(bbox["ymin"], bbox["ymax"]), 
    expand = FALSE
  ) +
  
  labs(
    title = "Survey Strata Reliability (V6 Threshold Check)",
    subtitle = "Evaluating how often strata fail to achieve \u2265 3 tows in a single year.",
    caption = "Strata with high failure rates (Orange/Vermillion) are structurally biased against inclusion in the V6 method."
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 15)),
    panel.background = element_rect(fill = "#f2f7fb", color = NA),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

map_file <- file.path(dir_out, "strata_tows_evaluation_map.png")
ggsave(map_file, plot = p_map, width = 10, height = 8, dpi = 300, bg = "white")

message("  Saved diagnostic map to: ", map_file)
message("Script complete.")