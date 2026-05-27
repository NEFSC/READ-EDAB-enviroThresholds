# plot_habitat_timeseries.R
#
# Purpose: Visualize the annual distribution of survey observations over time.
#          Creates a faceted map for each species, comparing the strata with 
#          >= 3 observations in a given year against the entire Historic V6 footprint.
#
# Output:
#   Plots : images/habitat_timeseries/<species>_habitat_timeseries.png
#
# Dependencies: tidyverse, sf, rnaturalearth, scales, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)
library(scales) 
library(here)

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_images <- here::here("images/habitat_timeseries")
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load Spatial Context Data (Land, Strata)
# -------------------------------------------------------------------

message("Loading spatial basemaps...")

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)

strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
strata_sf <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>       
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  mutate(strata_uid = row_number()) 
Sys.unsetenv("SHAPE_RESTORE_SHX")

historic_v6_file <- here::here("data/historic_habitat_V6/historic_habitat_V6.rds")
if (!file.exists(historic_v6_file)) stop("Historic V6 RDS not found.")
historic_v6 <- readRDS(historic_v6_file)


# -------------------------------------------------------------------
# 3. Load and Prep Survey Data
# -------------------------------------------------------------------

message("Loading and cleaning survey data...")

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat, inshore, by = join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ne_species <- species |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME)

obs_clean <- survdat |>
  inner_join(ne_species, by = "SVSPP") |>
  filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON), !is.na(DEPTH)) |>
  mutate(
    LAT  = as.numeric(LAT), 
    LON  = as.numeric(LON),
    year = as.numeric(YEAR)
  ) |>
  distinct(COMNAME, year, CRUISE6, STATION, LAT, LON, DEPTH) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

species_list <- sort(unique(obs_clean$COMNAME))


# -------------------------------------------------------------------
# 4. Generate Timeseries Plots per Species
# -------------------------------------------------------------------

message("Generating timeseries plots for ", length(species_list), " species...")

walk(species_list, function(sp) {
  
  sp_obs <- obs_clean |> filter(COMNAME == sp)
  historic_poly <- historic_v6[[sp]]
  
  if (nrow(sp_obs) == 0 || is.null(historic_poly)) return(NULL)
  
  message("  Plotting: ", sp)
  
  # --- Calculate Historic V6 Area for Subtitle ---
  v6_area_km2 <- as.numeric(sum(sf::st_area(historic_poly))) / 1e6
  v6_area_fmt <- scales::comma(round(v6_area_km2, 0))
  
  # --- Calculate Annual >= 3 Obs Habitat ---
  sp_obs_with_strata <- sf::st_join(sp_obs, strata_sf)
  
  annual_strata_counts <- sp_obs_with_strata |>
    sf::st_drop_geometry() |>
    filter(!is.na(strata_uid)) |>
    group_by(year, strata_uid) |>
    summarise(n_stations = n_distinct(paste(CRUISE6, STATION)), .groups = "drop") |>
    filter(n_stations >= 3)
  
  annual_habitat_sf <- strata_sf |>
    inner_join(annual_strata_counts, by = "strata_uid")
  
  # --- Dynamic Bounding Box ---
  bbox <- sf::st_bbox(historic_poly)
  xpad <- diff(c(bbox["xmin"], bbox["xmax"])) * 0.15
  ypad <- diff(c(bbox["ymin"], bbox["ymax"])) * 0.15
  xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
  ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)
  
  # --- Calculate Annual Area for Facet Labels ---
  all_years <- tibble(year = unique(sp_obs$year))
  
  annual_area_labels <- annual_habitat_sf |>
    mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6) |>
    sf::st_drop_geometry() |>
    group_by(year) |>
    summarise(total_annual_area = sum(area_km2), .groups = "drop") |>
    right_join(all_years, by = "year") |>
    mutate(
      total_annual_area = replace_na(total_annual_area, 0),
      label = paste0(scales::comma(round(total_annual_area, 0)), " km\u00b2"),
      x_pos = xlim[2], 
      y_pos = ylim[1]  
    )
  
  # --- Build the Plot ---
  p_timeseries <- ggplot() +
    
    # 1. Land (Green overlay)
    geom_sf(data = land, fill = "darkseagreen", color = "darkolivegreen", linewidth = 0.2) +
    
    # 2. Base Strata Outline
    geom_sf(data = strata_sf, fill = NA, color = "black", linewidth = 0.1, alpha = 0.4) +
    
    # 3. Annual Identified Habitat (Steelblue fill)
    geom_sf(data = annual_habitat_sf, fill = "steelblue", alpha = 0.5, color = NA) +
    
    # 4. Survey Observations
    geom_sf(data = sp_obs, color = "darkblue", size = 0.1, alpha = 0.3, shape = 16) +
    
    # 5. Historic V6 Footprint (Red outline)
    geom_sf(data = historic_poly, fill = NA, color = "firebrick", linewidth = 0.4) +
    
    # 6. Annual Area Text Label
    geom_text(
      data = annual_area_labels,
      aes(x = x_pos, y = y_pos, label = label),
      hjust = 1.1, vjust = -0.5, size = 1.8, fontface = "bold", color = "black"
    ) +
    
    facet_wrap(~year, ncol = 8) + 
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Annual vs. Historic Habitat Footprint"),
      subtitle = paste0("Red Outline: Long-term Historic V6 Habitat (Total Area: ", v6_area_fmt, " km\u00b2).\nSteelblue Fill: Strata with \u2265 3 observations in that specific year (Area in bottom right).\nDark Blue Points: Survey observations."),
      caption = "Data: NEFSC Bottom Trawl Survey"
    ) +
    
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
      plot.subtitle = element_text(color = "grey30", size = 12, margin = margin(b = 15)),
      plot.caption = element_text(color = "grey50", hjust = 0, margin = margin(t = 10)),
      strip.text = element_text(face = "bold", size = 10, margin = margin(b = 4, t = 4)),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
      panel.background = element_rect(fill = "#f2f7fb", color = NA)
    )
  
  # --- Save the Plot ---
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_images, paste0(safe_name, "_habitat_timeseries.png"))
  
  ggsave(filename = file_name, plot = p_timeseries, width = 16, height = 10, dpi = 300, bg = "white")
})

message("All timeseries plots saved to: ", dir_images)