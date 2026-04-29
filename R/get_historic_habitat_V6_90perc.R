# get_historic_habitat_V6_90perc.R
#
# Purpose: Define and visualize historic habitat for NEFMC-managed species.
#
# Version history:
#   V1-V4 — Interpolated approaches (KDE, Concave Hulls).
#   V5    — Strict empirical buffering: 10km dissolved buffers around points.
#   V6    — Strata-based envelope: Historic habitat is defined as the dissolved
#           union of all NEFSC bottom trawl survey strata.
#   V6_90 — Dynamically selects strata based on a Cumulative Observation Target.
#           For each species, identifies the strictest percentage-of-total-observations 
#           cutoff that still retains >= 90% of the species' historical observations.
#
# Output:
#   RDS  : data/historic_habitat_V6_90perc/historic_habitat_V6_90perc.rds
#   Maps : images/historic_habitat_V6_90perc/<species>.png
#
# Dependencies: tidyverse, sf, terra, rnaturalearth, ggnewscale, marmap, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggnewscale)  
library(marmap)      
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data   <- here::here("data/historic_habitat_V6_90perc")
dir_images <- here::here("images/historic_habitat_V6_90perc")

if (!dir.exists(dir_data))   dir.create(dir_data,   recursive = TRUE)
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load survey data
# -------------------------------------------------------------------

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")
survdat <- survdat$survdat

inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")
inshore <- inshore$survdat

survdat <- dplyr::full_join(survdat, inshore)

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

# Windowpane is managed by NEFMC — correct the Fed.Managed field
species <- species |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ne_species <- species |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME, SCINAME, Fed.Managed)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

# Land polygons used in all maps
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)


# -------------------------------------------------------------------
# 3. Habitat parameters
# -------------------------------------------------------------------

habitat_params <- list(
  # The target percentage of total historical observations to retain.
  # The script will find the highest strata inclusion threshold that 
  # still satisfies this target.
  target_retention = 90
)


# -------------------------------------------------------------------
# 4. Survey strata
# -------------------------------------------------------------------

strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"

Sys.setenv(SHAPE_RESTORE_SHX = "YES")

strata <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>       
  sf::st_transform(4326) |>
  sf::st_make_valid()

Sys.unsetenv("SHAPE_RESTORE_SHX")


# -------------------------------------------------------------------
# 5. Bathymetry (visualization only)
# -------------------------------------------------------------------

bathy_marmap <- marmap::getNOAA.bathy(
  lon1 = -82, lon2 = -60,
  lat1 = 34,  lat2 = 48,
  resolution = 1,
  keep = TRUE 
)

bathy <- terra::rast(marmap::as.raster(bathy_marmap))
crs(bathy) <- "EPSG:4326"


# -------------------------------------------------------------------
# 6. Core habitat polygon function
# -------------------------------------------------------------------
# Steps:
#   a. Filter to presence records with valid coordinates.
#   b. Calculate % of total observations falling into each stratum.
#   c. Dynamically identify the threshold that retains >= target_retention.
#   d. Filter strata, dissolve, and subtract land.

get_species_habitat <- function(species_name,
                                survdat_mgmt,
                                strata,
                                params = habitat_params) {
  
  message("Building habitat polygon: ", species_name)
  
  # --- a. Presence records ---
  pts_filtered <- survdat_mgmt |>
    filter(
      COMNAME   == species_name,
      ABUNDANCE >  0,
      !is.na(LAT),
      !is.na(LON)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(CRUISE6, STATION, LAT, LON)
  
  tot_obs <- nrow(pts_filtered)
  
  if (tot_obs == 0) {
    message("  No presence records — skipping.")
    return(NULL)
  }
  
  pts_sf <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
  
  # --- b. Calculate % of total observations per stratum ---
  strata_intersections <- sf::st_intersects(strata, pts_sf)
  strata_obs_counts <- lengths(strata_intersections)
  strata_pct <- (strata_obs_counts / tot_obs) * 100
  
  # --- c. Dynamically find the species-specific cutoff ---
  # Replicating the sensitivity sweep internally (0.1% to 10% steps)
  pct_thresholds <- seq(0.1, 10, by = 0.1)
  
  retained_at_thresh <- map_dbl(pct_thresholds, function(t) {
    sum(strata_obs_counts[strata_pct >= t]) / tot_obs * 100
  })
  
  # Find the highest threshold that retains >= target_retention
  valid_indices <- which(retained_at_thresh >= params$target_retention)
  
  if (length(valid_indices) > 0) {
    sp_thresh <- max(pct_thresholds[valid_indices])
    actual_retained <- retained_at_thresh[max(valid_indices)]
  } else {
    # Fallback if even 0.1% drops too many observations
    sp_thresh <- 0
    actual_retained <- 100
  }
  
  message("  Threshold: \u2265 ", sp_thresh, "% of obs/stratum (Retains ", round(actual_retained, 1), "% of total)")
  
  # --- d. Filter strata ---
  valid_strata <- strata[strata_pct >= sp_thresh & strata_obs_counts > 0, ]
  
  if (nrow(valid_strata) == 0) {
    message("  No strata met the threshold — skipping.")
    return(NULL)
  }
  
  # --- e. Dissolve valid strata ---
  habitat_sf <- valid_strata |>
    sf::st_union() |>
    sf::st_make_valid()
  
  if (is.null(habitat_sf) || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after dissolve — skipping.")
    return(NULL)
  }
  
  # --- f. Subtract land ---
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  habitat_marine <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_marine) || length(habitat_marine) == 0 || all(sf::st_is_empty(habitat_marine))) {
    message("  Empty polygon after land subtraction — skipping.")
    return(NULL)
  }
  
  # --- g. Metadata ---
  habitat_marine <- sf::st_as_sf(habitat_marine) |> 
    rename(geometry = x) |>
    mutate(
      COMNAME            = species_name,
      n_stations_total   = tot_obs,
      n_strata           = nrow(valid_strata),
      target_retention   = params$target_retention,
      threshold_pct_used = sp_thresh,
      actual_retained    = actual_retained
    )
  
  return(habitat_marine)
}


# -------------------------------------------------------------------
# 7. Build habitat polygons for all NEFMC species
# -------------------------------------------------------------------

historic_habitat <- map(
  unique(ne_species$COMNAME),
  ~get_species_habitat(
    species_name = .x,
    survdat_mgmt = survdat_mgmt,
    strata       = strata,
    params       = habitat_params
  )
) |>
  setNames(unique(ne_species$COMNAME))

historic_habitat <- Filter(Negate(is.null), historic_habitat)

message(length(historic_habitat), " species habitat polygons built.")


# -------------------------------------------------------------------
# 8. Save RDS
# -------------------------------------------------------------------

save_path <- here::here("data/historic_habitat_V6_90perc/historic_habitat_V6_90perc.rds")
saveRDS(historic_habitat, save_path)

message("RDS saved to: ", save_path)


# -------------------------------------------------------------------
# 9. Visualization function
# -------------------------------------------------------------------

map_historic_habitat_v6_90 <- function(species_name,
                                       historic_habitat,
                                       survdat_mgmt,
                                       strata,
                                       bathy,
                                       params  = habitat_params,
                                       out_dir = dir_images) {
  
  poly <- historic_habitat[[species_name]]
  if (is.null(poly)) return(invisible(NULL))
  
  all_pts <- survdat_mgmt |>
    filter(COMNAME == species_name, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(CRUISE6, STATION, LAT, LON, DEPTH)
  
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(c(bbox["xmin"], bbox["xmax"])) * 0.15)
  ypad <- max(2, diff(c(bbox["ymin"], bbox["ymax"])) * 0.15)
  xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
  ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)
  
  bathy_crop <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
  bathy_df   <- as.data.frame(bathy_crop, xy = TRUE)
  colnames(bathy_df)[3] <- "depth"
  bathy_df <- bathy_df |>
    filter(depth < 0) |>
    mutate(depth_capped = pmax(depth, -2000))
  
  p <- ggplot() +
    
    geom_tile(
      data = bathy_df,
      aes(x = x, y = y, fill = depth_capped)
    ) +
    scale_fill_gradientn(
      colors   = c("grey15", "grey45", "grey75", "grey92"),
      values   = scales::rescale(c(-2000, -500, -200, 0)),
      limits   = c(-2000, 0),
      name     = "Depth (m)",
      na.value = "white",
      labels   = function(x) ifelse(x == -2000, "\u2264 -2000", as.character(x))
    ) +
    
    ggnewscale::new_scale_fill() +
    
    # Habitat polygon (Dissolved Strata)
    geom_sf(
      data      = poly,
      fill      = "seagreen",
      color     = "seagreen4",
      alpha     = 0.45,
      linewidth = 0.6
    ) +
    
    # Unselected survey strata boundaries for context
    geom_sf(
      data      = strata,
      fill      = NA,
      color     = "orange",
      linetype  = "solid",
      linewidth = 0.2,
      alpha     = 0.5
    ) +
    
    # All presence points
    geom_point(
      data  = all_pts,
      aes(x = LON, y = LAT, color = DEPTH),
      size  = 1.0,
      alpha = 0.8
    ) +
    scale_color_viridis_c(
      name      = "Obs depth (m)",
      option    = "plasma",
      direction = -1,
      na.value  = "grey50"
    ) +
    
    geom_sf(data = land, fill = "grey35", color = NA) +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    labs(
      title = paste0(
        tools::toTitleCase(tolower(species_name)),
        " \u2014 Historic Habitat Envelope (V6 90%)"
      ),
      subtitle = paste0(
        "Strata: ", poly$n_strata,
        "  |  Threshold: \u2265 ", poly$threshold_pct_used, "% of total obs",
        "  |  Total Retained: ", round(poly$actual_retained, 1), "%"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Green: Union of NEFSC survey strata defining the core area containing \u2265 ", params$target_retention, "% of all historical observations. ",
        "Orange outlines: Full NEFSC bottom trawl survey grid. Bathymetry gradient capped at -2000 m."
      )
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "right",
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7, color = "grey50"),
      panel.grid.major = element_line(color = "grey70", linewidth = 0.2)
    )
  
  file_name <- file.path(
    out_dir,
    paste0(gsub(" ", "_", species_name), "_historic_habitat_V6_90perc.png")
  )
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 300)
  
  message("  Saved: ", file_name)
  invisible(p)
}


# -------------------------------------------------------------------
# 10. Generate maps for all species
# -------------------------------------------------------------------

walk(
  names(historic_habitat),
  ~map_historic_habitat_v6_90(
    species_name     = .x,
    historic_habitat = historic_habitat,
    survdat_mgmt     = survdat_mgmt,
    strata           = strata,
    bathy            = bathy,
    params           = habitat_params
  )
)

# -------------------------------------------------------------------
# 11. Summary table
# -------------------------------------------------------------------

habitat_summary <- map_dfr(
  names(historic_habitat),
  ~{
    poly <- historic_habitat[[.x]]
    tibble(
      species            = .x,
      n_stations_total   = poly$n_stations_total,
      n_strata           = poly$n_strata,
      threshold_pct_used = poly$threshold_pct_used,
      actual_retained    = poly$actual_retained,
      area_km2           = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)