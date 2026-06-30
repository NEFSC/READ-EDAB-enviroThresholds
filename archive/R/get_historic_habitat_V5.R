# get_historic_habitat_V5.R
#
# Purpose: Define and visualize historic habitat for NEFMC-managed species.
#
# Version history:
#   V1 — Initial approach: KDE binary raster threshold, concave hull envelope.
#   V2 — Geometric fixes: isoband contour polygon (no right-angle artifacts),
#         survey footprint clip (no coastal bleeding), bathymetry depth mask
#         (removes depth-avoidance areas such as the Great South Channel).
#   V3 — Collaborator feedback: depth masking removed. Bathymetry retained in
#         visualization only with gradient capped at -2000 m. KDE contour level
#         set to species-specific 95% station retention threshold from
#         explore_habitat_params.R. Buffer tightened to 5 km.
#   V4 — Collaborator feedback: KDE abandoned entirely. Habitat envelope is a
#         concave hull drawn directly around all density-filtered stations.
#         Buffer removed entirely.
#   V5 — Collaborator feedback: return to the original concept of defining
#         habitat as the observed station locations themselves. Each qualifying
#         station receives an equal-area
#         circular buffer, and all circles are dissolved into a single habitat
#         polygon. This avoids the interpolation across gaps that a concave hull
#         introduces — habitat only exists where the species was actually seen.
#         Buffer distance is a placeholder (10 km) pending sensitivity analysis
#         in sensitivity_site_buffer.R.
#
# Changes from V4:
#   - concaveman package no longer needed or loaded.
#   - S2 toggle workaround removed — buffered point unions are well-behaved
#     geometries that do not produce self-intersections.
#   - habitat_params: concavity removed, buffer_m added back.
#   - get_species_habitat() step c: concave hull replaced with per-point
#     buffering in a projected CRS (NAD83 / Conus Albers, EPSG:5070) followed
#     by st_union() to dissolve overlapping circles into one polygon.
#   - Survey footprint intersection retained (prevents extension into
#     unsurveyed areas).
#   - Land subtraction retained.
#   - Map subtitle and caption updated to reflect V5 approach.
#   - All output paths, filenames, and labels updated to V5.
#
# Output:
#   RDS  : data/historic_habitat_V5/historic_habitat_V5.rds
#   Maps : images/historic_habitat_V5/<species>.png
#
# Dependencies: tidyverse, sf, terra, rnaturalearth, ggnewscale, marmap, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggnewscale)  # new_scale_fill() — overlay two fill scales in ggplot
library(marmap)      # getNOAA.bathy — pulls ETOPO directly into R
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data   <- here::here("data/historic_habitat_V5")
dir_images <- here::here("images/historic_habitat_V5")

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

thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

# Land polygons used in all maps
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)


# -------------------------------------------------------------------
# 3. Habitat parameters
# -------------------------------------------------------------------

habitat_params <- list(
  
  # Buffer radius (meters) applied around each observation point to build
  # the habitat polygon. Overlapping circles are dissolved into one polygon.
  # This is a placeholder — run sensitivity_site_buffer.R to identify the
  # biologically appropriate distance for your species and survey grid.
  buffer_m = 10000,  # 10 km placeholder — update after sensitivity analysis
  
  # Maximum depth (meters) used to clip the buffered habitat polygon.
  # All observations contribute regardless of depth — this mask only removes
  # parts of buffer circles that fall into water deeper than this threshold,
  # preventing buffers around shelf-break stations from extending off-shelf.
  max_depth_m = 500
  
)


# -------------------------------------------------------------------
# 4. Survey strata
# -------------------------------------------------------------------
# Load the NEFSC bottom trawl survey strata shapefile from the StockEff
# repository. The dissolved union of all strata defines the survey extent
# used to clip habitat polygons (preventing buffers from extending into
# areas never sampled) and is shown as an overlay on the maps.

#   https://github.com/NEFSC/READ-PDB-StockEff/tree/dev/gis_files

strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"

Sys.setenv(SHAPE_RESTORE_SHX = "YES")

strata <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>       # assign NAD83 — change to 4267 if shapes look wrong
  sf::st_transform(4326) |>
  sf::st_make_valid()

Sys.unsetenv("SHAPE_RESTORE_SHX")

plot(sf::st_geometry(strata))

# Single dissolved polygon used for habitat clipping
survey_footprint <- strata |>
  sf::st_union() |>
  sf::st_make_valid()


# -------------------------------------------------------------------
# 5. Bathymetry (visualization only — not used for masking)
# -------------------------------------------------------------------
# Downloads ETOPO 2022 from NOAA via marmap and caches locally.
# resolution = 1 arc-minute (~1.8 km) is sufficient for shelf visualization.

bathy_marmap <- marmap::getNOAA.bathy(
  lon1 = -82, lon2 = -60,
  lat1 = 34,  lat2 = 48,
  resolution = 1,
  keep = TRUE   # saves CSV to working directory; reloads on repeat runs
)

# Convert to terra SpatRaster (negative values = ocean depth)
bathy <- terra::rast(marmap::as.raster(bathy_marmap))
crs(bathy) <- "EPSG:4326"


# -------------------------------------------------------------------
# 6. Core habitat polygon function
# -------------------------------------------------------------------
# Steps:
#   a. Filter to presence records with valid coordinates.
#   b. Buffer all unique observation locations by buffer_m in a projected CRS,
#      then dissolve all circles into a single polygon.
#      Using equal-area projection (NAD83 / Conus Albers, EPSG:5070) ensures
#      buffer distances are in true meters rather than decimal degrees.
#   c. Depth mask — clip polygon to water <= max_depth_m.
#   d. Intersect with survey extent (dissolved strata union — prevents extension
#      into areas never sampled by the NEFSC bottom trawl survey).
#   e. Subtract land.
#   f. Attach metadata.

get_species_habitat <- function(species_name,
                                survdat_mgmt,
                                survey_footprint,
                                bathy,
                                params = habitat_params) {
  
  message("Building habitat polygon: ", species_name)
  
  # --- a. Presence records ---
  # All presence records are included. The depth threshold in params$max_depth_m
  # is applied later to clip the buffered polygon, not to filter observations.
  # Collapse to one row per unique lat/lon so each location gets one buffer
  # circle regardless of how many times it appears in the data.
  pts_filtered <- survdat_mgmt |>
    filter(
      COMNAME   == species_name,
      ABUNDANCE >  0,
      !is.na(LAT),
      !is.na(LON)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(LAT, LON)
  
  if (nrow(pts_filtered) == 0) {
    message("  No presence records — skipping.")
    return(NULL)
  }
  
  message("  ", nrow(pts_filtered), " unique observation locations.")
  
  # --- b. Buffer and dissolve ---
  # Project to NAD83 / Conus Albers (meters) for accurate buffering,
  # apply equal-radius buffer to every station, dissolve overlapping
  # circles into one polygon, then reproject back to WGS84.
  habitat_sf <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    sf::st_transform(5070) |>
    sf::st_buffer(params$buffer_m) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_as_sf() |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
  
  if (nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after buffering — skipping.")
    return(NULL)
  }
  
  # --- c. Depth mask ---
  # Clip the buffered polygon to water <= max_depth_m. This removes the parts
  # of buffer circles that extend into deep off-shelf water without excluding
  # any of the observations that drove the buffer placement.
  bathy_ext <- terra::ext(
    sf::st_bbox(habitat_sf)[c("xmin", "xmax", "ymin", "ymax")]
  )
  bathy_crop  <- terra::crop(bathy, bathy_ext)
  depth_mask  <- terra::ifel(bathy_crop >= -params$max_depth_m & bathy_crop < 0, 1, NA)
  depth_poly  <- terra::as.polygons(depth_mask, dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  if (nrow(depth_poly) > 0) {
    habitat_sf <- suppressWarnings(
      sf::st_intersection(habitat_sf, sf::st_union(depth_poly))
    ) |> sf::st_make_valid()
  }
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after depth mask — skipping.")
    return(NULL)
  }
  
  # --- d. Intersect with survey footprint ---
  habitat_sf <- suppressWarnings(
    sf::st_intersection(habitat_sf, survey_footprint)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after survey footprint intersection — skipping.")
    return(NULL)
  }
  
  # --- e. Subtract land ---
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  habitat_marine <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_marine) || nrow(habitat_marine) == 0 || all(sf::st_is_empty(habitat_marine))) {
    message("  Empty polygon after land subtraction — skipping.")
    return(NULL)
  }
  
  # --- f. Metadata ---
  habitat_marine |>
    mutate(
      COMNAME    = species_name,
      n_stations = nrow(pts_filtered),
      buffer_m   = params$buffer_m,
      max_depth_m = params$max_depth_m
    )
}


# -------------------------------------------------------------------
# 7. Build habitat polygons for all NEFMC species
# -------------------------------------------------------------------

historic_habitat <- map(
  unique(ne_species$COMNAME),
  ~get_species_habitat(
    species_name     = .x,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    bathy            = bathy,
    params           = habitat_params
  )
) |>
  setNames(unique(ne_species$COMNAME))

historic_habitat <- Filter(Negate(is.null), historic_habitat)

message(length(historic_habitat), " species habitat polygons built.")


# -------------------------------------------------------------------
# 8. Save RDS
# -------------------------------------------------------------------

saveRDS(
  historic_habitat,
  here::here("data/historic_habitat_V5/historic_habitat_V5.rds")
)

message("RDS saved to: ", here::here("data/historic_habitat_V5/historic_habitat_V5.rds"))


# -------------------------------------------------------------------
# 9. Visualization function
# -------------------------------------------------------------------

map_historic_habitat_v5 <- function(species_name,
                                    historic_habitat,
                                    survdat_mgmt,
                                    strata,
                                    bathy,
                                    params  = habitat_params,
                                    out_dir = dir_images) {
  
  poly <- historic_habitat[[species_name]]
  if (is.null(poly)) {
    message("No polygon available for ", species_name)
    return(invisible(NULL))
  }
  
  # All presence points — all depths shown for context
  all_pts <- survdat_mgmt |>
    filter(COMNAME == species_name, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(CRUISE6, STATION, LAT, LON, DEPTH)
  
  # All presence points also serve as the colored points — no density filter.
  # Every location where the species was observed shaped the habitat polygon.
  density_pts <- all_pts |> distinct(LAT, LON, DEPTH)
  
  # Map extent: polygon bbox + padding
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(c(bbox["xmin"], bbox["xmax"])) * 0.15)
  ypad <- max(2, diff(c(bbox["ymin"], bbox["ymax"])) * 0.15)
  xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
  ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)
  
  # Bathymetry cropped to map extent, gradient capped at -2000 m
  bathy_crop <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
  bathy_df   <- as.data.frame(bathy_crop, xy = TRUE)
  colnames(bathy_df)[3] <- "depth"
  bathy_df <- bathy_df |>
    filter(depth < 0) |>
    mutate(depth_capped = pmax(depth, -2000))
  
  p <- ggplot() +
    
    # Bathymetry — gradient capped at -2000 m for on-shelf contrast
    # geom_tile() used instead of geom_raster() to handle slightly uneven
    # pixel spacing in the reprojected ETOPO raster without warnings.
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
    
    # Second fill scale for the habitat polygon
    ggnewscale::new_scale_fill() +
    
    # Habitat polygon
    geom_sf(
      data      = poly,
      fill      = "steelblue",
      color     = "steelblue4",
      alpha     = 0.45,
      linewidth = 0.5
    ) +
    
    # Survey strata boundaries — thin orange outlines
    # Individual strata polygons are shown so strata structure is visible.
    geom_sf(
      data      = strata,
      fill      = NA,
      color     = "orange",
      linetype  = "solid",
      linewidth = 0.2,
      alpha     = 0.7
    ) +
    
    # All presence points (light grey background layer)
    geom_point(
      data  = all_pts,
      aes(x = LON, y = LAT),
      color = "grey90",
      size  = 0.5,
      alpha = 0.5
    ) +
    
    # Density-filtered points colored by observed depth
    geom_point(
      data = density_pts,
      aes(x = LON, y = LAT, color = DEPTH),
      size  = 1.2,
      alpha = 0.8
    ) +
    scale_color_viridis_c(
      name      = "Obs depth (m)",
      option    = "plasma",
      direction = -1,
      na.value  = "grey50"
    ) +
    
    # Land on top
    geom_sf(data = land, fill = "grey35", color = NA) +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    labs(
      title = paste0(
        tools::toTitleCase(tolower(species_name)),
        " \u2014 Historic Habitat Envelope (V5)"
      ),
      subtitle = paste0(
        "Unique locations: ", nrow(density_pts |> distinct(LAT, LON)),
        "  |  Buffer: ", params$buffer_m / 1000, " km",
        "  |  Depth clip: ", params$max_depth_m, " m"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Blue: union of ", params$buffer_m / 1000, " km buffers around all presence locations ",
        "(survey-footprint clip applied; buffer clipped to \u2264 ", params$max_depth_m, " m depth). ",
        "Orange outlines: NEFSC bottom trawl survey strata (StockEff). Bathymetry gradient capped at -2000 m."
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
    paste0(gsub(" ", "_", species_name), "_historic_habitat_V5.png")
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
  ~map_historic_habitat_v5(
    species_name     = .x,
    historic_habitat = historic_habitat,
    survdat_mgmt     = survdat_mgmt,
    strata           = strata,
    bathy            = bathy,
    params           = habitat_params
  )
)


# -------------------------------------------------------------------
# 11. Spot-check: Atlantic Cod
# -------------------------------------------------------------------
# Uncomment to run interactively

# map_historic_habitat_v5(
#   species_name     = "ATLANTIC COD",
#   historic_habitat = historic_habitat,
#   survdat_mgmt     = survdat_mgmt,
#   strata           = strata,
#   bathy            = bathy
# )

# historic_habitat[["ATLANTIC COD"]]


# -------------------------------------------------------------------
# 12. Summary table
# -------------------------------------------------------------------

habitat_summary <- map_dfr(
  names(historic_habitat),
  ~{
    poly <- historic_habitat[[.x]]
    tibble(
      species    = .x,
      n_stations = poly$n_stations,
      buffer_m   = poly$buffer_m,
      area_km2   = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)
