# get_historic_habitat_V4.R
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
#   V4 — Collaborator feedback: KDE abandoned entirely. Habitat envelope is now
#         a concave hull drawn directly around all density-filtered stations
#         (stations observed >= 5 times), giving smooth rounded edges without
#         any contour level parameter to tune. Buffer removed entirely — the
#         5 km buffer in V3 was including too much off-shelf area. The concave
#         hull already produces a naturally tight boundary around observations.
#
# Changes from V3:
#   - kde_to_polygon() helper removed. MASS and isoband no longer needed.
#   - KDE parameters (kde_bw_multiplier, kde_n, kde_contour_level) removed
#     from habitat_params. A concavity parameter is added instead (see below).
#   - buffer_m removed from habitat_params. st_buffer() step removed from
#     get_species_habitat().
#   - Species-specific contour lookup (Section 4 in V3) removed entirely.
#   - get_species_habitat() step c now builds a concave hull via concaveman().
#   - Map subtitle updated to reflect new approach.
#   - All output paths, filenames, and labels updated to V4.
#
# Output:
#   RDS  : data/historic_habitat_V4/historic_habitat_V4.rds
#   Maps : images/historic_habitat_V4/<species>.png
#
# Dependencies: tidyverse, sf, terra, concaveman, rnaturalearth,
#               ggnewscale, marmap, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(concaveman)  # concave hull around point set
library(rnaturalearth)
library(ggnewscale)  # new_scale_fill() — overlay two fill scales in ggplot
library(marmap)      # getNOAA.bathy — pulls ETOPO directly into R
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data   <- here::here("data/historic_habitat_V4")
dir_images <- here::here("images/historic_habitat_V4")

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
  
  # Minimum number of times a station must appear in presence records
  # across all years before it contributes to the habitat envelope.
  min_station_count = 5,
  
  # Concavity parameter passed to concaveman::concaveman().
  # Controls how tightly the hull wraps around the point cloud.
  # 1 = tightest possible concave hull (may be spiky for sparse data).
  # 2 = default, good balance between detail and smoothness.
  # Higher values approach the convex hull.
  # Increase if the hull produces narrow inlets or bridges over open water.
  concavity = 2
  
)


# -------------------------------------------------------------------
# 4. Survey footprint
# -------------------------------------------------------------------
# Convex hull of all ever-sampled stations. Each species' habitat polygon
# is intersected with this so the concave hull cannot extend into areas
# never surveyed.

all_stations <- survdat |>
  filter(!is.na(LAT), !is.na(LON)) |>
  mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
  distinct(STATION, LAT, LON)

survey_footprint <- all_stations |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
  sf::st_union() |>
  sf::st_convex_hull() |>
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
#   b. Minimum station observation count filter (>= min_station_count).
#   c. Concave hull around filtered stations (replaces KDE + contour in V3).
#      concaveman() produces smooth, rounded edges without any density
#      surface or contour level parameter to tune.
#   d. Intersect with survey footprint (prevents extension into unsurveyed areas).
#   e. Subtract land.
#   f. Attach metadata.
#
# Note: buffer step removed in V4 — the 5 km buffer in V3 was pulling in
# too much off-shelf area. The concave hull already sits tightly around
# the observations.

get_species_habitat <- function(species_name,
                                survdat_mgmt,
                                survey_footprint,
                                params = habitat_params) {
  
  message("Building habitat polygon: ", species_name)
  
  # --- a. Presence records ---
  pts <- survdat_mgmt |>
    filter(
      COMNAME   == species_name,
      ABUNDANCE >  0,
      !is.na(LAT),
      !is.na(LON)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON))
  
  if (nrow(pts) < params$min_station_count) {
    message("  Insufficient records — skipping.")
    return(NULL)
  }
  
  # --- b. Density filter ---
  # Retain only stations observed >= min_station_count times across all years.
  station_counts <- pts |> count(STATION, name = "n_obs")
  
  pts_filtered <- pts |>
    inner_join(station_counts, by = "STATION") |>
    filter(n_obs >= params$min_station_count) |>
    distinct(STATION, LAT, LON)
  
  if (nrow(pts_filtered) < 3) {
    message("  Too few stations after density filter — skipping.")
    return(NULL)
  }
  
  # --- c. Concave hull ---
  # Convert filtered stations to sf and compute a concave hull.
  # concaveman() wraps tightly around the point cloud and produces
  # smooth, naturally rounded edges — no KDE, no contour level needed.
  pts_sf <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
  
  # Self-intersection handling:
  # sf uses the S2 spherical geometry engine by default, which enforces strict
  # edge-crossing rules and throws "Edge X crosses Edge Y" errors during
  # st_make_valid() and boolean operations on geometries that GEOS would
  # silently repair. Temporarily disabling S2 allows GEOS to handle the repair
  # instead, which is more tolerant of minor self-intersections from concaveman().
  # S2 is restored in an on.exit() call so it is always re-enabled even if the
  # function exits early due to an error or skipped species.
  s2_was_on <- sf::sf_use_s2()
  if (s2_was_on) {
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(TRUE)), add = TRUE)
  }
  
  # concaveman() itself can also fail for very sparse or collinear point sets.
  # Fall back to a convex hull in that case — always topologically valid.
  habitat_sfc <- tryCatch(
    concaveman::concaveman(pts_sf, concavity = params$concavity),
    error = function(e) {
      message("  concaveman() failed (", conditionMessage(e), ")",
              " — falling back to convex hull.")
      pts_sf |>
        sf::st_union() |>
        sf::st_convex_hull()
    }
  )
  
  if (is.null(habitat_sfc) || all(sf::st_is_empty(habitat_sfc))) {
    message("  Hull returned empty geometry — skipping.")
    return(NULL)
  }
  
  # Two-stage repair with GEOS (S2 is off at this point):
  #   1. st_make_valid() resolves most self-intersections.
  #   2. st_buffer(0) is a reliable GEOS trick for any remaining issues.
  habitat_sf <- sf::st_sf(geometry = sf::st_geometry(habitat_sfc), crs = 4326) |>
    sf::st_make_valid() |>
    sf::st_buffer(0) |>
    sf::st_make_valid()
  
  if (!all(sf::st_is_valid(habitat_sf))) {
    message("  Geometry invalid after repair attempts for ", species_name, " — skipping.")
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
      concavity  = params$concavity
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
  here::here("data/historic_habitat_V4/historic_habitat_V4.rds")
)

message("RDS saved to: ", here::here("data/historic_habitat_V4/historic_habitat_V4.rds"))


# -------------------------------------------------------------------
# 9. Visualization function
# -------------------------------------------------------------------

map_historic_habitat_v4 <- function(species_name,
                                    historic_habitat,
                                    survdat_mgmt,
                                    survey_footprint,
                                    bathy,
                                    params  = habitat_params,
                                    out_dir = dir_images) {
  
  poly <- historic_habitat[[species_name]]
  if (is.null(poly)) {
    message("No polygon available for ", species_name)
    return(invisible(NULL))
  }
  
  # All presence points
  all_pts <- survdat_mgmt |>
    filter(COMNAME == species_name, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(STATION, LAT, LON, DEPTH)
  
  # Density-filtered presence points (those that shaped the hull)
  station_counts <- all_pts |> count(STATION, name = "n_obs")
  density_pts    <- all_pts |>
    inner_join(station_counts, by = "STATION") |>
    filter(n_obs >= params$min_station_count)
  
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
      colors  = c("grey15", "grey45", "grey75", "grey92"),
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
    
    # Survey footprint boundary — dashed orange line
    geom_sf(
      data      = survey_footprint,
      fill      = NA,
      color     = "orange",
      linetype  = "dashed",
      linewidth = 0.4
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
        " \u2014 Historic Habitat Envelope (V4)"
      ),
      subtitle = paste0(
        "Stations retained: ", nrow(density_pts |> distinct(STATION)),
        " / ", nrow(all_pts |> distinct(STATION)),
        "  |  Concavity: ", params$concavity,
        "  |  No buffer"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Blue: concave hull of stations with \u2265 ", params$min_station_count, " observations ",
        "(survey-footprint clip applied; no KDE, no buffer, no depth mask). ",
        "Orange dashed: survey footprint. ",
        "Bathymetry gradient capped at -2000 m."
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
    paste0(gsub(" ", "_", species_name), "_historic_habitat_V4.png")
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
  ~map_historic_habitat_v4(
    species_name     = .x,
    historic_habitat = historic_habitat,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    bathy            = bathy,
    params           = habitat_params
  )
)


# -------------------------------------------------------------------
# 11. Spot-check: Atlantic Cod
# -------------------------------------------------------------------
# Uncomment to run interactively

# map_historic_habitat_v4(
#   species_name     = "ATLANTIC COD",
#   historic_habitat = historic_habitat,
#   survdat_mgmt     = survdat_mgmt,
#   survey_footprint = survey_footprint,
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
      concavity  = poly$concavity,
      area_km2   = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)
