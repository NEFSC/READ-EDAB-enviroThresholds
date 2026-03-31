# get_historic_habitat.R
# MTG 03/23/2026
# 
# Purpose: Define and visualize historic habitat for NEFMC-managed species.
#
# This script refines the historic habitat definition from thermal_suitability.R
# based on collaborator feedback:
#   1. Buffer observed points so the habitat envelope is not bound to the
#      extreme outer edges of a species' observed range.
#   2. Apply a minimum observation density filter to eliminate one-off
#      observations that are unlikely to represent true habitat.
#   3. Incorporate depth as an additional habitat axis alongside temperature,
#      reflecting clear species preferences and avoidances seen in survey data.
#
# Output: a named list of sf polygon objects (one per species) saved to
#         data-raw/historic_habitat.rds, plus diagnostic maps saved to
#         images/historic_habitat/.
#
# Dependencies: tidyverse, sf, terra, concaveman, rnaturalearth, MASS (for KDE)


# 0. Packages ----------


library(tidyverse)
library(sf)
library(terra)
library(concaveman)
library(rnaturalearth)
library(MASS)        # kde2d
library(isoband)     # iso_to_sfg — smooth contour polygon from KDE matrix
library(ggnewscale)  # new_scale_fill() — overlay two fill scales in ggplot



# 1. Load survey data ----------


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
  dplyr::select(SVSPP, COMNAME, SCINAME, Fed.Managed)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))

# Land polygons used in all maps
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)



# 2. Habitat parameters ----------


habitat_params <- list(
  
  # Minimum number of unique station-year records at which a species must
  # have been observed before that station is included in the KDE.
  # Filters one-off stray observations.
  min_station_count = 5,
  
  # KDE bandwidth multiplier applied to MASS::bandwidth.nrd estimates.
  # > 1 = smoother surface; < 1 = tighter fit to observations.
  kde_bw_multiplier = 1.5,
  
  # KDE grid resolution. Higher values reduce staircase / right-angle artifacts
  # in the contour output.
  kde_n = 400,
  
  # Density contour level as a proportion of maximum KDE density used to
  # extract the habitat polygon via isoband. 0.10 means the contour is drawn
  # at 10 % of the peak density — captures the broad occupied area while
  # excluding the very lowest-density periphery that caused coastal bleeding
  # in v1. Raise toward 0.20–0.30 for a tighter core-habitat polygon.
  kde_contour_level = 0.10,
  
  # Buffer radius (metres) added after contouring.
  buffer_m = 20000,   # 20 km
  
  # Depth range expressed as quantiles of observed depths (DEPTH, m).
  # Trims the extreme tails of the observed depth distribution.
  depth_lower_quantile = 0.05,
  depth_upper_quantile = 0.95
)



# 3. Depth niche: derive per-species depth range from survey data ----------


depth_niche <- survdat_mgmt |>
  filter(ABUNDANCE > 0, !is.na(DEPTH)) |>
  group_by(SVSPP, COMNAME) |>
  summarise(
    depth_min   = quantile(DEPTH, habitat_params$depth_lower_quantile, na.rm = TRUE),
    depth_max   = quantile(DEPTH, habitat_params$depth_upper_quantile, na.rm = TRUE),
    n_depth_obs = n(),
    .groups = "drop"
  )

# Spot-check
depth_niche |> filter(COMNAME == "ATLANTIC COD")



# 4. Survey footprint  ----------

# Build a convex hull of ALL stations ever sampled, regardless of species.
# Each species' habitat polygon will be intersected with this footprint so
# the KDE cannot extend into areas that were never surveyed — the primary
# guard against coastal bleeding for offshore species.
#
# If the convex hull is too generous (e.g. it bridges over land or includes
# large unsurveyed gaps), replace st_convex_hull() with concaveman() using
# the full station point set for a tighter survey-region boundary.

all_stations <- survdat |>
  filter(!is.na(LAT), !is.na(LON)) |>
  mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
  distinct(STATION, LAT, LON)

survey_footprint <- all_stations |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
  sf::st_union() |>
  sf::st_convex_hull() |>
  sf::st_make_valid()

# Optional: inspect
# plot(survey_footprint)



# 5. Bathymetry ----------

# Downloads ETOPO 2022 from NOAA and saves locally for reuse.
# resolution = 1 gives 1 arc-minute (~1.8 km) — sufficient for shelf-scale
# depth masking. Drop to resolution = 0.5 for finer detail if needed.
bathy_marmap <- marmap::getNOAA.bathy(
  lon1 = -82, lon2 = -60,
  lat1 = 34,  lat2 = 48,
  resolution = 1,
  keep = TRUE   # saves as CSV in working directory for reuse
)

# Convert marmap's 'bathy' class to a terra SpatRaster
# marmap uses negative values for ocean depth — same convention the script expects
bathy <- terra::rast(marmap::as.raster(bathy_marmap))
crs(bathy) <- "EPSG:4326"

ocean_mask <- terra::ifel(bathy < 0, 1, NA)



# 6. Helper: KDE contour polygon ----------

# Extracts smooth contour polygons directly from the KDE density matrix using
# isoband::isobands(), avoiding the staircase / right-angle edge artifact that
# arose from rasterizing a binary threshold mask in v1.

kde_to_polygon <- function(pts_filtered, params) {
  
  # bandwidth.nrd() returns NULL with a warning (not a hard error) when its
  # input is NULL or has fewer than 2 unique values. NULL * multiplier is also
  # NULL, and is.na(NULL) returns logical(0) — a zero-length vector — which
  # causes if() to fail with "missing value where TRUE/FALSE needed".
  # Use a helper that collapses all failure modes to a single NA-safe scalar.
  safe_bw <- function(x, multiplier) {
    raw <- tryCatch(
      MASS::bandwidth.nrd(x),
      warning = function(w) NULL,   # catches the "'x' is NULL" warning path
      error   = function(e) NULL
    )
    bw <- if (is.null(raw)) NULL else raw * multiplier
    # isTRUE/isFALSE are length-safe; they return FALSE rather than
    # logical(0) when bw is NULL, so the || chain always resolves.
    if (is.null(bw) || isTRUE(is.na(bw)) || isTRUE(bw == 0)) NA_real_ else bw
  }
  
  bw_x <- safe_bw(pts_filtered$LON, params$kde_bw_multiplier)
  bw_y <- safe_bw(pts_filtered$LAT, params$kde_bw_multiplier)
  
  # Fall back to a 0.5-degree fixed bandwidth for degenerate coordinate ranges
  if (is.na(bw_x)) {
    warning("bandwidth.nrd failed for LON — using fallback bandwidth of 0.5 degrees")
    bw_x <- 0.5
  }
  if (is.na(bw_y)) {
    warning("bandwidth.nrd failed for LAT — using fallback bandwidth of 0.5 degrees")
    bw_y <- 0.5
  }
  
  # Enforce a sensible minimum even when bandwidth.nrd succeeds
  bw_x <- max(bw_x, 0.1)
  bw_y <- max(bw_y, 0.1)
  
  kde <- MASS::kde2d(
    x = pts_filtered$LON,
    y = pts_filtered$LAT,
    h = c(bw_x, bw_y),
    n = params$kde_n
  )
  
  # Guard against a degenerate density surface (all-zero or all-NA)
  if (is.null(kde$z) || all(is.na(kde$z)) || max(kde$z, na.rm = TRUE) == 0) {
    return(NULL)
  }
  
  # Contour level as a fraction of peak density — must be a length-1 scalar.
  # isobands() requires levels_low and levels_high to be the same length, so
  # explicitly wrap in as.numeric() to drop any accidental attributes.
  contour_val <- as.numeric(max(kde$z, na.rm = TRUE) * params$kde_contour_level)
  
  # Sanity check: contour_val must be finite and positive
  if (!is.finite(contour_val) || contour_val <= 0) {
    return(NULL)
  }
  
  # isobands() returns filled polygons between two density levels.
  # Setting the upper bound to Inf captures everything above the threshold.
  # isoband expects z[row = y, col = x]; kde2d returns z[x, y] — transpose.
  bands <- isoband::isobands(
    x           = kde$x,
    y           = kde$y,
    z           = t(kde$z),
    levels_low  = contour_val,
    levels_high = Inf
  )
  
  polys <- isoband::iso_to_sfg(bands)
  
  if (length(polys) == 0 || all(sapply(polys, is.null))) {
    return(NULL)
  }
  
  habitat_sfc <- sf::st_sfc(polys, crs = 4326) |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_make_valid()
  
  list(polygon = habitat_sfc, bw_x = bw_x, bw_y = bw_y)
}



# 7. Core habitat polygon function ----------

# Steps:
#   a. Filter to presence records with valid coordinates.
#   b. Minimum station observation count filter.
#   c. KDE contour polygon (smooth edges — FIX A).
#   d. Buffer.
#   e. Intersect with survey footprint (no coastal bleed — FIX B).
#   f. Bathymetry mask (depth-avoidance areas removed — FIX C).
#   g. Subtract land.
#   h. Attach metadata.

get_species_habitat <- function(species_name,
                                survdat_mgmt,
                                depth_niche,
                                survey_footprint,
                                bathy,
                                ocean_mask,
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
  station_counts <- pts |> count(STATION, name = "n_obs")
  
  pts_filtered <- pts |>
    inner_join(station_counts, by = "STATION") |>
    filter(n_obs >= params$min_station_count) |>
    distinct(STATION, LAT, LON)
  
  if (nrow(pts_filtered) < 3) {
    message("  Too few stations after density filter — skipping.")
    return(NULL)
  }
  
  # --- c. KDE contour polygon ---
  kde_result <- kde_to_polygon(pts_filtered, params)
  
  if (is.null(kde_result)) {
    message("  KDE contour returned no polygon — skipping.")
    return(NULL)
  }
  
  # --- d. Buffer ---
  habitat_sf <- sf::st_sf(geometry = kde_result$polygon) |>
    sf::st_transform(5070) |>           # NAD83 / Conus Albers (metres)
    sf::st_buffer(params$buffer_m) |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
  
  # --- e. Intersect with survey footprint ---
  habitat_sf <- suppressWarnings(
    sf::st_intersection(habitat_sf, survey_footprint)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after survey footprint intersection — skipping.")
    return(NULL)
  }
  
  # --- f. Bathymetry mask ---
  dn <- depth_niche |> filter(COMNAME == species_name)
  
  if (nrow(dn) > 0) {
    depth_min <- dn$depth_min   # positive metres (e.g. 20)
    depth_max <- dn$depth_max   # positive metres (e.g. 300)
    
    # Bathy is negative for ocean, so:
    #   depth_min (shallow limit) → bathy <= -depth_min
    #   depth_max (deep limit)    → bathy >= -depth_max
    depth_mask <- terra::ifel(
      bathy <= -depth_min & bathy >= -depth_max,
      1, NA
    ) * ocean_mask
    
    # Crop to this species' approximate extent before polygonizing
    sp_ext <- terra::ext(
      sf::st_bbox(habitat_sf)[c("xmin", "xmax", "ymin", "ymax")]
    )
    depth_mask_crop <- terra::crop(depth_mask, sp_ext)
    
    depth_mask_poly <- terra::as.polygons(depth_mask_crop, dissolve = TRUE) |>
      sf::st_as_sf() |>
      sf::st_make_valid()
    
    if (nrow(depth_mask_poly) > 0) {
      habitat_sf <- suppressWarnings(
        sf::st_intersection(habitat_sf, sf::st_union(depth_mask_poly))
      ) |> sf::st_make_valid()
    }
    
    if (is.null(habitat_sf) || nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
      message("  Empty polygon after bathymetry mask — skipping.")
      return(NULL)
    }
  } else {
    message("  No depth niche for ", species_name, " — skipping bathymetry mask.")
  }
  
  # --- g. Subtract land ---
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  habitat_marine <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_marine) || nrow(habitat_marine) == 0 || all(sf::st_is_empty(habitat_marine))) {
    message("  Empty polygon after land subtraction — skipping.")
    return(NULL)
  }
  
  # --- h. Metadata ---
  habitat_marine |>
    mutate(
      COMNAME     = species_name,
      n_stations  = nrow(pts_filtered),
      depth_min_m = if (nrow(dn) > 0) dn$depth_min else NA_real_,
      depth_max_m = if (nrow(dn) > 0) dn$depth_max else NA_real_,
      kde_bw_x    = kde_result$bw_x,
      kde_bw_y    = kde_result$bw_y,
      contour_lvl = params$kde_contour_level,
      buffer_m    = params$buffer_m
    )
}



# 8. Build habitat polygons for all NEFMC species ----------


historic_habitat <- map(
  unique(ne_species$COMNAME),
  ~get_species_habitat(
    species_name     = .x,
    survdat_mgmt     = survdat_mgmt,
    depth_niche      = depth_niche,
    survey_footprint = survey_footprint,
    bathy            = bathy,
    ocean_mask       = ocean_mask,
    params           = habitat_params
  )
) |>
  setNames(unique(ne_species$COMNAME))

historic_habitat <- Filter(Negate(is.null), historic_habitat)

message(length(historic_habitat), " species habitat polygons built.")

if (!dir.exists("data-raw")) dir.create("data-raw", recursive = TRUE)
saveRDS(historic_habitat, here::here("data-raw/historic_habitat.rds"))






# 9. Visualisation function ----------

# Diagnostic additions vs v1:
#   - Bathymetry shown as a greyscale raster so depth-driven exclusions
#     (e.g. Great South Channel) are clearly visible.
#   - Survey footprint boundary shown as a dashed orange line so you can
#     see exactly where it clips the KDE for offshore species.

map_historic_habitat <- function(species_name,
                                 historic_habitat,
                                 survdat_mgmt,
                                 depth_niche,
                                 survey_footprint,
                                 bathy,
                                 params  = habitat_params,
                                 out_dir = "images/historic_habitat") {
  
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
  
  # Density-filtered presence points
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
  
  # Bathymetry cropped to map extent
  bathy_crop <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
  bathy_df   <- as.data.frame(bathy_crop, xy = TRUE)
  colnames(bathy_df)[3] <- "depth"
  bathy_df <- bathy_df |> filter(depth < 0)   # ocean cells only
  
  # Subtitle
  dn <- depth_niche |> filter(COMNAME == species_name)
  depth_label <- if (nrow(dn) > 0) {
    paste0("Depth niche: ", round(dn$depth_min), "\u2013", round(dn$depth_max), " m  |  ")
  } else { "" }
  
  p <- ggplot() +
    
    # Greyscale bathymetry — makes shallow features (GSC, etc.) visible
    geom_raster(
      data = bathy_df,
      aes(x = x, y = y, fill = depth)
    ) +
    scale_fill_gradientn(
      colours  = c("grey20", "grey60", "grey85"),
      name     = "Bathy (m)",
      na.value = "white"
    ) +
    
    # Second fill scale for the habitat polygon (requires ggnewscale)
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
    
    # Density-filtered points coloured by observed depth
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
        " \u2014 Historic Habitat Envelope (v2)"
      ),
      subtitle = paste0(
        depth_label,
        "Stations retained: ", nrow(density_pts |> distinct(STATION)),
        " / ", nrow(all_pts |> distinct(STATION)),
        "  |  KDE contour: ", params$kde_contour_level,
        "  |  Buffer: ", params$buffer_m / 1000, " km"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Blue: habitat after survey-footprint clip + depth mask. ",
        "Orange dashed: survey footprint. ",
        "Coloured points: stations \u2265 ", params$min_station_count, " obs."
      )
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "right",
      plot.subtitle    = element_text(size = 8, color = "grey40"),
      plot.caption     = element_text(size = 7, color = "grey50"),
      panel.grid.major = element_line(color = "grey70", linewidth = 0.2)
    )
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  file_name <- file.path(
    out_dir,
    paste0(gsub(" ", "_", species_name), "_historic_habitat.png")
  )
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 300)
  
  message("  Saved: ", file_name)
  invisible(p)
}



# 10. Generate maps for all species ------------


walk(
  names(historic_habitat),
  ~map_historic_habitat(
    species_name     = .x,
    historic_habitat = historic_habitat,
    survdat_mgmt     = survdat_mgmt,
    depth_niche      = depth_niche,
    survey_footprint = survey_footprint,
    bathy            = bathy,
    params           = habitat_params
  )
)



# 11. Spot-check: Atlantic Cod  ------------

# Uncomment to run interactively

# map_historic_habitat(
#   species_name     = "ATLANTIC COD",
#   historic_habitat = historic_habitat,
#   survdat_mgmt     = survdat_mgmt,
#   depth_niche      = depth_niche,
#   survey_footprint = survey_footprint,
#   bathy            = bathy
# )

# depth_niche |> filter(COMNAME == "ATLANTIC COD")
# historic_habitat[["ATLANTIC COD"]]



# 12. Summary table -----------


habitat_summary <- map_dfr(
  names(historic_habitat),
  ~{
    poly <- historic_habitat[[.x]]
    tibble(
      species     = .x,
      n_stations  = poly$n_stations,
      depth_min_m = poly$depth_min_m,
      depth_max_m = poly$depth_max_m,
      area_km2    = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)


