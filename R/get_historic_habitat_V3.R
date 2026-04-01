# get_historic_habitat_V3.R
#
# Purpose: Define and visualize historic habitat for NEFMC-managed species.
#
# Version history:
#   V1 — Initial approach: KDE binary raster threshold, concave hull envelope.
#   V2 — Geometric fixes: isoband contour polygon (no right-angle artifacts),
#         survey footprint clip (no coastal bleeding), bathymetry depth mask
#         (removes depth-avoidance areas such as the Great South Channel).
#   V3 — Collaborator feedback: depth masking removed from polygon construction
#         after review showed it produced similar results to the observation-
#         density filter with added complexity. Bathymetry is retained in the
#         visualization only, with the color gradient capped at -2000 m to
#         improve on-shelf contrast. Shapefiles replace RDS output.
#
# Changes from V2:
#   - get_species_habitat(): step f (bathymetry mask) removed entirely.
#     ocean_mask is no longer computed or passed to the function.
#   - depth_niche removed entirely: not used for filtering or map annotation.
#   - Bathymetry fill scale capped at -2000 m so the shelf gradient is
#     clearly visible (everything >= 2000 m deep shares the darkest color).
#   - Output paths updated to V3 directories.
#   - Habitat polygons saved as a single RDS file (named list, one entry per species).
#
# Output:
#   RDS  : data/historic_habitat_V3_95pct/historic_habitat_V3_95pct.rds
#   Maps : images/historic_habitat_V3_95pct/<species>.png
#
# Dependencies: tidyverse, sf, terra, concaveman, rnaturalearth, MASS,
#               isoband, ggnewscale, marmap, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(concaveman)
library(rnaturalearth)
library(MASS)        # kde2d
library(isoband)     # smooth contour polygon from KDE matrix
library(ggnewscale)  # new_scale_fill() — overlay two fill scales in ggplot
library(marmap)      # getNOAA.bathy — pulls ETOPO directly into R
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data   <- here::here("data/historic_habitat_V3_95pct")
dir_images <- here::here("images/historic_habitat_V3_95pct")

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
  dplyr::select(SVSPP, COMNAME, SCINAME, Fed.Managed)

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
  # across all years before it contributes to the KDE.
  min_station_count = 5,
  
  # KDE bandwidth multiplier (> 1 = smoother; < 1 = tighter).
  kde_bw_multiplier = 1.5,
  
  # KDE grid resolution — higher values give smoother contour edges.
  kde_n = 400,
  
  # Fallback contour level used for any species absent from the sensitivity
  # results loaded in Section 4. Species-specific inflection-point values
  # from explore_habitat_params.R take precedence when available.
  kde_contour_level = 0.10,
  
  # Buffer radius (metres) applied after contouring.
  buffer_m = 5000     # 5 km
  
)


# -------------------------------------------------------------------
# 4. Species-specific KDE contour levels from sensitivity analysis
# -------------------------------------------------------------------
# Load the inflection-point estimates produced by explore_habitat_params.R.
# These replace the single global kde_contour_level with the contour level
# at which each species retains 95% of its density-filtered stations,
# giving a less restrictive envelope than the inflection point while still
# trimming the lowest-density fringe.
#
# contour_lookup is a named numeric vector: names = COMNAME, values = contour level.
# get_species_habitat() uses this to override params$kde_contour_level per species.
#
# If the sensitivity RDS does not exist yet (e.g. first run), the script falls
# back gracefully to the global default in habitat_params for all species.

sensitivity_path <- here::here("data/sensitivity/sensitivity_results.rds")

if (file.exists(sensitivity_path)) {
  sensitivity_results <- readRDS(sensitivity_path)
  
  contour_lookup <- sensitivity_results$contour_95pct |>
    filter(!is.na(contour_95pct)) |>
    dplyr::select(species, contour_95pct) |>
    tibble::deframe()   # named vector: species name -> contour value
  
  message(length(contour_lookup), " species-specific 95% retention contour levels loaded from sensitivity results.")
} else {
  contour_lookup <- c()   # empty — all species will use the global default
  message("Sensitivity results not found at ", sensitivity_path,
          " — using global default contour level (", habitat_params$kde_contour_level, ") for all species.\n",
          "  Re-run explore_habitat_params.R to generate sensitivity_results.rds.")
}


# -------------------------------------------------------------------
# 5. Survey footprint
# -------------------------------------------------------------------
# Convex hull of all ever-sampled stations. Each species' habitat polygon
# is intersected with this so the KDE cannot extend into areas never surveyed.

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
# 6. Bathymetry (visualization only in V3 — not used for masking)
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

# Note: ocean_mask is NOT computed in V3 — bathymetry is used for
# visualization only and does not filter the habitat polygon.


# -------------------------------------------------------------------
# 7. Helper: KDE contour polygon
# -------------------------------------------------------------------

kde_to_polygon <- function(pts_filtered, params) {
  
  # bandwidth.nrd() returns NULL with a warning (not a hard error) when its
  # input is NULL or has fewer than 2 unique values. NULL * multiplier is also
  # NULL, and is.na(NULL) returns logical(0) — a zero-length vector — which
  # causes if() to fail with "missing value where TRUE/FALSE needed".
  # safe_bw() collapses all failure modes to NA_real_ before the checks below.
  safe_bw <- function(x, multiplier) {
    raw <- tryCatch(
      MASS::bandwidth.nrd(x),
      warning = function(w) NULL,
      error   = function(e) NULL
    )
    bw <- if (is.null(raw)) NULL else raw * multiplier
    if (is.null(bw) || isTRUE(is.na(bw)) || isTRUE(bw == 0)) NA_real_ else bw
  }
  
  bw_x <- safe_bw(pts_filtered$LON, params$kde_bw_multiplier)
  bw_y <- safe_bw(pts_filtered$LAT, params$kde_bw_multiplier)
  
  if (is.na(bw_x)) {
    warning("bandwidth.nrd failed for LON — using fallback bandwidth of 0.5 degrees")
    bw_x <- 0.5
  }
  if (is.na(bw_y)) {
    warning("bandwidth.nrd failed for LAT — using fallback bandwidth of 0.5 degrees")
    bw_y <- 0.5
  }
  
  bw_x <- max(bw_x, 0.1)
  bw_y <- max(bw_y, 0.1)
  
  kde <- MASS::kde2d(
    x = pts_filtered$LON,
    y = pts_filtered$LAT,
    h = c(bw_x, bw_y),
    n = params$kde_n
  )
  
  if (is.null(kde$z) || all(is.na(kde$z)) || max(kde$z, na.rm = TRUE) == 0) {
    return(NULL)
  }
  
  contour_val <- as.numeric(max(kde$z, na.rm = TRUE) * params$kde_contour_level)
  
  if (!is.finite(contour_val) || contour_val <= 0) {
    return(NULL)
  }
  
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


# -------------------------------------------------------------------
# 8. Core habitat polygon function
# -------------------------------------------------------------------
# Steps:
#   a. Filter to presence records with valid coordinates.
#   b. Minimum station observation count filter.
#   c. KDE contour polygon (smooth edges).
#   d. Buffer.
#   e. Intersect with survey footprint (prevents coastal bleeding).
#   f. Subtract land.
#   g. Attach metadata.
#
# Note: bathymetry depth masking (V2 step f) is intentionally omitted in V3.

get_species_habitat <- function(species_name,
                                survdat_mgmt,
                                survey_footprint,
                                contour_lookup = c(),
                                params         = habitat_params) {
  
  message("Building habitat polygon: ", species_name)
  
  # Override the global kde_contour_level with the species-specific inflection
  # point if one is available in contour_lookup.
  if (!is.null(contour_lookup) && species_name %in% names(contour_lookup)) {
    params$kde_contour_level <- contour_lookup[[species_name]]
    message("  Using species-specific contour level: ", round(params$kde_contour_level, 3))
  } else {
    message("  Using default contour level: ", params$kde_contour_level)
  }
  
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
  
  # --- f. Subtract land ---
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  habitat_marine <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_marine) || nrow(habitat_marine) == 0 || all(sf::st_is_empty(habitat_marine))) {
    message("  Empty polygon after land subtraction — skipping.")
    return(NULL)
  }
  
  # --- g. Metadata ---
  habitat_marine |>
    mutate(
      COMNAME     = species_name,
      n_stations  = nrow(pts_filtered),
      kde_bw_x    = kde_result$bw_x,
      kde_bw_y    = kde_result$bw_y,
      contour_lvl = params$kde_contour_level,
      buffer_m    = params$buffer_m
    )
}


# -------------------------------------------------------------------
# 9. Build habitat polygons for all NEFMC species
# -------------------------------------------------------------------

historic_habitat <- map(
  unique(ne_species$COMNAME),
  ~get_species_habitat(
    species_name     = .x,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    contour_lookup   = contour_lookup,
    params           = habitat_params
  )
) |>
  setNames(unique(ne_species$COMNAME))

historic_habitat <- Filter(Negate(is.null), historic_habitat)

message(length(historic_habitat), " species habitat polygons built.")


# -------------------------------------------------------------------
# 10. Save RDS
# -------------------------------------------------------------------
# The full named list is saved as a single RDS file, preserving the
# list structure, full column names, geometry types, and any R-specific
# attributes — exactly as downstream indicator code expects to find it.

saveRDS(
  historic_habitat,
  here::here("data/historic_habitat_V3_95pct/historic_habitat_V3_95pct.rds")
)

message("RDS saved to: ", here::here("data/historic_habitat_V3_95pct/historic_habitat_V3_95pct.rds"))


# -------------------------------------------------------------------
# 11. Visualisation function
# -------------------------------------------------------------------
# Key change from V2: bathymetry fill gradient is capped at -2000 m.
# Ocean cells deeper than 2000 m share the darkest color, which
# compresses the deep-water end of the scale and stretches the
# on-shelf gradient so shelf features are clearly distinguishable.
# Depth masking caption text is updated to reflect V3 behavior.

map_historic_habitat_v3 <- function(species_name,
                                    historic_habitat,
                                    survdat_mgmt,
                                    survey_footprint,
                                    bathy,
                                    contour_lookup = c(),
                                    params         = habitat_params,
                                    out_dir        = dir_images) {
  
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
  # Cap depth values at -2000 m so the gradient focuses on shelf variation.
  # Values deeper than -2000 m are set to -2000 m and share the darkest color.
  bathy_crop <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
  bathy_df   <- as.data.frame(bathy_crop, xy = TRUE)
  colnames(bathy_df)[3] <- "depth"
  bathy_df <- bathy_df |>
    filter(depth < 0) |>
    mutate(depth_capped = pmax(depth, -2000))   # cap: anything deeper = -2000
  
  p <- ggplot() +
    
    # Bathymetry — gradient capped at -2000 m for on-shelf contrast
    geom_raster(
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
        " \u2014 Historic Habitat Envelope (V3 — 95% retention)"
      ),
      subtitle = paste0(
        "Stations retained: ", nrow(density_pts |> distinct(STATION)),
        " / ", nrow(all_pts |> distinct(STATION)),
        "  |  KDE contour: ",
        round(
          if (!is.null(contour_lookup) && species_name %in% names(contour_lookup))
            contour_lookup[[species_name]]
          else
            params$kde_contour_level,
          3
        ),
        "  |  Buffer: ", params$buffer_m / 1000, " km"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Blue: habitat envelope (KDE + survey-footprint clip; no depth mask). KDE contour = 95% station retention. ",
        "Orange dashed: survey footprint. ",
        "Colored points: stations \u2265 ", params$min_station_count, " obs. ",
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
    paste0(gsub(" ", "_", species_name), "_historic_habitat_V3_95pct.png")
  )
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 300)
  
  message("  Saved: ", file_name)
  invisible(p)
}


# -------------------------------------------------------------------
# 12. Generate maps for all species
# -------------------------------------------------------------------

walk(
  names(historic_habitat),
  ~map_historic_habitat_v3(
    species_name     = .x,
    historic_habitat = historic_habitat,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    bathy            = bathy,
    contour_lookup   = contour_lookup,
    params           = habitat_params
  )
)


# -------------------------------------------------------------------
# 13. Spot-check: Atlantic Cod
# -------------------------------------------------------------------
# Uncomment to run interactively

# map_historic_habitat_v3(
#   species_name     = "ATLANTIC COD",
#   historic_habitat = historic_habitat,
#   survdat_mgmt     = survdat_mgmt,
#   survey_footprint = survey_footprint,
#   bathy            = bathy,
#   contour_lookup   = contour_lookup
# )

# historic_habitat[["ATLANTIC COD"]]


# -------------------------------------------------------------------
# 14. Summary table
# -------------------------------------------------------------------

habitat_summary <- map_dfr(
  names(historic_habitat),
  ~{
    poly <- historic_habitat[[.x]]
    tibble(
      species    = .x,
      n_stations = poly$n_stations,
      area_km2   = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)
