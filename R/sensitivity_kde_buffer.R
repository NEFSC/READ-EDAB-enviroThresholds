# sensitivy_kde_buffer.R
#
# Purpose: Sensitivity analysis for the two adjustable habitat-polygon
#          parameters in get_historic_habitat_V3.R — KDE contour level and
#          buffer distance — following a suggestion from Joe that an optimal
#          value for each parameter can be identified by finding the inflection
#          point where small parameter changes shift from having minor effects
#          on retained stations to having major effects.
#
# Approach:
#   For KDE contour level: the contour defines the outer boundary of the
#   habitat polygon. As the contour level rises (polygon shrinks), stations
#   in low-density fringe areas are progressively excluded. The inflection
#   point marks the transition from fringe to core habitat and is a
#   principled basis for choosing a contour value.
#
#   For buffer distance: the buffer expands the polygon outward after
#   contouring. Retained stations are non-decreasing as buffer grows.
#   The inflection point is where the expanding ring first encounters a
#   dense cluster of additional stations — i.e. where buffer distance
#   begins to meaningfully change the habitat boundary.
#
# Output:
#   - Line plots of stations retained vs. parameter value, per species,
#     with an estimated inflection point marked.
#   - A summary table of inflection point estimates per species.
#   Saved to: READ-EDAB-enviroThresholds/images/sensitivity/
#
# Note: run get_historic_habitat_V3.R through Section 6 (bathymetry) first,
#       or source the shared data-loading block below, before running this
#       script. The KDE helper function kde_to_polygon() is re-defined here
#       so this script is self-contained.

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(MASS)
library(isoband)
library(here)
library(scales)     # rescale() for inflection detection
library(ggrepel)    # label inflection points without overlap


# -------------------------------------------------------------------
# 1. Output directory
# -------------------------------------------------------------------

dir_sensitivity <- here::here("images/sensitivity")
if (!dir.exists(dir_sensitivity)) dir.create(dir_sensitivity, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load data (mirrors get_historic_habitat_V3.R sections 2-5)
# -------------------------------------------------------------------
# If you have already run V3 and these objects are in your environment,
# you can skip to Section 3.

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")
survdat <- survdat$survdat

inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")
inshore <- inshore$survdat

survdat <- dplyr::full_join(survdat, inshore)

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

species <- species |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ne_species <- species |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  dplyr::select(SVSPP, COMNAME, SCINAME, Fed.Managed)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)

all_stations <- survdat |>
  filter(!is.na(LAT), !is.na(LON)) |>
  mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
  distinct(STATION, LAT, LON)

survey_footprint <- all_stations |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
  sf::st_union() |>
  sf::st_convex_hull() |>
  sf::st_make_valid()

library(marmap)
bathy_marmap <- marmap::getNOAA.bathy(
  lon1 = -82, lon2 = -60,
  lat1 = 34,  lat2 = 48,
  resolution = 1,
  keep = TRUE
)
bathy <- terra::rast(marmap::as.raster(bathy_marmap))
crs(bathy) <- "EPSG:4326"


# -------------------------------------------------------------------
# 3. Base parameters
# -------------------------------------------------------------------
# These are the V3 defaults. The sensitivity analysis will vary one
# parameter at a time while holding the other at its default.

base_params <- list(
  min_station_count = 5,
  kde_bw_multiplier = 1.5,
  kde_n             = 400,
  kde_contour_level = 0.10,   # default — varied in contour sweep
  buffer_m          = 20000   # default — varied in buffer sweep
)

# Parameter grids to sweep
contour_levels <- seq(0.02, 0.40, by = 0.02)   # 2 % to 40 % of peak KDE density
buffer_values  <- seq(0, 100000, by = 5000)     # 0 to 100 km in 5 km steps


# -------------------------------------------------------------------
# 4. KDE helper (self-contained copy from V3)
# -------------------------------------------------------------------

kde_to_polygon <- function(pts_filtered, params) {
  
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
  if (is.na(bw_x)) bw_x <- 0.5
  if (is.na(bw_y)) bw_y <- 0.5
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
  if (!is.finite(contour_val) || contour_val <= 0) return(NULL)
  
  bands <- isoband::isobands(
    x           = kde$x,
    y           = kde$y,
    z           = t(kde$z),
    levels_low  = contour_val,
    levels_high = Inf
  )
  
  polys <- isoband::iso_to_sfg(bands)
  if (length(polys) == 0 || all(sapply(polys, is.null))) return(NULL)
  
  habitat_sfc <- sf::st_sfc(polys, crs = 4326) |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_make_valid()
  
  list(polygon = habitat_sfc, bw_x = bw_x, bw_y = bw_y)
}


# -------------------------------------------------------------------
# 5. Helper: count stations inside a habitat polygon
# -------------------------------------------------------------------
# Given a habitat polygon (sf) and a set of presence points, returns
# the number of unique stations that fall within the polygon.

count_stations_in_poly <- function(poly_sf, pts) {
  
  if (is.null(poly_sf) || nrow(poly_sf) == 0 || all(sf::st_is_empty(poly_sf))) {
    return(0L)
  }
  
  pts_sf <- pts |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
  
  # st_intersects returns a sparse list — any non-empty entry = inside polygon
  inside <- lengths(sf::st_intersects(pts_sf, sf::st_union(poly_sf))) > 0
  sum(inside)
}


# -------------------------------------------------------------------
# 6. Core sensitivity function
# -------------------------------------------------------------------
# Builds habitat polygons across a parameter grid for one species and
# returns a data frame of (parameter_value, n_stations_retained).
#
# vary: "contour" or "buffer"

sensitivity_sweep <- function(species_name,
                              survdat_mgmt,
                              survey_footprint,
                              land,
                              vary         = "contour",
                              param_values = if (vary == "contour") contour_levels else buffer_values,
                              base_params  = base_params) {
  
  message("Sweeping ", vary, " for: ", species_name)
  
  # Prepare filtered station set once — this is constant across the sweep
  pts <- survdat_mgmt |>
    filter(COMNAME == species_name, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON))
  
  if (nrow(pts) < base_params$min_station_count) {
    message("  Insufficient records — skipping.")
    return(NULL)
  }
  
  station_counts <- pts |> count(STATION, name = "n_obs")
  
  pts_filtered <- pts |>
    inner_join(station_counts, by = "STATION") |>
    filter(n_obs >= base_params$min_station_count) |>
    distinct(STATION, LAT, LON)
  
  if (nrow(pts_filtered) < 3) {
    message("  Too few stations after density filter — skipping.")
    return(NULL)
  }
  
  # Total stations available (ceiling for the plot)
  n_total <- nrow(pts_filtered)
  
  # All presence points as sf for containment checks
  all_pts_df <- pts_filtered
  
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  # For contour sweep: compute KDE once, vary only the contour threshold
  if (vary == "contour") {
    # Pre-compute bandwidth — same across all contour levels
    safe_bw <- function(x, mult) {
      raw <- tryCatch(MASS::bandwidth.nrd(x), warning = function(w) NULL, error = function(e) NULL)
      bw  <- if (is.null(raw)) NULL else raw * mult
      if (is.null(bw) || isTRUE(is.na(bw)) || isTRUE(bw == 0)) 0.5 else max(bw, 0.1)
    }
    bw_x <- safe_bw(pts_filtered$LON, base_params$kde_bw_multiplier)
    bw_y <- safe_bw(pts_filtered$LAT, base_params$kde_bw_multiplier)
    
    kde <- MASS::kde2d(
      x = pts_filtered$LON,
      y = pts_filtered$LAT,
      h = c(bw_x, bw_y),
      n = base_params$kde_n
    )
  }
  
  results <- map_dfr(param_values, function(val) {
    
    params_i <- base_params
    
    if (vary == "contour") {
      params_i$kde_contour_level <- val
      
      # Re-use the pre-computed KDE — only re-extract contour polygon
      contour_val <- as.numeric(max(kde$z, na.rm = TRUE) * val)
      if (!is.finite(contour_val) || contour_val <= 0) {
        return(tibble(param_value = val, n_stations = NA_integer_))
      }
      
      bands <- isoband::isobands(
        x           = kde$x,
        y           = kde$y,
        z           = t(kde$z),
        levels_low  = contour_val,
        levels_high = Inf
      )
      polys <- isoband::iso_to_sfg(bands)
      if (length(polys) == 0 || all(sapply(polys, is.null))) {
        return(tibble(param_value = val, n_stations = NA_integer_))
      }
      
      habitat_sfc <- sf::st_sfc(polys, crs = 4326) |>
        sf::st_make_valid() |> sf::st_union() |> sf::st_make_valid()
      
      # Apply buffer at the default value
      habitat_sf <- sf::st_sf(geometry = habitat_sfc) |>
        sf::st_transform(5070) |>
        sf::st_buffer(base_params$buffer_m) |>
        sf::st_transform(4326) |>
        sf::st_make_valid()
      
    } else {
      # Buffer sweep: build the KDE polygon fresh at default contour, vary buffer
      kde_result <- kde_to_polygon(pts_filtered, params_i)
      if (is.null(kde_result)) {
        return(tibble(param_value = val, n_stations = NA_integer_))
      }
      
      habitat_sf <- sf::st_sf(geometry = kde_result$polygon) |>
        sf::st_transform(5070) |>
        sf::st_buffer(val) |>
        sf::st_transform(4326) |>
        sf::st_make_valid()
    }
    
    # Clip to survey footprint and remove land
    habitat_sf <- suppressWarnings(
      sf::st_intersection(habitat_sf, survey_footprint)
    ) |> sf::st_make_valid()
    
    if (is.null(habitat_sf) || nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) {
      return(tibble(param_value = val, n_stations = NA_integer_))
    }
    
    habitat_sf <- suppressWarnings(
      sf::st_difference(habitat_sf, land_union)
    ) |> sf::st_make_valid()
    
    n <- count_stations_in_poly(habitat_sf, all_pts_df)
    
    tibble(param_value = val, n_stations = as.integer(n))
  })
  
  results |>
    mutate(
      species = species_name,
      vary    = vary,
      n_total = n_total
    )
}


# -------------------------------------------------------------------
# 7. Inflection point detection
# -------------------------------------------------------------------
# Estimates the inflection point as the parameter value where the
# first derivative (change in stations per unit parameter change)
# is steepest — i.e. where the curve bends most sharply.
#
# For contour: stations decline as contour rises, so we look for the
# steepest drop — the point where excluding more fringe area starts
# cutting into the core.
#
# For buffer: stations increase as buffer grows, so we look for the
# steepest gain — the point where the expanding ring first hits a
# dense cluster.

find_inflection <- function(df) {
  df <- df |> filter(!is.na(n_stations)) |> arrange(param_value)
  if (nrow(df) < 3) return(NA_real_)
  
  # First derivative: change in stations per unit change in parameter
  d1 <- diff(df$n_stations) / diff(df$param_value)
  
  # Index of steepest absolute change
  idx <- which.max(abs(d1))
  
  # Return the midpoint between the two parameter values straddling the steepest drop/gain
  mean(df$param_value[c(idx, idx + 1)])
}


# -------------------------------------------------------------------
# 8. Run sweeps for all species
# -------------------------------------------------------------------

all_species <- unique(ne_species$COMNAME)

contour_results <- map_dfr(
  all_species,
  ~sensitivity_sweep(
    species_name     = .x,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    land             = land,
    vary             = "contour",
    param_values     = contour_levels,
    base_params      = base_params
  )
)

buffer_results <- map_dfr(
  all_species,
  ~sensitivity_sweep(
    species_name     = .x,
    survdat_mgmt     = survdat_mgmt,
    survey_footprint = survey_footprint,
    land             = land,
    vary             = "buffer",
    param_values     = buffer_values,
    base_params      = base_params
  )
)


# -------------------------------------------------------------------
# 9. Inflection point summary tables
# -------------------------------------------------------------------

contour_inflections <- contour_results |>
  filter(!is.na(n_stations)) |>
  group_by(species) |>
  summarise(
    inflection_contour = find_inflection(pick(param_value, n_stations)),
    n_total            = first(n_total),
    .groups = "drop"
  ) |>
  arrange(inflection_contour)

buffer_inflections <- buffer_results |>
  filter(!is.na(n_stations)) |>
  group_by(species) |>
  summarise(
    inflection_buffer_m = find_inflection(pick(param_value, n_stations)),
    n_total             = first(n_total),
    .groups = "drop"
  ) |>
  arrange(inflection_buffer_m)

print(contour_inflections, n = Inf)
print(buffer_inflections,  n = Inf)


# -------------------------------------------------------------------
# 10. Plots
# -------------------------------------------------------------------

# --- 10a. Contour sensitivity: all species on one faceted plot ----

contour_inflect_pts <- contour_results |>
  filter(!is.na(n_stations)) |>
  group_by(species) |>
  group_modify(~{
    infl <- find_inflection(.x)
    .x |> filter(abs(param_value - infl) == min(abs(param_value - infl))) |> slice(1)
  })

p_contour <- contour_results |>
  filter(!is.na(n_stations)) |>
  mutate(pct_retained = n_stations / n_total * 100) |>
  ggplot(aes(x = param_value, y = pct_retained)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.2) +
  geom_vline(
    data = contour_inflect_pts |>
      mutate(pct_retained = n_stations / n_total * 100),
    aes(xintercept = param_value),
    color    = "firebrick",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = base_params$kde_contour_level,
    color      = "orange",
    linetype   = "dotted",
    linewidth  = 0.6
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~species, scales = "free_y") +
  labs(
    title    = "Sensitivity to KDE Contour Level",
    subtitle = "Red dashed: estimated inflection point. Orange dotted: V3 default (10%).",
    x        = "KDE contour level (proportion of peak density)",
    y        = "Stations retained (%)",
    caption  = "Buffer held at V3 default (20 km). Stations = unique filtered stations inside polygon."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text   = element_text(size = 7),
    plot.caption = element_text(size = 7, color = "grey50")
  )

ggsave(
  file.path(dir_sensitivity, "sensitivity_contour_all_species.png"),
  plot   = p_contour,
  width  = 16, height = 12, dpi = 300
)

# --- 10b. Buffer sensitivity: all species on one faceted plot -----

buffer_inflect_pts <- buffer_results |>
  filter(!is.na(n_stations)) |>
  group_by(species) |>
  group_modify(~{
    infl <- find_inflection(.x)
    .x |> filter(abs(param_value - infl) == min(abs(param_value - infl))) |> slice(1)
  })

p_buffer <- buffer_results |>
  filter(!is.na(n_stations)) |>
  mutate(pct_retained = n_stations / n_total * 100) |>
  ggplot(aes(x = param_value / 1000, y = pct_retained)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1.2) +
  geom_vline(
    data = buffer_inflect_pts |>
      mutate(pct_retained = n_stations / n_total * 100),
    aes(xintercept = param_value / 1000),
    color     = "firebrick",
    linetype  = "dashed",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = base_params$buffer_m / 1000,
    color      = "orange",
    linetype   = "dotted",
    linewidth  = 0.6
  ) +
  facet_wrap(~species, scales = "free_y") +
  labs(
    title    = "Sensitivity to Buffer Distance",
    subtitle = "Red dashed: estimated inflection point. Orange dotted: V3 default (20 km).",
    x        = "Buffer distance (km)",
    y        = "Stations retained (%)",
    caption  = "KDE contour held at V3 default (10%). Stations = unique filtered stations inside polygon."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text   = element_text(size = 7),
    plot.caption = element_text(size = 7, color = "grey50")
  )

ggsave(
  file.path(dir_sensitivity, "sensitivity_buffer_all_species.png"),
  plot   = p_buffer,
  width  = 16, height = 12, dpi = 300
)

# --- 10c. Summary: inflection points across species ---------------
# A dot plot showing where each species' inflection falls relative to
# the V3 default, useful for assessing whether one default value is
# appropriate for all species or whether per-species tuning is needed.

p_inflect_summary <- bind_rows(
  contour_inflections |>
    transmute(species, inflection = inflection_contour, param = "KDE contour",
              default = base_params$kde_contour_level),
  buffer_inflections |>
    transmute(species, inflection = inflection_buffer_m / 1000,
              param = "Buffer (km)", default = base_params$buffer_m / 1000)
) |>
  ggplot(aes(x = inflection, y = reorder(species, inflection))) +
  geom_point(size = 2.5, color = "steelblue") +
  geom_vline(aes(xintercept = default), color = "orange", linetype = "dotted", linewidth = 0.7) +
  facet_wrap(~param, scales = "free_x") +
  labs(
    title    = "Estimated Inflection Points by Species",
    subtitle = "Orange dotted: V3 default value for each parameter.",
    x        = "Parameter value at inflection",
    y        = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 7))

ggsave(
  file.path(dir_sensitivity, "sensitivity_inflection_summary.png"),
  plot   = p_inflect_summary,
  width  = 12, height = 8, dpi = 300
)

message("Sensitivity plots saved to: ", dir_sensitivity)


# -------------------------------------------------------------------
# 11. Contour level at 95% station retention
# -------------------------------------------------------------------
# For each species, find the highest (most restrictive) KDE contour level
# at which at least 95% of the density-filtered stations are still retained
# inside the polygon. This gives a less restrictive envelope than the
# inflection point while still trimming the lowest-density fringe.
#
# Logic: sort contour levels ascending, compute cumulative retention at each
# level, and take the last value where retention >= 0.95. If no tested level
# retains 95% (e.g. very sparse species), NA is returned and the global
# default will be used as a fallback in get_historic_habitat_V3.R.

contour_95pct <- contour_results |>
  filter(!is.na(n_stations)) |>
  mutate(pct_retained = n_stations / n_total) |>
  group_by(species) |>
  arrange(param_value) |>
  summarise(
    contour_95pct = {
      eligible <- param_value[pct_retained >= 0.95]
      if (length(eligible) == 0) NA_real_ else max(eligible)
    },
    n_total = first(n_total),
    .groups = "drop"
  ) |>
  arrange(species)

print(contour_95pct, n = Inf)


# -------------------------------------------------------------------
# 12. Save results tables
# -------------------------------------------------------------------

dir_results <- here::here("data/sensitivity")
if (!dir.exists(dir_results)) dir.create(dir_results, recursive = TRUE)

saveRDS(
  list(
    contour_results     = contour_results,
    buffer_results      = buffer_results,
    contour_inflections = contour_inflections,
    buffer_inflections  = buffer_inflections,
    contour_95pct       = contour_95pct
  ),
  file.path(dir_results, "sensitivity_results.rds")
)
