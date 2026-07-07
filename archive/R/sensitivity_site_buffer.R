# sensitivity_site_buffer.R
#
# Purpose: Identify an appropriate buffer distance for get_historic_habitat_V5.R
#          by evaluating how total habitat area changes as buffer radius increases
#          from 0 to 25 km in 1 km steps.
#
# Rationale: Each observation point in V5 receives a circular buffer of radius
#   buffer_m. Overlapping circles are dissolved into a single polygon. At small
#   buffer values the circles are largely isolated and total area grows roughly
#   as n_points * pi * r^2. As the buffer grows, circles begin to overlap and
#   merge — area growth decelerates. The curve eventually plateaus when adding
#   more radius only slightly expands the outer edge of an already-contiguous
#   polygon. The inflection point at which growth visibly decelerates is the
#   natural choice for buffer_m: it captures the biologically meaningful area
#   around each observation without wasteful expansion into unoccupied water.
#
# The analysis is run species-specifically so you can assess whether a single
#   buffer value is appropriate across all species or whether some species have
#   notably different plateau behavior. The final buffer selection will apply
#   one value to all species.
#
# Post-processing clips are applied identically to V5 (depth mask at 500 m,
#   survey footprint intersection, land subtraction) so the sensitivity curves
#   reflect the actual habitat area that would be reported, not raw buffer area.
#
# Output:
#   Plots : READ-EDAB-enviroThresholds/images/sensitivity/buffer/
#   RDS   : READ-EDAB-enviroThresholds/data/sensitivity/buffer_sensitivity.rds
#
# Dependencies: tidyverse, sf, terra, rnaturalearth, marmap, scales, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(marmap)
library(scales)
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_plots   <- here::here("images/sensitivity/buffer")
dir_results <- here::here("data/sensitivity")

if (!dir.exists(dir_plots))   dir.create(dir_plots,   recursive = TRUE)
if (!dir.exists(dir_results)) dir.create(dir_results, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load survey data
# -------------------------------------------------------------------

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
  select(SVSPP, COMNAME, SCINAME, Fed.Managed)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

# Land polygons
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)

land_union <- sf::st_union(land) |> sf::st_make_valid()


# -------------------------------------------------------------------
# 3. Survey strata (footprint for clipping)
# -------------------------------------------------------------------

strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"

Sys.setenv(SHAPE_RESTORE_SHX = "YES")

strata <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>
  sf::st_transform(4326) |>
  sf::st_make_valid()

Sys.unsetenv("SHAPE_RESTORE_SHX")

survey_footprint <- strata |>
  sf::st_union() |>
  sf::st_make_valid()


# -------------------------------------------------------------------
# 4. Bathymetry (for depth mask)
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
# 5. Analysis parameters
# -------------------------------------------------------------------

buffer_values <- seq(0, 25000, by = 1000)   # 0 to 25 km in 1 km steps
max_depth_m   <- 500                          # matches V5 default


# -------------------------------------------------------------------
# 6. Core sweep function
# -------------------------------------------------------------------
# Replicates the V5 buffering pipeline exactly for a given species and
# buffer distance, returning the resulting habitat area in km².
# Returns NA if the pipeline produces an empty polygon at any step.

compute_habitat_area <- function(species_name,
                                 buffer_m,
                                 pts_filtered,
                                 bathy,
                                 survey_footprint,
                                 land_union,
                                 max_depth_m) {
  
  # Buffer 0 m is a degenerate case — points have no area
  if (buffer_m == 0) return(0)
  
  # Buffer and dissolve (identical to V5 step b)
  habitat_sf <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    sf::st_transform(5070) |>
    sf::st_buffer(buffer_m) |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_as_sf() |>
    sf::st_transform(4326) |>
    sf::st_make_valid()
  
  if (nrow(habitat_sf) == 0 || all(sf::st_is_empty(habitat_sf))) return(NA)
  
  # Depth mask (identical to V5 step c)
  bathy_ext <- terra::ext(
    sf::st_bbox(habitat_sf)[c("xmin", "xmax", "ymin", "ymax")]
  )
  bathy_crop <- terra::crop(bathy, bathy_ext)
  depth_mask <- terra::ifel(bathy_crop >= -max_depth_m & bathy_crop < 0, 1, NA)
  depth_poly <- terra::as.polygons(depth_mask, dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_make_valid()
  
  if (nrow(depth_poly) > 0) {
    habitat_sf <- suppressWarnings(
      sf::st_intersection(habitat_sf, sf::st_union(depth_poly))
    ) |> sf::st_make_valid()
  }
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 ||
      all(sf::st_is_empty(habitat_sf))) return(NA)
  
  # Survey footprint clip (identical to V5 step d)
  habitat_sf <- suppressWarnings(
    sf::st_intersection(habitat_sf, survey_footprint)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 ||
      all(sf::st_is_empty(habitat_sf))) return(NA)
  
  # Land subtraction (identical to V5 step e)
  habitat_sf <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_sf) || nrow(habitat_sf) == 0 ||
      all(sf::st_is_empty(habitat_sf))) return(NA)
  
  # Area in km² (project to equal-area CRS for accurate measurement)
  as.numeric(sf::st_area(sf::st_transform(habitat_sf, 5070))) / 1e6
}


# -------------------------------------------------------------------
# 7. Run sweep for all species
# -------------------------------------------------------------------
# Observation points are extracted once per species outside the buffer
# loop — they don't change with buffer distance, only the polygon does.

all_species <- unique(ne_species$COMNAME)

buffer_results <- map_dfr(all_species, function(sp) {
  
  message("Sweeping: ", sp)
  
  # Unique observation locations (mirrors V5 step a)
  pts <- survdat_mgmt |>
    filter(COMNAME == sp, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(LAT, LON)
  
  if (nrow(pts) == 0) {
    message("  No presence records — skipping.")
    return(NULL)
  }
  
  message("  ", nrow(pts), " unique locations across ", length(buffer_values), " buffer values.")
  
  map_dfr(buffer_values, function(buf) {
    area <- compute_habitat_area(
      species_name     = sp,
      buffer_m         = buf,
      pts_filtered     = pts,
      bathy            = bathy,
      survey_footprint = survey_footprint,
      land_union       = land_union,
      max_depth_m      = max_depth_m
    )
    tibble(
      species   = sp,
      buffer_km = buf / 1000,
      area_km2  = area
    )
  })
})


# -------------------------------------------------------------------
# 8. Inflection point detection
# -------------------------------------------------------------------
# Estimates the buffer distance where area growth decelerates most sharply
# (i.e. where the first derivative of area with respect to buffer distance
# is at its maximum — the steepest gain). Beyond this point, additional
# buffer adds relatively little new habitat area.

find_inflection <- function(df) {
  df <- df |> filter(!is.na(area_km2)) |> arrange(buffer_km)
  if (nrow(df) < 3) return(NA_real_)
  d1  <- diff(df$area_km2) / diff(df$buffer_km)
  idx <- which.max(d1)
  mean(df$buffer_km[c(idx, idx + 1)])
}

inflection_summary <- buffer_results |>
  filter(!is.na(area_km2)) |>
  group_by(species) |>
  group_modify(~tibble(inflection_km = find_inflection(.x))) |>
  ungroup() |>
  arrange(inflection_km)

print(inflection_summary, n = Inf)


# -------------------------------------------------------------------
# 9. Save results
# -------------------------------------------------------------------

saveRDS(
  list(
    buffer_results    = buffer_results,
    inflection_summary = inflection_summary,
    buffer_values_km  = buffer_values / 1000,
    max_depth_m       = max_depth_m
  ),
  file.path(dir_results, "buffer_sensitivity.rds")
)

message("Results saved to: ", file.path(dir_results, "buffer_sensitivity.rds"))


# -------------------------------------------------------------------
# 10. Plots
# -------------------------------------------------------------------

# --- 10a. Area vs buffer: all species faceted ---

inflect_pts <- buffer_results |>
  filter(!is.na(area_km2)) |>
  group_by(species) |>
  group_modify(~{
    infl <- find_inflection(.x)
    .x |>
      filter(abs(buffer_km - infl) == min(abs(buffer_km - infl))) |>
      slice(1)
  }) |>
  ungroup()

p_facet <- buffer_results |>
  filter(!is.na(area_km2)) |>
  ggplot(aes(x = buffer_km, y = area_km2)) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  geom_point(color = "steelblue", size = 1) +
  geom_vline(
    data      = inflect_pts,
    aes(xintercept = buffer_km),
    color     = "firebrick",
    linetype  = "dashed",
    linewidth = 0.5
  ) +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~species, scales = "free_y") +
  labs(
    title    = "Habitat Area vs Buffer Distance — Species-Specific",
    subtitle = paste0("Red dashed: estimated inflection point (steepest area gain). ",
                      "Depth mask: ", max_depth_m, " m."),
    x        = "Buffer distance (km)",
    y        = expression("Habitat area (km"^2*")"),
    caption  = "Area after depth mask, survey footprint clip, and land subtraction."
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text   = element_text(size = 7),
    plot.caption = element_text(size = 7, color = "grey50"),
    plot.subtitle = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(dir_plots, "buffer_sensitivity_all_species.png"),
  plot   = p_facet,
  width  = 16, height = 12, dpi = 300
)

message("Saved: buffer_sensitivity_all_species.png")


# --- 10b. Normalized area curves: all species on one plot ---
# Normalizing to [0, 1] puts all species on the same scale so the
# shape of the curve (and where it plateaus) can be compared directly
# regardless of differences in total habitat size.

p_normalized <- buffer_results |>
  filter(!is.na(area_km2)) |>
  group_by(species) |>
  mutate(area_norm = (area_km2 - min(area_km2, na.rm = TRUE)) /
           (max(area_km2, na.rm = TRUE) - min(area_km2, na.rm = TRUE))) |>
  ungroup() |>
  ggplot(aes(x = buffer_km, y = area_norm, group = species, color = species)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 25, by = 5)) +
  scale_color_viridis_d(option = "turbo", name = "Species") +
  labs(
    title    = "Normalized Habitat Area vs Buffer Distance — All Species",
    subtitle = paste0("Area normalized to [0, 1] per species for shape comparison. ",
                      "Depth mask: ", max_depth_m, " m."),
    x        = "Buffer distance (km)",
    y        = "Normalized habitat area",
    caption  = "Curves that plateau earlier indicate species with denser/more clustered observations."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.text  = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    plot.caption = element_text(size = 7, color = "grey50"),
    plot.subtitle = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(dir_plots, "buffer_sensitivity_normalized.png"),
  plot   = p_normalized,
  width  = 10, height = 7, dpi = 300
)

message("Saved: buffer_sensitivity_normalized.png")


# --- 10c. Inflection point summary dot plot ---

p_inflect <- inflection_summary |>
  filter(!is.na(inflection_km)) |>
  ggplot(aes(x = inflection_km, y = reorder(species, inflection_km))) +
  geom_point(size = 3, color = "steelblue") +
  geom_vline(
    xintercept = median(inflection_summary$inflection_km, na.rm = TRUE),
    color      = "firebrick",
    linetype   = "dashed",
    linewidth  = 0.6
  ) +
  scale_x_continuous(breaks = seq(0, 25, by = 2)) +
  labs(
    title    = "Inflection Point by Species",
    subtitle = "Red dashed: median across all species — candidate for universal buffer distance.",
    x        = "Buffer distance at inflection (km)",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y  = element_text(size = 8),
    plot.subtitle = element_text(size = 8, color = "grey40")
  )

ggsave(
  file.path(dir_plots, "buffer_sensitivity_inflections.png"),
  plot   = p_inflect,
  width  = 9, height = 7, dpi = 300
)

message("Saved: buffer_sensitivity_inflections.png")
message("All sensitivity plots saved to: ", dir_plots)
