# R/01_get_historic_habitat.R
#
# Purpose: Define each species' fall historic habitat footprint
#          (manuscript Section 2.2).
#
# Logic:   Footprint = dissolved union of NEFSC survey strata in which the
#          species was caught at >= params$min_stations_per_stratum_year
#          stations in a single fall survey, in any year
#          (params$footprint_years; NULL = full time series). Land removed.
#          Positive tows come from both NEFSC and MassDMF. Tows outside every
#          NEFSC stratum cannot contribute; Section 1b reports how many.
#
# Output:
#   RDS : data/historic_habitat/historic_habitat_fall.rds  (named list by species)
#   CSV : data/historic_habitat/historic_habitat_summary.csv
#   CSV : data/historic_habitat/strata_coverage_by_survey.csv
#   PNG : images/historic_habitat/<species>_fall_historic_habitat.png
#
# Dependencies: tidyverse, sf, terra, rnaturalearth, ggnewscale, marmap, here

library(tidyverse)
library(sf)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

dir_data   <- ensure_dir(paths$habitat_dir)
dir_images <- ensure_dir(paths$images_dir, "historic_habitat")


# -------------------------------------------------------------------
# 1. Inputs
# -------------------------------------------------------------------

message("Loading survey data, strata, and land...")

strata     <- load_strata(paths)
land       <- load_land()
land_union <- sf::st_union(land) |> sf::st_make_valid()

tows <- load_fall_positive_tows(paths, params) |>
  assign_strata(strata)

# Only species with an included thermal niche get a footprint
niche <- readRDS(paths$niche) |> filter(included)
species_list <- sort(intersect(unique(tows$COMNAME), niche$COMNAME))


# -------------------------------------------------------------------
# 1b. Check: do MassDMF stations fall inside NEFSC strata?
# -------------------------------------------------------------------

coverage <- tows |>
  distinct(SURVEY, tow_id, strata_uid) |>
  group_by(SURVEY) |>
  summarise(
    n_stations        = n(),
    n_in_strata       = sum(!is.na(strata_uid)),
    pct_in_strata     = 100 * n_in_strata / n_stations,
    .groups = "drop"
  )

write_csv(coverage, file.path(dir_data, "strata_coverage_by_survey.csv"))

message("Share of fall positive-catch stations inside an NEFSC stratum:")
for (i in seq_len(nrow(coverage))) {
  message(sprintf("  %-8s %5.1f%% (%d of %d)", coverage$SURVEY[i], coverage$pct_in_strata[i],
                  coverage$n_in_strata[i], coverage$n_stations[i]))
}


# -------------------------------------------------------------------
# 2. Build footprints
# -------------------------------------------------------------------

message("Building fall footprints for ", length(species_list), " species (threshold: ",
        params$min_stations_per_stratum_year, " stations/stratum/year; years: ",
        if (is.null(params$footprint_years)) "all" else
          paste(range(params$footprint_years), collapse = "-"), ")...")

historic_habitat <- map(species_list, function(sp) {
  build_footprint(
    tows_sp      = filter(tows, COMNAME == sp),
    strata       = strata,
    land_union   = land_union,
    min_stations = params$min_stations_per_stratum_year,
    years        = params$footprint_years
  )
}) |>
  set_names(species_list) |>
  compact()

# Flag footprints that cover too little of where the species is caught
historic_habitat <- imap(historic_habitat, function(poly, sp) {
  poly$pct_tows_in_footprint <- 100 * poly$n_tows_in_footprint / poly$n_tows
  poly$adequate_footprint    <- poly$pct_tows_in_footprint >= params$min_pct_tows_in_footprint
  poly
})

inadequate <- keep(historic_habitat, ~ !.x$adequate_footprint)
if (length(inadequate) > 0) {
  message("Footprint contains < ", params$min_pct_tows_in_footprint,
          "% of positive tows (not scored): ",
          paste0(species_label(names(inadequate)), " (",
                 map_chr(inadequate, ~ sprintf("%.1f%%", .x$pct_tows_in_footprint)), ")",
                 collapse = ", "))
}

missing <- setdiff(species_list, names(historic_habitat))
if (length(missing) > 0) {
  message("No qualifying strata for: ", paste(species_label(missing), collapse = ", "))
}

saveRDS(historic_habitat, paths$habitat)
message(length(historic_habitat), " footprints saved to: ", paths$habitat)


# -------------------------------------------------------------------
# 3. Summary table
# -------------------------------------------------------------------

massdmf_counts <- tows |>
  filter(SURVEY == "MassDMF") |>
  group_by(COMNAME) |>
  summarise(n_tows_massdmf = n(), n_massdmf_in_strata = sum(!is.na(strata_uid)), .groups = "drop")

habitat_summary <- imap_dfr(historic_habitat, function(poly, sp) {
  poly |>
    sf::st_drop_geometry() |>
    as_tibble() |>
    select(-strata_uids) |>
    mutate(species = sp, .before = 1)
}) |>
  left_join(massdmf_counts, by = c("species" = "COMNAME")) |>
  mutate(across(c(n_tows_massdmf, n_massdmf_in_strata), ~ replace_na(.x, 0L))) |>
  arrange(pct_tows_in_footprint)

write_csv(habitat_summary, file.path(dir_data, "historic_habitat_summary.csv"))
print(habitat_summary, n = Inf)


# -------------------------------------------------------------------
# 4. Maps
# -------------------------------------------------------------------

map_footprint <- function(sp, poly, tows_sp, strata, land, bathy = NULL) {
  
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(bbox[c("xmin", "xmax")]) * 0.15)
  ypad <- max(2, diff(bbox[c("ymin", "ymax")]) * 0.15)
  xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
  ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)
  
  p <- ggplot()
  
  if (!is.null(bathy)) {
    bathy_df <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2])) |>
      as.data.frame(xy = TRUE) |>
      setNames(c("x", "y", "depth")) |>
      filter(depth < 0) |>
      mutate(depth_capped = pmax(depth, -2000))
    
    p <- p +
      geom_tile(data = bathy_df, aes(x, y, fill = depth_capped)) +
      scale_fill_gradientn(
        colors = c("grey15", "grey45", "grey75", "grey92"),
        values = scales::rescale(c(-2000, -500, -200, 0)),
        limits = c(-2000, 0), name = "Depth (m)", na.value = "white",
        labels = function(x) ifelse(x == -2000, "≤ -2000", as.character(x))
      ) +
      ggnewscale::new_scale_fill()
  }
  
  p +
    geom_sf(data = poly, fill = "steelblue", colour = "steelblue4", alpha = 0.45, linewidth = 0.6) +
    geom_sf(data = strata, fill = NA, colour = "orange", linewidth = 0.2, alpha = 0.5) +
    geom_point(data = tows_sp, aes(LON, LAT, colour = SURVEY), size = 0.8, alpha = 0.7) +
    scale_colour_manual(values = c(NEFSC = "#4b2991", MassDMF = "#e7298a"), name = "Survey") +
    geom_sf(data = land, fill = "grey35", colour = NA) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(
      title = paste0(species_label(sp), " — Fall historic habitat"),
      subtitle = paste0(
        "Positive tows: ", poly$n_tows, " (", round(100 * poly$n_tows_in_footprint / poly$n_tows),
        "% inside footprint)  |  Strata: ", poly$n_strata,
        "  |  Threshold: ≥ ", params$min_stations_per_stratum_year, " stations/stratum/year"
      ),
      caption = paste0("Blue: footprint. Orange outlines: NEFSC survey strata.",
                       if (!poly$adequate_footprint)
                         paste0("  NOT SCORED: footprint contains < ",
                                params$min_pct_tows_in_footprint, "% of positive tows.")
                       else ""),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(size = 8, colour = "grey40"),
      plot.caption  = element_text(size = 7, colour = "grey50"),
      panel.grid.major = element_line(colour = "grey70", linewidth = 0.2)
    )
}

if (params$make_maps) {
  
  message("Drawing maps...")
  
  bathy <- tryCatch({
    b <- marmap::getNOAA.bathy(lon1 = -82, lon2 = -60, lat1 = 34, lat2 = 48,
                               resolution = 1, keep = TRUE)
    r <- terra::rast(marmap::as.raster(b))
    terra::crs(r) <- "EPSG:4326"
    r
  }, error = function(e) {
    message("  Bathymetry unavailable (", conditionMessage(e), "); maps drawn without it.")
    NULL
  })
  
  iwalk(historic_habitat, function(poly, sp) {
    p <- map_footprint(sp, poly, filter(tows, COMNAME == sp), strata, land, bathy)
    ggsave(file.path(dir_images, paste0(safe_name(sp), "_fall_historic_habitat.png")),
           p, width = 8, height = 7, dpi = 300, bg = "white")
  })
  
  message("Maps saved to: ", dir_images)
}

message("Script complete.")