# R/02_get_perc_suitable_thermal_habitat.R
#
# Purpose: Calculate the fall thermal suitability indicator S (manuscript
#          Section 2.3): the area-weighted percentage of footprint cell-days
#          from September-November whose bottom temperature fell inside the
#          species' thermal niche.
#
# Logic:   - Weight of each grid cell = fraction of the cell inside the
#            footprint x cell area, so boundary cells count by overlap and
#            cells are weighted by their true area, not equally.
#          - Bottom temperature: du Pontavice ERDDAP product through
#            params$erddap_last_year; daily GLORYS files for later years.
#          - Reports the number of daily layers per year; warns if a fall is
#            incomplete or a year is missing.
#
# Output:
#   RDS : data/indicators/perc_suitable_thermal_habitat_fall.rds
#   CSV : data/indicators/perc_suitable_thermal_habitat_fall.csv
#
# Dependencies: tidyverse, sf, terra, exactextractr, here

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

ensure_dir(dirname(paths$indicator))


# -------------------------------------------------------------------
# 1. Inputs
# -------------------------------------------------------------------

footprints <- readRDS(paths$habitat)
niche      <- readRDS(paths$niche) |> filter(included)

adequate     <- names(keep(footprints, ~ isTRUE(.x$adequate_footprint)))
species_list <- sort(intersect(adequate, niche$COMNAME))

message("Calculating fall thermal suitability for ", length(species_list), " species (",
        length(footprints) - length(adequate), " excluded for inadequate footprints).")

# Common spatial domain: cells where the ERDDAP product has temperature.
# GLORYS covers more nearshore area; clipping to the ERDDAP domain keeps the
# area identical across sources so the source switch can't change S.
erddap_first <- terra::rast(paths$bt_erddap, lyrs = 1)

erddap_domain <- terra::ifel(is.na(erddap_first), NA, 1) |>
  terra::as.polygons() |>
  sf::st_as_sf() |>
  sf::st_union() |>
  sf::st_make_valid()

footprints_scored <- imap(footprints[species_list], function(p, sp) {
  clipped <- suppressWarnings(sf::st_intersection(sf::st_geometry(p), erddap_domain))
  sf::st_sf(geometry = sf::st_union(clipped))
})

domain_coverage <- tibble(
  species        = species_list,
  footprint_km2  = map_dbl(footprints[species_list], ~ .x$area_km2),
  in_domain_km2  = map_dbl(footprints_scored, ~ as.numeric(sf::st_area(.x)) / 1e6)
) |>
  mutate(pct_in_domain = 100 * in_domain_km2 / footprint_km2) |>
  arrange(pct_in_domain)

write_csv(domain_coverage, file.path(dirname(paths$indicator), "footprint_temperature_domain.csv"))
message("Footprint area covered by both temperature products: median ",
        round(median(domain_coverage$pct_in_domain)), "% (min ",
        round(min(domain_coverage$pct_in_domain)), "%, ",
        domain_coverage$species[1], ")")

# Footprints as terra extents once, for cropping
footprint_ext <- map(footprints_scored, function(p) terra::ext(terra::vect(p)))


# -------------------------------------------------------------------
# 2. Core calculation for one fall
# -------------------------------------------------------------------

calc_fall_suitability <- function(bt_fall, year, bt_source) {
  
  n_days <- terra::nlyr(bt_fall)
  if (n_days < params$season_days) {
    warning(year, ": only ", n_days, " of ", params$season_days, " fall days available.")
  }
  
  # Unique layer names so extraction columns never collide
  names(bt_fall) <- paste0("d", seq_len(n_days))
  
  map_dfr(species_list, function(sp) {
    
    th   <- niche[niche$COMNAME == sp, ]
    poly <- footprints_scored[[sp]]
    
    bt_crop <- terra::crop(bt_fall, footprint_ext[[sp]], snap = "out")
    
    ex <- exactextractr::exact_extract(
      bt_crop, poly, include_area = TRUE, progress = FALSE
    )[[1]]
    
    temps <- as.matrix(ex[, setdiff(names(ex), c("coverage_fraction", "area")), drop = FALSE])
    w     <- ex$coverage_fraction * ex$area          # area in m2
    
    has_temp <- !is.na(temps)
    suitable <- has_temp & temps >= th$tmin & temps <= th$tmax
    
    total_w    <- sum(w * rowSums(has_temp))
    suitable_w <- sum(w * rowSums(suitable))
    
    tibble(
      species       = sp,
      year          = year,
      season        = params$season,
      perc_suitable = if (total_w > 0) 100 * suitable_w / total_w else NA_real_,
      n_days        = n_days,
      footprint_km2_with_temp = sum(w[rowSums(has_temp) > 0]) / 1e6,
      bt_source     = bt_source,
      tmin          = th$tmin,
      tmax          = th$tmax
    )
  })
}

fall_layers <- function(r, year) {
  t <- terra::time(r)
  idx <- which(as.integer(format(t, "%Y")) == year &
                 as.integer(format(t, "%m")) %in% params$season_months)
  if (length(idx) == 0) return(NULL)
  r[[idx]]
}


# -------------------------------------------------------------------
# 3. du Pontavice ERDDAP product (through params$erddap_last_year)
# -------------------------------------------------------------------

if (!file.exists(paths$bt_erddap)) stop("ERDDAP bottom temperature NetCDF not found: ", paths$bt_erddap)

bt_all     <- terra::rast(paths$bt_erddap)
erddap_yrs <- sort(unique(as.integer(format(terra::time(bt_all), "%Y"))))
erddap_yrs <- erddap_yrs[erddap_yrs <= params$erddap_last_year]

message("--- ERDDAP product: ", min(erddap_yrs), "-", max(erddap_yrs), " ---")

results_erddap <- map(erddap_yrs, function(yr) {
  bt_fall <- fall_layers(bt_all, yr)
  if (is.null(bt_fall)) return(NULL)
  message("  ", yr)
  calc_fall_suitability(bt_fall, yr, "ERDDAP (du Pontavice)")
})


# -------------------------------------------------------------------
# 4. Daily GLORYS files (after params$erddap_last_year)
# -------------------------------------------------------------------

glorys_files <- list.files(paths$bt_glorys_dir, pattern = "^GLORYS_daily_BottomTemp_\\d{4}\\.nc$",
                           full.names = TRUE)
glorys_yrs   <- as.integer(str_extract(basename(glorys_files), "\\d{4}"))
keep         <- glorys_yrs > params$erddap_last_year
glorys_files <- glorys_files[keep]
glorys_yrs   <- glorys_yrs[keep]

message("--- GLORYS: ", if (length(glorys_yrs)) paste(range(glorys_yrs), collapse = "-") else "none", " ---")

results_glorys <- map2(glorys_files, glorys_yrs, function(f, yr) {
  bt_fall <- fall_layers(terra::rast(f), yr)
  if (is.null(bt_fall)) {
    warning(basename(f), ": no fall layers found.")
    return(NULL)
  }
  message("  ", yr)
  calc_fall_suitability(bt_fall, yr, "GLORYS")
})


# -------------------------------------------------------------------
# 5. Combine, check, save
# -------------------------------------------------------------------

indicator <- bind_rows(results_erddap, results_glorys) |>
  arrange(species, year)

all_years     <- seq(min(indicator$year), max(indicator$year))
missing_years <- setdiff(all_years, unique(indicator$year))
if (length(missing_years) > 0) {
  warning("Years with no fall bottom temperature: ", paste(missing_years, collapse = ", "))
}

saveRDS(indicator, paths$indicator)
write_csv(indicator, sub("\\.rds$", ".csv", paths$indicator))

message("Indicator saved to: ", paths$indicator)
message("Time series: ", min(indicator$year), "-", max(indicator$year),
        " | species: ", n_distinct(indicator$species))