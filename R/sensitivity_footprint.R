# R/sensitivity_footprint.R
#
# Purpose: Sensitivity checks for the historic footprint definition
#          (manuscript Section 2.2 / supplement). Not part of the main pipeline.
#
#   A. Station threshold: how footprint area and the share of positive tows
#      inside the footprint change as the minimum number of positive stations
#      per stratum per year varies (1-6). The pipeline uses
#      params$min_stations_per_stratum_year.
#
#   B. Footprint period: full time series vs. baseline years only
#      (params$baseline_years). Shows which species gain strata from
#      post-baseline occupancy (e.g., range expansion).
#
# Output:
#   CSV : data/sensitivity/footprint_threshold.csv
#   CSV : data/sensitivity/footprint_period.csv
#   PNG : images/sensitivity/footprint_threshold.png
#
# Run after 00_get_thermal_niches.R.
# Dependencies: tidyverse, sf, rnaturalearth, here

library(tidyverse)
library(sf)
library(here)

source(here::here("R/utils_config.R"))
source(here::here("R/utils_pipeline.R"))

dir_data   <- ensure_dir(here::here("data/sensitivity"))
dir_images <- ensure_dir(paths$images_dir, "sensitivity")

strata     <- load_strata(paths)
land_union <- load_land() |> sf::st_union() |> sf::st_make_valid()
tows       <- load_fall_positive_tows(paths, params) |> assign_strata(strata)

niche        <- readRDS(paths$niche) |> filter(included)
species_list <- sort(intersect(unique(tows$COMNAME), niche$COMNAME))

footprint_stats <- function(sp, min_stations, years = NULL) {
  fp <- build_footprint(filter(tows, COMNAME == sp), strata, land_union, min_stations, years)
  # Share of ALL positive tows (all years) that fall inside this footprint
  all_sp <- filter(tows, COMNAME == sp)
  if (is.null(fp)) {
    return(tibble(species = sp, area_km2 = 0, n_strata = 0L, pct_tows_in_footprint = 0))
  }
  tibble(
    species  = sp,
    area_km2 = fp$area_km2,
    n_strata = fp$n_strata,
    pct_tows_in_footprint = 100 * mean(all_sp$strata_uid %in% fp$strata_uids[[1]])
  )
}


# -------------------------------------------------------------------
# A. Station threshold
# -------------------------------------------------------------------

message("A. Station threshold sensitivity...")

thresholds <- 1:6

threshold_df <- crossing(species = species_list, min_stations = thresholds) |>
  pmap_dfr(function(species, min_stations) {
    footprint_stats(species, min_stations, params$footprint_years) |>
      mutate(min_stations = min_stations)
  }) |>
  group_by(species) |>
  mutate(rel_area = area_km2 / area_km2[min_stations == 1]) |>
  ungroup()

write_csv(threshold_df, file.path(dir_data, "footprint_threshold.csv"))

threshold_summary <- threshold_df |>
  pivot_longer(c(rel_area, pct_tows_in_footprint), names_to = "metric") |>
  mutate(value = ifelse(metric == "rel_area", 100 * value, value)) |>
  group_by(min_stations, metric) |>
  summarise(median = median(value, na.rm = TRUE),
            lo = quantile(value, 0.1, na.rm = TRUE),
            hi = quantile(value, 0.9, na.rm = TRUE), .groups = "drop")

p_thr <- ggplot(threshold_summary, aes(min_stations, median)) +
  geom_vline(xintercept = params$min_stations_per_stratum_year, linetype = "dotted") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) +
  geom_line() + geom_point() +
  facet_wrap(~metric, scales = "free_y",
             labeller = as_labeller(c(rel_area = "Footprint area (% of threshold = 1)",
                                      pct_tows_in_footprint = "Positive tows inside footprint (%)"))) +
  scale_x_continuous(breaks = thresholds) +
  labs(title = "Footprint sensitivity to the station threshold",
       subtitle = "Median across species; ribbon = 10th-90th percentile. Dotted = value used.",
       x = "Minimum positive stations per stratum in a single year", y = NULL) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_images, "footprint_threshold.png"), p_thr, width = 10, height = 4.5, dpi = 300, bg = "white")


# -------------------------------------------------------------------
# B. Full series vs. baseline-only footprint
# -------------------------------------------------------------------

message("B. Footprint period comparison...")

period_df <- map_dfr(species_list, function(sp) {
  full <- footprint_stats(sp, params$min_stations_per_stratum_year, NULL)
  base <- footprint_stats(sp, params$min_stations_per_stratum_year, params$baseline_years)
  tibble(
    species            = sp,
    area_full_km2      = full$area_km2,
    area_baseline_km2  = base$area_km2,
    strata_full        = full$n_strata,
    strata_baseline    = base$n_strata,
    pct_area_only_outside_baseline = 100 * (1 - base$area_km2 / full$area_km2)
  )
}) |>
  arrange(desc(pct_area_only_outside_baseline))

write_csv(period_df, file.path(dir_data, "footprint_period.csv"))

message("Species whose full-series footprint is most larger than the baseline-only footprint:")
print(head(period_df, 15))

message("Script complete.")