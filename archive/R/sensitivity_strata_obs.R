# sensitivity_strata_obs.R
#
# Purpose: Identify the appropriate observation cutoff for historic habitat (V6).
#          Evaluates how total habitat area changes as the PERCENTAGE threshold 
#          for inclusion increases (stratum must hold 0.1% to 10% of total obs).
#
#          Implements the "Cumulative Observation Target" (Method 2) to identify 
#          the percentage thresholds that retain exactly >= 90% and >= 95% 
#          of all historical observations for each species.
#
# Output:
#   RDS   : data/sensitivity/strata_sensitivity.rds
#   Plots : images/sensitivity/strata/
#
# Dependencies: tidyverse, sf, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_plots   <- here::here("images/sensitivity/strata")
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


# -------------------------------------------------------------------
# 3. Survey strata & Optimization
# -------------------------------------------------------------------

strata_path <- "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp"
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

strata <- sf::st_read(strata_path, quiet = TRUE) |>
  sf::st_set_crs(4269) |>       
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  mutate(
    stratum_id = row_number(),
    area_km2   = as.numeric(sf::st_area(sf::st_transform(geometry, 5070))) / 1e6
  ) |>
  select(stratum_id, area_km2)

Sys.unsetenv("SHAPE_RESTORE_SHX")


# -------------------------------------------------------------------
# 4. Pre-compute Observation Counts
# -------------------------------------------------------------------

message("Intersecting historical observations with survey strata...")

pts_sf <- survdat_mgmt |>
  filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
  mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
  distinct(COMNAME, CRUISE6, STATION, LAT, LON) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

pts_with_strata <- sf::st_join(pts_sf, strata, join = sf::st_intersects)

strata_counts <- pts_with_strata |>
  sf::st_drop_geometry() |>
  filter(!is.na(stratum_id)) |> 
  group_by(COMNAME, stratum_id, area_km2) |>
  summarize(n_obs = n(), .groups = "drop")

total_counts <- pts_with_strata |>
  sf::st_drop_geometry() |>
  filter(!is.na(stratum_id)) |>
  group_by(COMNAME) |>
  summarize(total_obs = n(), .groups = "drop")

strata_summary <- strata_counts |>
  left_join(total_counts, by = "COMNAME") |>
  mutate(pct_of_total = (n_obs / total_obs) * 100)


# -------------------------------------------------------------------
# 5. Core Sensitivity Sweep (Percentage Thresholds)
# -------------------------------------------------------------------

# Sweep from 0.1% to 10% of total observations
pct_thresholds <- seq(0.1, 10, by = 0.1)       
all_species <- unique(ne_species$COMNAME)

message("Sweeping percentage thresholds and calculating retained observations...")

pct_results <- map_dfr(all_species, function(sp) {
  df <- strata_summary |> filter(COMNAME == sp)
  if(nrow(df) == 0) return(NULL)
  
  tot_obs <- df$total_obs[1]
  
  map_dfr(pct_thresholds, function(thresh) {
    # Stratum is kept if it holds at least 'thresh' percent of the total species observations
    qualifying <- df |> filter(pct_of_total >= thresh)
    
    # Calculate how many total observations survive this threshold
    obs_retained <- sum(qualifying$n_obs)
    pct_retained <- (obs_retained / tot_obs) * 100
    
    tibble(
      species          = sp,
      threshold        = thresh,
      area_km2         = sum(qualifying$area_km2),
      n_strata         = nrow(qualifying),
      pct_obs_retained = pct_retained
    )
  })
})


# -------------------------------------------------------------------
# 6. Extract Target Cutoffs (90% and 95%)
# -------------------------------------------------------------------

safe_max <- function(x) {
  if (length(x) > 0 && !all(is.na(x))) max(x, na.rm = TRUE) else NA_real_
}

message("Identifying 90% and 95% target cutoffs...")

target_cutoffs <- pct_results |>
  group_by(species) |>
  summarize(
    # Find the highest % threshold that still retains >= 95% of historical observations
    cutoff_95 = safe_max(threshold[pct_obs_retained >= 95]),
    # Find the highest % threshold that still retains >= 90% of historical observations
    cutoff_90 = safe_max(threshold[pct_obs_retained >= 90]),
    .groups = "drop"
  )

print(target_cutoffs, n = Inf)


# -------------------------------------------------------------------
# 7. Save RDS
# -------------------------------------------------------------------

saveRDS(
  list(
    percentage_sweep = pct_results,
    strata_summary   = strata_summary,
    target_cutoffs   = target_cutoffs
  ),
  file.path(dir_results, "strata_sensitivity_cumulative.rds")
)
message("Results saved to: ", file.path(dir_results, "strata_sensitivity_cumulative.rds"))


# -------------------------------------------------------------------
# 8. Visualization
# -------------------------------------------------------------------

message("Generating faceted area decay plots...")

p_pct_area <- pct_results |>
  ggplot(aes(x = threshold, y = area_km2)) +
  geom_line(color = "seagreen", linewidth = 0.8) +
  geom_point(color = "seagreen4", size = 1) +
  
  # 95% Observation Target
  geom_vline(data = target_cutoffs, aes(xintercept = cutoff_95), 
             color = "seagreen", linetype = "solid", linewidth = 0.7) +
  
  # 90% Observation Target
  geom_vline(data = target_cutoffs, aes(xintercept = cutoff_90), 
             color = "firebrick", linetype = "solid", linewidth = 0.7) +
  
  facet_wrap(~species, scales = "free_y") +
  labs(
    title = "Habitat Area vs. Percentage Observation Threshold",
    subtitle = "Green solid: Retains 95% of obs. Red solid: Retains 90% of obs.",
    x = "Minimum % of Total Observations Required in Stratum",
    y = expression("Habitat Area (km"^2*")")
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7))

ggsave(file.path(dir_plots, "strata_sens_cumulative_targets.png"), 
       plot = p_pct_area, width = 16, height = 12, dpi = 300)


# --- 8b. Percentage Retained Curve (Secondary Plot) ---
# Zoomed in to 85-100% to clearly display the cutoff choices

p_retained <- pct_results |>
  ggplot(aes(x = threshold, y = pct_obs_retained)) +
  geom_line(color = "darkorchid4", linewidth = 0.8) +
  geom_hline(yintercept = 95, color = "seagreen", linetype = "dashed") +
  geom_hline(yintercept = 90, color = "firebrick", linetype = "dashed") +
  
  # coord_cartesian zooms the axis without dropping out-of-bounds data points
  coord_cartesian(ylim = c(85, 100)) +
  scale_y_continuous(breaks = seq(85, 100, by = 5)) +
  
  facet_wrap(~species) +
  labs(
    title = "Percentage of Total Observations Retained (Zoomed 85-100%)",
    subtitle = "Green dashed = 95% Target. Red dashed = 90% Target.",
    x = "Minimum % of Total Observations Required in Stratum",
    y = "% of Total Historical Observations"
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7))

ggsave(file.path(dir_plots, "strata_sens_obs_retained.png"), 
       plot = p_retained, width = 16, height = 12, dpi = 300)

message("Plots saved to: ", dir_plots)