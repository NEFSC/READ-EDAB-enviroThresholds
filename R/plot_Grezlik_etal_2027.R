# R/plot_Grezlik_etal_2027.R
#
# Purpose: Generate finalized, publication-ready figures for Grezlik et al. (2027).
#          Outputs are saved to images/Grezlik_et_al._2027.
#          Includes ALL NEFMC and JOINT managed species (excluding Atlantic Salmon),
#          with a consistent alphabetical grid ordering across all multi-species figures.
#          Restricted to FALL data only, through terminal year 2023.

library(tidyverse)
library(here)
library(patchwork)
library(scales)
library(sf)
library(gridExtra)
library(grid)
library(rnaturalearth)

# -------------------------------------------------------------------
# 0. Setup & Global Aesthetics
# -------------------------------------------------------------------
dir_out <- here::here("images/Grezlik_et_al._2027")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Publication-ready theme base
theme_pub <- function() {
  theme_classic(base_size = 10) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.text = element_text(color = "black")
    )
}

# -------------------------------------------------------------------
# 1. Define Standardized Species, Survey Data, & Spatial Basemaps
# -------------------------------------------------------------------
message("Loading species metadata, survey data, and spatial basemaps...")

# Species List - Exclude Atlantic Salmon globally
species_raw <- readRDS("~/EDAB_Resources/static_workflow_inputs/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ms_species <- species_raw |>
  dplyr::filter(Fed.Managed %in% c("NEFMC", "JOINT")) |>
  dplyr::filter(COMNAME != "ATLANTIC SALMON") |> # Exclude Atlantic Salmon from all figures/tables
  dplyr::distinct(SVSPP, COMNAME, Fed.Managed) |>
  dplyr::mutate(
    species_label = tools::toTitleCase(tolower(COMNAME))
  ) |>
  dplyr::arrange(species_label) |>
  dplyr::mutate(
    species_factor = factor(species_label, levels = unique(species_label))
  )

# Prep Survey Data - strictly FALL and <= 2023
survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/review_for_deletion/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/review_for_deletion/massInshoreData.rds")$survdat
survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
)) |>
  dplyr::filter(SEASON == "FALL", as.numeric(YEAR) <= 2023)

# Load Common Spatial Elements (Strata & Land) for Figs 2 & 6
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
strata <- sf::st_read("~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp", quiet = TRUE) |>
  sf::st_set_crs(4269) |> 
  sf::st_transform(4326) |>
  sf::st_make_valid() |> 
  dplyr::mutate(
    strata_area_km2 = as.numeric(sf::st_area(sf::st_transform(geometry, 5070))) / 1e6,
    strata_uid = dplyr::row_number()
  )
Sys.unsetenv("SHAPE_RESTORE_SHX")

# Get bounding box for standardized map views
strata_bbox <- sf::st_bbox(strata)

# Load coastline
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  sf::st_transform(4326)


# ===================================================================
# FIGURE 1: Thermal Niche Histograms (Survey vs. Literature)
# ===================================================================
message("\nProcessing data for Figure 1...")

lit_adults <- read_csv(here::here('data', 'MS_screening_analysis_cleaned.csv'), show_col_types = FALSE) |> 
  dplyr::mutate(
    common.name = stringr::str_to_upper(common.name),
    dplyr::across(
      c(opt.temp.min, opt.temp.max, lethal.temp.min, lethal.temp.max),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) |> 
  dplyr::filter(age.group == "Adult") |> 
  dplyr::filter(location == 'Northeast US')

# Left join ensures ALL manuscript species remain in the dataset
fig1_lit_bounds <- ms_species |>
  dplyr::left_join(lit_adults, by = c("COMNAME" = "common.name")) |>
  dplyr::select(species_factor, opt.temp.min, opt.temp.max, lethal.temp.min, lethal.temp.max)

fig1_survdat <- survdat |>
  dplyr::inner_join(ms_species, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(BOTTEMP)) |>
  dplyr::mutate(BOTTEMP = as.numeric(BOTTEMP))

fig1_survey_bounds <- fig1_survdat |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    tmin = quantile(BOTTEMP, probs = 0.10, na.rm = TRUE),
    tmax = quantile(BOTTEMP, probs = 0.90, na.rm = TRUE),
    .groups = "drop"
  )

message("Generating Figure 1 (Grid of Histograms)...")
p_fig1 <- ggplot() +
  geom_histogram(
    data = fig1_survdat, aes(x = BOTTEMP),
    binwidth = 1, fill = "grey75", color = "grey30", alpha = 0.8
  ) +
  geom_vline(data = fig1_lit_bounds, aes(xintercept = opt.temp.min, color = "Lit Opt Low"), linewidth = 0.6, linetype = "dashed", na.rm = TRUE) +
  geom_vline(data = fig1_lit_bounds, aes(xintercept = opt.temp.max, color = "Lit Opt High"), linewidth = 0.6, linetype = "dashed", na.rm = TRUE) +
  geom_vline(data = fig1_lit_bounds, aes(xintercept = lethal.temp.min, color = "Lit Lethal Low"), linewidth = 0.6, linetype = "dotted", na.rm = TRUE) +
  geom_vline(data = fig1_lit_bounds, aes(xintercept = lethal.temp.max, color = "Lit Lethal High"), linewidth = 0.6, linetype = "dotted", na.rm = TRUE) +
  geom_vline(data = fig1_survey_bounds, aes(xintercept = tmin, color = "Survey 10-90th"), linewidth = 0.8, linetype = "solid") +
  geom_vline(data = fig1_survey_bounds, aes(xintercept = tmax, color = "Survey 10-90th"), linewidth = 0.8, linetype = "solid") +
  facet_wrap(~ species_factor, scales = "free_y", ncol = 4) +
  scale_color_manual(
    name = "Niche Boundaries",
    values = c("Survey 10-90th" = "#000000", "Lit Lethal Low" = "#009E73", "Lit Opt Low" = "#56B4E9", "Lit Opt High" = "#0072B2", "Lit Lethal High" = "#D55E00"),
    breaks = c("Lit Lethal Low", "Lit Opt Low", "Survey 10-90th", "Lit Opt High", "Lit Lethal High")
  ) +
  labs(x = "Bottom Temperature (\u00B0C)", y = "Frequency (Positive Tows)") +
  theme_pub()

fig1_file <- file.path(dir_out, "Fig1_Thermal_Niche_Histograms.png")
ggsave(fig1_file, plot = p_fig1, width = 12, height = 12, dpi = 300, bg = "white")
message("Saved Figure 1 to: ", fig1_file)


# ===================================================================
# FIGURE 2: Historic Fall Habitat Polygons
# ===================================================================
message("\nProcessing data for Figure 2 (Historic Habitat Grid)...")

# Load V6 Historic Habitat (List of SF objects)
habitat_v6 <- readRDS(here::here("data/historic_habitat_V6_seasonal/historic_habitat_V6_seasonal.rds"))

fall_polys <- habitat_v6[grep("_FALL$", names(habitat_v6))]
fig2_sf <- dplyr::bind_rows(fall_polys) |>
  dplyr::inner_join(ms_species, by = "COMNAME")

message("Generating Figure 2 (Gridded Fall Habitat Maps)...")
p_fig2 <- ggplot() +
  geom_sf(data = land, fill = "grey70", color = NA) +
  geom_sf(data = strata, fill = NA, color = "grey85", linewidth = 0.1) +
  geom_sf(data = fig2_sf, fill = "steelblue", color = "steelblue4", alpha = 0.7, linewidth = 0.2) +
  
  facet_wrap(~ species_factor, ncol = 5) +
  
  coord_sf(
    xlim = c(strata_bbox["xmin"], strata_bbox["xmax"]), 
    ylim = c(strata_bbox["ymin"], strata_bbox["ymax"]), 
    expand = FALSE
  ) +
  theme_pub() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.3, "lines")
  )

fig2_file <- file.path(dir_out, "Fig2_Historic_Habitat_Grid.png")
ggsave(fig2_file, plot = p_fig2, width = 14, height = 14, dpi = 300, bg = "white")
message("Saved Figure 2 to: ", fig2_file)


# ===================================================================
# FIGURE 3: Time Series Grid of Fall Anomalies
# ===================================================================
message("\nProcessing data for Figure 3 (Continuous Fall Anomaly)...")

# Force continuous year sequence (1959-2023) to explicitly capture missing year 2021
all_years <- 1959:2023

fig3_data <- readRDS(here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")) |>
  dplyr::filter(season == "FALL", year <= 2023) |>
  dplyr::inner_join(ms_species, by = c("species" = "COMNAME")) |>
  tidyr::complete(species_factor, year = all_years) |>
  dplyr::arrange(species_factor, year) |>
  dplyr::group_by(species_factor) |>
  dplyr::mutate(
    baseline_val = dplyr::first(perc_within_hist[!is.na(perc_within_hist)]),
    anomaly = perc_within_hist - baseline_val
  ) |>
  dplyr::ungroup()

message("Generating Figure 3 (Fall Time Series Grid)...")
p_fig3 <- ggplot(fig3_data, aes(x = year, y = anomaly)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "solid", linewidth = 0.5) +
  geom_line(color = "black", linewidth = 0.8, alpha = 0.85) +
  facet_wrap(~ species_factor, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 20)) +
  labs(
    x = "Year", 
    y = "Fall Thermal Habitat Anomaly (%)"
  ) +
  theme_pub()

fig3_file <- file.path(dir_out, "Fig3_Anomaly_Time_Series.png")
ggsave(fig3_file, plot = p_fig3, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Figure 3 to: ", fig3_file)


# ===================================================================
# FIGURE 4: Risk Score Distribution (Fall Time Series)
# ===================================================================
message("\nProcessing data for Figure 4...")

# Calculate baseline stats (1982-2011) using pure ocean model data
baseline_stats <- fig3_data |>
  dplyr::filter(year >= 1982 & year <= 2011) |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    base_mean = mean(anomaly, na.rm = TRUE),
    base_sd = sd(anomaly, na.rm = TRUE),
    .groups = "drop"
  )

fig4_scored <- fig3_data |>
  dplyr::inner_join(baseline_stats, by = "species_factor") |>
  dplyr::mutate(
    z_score = (anomaly - base_mean) / base_sd,
    annual_risk_score = as.integer(pmax(-4, pmin(4, round(z_score))))
  )

fig4_plot_data <- fig4_scored |>
  dplyr::filter(!is.na(annual_risk_score)) |>
  dplyr::group_by(annual_risk_score) |>
  dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
  tidyr::complete(annual_risk_score = -4:4, fill = list(n = 0))

total_obs <- sum(fig4_plot_data$n)
normal_df <- data.frame(annual_risk_score = seq(-4.5, 4.5, length.out = 200)) |>
  dplyr::mutate(n = total_obs * dnorm(annual_risk_score, mean = 0, sd = 1))

message("Generating Figure 4 (Risk Score Distribution Histogram)...")
p_fig4 <- ggplot() +
  geom_col(data = fig4_plot_data, aes(x = annual_risk_score, y = n), fill = "grey75", width = 1, color = "black", linewidth = 0.4, alpha = 0.9) +
  geom_line(data = normal_df, aes(x = annual_risk_score, y = n), linetype = "dashed", color = "black", linewidth = 1) +
  scale_x_continuous(breaks = -4:4, limits = c(-4.5, 4.5), expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "NEFMC Risk Policy Score", 
    y = "Number of Observations",
    title = "Risk Policy Score Distribution (Fall Habitat Anomaly)"
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(size = 11, face = "bold"), axis.title = element_text(face = "bold"))

fig4_file <- file.path(dir_out, "Fig4_Risk_Score_Distribution.png")
ggsave(fig4_file, plot = p_fig4, width = 8, height = 5, dpi = 300, bg = "white")
message("Saved Figure 4 to: ", fig4_file)


# ===================================================================
# FIGURE 5: Continuous Risk Score Heatmap over Time (1959–2023)
# ===================================================================
message("\nProcessing data for Figure 5 (Continuous Heatmap)...")

fig5_data <- fig4_scored |>
  tidyr::complete(species_factor, year = all_years)

message("Generating Figure 5 (Risk Score Heatmap)...")
p_fig5 <- ggplot(fig5_data, aes(x = year, y = forcats::fct_rev(species_factor), fill = annual_risk_score)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # Bounding box + translucent dark shading over 1982-2011 baseline window
  annotate("rect", xmin = 1981.5, xmax = 2011.5, ymin = -Inf, ymax = Inf, 
           fill = "black", alpha = 0.12, color = "black", linewidth = 1.1, linetype = "solid") +
  scale_fill_gradient2(
    low = "#0072B2", mid = "grey95", high = "#D55E00", midpoint = 0,
    name = "Risk\nScore", breaks = -4:4, limits = c(-4, 4),
    na.value = "grey85" # Missing 2021 indicator will render as neutral grey
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 5), expand = c(0, 0)) +
  labs(
    title = "Thermal Habitat Risk Scores: Fall Anomaly w/ 30-Year Baseline",
    x = "Year", y = NULL
  ) +
  theme_pub() +
  theme(legend.position = "right", legend.key.height = unit(1.2, "cm"), axis.text.x = element_text(angle = 45, hjust = 1))

fig5_file <- file.path(dir_out, "Fig5_Risk_Score_Heatmap.png")
ggsave(fig5_file, plot = p_fig5, width = 11, height = 12, dpi = 300, bg = "white")
message("Saved Figure 5 to: ", fig5_file)


# ===================================================================
# FIGURE 6: Correlation (Fall Modeled Suitability vs. Fall Empirical Ratio)
# ===================================================================
message("\nProcessing data for Figure 6 (Spatial Validation)...")

# 1. Modeled Thermal Suitability (FALL <= 2023)
mod_suitability <- readRDS(here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")) |>
  dplyr::filter(season == "FALL", year <= 2023) |>
  dplyr::select(species, year, season, perc_within_hist)

# 2. Total Historic Area (FALL)
hab_comp <- read_csv(here::here("data/historic_habitat_V6_seasonal/habitat_comparison_spring_vs_fall.csv"), show_col_types = FALSE) |>
  dplyr::select(species, historic_area_km2 = area_km2_FALL) |>
  dplyr::mutate(season = "FALL")

# 3. Create a strict "spine" of valid survey events (FALL <= 2023) - ONLY used for validation figure
valid_survey_events <- survdat |>
  dplyr::distinct(year = as.numeric(YEAR), season = SEASON)

survey_spine <- tidyr::expand_grid(
  species = ms_species$COMNAME,
  valid_survey_events
) |>
  dplyr::inner_join(ms_species, by = c("species" = "COMNAME"))

# 4. Calculate Fall Realized Area using the rigorous V6 threshold (>= 3 obs per stratum)
obs_clean <- survdat |>
  dplyr::inner_join(ms_species, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
  dplyr::mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), year = as.numeric(YEAR)) |>
  dplyr::distinct(COMNAME, year, SEASON, CRUISE6, STATION, LAT, LON) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

obs_with_strata <- sf::st_join(obs_clean, strata) |> sf::st_drop_geometry()

realized_area <- obs_with_strata |>
  dplyr::filter(!is.na(strata_uid)) |>
  dplyr::group_by(species = COMNAME, year, season = SEASON, strata_uid, strata_area_km2) |>
  dplyr::summarise(n_stations = dplyr::n_distinct(paste(CRUISE6, STATION)), .groups = "drop") |>
  dplyr::filter(n_stations >= 3) |>
  dplyr::group_by(species, year, season) |>
  dplyr::summarise(realized_area_km2 = sum(strata_area_km2), .groups = "drop")

# 5. Join to the spine and calculate empirical ratio
fig6_data <- survey_spine |>
  dplyr::left_join(realized_area, by = c("species", "year", "season")) |>
  dplyr::mutate(realized_area_km2 = tidyr::replace_na(realized_area_km2, 0)) |>
  dplyr::inner_join(hab_comp, by = c("species", "season")) |>
  dplyr::mutate(empirical_ratio = (realized_area_km2 / historic_area_km2) * 100) |>
  dplyr::inner_join(mod_suitability, by = c("species", "year", "season")) |>
  dplyr::filter(!is.na(perc_within_hist))

# 6. Calculate Pearson Correlation
cor_data <- fig6_data |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    r = if (sum(complete.cases(perc_within_hist, empirical_ratio)) > 2) cor(perc_within_hist, empirical_ratio, use = "complete.obs", method = "pearson") else NA_real_,
    x_pos = min(perc_within_hist, na.rm = TRUE),
    y_pos = max(empirical_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(label = ifelse(!is.na(r), sprintf("r = %.2f", r), ""))

message("Generating Figure 6 (Fall Validation Correlation)...")
p_fig6 <- ggplot(fig6_data, aes(x = perc_within_hist, y = empirical_ratio)) +
  geom_point(color = "black", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.8, se = FALSE) +
  geom_text(data = cor_data, aes(x = x_pos, y = y_pos, label = label), 
            hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "black") +
  facet_wrap(~ species_factor, scales = "free", ncol = 5) +
  labs(
    title = "Validation: Modeled Suitability vs. Empirical Ratio (FALL)",
    x = "Modeled Thermal Suitability (% of Habitat-Days)",
    y = "Empirical Survey Ratio (% of Area Occupied)"
  ) +
  theme_pub()

fig6_file <- file.path(dir_out, "Fig6_Validation_Correlation_Fall.png")
ggsave(fig6_file, plot = p_fig6, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Figure 6 to: ", fig6_file)


# ===================================================================
# TABLE 1: Finalized Thermal Niches and Observation Counts (Fall Only)
# ===================================================================
message("\nProcessing data for Table 1 (Thermal Niches)...")

# Identify all unique Fall survey years in the dataset (<= 2023)
all_fall_years   <- unique(survdat$YEAR)
total_fall_years <- length(all_fall_years)

# 1. Calculate positive observation counts per year for each species
annual_obs_counts <- fig1_survdat |>
  dplyr::group_by(species_label, YEAR) |>
  dplyr::summarize(n_obs_year = dplyr::n(), .groups = "drop")

# 2. Expand across ALL survey years to ensure years with 0 observations (which is <= 5) are included
prop_lte5_by_species <- tidyr::expand_grid(
  species_label = ms_species$species_label,
  YEAR = all_fall_years
) |>
  dplyr::left_join(annual_obs_counts, by = c("species_label", "YEAR")) |>
  dplyr::mutate(n_obs_year = tidyr::replace_na(n_obs_year, 0)) |>
  dplyr::group_by(species_label) |>
  dplyr::summarize(
    prop_years_lte_5 = round(sum(n_obs_year <= 5) / total_fall_years, 2),
    .groups = "drop"
  )

# 3. Build Table 1
table1_data <- ms_species |>
  dplyr::select(species_label) |>
  dplyr::left_join(fig1_survdat, by = "species_label") |>
  dplyr::group_by(species_label) |>
  dplyr::summarize(
    thermal_niche_min = if (all(is.na(BOTTEMP))) NA_real_ else round(quantile(BOTTEMP, probs = 0.10, na.rm = TRUE), 1),
    thermal_niche_max = if (all(is.na(BOTTEMP))) NA_real_ else round(quantile(BOTTEMP, probs = 0.90, na.rm = TRUE), 1),
    n_observations    = sum(!is.na(BOTTEMP)),
    .groups = "drop"
  ) |>
  dplyr::left_join(prop_lte5_by_species, by = "species_label") |>
  dplyr::arrange(species_label) |>
  dplyr::rename(
    "Species Name"                                 = species_label,
    "Thermal Niche Minimum (°C)"                   = thermal_niche_min,
    "Thermal Niche Maximum (°C)"                   = thermal_niche_max,
    "Number of Survey Observations"                = n_observations,
    "Proportion of Years with 5 or Fewer Observations" = prop_years_lte_5
  )

# 4. Save as CSV (Raw data backup)
table1_csv <- file.path(dir_out, "Table1_Thermal_Niches.csv")
readr::write_csv(table1_data, table1_csv)

# 5. Save as PNG (For the manuscript)
pub_theme <- gridExtra::ttheme_minimal(
  core = list(
    bg_params = list(fill = c("grey95", "white"), col = NA),
    fg_params = list(fontsize = 10, fontfamily = "sans")
  ),
  colhead = list(
    fg_params = list(fontsize = 11, fontface = "bold", fontfamily = "sans"),
    bg_params = list(fill = "grey80", col = "black", lwd = 1.5)
  )
)

table_grob <- gridExtra::tableGrob(table1_data, rows = NULL, theme = pub_theme)

# Adjust dimensions for 5 columns
plot_width  <- grid::convertWidth(sum(table_grob$widths), "in", valueOnly = TRUE) + 2.5
plot_height <- grid::convertHeight(sum(table_grob$heights), "in", valueOnly = TRUE) + 1.0

table1_png <- file.path(dir_out, "Table1_Thermal_Niches.png")
ggsave(table1_png, plot = table_grob, width = plot_width, height = plot_height, dpi = 300, bg = "white")

message("Saved Table 1 CSV to: ", table1_csv)
message("Saved Table 1 PNG to: ", table1_png)


# ===================================================================
# CANDIDATE REPLACEMENT PLOTS FOR FIGURE 6
# ===================================================================
message("\nProcessing Candidate Validation Plots for Figure 6...")

# -------------------------------------------------------------------
# CANDIDATE 6A: Dual Time-Series Overlay (Standardized Area Trends)
# -------------------------------------------------------------------
# Z-score standardize both time series so they share a common y-axis
fig6a_data <- fig6_data |>
  dplyr::group_by(species_factor) |>
  dplyr::mutate(
    z_thermal = scale(perc_within_hist)[,1],
    z_area    = scale(empirical_ratio)[,1]
  ) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = c(z_thermal, z_area),
    names_to = "metric",
    values_to = "z_value"
  ) |>
  dplyr::mutate(
    metric_label = ifelse(metric == "z_thermal", "Modeled Thermal Suitability", "Empirical Area Occupied")
  )

p_fig6a <- ggplot(fig6a_data, aes(x = year, y = z_value, color = metric_label, linetype = metric_label)) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ species_factor, ncol = 5) +
  scale_color_manual(values = c("Modeled Thermal Suitability" = "#0072B2", "Empirical Area Occupied" = "#D55E00")) +
  scale_linetype_manual(values = c("Modeled Thermal Suitability" = "solid", "Empirical Area Occupied" = "solid")) +
  labs(
    title = "Validation Option A: Dual Time-Series Trajectories (1963–2023)",
    x = "Year",
    y = "Standardized Anomaly (Z-Score)",
    color = "Metric",
    linetype = "Metric"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

fig6a_file <- file.path(dir_out, "Fig6A_Dual_TimeSeries_Overlay.png")
ggsave(fig6a_file, plot = p_fig6a, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Candidate Figure 6A to: ", fig6a_file)


# -------------------------------------------------------------------
# CANDIDATE 6B: Suitability vs. Encounter Rate (% Positive Tows)
# -------------------------------------------------------------------
annual_tows <- survdat |>
  dplyr::distinct(YEAR, CRUISE6, STATION) |>
  dplyr::group_by(year = as.numeric(YEAR)) |>
  dplyr::summarize(total_tows = dplyr::n(), .groups = "drop")

positive_tows <- fig1_survdat |>
  dplyr::distinct(species_factor, year = as.numeric(YEAR), CRUISE6, STATION) |>
  dplyr::group_by(species_factor, year) |>
  dplyr::summarize(pos_tows = dplyr::n(), .groups = "drop")

encounter_data <- ms_species |>
  dplyr::select(species_factor) |>
  tidyr::expand_grid(year = unique(annual_tows$year)) |>
  dplyr::left_join(positive_tows, by = c("species_factor", "year")) |>
  dplyr::left_join(annual_tows, by = "year") |>
  dplyr::mutate(
    pos_tows = tidyr::replace_na(pos_tows, 0),
    encounter_rate = (pos_tows / total_tows) * 100
  ) |>
  dplyr::inner_join(fig3_data, by = c("species_factor", "year")) |>
  dplyr::filter(!is.na(perc_within_hist))

cor_data_6b <- encounter_data |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    r = if (sum(complete.cases(perc_within_hist, encounter_rate)) > 2) cor(perc_within_hist, encounter_rate, use = "complete.obs") else NA_real_,
    x_pos = min(perc_within_hist, na.rm = TRUE),
    y_pos = max(encounter_rate, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(label = ifelse(!is.na(r), sprintf("r = %.2f", r), ""))

p_fig6b <- ggplot(encounter_data, aes(x = perc_within_hist, y = encounter_rate)) +
  geom_point(color = "black", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "#0072B2", linewidth = 0.8, se = FALSE) +
  geom_text(data = cor_data_6b, aes(x = x_pos, y = y_pos, label = label),
            hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "black") +
  facet_wrap(~ species_factor, scales = "free", ncol = 5) +
  labs(
    title = "Validation Option B: Modeled Suitability vs. Survey Encounter Rate",
    x = "Modeled Thermal Suitability (% of Habitat-Days)",
    y = "Survey Encounter Rate (% of Positive Tows)"
  ) +
  theme_pub()

fig6b_file <- file.path(dir_out, "Fig6B_Suitability_vs_EncounterRate.png")
ggsave(fig6b_file, plot = p_fig6b, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Candidate Figure 6B to: ", fig6b_file)


# -------------------------------------------------------------------
# CANDIDATE 6C: Dual Time-Series Overlay (Suitability & Encounter Rate)
# -------------------------------------------------------------------
fig6c_data <- encounter_data |>
  dplyr::group_by(species_factor) |>
  dplyr::mutate(
    z_thermal   = scale(perc_within_hist)[,1],
    z_encounter = scale(encounter_rate)[,1]
  ) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = c(z_thermal, z_encounter),
    names_to = "metric",
    values_to = "z_value"
  ) |>
  dplyr::mutate(
    metric_label = ifelse(metric == "z_thermal", "Modeled Thermal Suitability", "Survey Encounter Rate")
  )

p_fig6c <- ggplot(fig6c_data, aes(x = year, y = z_value, color = metric_label, linetype = metric_label)) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ species_factor, ncol = 5) +
  scale_color_manual(values = c("Modeled Thermal Suitability" = "#0072B2", "Survey Encounter Rate" = "#009E73")) +
  scale_linetype_manual(values = c("Modeled Thermal Suitability" = "solid", "Survey Encounter Rate" = "solid")) +
  labs(
    title = "Validation Option C: Suitability & Encounter Rate Time Series (1963–2023)",
    x = "Year",
    y = "Standardized Anomaly (Z-Score)",
    color = "Metric",
    linetype = "Metric"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

fig6c_file <- file.path(dir_out, "Fig6C_Dual_TimeSeries_EncounterRate.png")
ggsave(fig6c_file, plot = p_fig6c, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Candidate Figure 6C to: ", fig6c_file)

# -------------------------------------------------------------------
# CANDIDATES 6D & 6E: Within-Historic-Range Encounter Rate
# -------------------------------------------------------------------
message("\nProcessing Candidate Figures 6D & 6E (Within-Historic-Range Encounter Rate)...")

# 1. Convert all survey tow events to sf points
all_tows_sf <- survdat |>
  dplyr::filter(!is.na(LAT), !is.na(LON)) |>
  dplyr::distinct(YEAR, CRUISE6, STATION, LAT, LON) |>
  dplyr::mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), year = as.numeric(YEAR)) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

# 2. Spatial join: Find which survey tows fell within each species' V6 Fall polygon
# (fig2_sf already has species_factor attached)
tows_in_habitat <- sf::st_join(all_tows_sf, fig2_sf, join = sf::st_intersects) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(COMNAME))

# 3. Calculate total survey tows executed WITHIN each species' V6 footprint per year
annual_tows_in_range <- tows_in_habitat |>
  dplyr::group_by(species_factor, year) |>
  dplyr::summarize(total_range_tows = dplyr::n_distinct(paste(CRUISE6, STATION)), .groups = "drop")

# 4. Calculate positive survey tows executed WITHIN each species' V6 footprint per year
positive_tows_in_range <- survdat |>
  dplyr::inner_join(ms_species, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
  dplyr::mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), year = as.numeric(YEAR)) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
  sf::st_join(fig2_sf, join = sf::st_intersects) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(COMNAME.y), COMNAME.x == COMNAME.y) |>
  dplyr::group_by(species_factor = species_factor.x, year) |>
  dplyr::summarize(pos_range_tows = dplyr::n_distinct(paste(CRUISE6, STATION)), .groups = "drop")

# 5. Build full spine and calculate Within-Range Encounter Rate
encounter_in_range_data <- ms_species |>
  dplyr::select(species_factor) |>
  tidyr::expand_grid(year = unique(annual_tows_in_range$year)) |>
  dplyr::left_join(annual_tows_in_range, by = c("species_factor", "year")) |>
  dplyr::left_join(positive_tows_in_range, by = c("species_factor", "year")) |>
  dplyr::mutate(
    total_range_tows = tidyr::replace_na(total_range_tows, 0),
    pos_range_tows   = tidyr::replace_na(pos_range_tows, 0),
    encounter_rate_in_range = dplyr::if_else(total_range_tows > 0, (pos_range_tows / total_range_tows) * 100, NA_real_)
  ) |>
  dplyr::inner_join(fig3_data, by = c("species_factor", "year")) |>
  dplyr::filter(!is.na(perc_within_hist), !is.na(encounter_rate_in_range))


# --- FIGURE 6D: Scatterplot (Suitability vs Within-Range Encounter Rate) ---
cor_data_6d <- encounter_in_range_data |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    r = if (sum(complete.cases(perc_within_hist, encounter_rate_in_range)) > 2) cor(perc_within_hist, encounter_rate_in_range, use = "complete.obs") else NA_real_,
    x_pos = min(perc_within_hist, na.rm = TRUE),
    y_pos = max(encounter_rate_in_range, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(label = ifelse(!is.na(r), sprintf("r = %.2f", r), ""))

p_fig6d <- ggplot(encounter_in_range_data, aes(x = perc_within_hist, y = encounter_rate_in_range)) +
  geom_point(color = "black", alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", color = "#0072B2", linewidth = 0.8, se = FALSE) +
  geom_text(data = cor_data_6d, aes(x = x_pos, y = y_pos, label = label),
            hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "black") +
  facet_wrap(~ species_factor, scales = "free", ncol = 5) +
  labs(
    title = "Validation Option D: Suitability vs. Within-Historic-Range Encounter Rate",
    x = "Modeled Thermal Suitability (% of Habitat-Days)",
    y = "Encounter Rate Within Historic Range (%)"
  ) +
  theme_pub()

fig6d_file <- file.path(dir_out, "Fig6D_Suitability_vs_EncounterRate_WithinRange.png")
ggsave(fig6d_file, plot = p_fig6d, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Candidate Figure 6D to: ", fig6d_file)


# --- FIGURE 6E: Dual Time-Series Overlay (Within-Range Encounter Rate) ---
fig6e_data <- encounter_in_range_data |>
  dplyr::group_by(species_factor) |>
  dplyr::mutate(
    z_thermal   = scale(perc_within_hist)[,1],
    z_encounter = scale(encounter_rate_in_range)[,1]
  ) |>
  dplyr::ungroup() |>
  tidyr::pivot_longer(
    cols = c(z_thermal, z_encounter),
    names_to = "metric",
    values_to = "z_value"
  ) |>
  dplyr::mutate(
    metric_label = ifelse(metric == "z_thermal", "Modeled Thermal Suitability", "Within-Range Encounter Rate")
  )

p_fig6e <- ggplot(fig6e_data, aes(x = year, y = z_value, color = metric_label, linetype = metric_label)) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dotted") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ species_factor, ncol = 5) +
  scale_color_manual(values = c("Modeled Thermal Suitability" = "#0072B2", "Within-Range Encounter Rate" = "#009E73")) +
  scale_linetype_manual(values = c("Modeled Thermal Suitability" = "solid", "Within-Range Encounter Rate" = "solid")) +
  labs(
    title = "Validation Option E: Suitability & Within-Range Encounter Rate Time Series",
    x = "Year",
    y = "Standardized Anomaly (Z-Score)",
    color = "Metric",
    linetype = "Metric"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

fig6e_file <- file.path(dir_out, "Fig6E_Dual_TimeSeries_EncounterRate_WithinRange.png")
ggsave(fig6e_file, plot = p_fig6e, width = 14, height = 12, dpi = 300, bg = "white")
message("Saved Candidate Figure 6E to: ", fig6e_file)