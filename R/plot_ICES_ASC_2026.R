# R/plot_ICES_ASC_2026.R
#
# Purpose: Generate presentation-ready figures for ICES ASC 2026.
#          Targeted for a 16:9 aspect ratio slide deck.
#          Species Highlight: American Plaice (FALL).
# Outputs: Saved to images/ICES_ASC_2026/

library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(patchwork)

# -------------------------------------------------------------------
# 0. Setup & Presentation Aesthetics
# -------------------------------------------------------------------
dir_out <- here::here("images/ICES_ASC_2026")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Presentation-ready theme base 
theme_pres <- function() {
  theme_classic(base_size = 18) +
    theme(
      # Center titles over the plots (hjust = 0.5)
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.subtitle = element_text(size = 16, color = "grey30", hjust = 0.5),
      plot.caption = element_text(size = 14, color = "grey40", hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.text = element_text(color = "black")
    )
}

# -------------------------------------------------------------------
# 1. Load Data (Filtered for American Plaice)
# -------------------------------------------------------------------
message("Loading data for American Plaice...")
target_species <- "AMERICAN PLAICE"
target_season <- "FALL"

# Load survey data
survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/review_for_deletion/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/review_for_deletion/massInshoreData.rds")$survdat
survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
)) |>
  dplyr::filter(SEASON == target_season, as.numeric(YEAR) <= 2023)

# Filter for just American Plaice positive catches
plaice_svspp <- 74 # SVSPP for American Plaice
plaice_obs <- survdat |>
  dplyr::filter(SVSPP == plaice_svspp, ABUNDANCE > 0, !is.na(LAT), !is.na(LON)) |>
  dplyr::mutate(
    LAT = as.numeric(LAT), 
    LON = as.numeric(LON), 
    BOTTEMP = as.numeric(BOTTEMP)
  )

# Spatial data
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
strata <- sf::st_read("~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp", quiet = TRUE) |>
  sf::st_set_crs(4269) |> 
  sf::st_transform(4326) |>
  sf::st_make_valid()
Sys.unsetenv("SHAPE_RESTORE_SHX")

strata_bbox <- sf::st_bbox(strata)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  sf::st_transform(4326)

# Load V6 Historic Habitat for American Plaice Fall
habitat_v6 <- readRDS(here::here("data/historic_habitat_V6_seasonal/historic_habitat_V6_seasonal.rds"))
plaice_v6_poly <- habitat_v6[[paste0(target_species, "_", target_season)]]

# -------------------------------------------------------------------
# 2. GENERATE PLOT 1: Historic Envelope Map (Tight bounds w/ Inset)
# -------------------------------------------------------------------
message("Generating Map Plot with Inset...")

# Convert observations to sf points for plotting
plaice_sf <- plaice_obs |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

# Main Map
p_map <- ggplot() +
  geom_sf(data = land, fill = "grey70", color = NA) +
  geom_sf(data = strata, fill = NA, color = "grey80", linewidth = 0.2) +
  # V6 Historic Range footprint
  geom_sf(data = plaice_v6_poly, fill = "#0072B2", color = "#005a8f", alpha = 0.5, linewidth = 0.5) +
  # Actual survey observation points
  geom_sf(data = plaice_sf, color = "black", size = 0.8, alpha = 0.6, shape = 16) +
  coord_sf(
    xlim = c(strata_bbox["xmin"], strata_bbox["xmax"]), 
    ylim = c(strata_bbox["ymin"], strata_bbox["ymax"]), 
    expand = FALSE
  ) +
  labs(
    title = "American Plaice: Historic Range",
    subtitle = "Historic range (blue) derived from survey observations (dots)"
  ) +
  theme_pres() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 12)
  )

# Contextual Inset Map for International Audience
p_inset <- ggplot() +
  geom_sf(data = land, fill = "grey60", color = "grey80", linewidth = 0.1) +
  annotate("rect", xmin = strata_bbox["xmin"], xmax = strata_bbox["xmax"],
           ymin = strata_bbox["ymin"], ymax = strata_bbox["ymax"],
           fill = NA, color = "#D55E00", linewidth = 1.2) +
  coord_sf(xlim = c(-85, -50), ylim = c(28, 55), expand = FALSE) +
  theme_void() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "aliceblue")
  )

# Combine using patchwork
# Place inset in the bottom right corner (adjust coordinates if needed)
p_map_final <- p_map + patchwork::inset_element(p_inset, left = 0.55, bottom = 0.03, right = 0.98, top = 0.45)

map_file <- file.path(dir_out, "Slide_American_Plaice_Map.png")
# Tighter aspect ratio (7.5 x 8) removes the left/right whitespace padding
ggsave(map_file, plot = p_map_final, width = 7.5, height = 8, dpi = 300, bg = "white")
message("Saved Map to: ", map_file)

# -------------------------------------------------------------------
# 3. GENERATE PLOT 2: Thermal Niche Histogram (Landscape)
# -------------------------------------------------------------------
message("Generating Histogram Plot...")

# Calculate N and Percentiles
plaice_temp <- plaice_obs |> dplyr::filter(!is.na(BOTTEMP))
n_tows <- nrow(plaice_temp)
tmin <- quantile(plaice_temp$BOTTEMP, probs = 0.10, na.rm = TRUE)
tmax <- quantile(plaice_temp$BOTTEMP, probs = 0.90, na.rm = TRUE)

p_hist <- ggplot(plaice_temp, aes(x = BOTTEMP)) +
  geom_histogram(binwidth = 0.5, fill = "grey75", color = "grey30", alpha = 0.9) +
  # 10th and 90th percentile vertical lines
  geom_vline(xintercept = tmin, color = "#D55E00", linewidth = 1.5, linetype = "solid") +
  geom_vline(xintercept = tmax, color = "#D55E00", linewidth = 1.5, linetype = "solid") +
  # Add shaded region to visually highlight the niche
  annotate("rect", xmin = tmin, xmax = tmax, ymin = 0, ymax = Inf, 
           fill = "#D55E00", alpha = 0.15) +
  scale_x_continuous(breaks = seq(0, 25, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Thermal Niche Definition",
    subtitle = paste0("Total Positive Tows: ", format(n_tows, big.mark = ",")),
    x = "Bottom Temperature (°C)",
    y = "Frequency (Positive Tows)",
    caption = "* Vertical lines represent the 10th and 90th percentiles of observed temperatures."
  ) +
  theme_pres() +
  theme(
    plot.caption = element_text(face = "italic", margin = margin(t = 15))
  )

hist_file <- file.path(dir_out, "Slide_American_Plaice_Histogram.png")
# Histogram remains landscape to stretch out the X-axis
ggsave(hist_file, plot = p_hist, width = 10, height = 6, dpi = 300, bg = "white")
message("Saved Histogram to: ", hist_file)

# -------------------------------------------------------------------
# 4. GENERATE PLOT 3: Full Species Risk Score Heatmap
# -------------------------------------------------------------------
message("Generating Full Species Risk Score Heatmap...")

# We need to process the full multi-species dataset for the heatmap
all_years <- 1959:2023

# Load and calculate anomalies for ALL species
full_fig3_data <- readRDS(here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")) |>
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

# Calculate baseline stats (1982-2011)
full_baseline_stats <- full_fig3_data |>
  dplyr::filter(year >= 1982 & year <= 2011) |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    base_mean = mean(anomaly, na.rm = TRUE),
    base_sd = sd(anomaly, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate Risk Scores
full_fig4_scored <- full_fig3_data |>
  dplyr::inner_join(full_baseline_stats, by = "species_factor") |>
  dplyr::mutate(
    # INVERT the Z-score so that MORE suitable habitat = NEGATIVE risk score (relaxed management)
    z_score = -1 * ((anomaly - base_mean) / base_sd),
    annual_risk_score = as.integer(pmax(-4, pmin(4, round(z_score))))
  ) |>
  tidyr::complete(species_factor, year = all_years)

# Build Heatmap
p_heatmap <- ggplot(full_fig4_scored, aes(x = year, y = forcats::fct_rev(species_factor), fill = annual_risk_score)) +
  geom_tile(color = "white", linewidth = 0.2) +
  # Bounding box + translucent dark shading over 1982-2011 baseline window
  annotate("rect", xmin = 1981.5, xmax = 2011.5, ymin = -Inf, ymax = Inf, 
           fill = "black", alpha = 0.12, color = "black", linewidth = 1.1, linetype = "solid") +
  scale_fill_gradient2(
    low = "#3C78D8",    # Vibrant Cornflower Blue (Negative risk = more habitat)
    mid = "white",      # Crisp white midpoint creates better contrast than grey
    high = "#E69138",   # Vibrant Orange (Positive risk = less habitat)
    midpoint = 0,
    name = "Risk\nScore", 
    breaks = -4:4, 
    limits = c(-4, 4),
    na.value = "grey85",
    guide = guide_colorbar(
      barheight = unit(3.5, "in"), 
      barwidth = unit(0.4, "in"),
      ticks.colour = "black", 
      frame.colour = "black",
      nbin = 9 # Forces exactly 9 discrete color blocks in the legend to match integers -4 to 4
    )
  ) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 5), expand = c(0, 0)) +
  labs(
    title = "System-Wide Thermal Habitat Risk Scores (1959–2023)",
    x = "Year", y = NULL
  ) +
  theme_pres() +
  theme(
    # Shrink y-axis text slightly so all 40+ species fit cleanly
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right", 
    legend.title.align = 0.5
  )

heatmap_file <- file.path(dir_out, "Slide_Risk_Score_Heatmap.png")
# Exact 16:9 Landscape dimensions to perfectly fill the presentation slide
ggsave(heatmap_file, plot = p_heatmap, width = 16, height = 9, dpi = 300, bg = "white")
message("Saved Heatmap to: ", heatmap_file)

# -------------------------------------------------------------------
# 5. GENERATE PLOTS 4 & 5: System-Wide Validation (Scatter & Time-Series)
# -------------------------------------------------------------------
message("Generating System-Wide Validation Plots...")

# 1. Load ALL V6 Historic Habitat Polygons for the spatial join
fall_polys <- habitat_v6[grep("_FALL$", names(habitat_v6))]
fig2_sf <- dplyr::bind_rows(fall_polys) |>
  dplyr::inner_join(ms_species, by = "COMNAME")

# 2. Convert all survey tow events to sf points
all_tows_sf <- survdat |>
  dplyr::filter(!is.na(LAT), !is.na(LON)) |>
  dplyr::distinct(YEAR, CRUISE6, STATION, LAT, LON) |>
  dplyr::mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), year = as.numeric(YEAR)) |>
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

# 3. Spatial join: Find which survey tows fell within each species' V6 Fall polygon
tows_in_habitat <- sf::st_join(all_tows_sf, fig2_sf, join = sf::st_intersects) |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(COMNAME))

# 4. Calculate total survey tows executed WITHIN each species' V6 footprint per year
annual_tows_in_range <- tows_in_habitat |>
  dplyr::group_by(species_factor, year) |>
  dplyr::summarize(total_range_tows = dplyr::n_distinct(paste(CRUISE6, STATION)), .groups = "drop")

# 5. Calculate positive survey tows executed WITHIN each species' V6 footprint per year
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

# 6. Build full spine and calculate Within-Range Encounter Rate
# NOTE: Using full_fig3_data generated in the Heatmap step
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
  dplyr::inner_join(full_fig3_data, by = c("species_factor", "year")) |>
  dplyr::filter(!is.na(perc_within_hist), !is.na(encounter_rate_in_range))


# --- PLOT 4: Scatterplot (Suitability vs Within-Range Encounter Rate) ---
cor_data_val <- encounter_in_range_data |>
  dplyr::group_by(species_factor) |>
  dplyr::summarize(
    r = if (sum(complete.cases(perc_within_hist, encounter_rate_in_range)) > 2) cor(perc_within_hist, encounter_rate_in_range, use = "complete.obs") else NA_real_,
    x_pos = min(perc_within_hist, na.rm = TRUE),
    y_pos = max(encounter_rate_in_range, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(label = ifelse(!is.na(r), sprintf("r = %.2f", r), ""))

p_val_scatter <- ggplot(encounter_in_range_data, aes(x = perc_within_hist, y = encounter_rate_in_range)) +
  geom_point(color = "black", alpha = 0.5, size = 1.0) +
  geom_smooth(method = "lm", color = "#0072B2", linewidth = 1, se = FALSE) +
  geom_text(data = cor_data_val, aes(x = x_pos, y = y_pos, label = label),
            hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "black") +
  facet_wrap(~ species_factor, scales = "free", ncol = 7) + # ncol=7 fits 16:9 screen perfectly
  labs(
    title = "Validation: Within-Historic-Range Encounter Rate vs. Suitability",
    x = "Modeled Thermal Suitability (% of Habitat-Days)",
    y = "Encounter Rate Within Historic Range (%)"
  ) +
  theme_pres() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9)
  )

val_scatter_file <- file.path(dir_out, "Slide_Validation_Scatter.png")
ggsave(val_scatter_file, plot = p_val_scatter, width = 16, height = 9, dpi = 300, bg = "white")
message("Saved Validation Scatterplot to: ", val_scatter_file)


# --- PLOT 5: Dual Time-Series Overlay (Within-Range Encounter Rate) ---
fig_val_ts_data <- encounter_in_range_data |>
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

p_val_ts <- ggplot(fig_val_ts_data, aes(x = year, y = z_value, color = metric_label, linetype = metric_label)) +
  geom_hline(yintercept = 0, color = "grey70", linetype = "dotted") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ species_factor, ncol = 7) +
  scale_color_manual(values = c("Modeled Thermal Suitability" = "#0072B2", "Within-Range Encounter Rate" = "#D55E00")) +
  scale_linetype_manual(values = c("Modeled Thermal Suitability" = "solid", "Within-Range Encounter Rate" = "solid")) +
  labs(
    title = "Validation: Suitability & Within-Range Encounter Rate Time Series",
    x = "Year",
    y = "Standardized Anomaly (Z-Score)",
    color = NULL,
    linetype = NULL
  ) +
  theme_pres() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1)
  )

val_ts_file <- file.path(dir_out, "Slide_Validation_TimeSeries.png")
ggsave(val_ts_file, plot = p_val_ts, width = 16, height = 9, dpi = 300, bg = "white")
message("Saved Validation Time-Series to: ", val_ts_file)