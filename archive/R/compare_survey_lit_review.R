# archive/R/compare_survey_lit_review.R
#
# Purpose: Compare thermal niches generated from survey data (middle 80th percentile)
#          to those from the literature review. Filtered to only evaluate Adult
#          literature values. Generates histograms of raw observed bottom 
#          temperatures to visually evaluate the impact of using empirical 
#          percentiles vs. the spread of available literature values.

library(tidyverse)
library(here)

# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_plots <- here::here("images/thermal_niche_explorations")
if (!dir.exists(dir_plots)) dir.create(dir_plots, recursive = TRUE)

# -------------------------------------------------------------------
# 2. Load and prep thermal niche data
# -------------------------------------------------------------------

# Call in data from survey
survey <- readRDS(here::here('data-raw', 'survey_thermal_niche.rds')) |> 
  dplyr::rename(common.name = COMNAME) |> 
  dplyr::mutate(
    tmin = as.numeric(tmin),
    tmax = as.numeric(tmax)
  )

# Call in data from lit review
lit <- read_csv(here::here('data', 'MS_screening_analysis_cleaned.csv'), show_col_types = FALSE) |> 
  dplyr::mutate(
    common.name = stringr::str_to_upper(common.name),
    # Ensure all temperature columns are numeric
    dplyr::across(
      c(opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max),
      ~ as.numeric(.x)
    )
  )

# Filter for Adult literature values
lit_adults <- lit |> 
  dplyr::filter(age.group == "Adult")


# -------------------------------------------------------------------
# 3. Create Comparison Table (Survey vs. Adult Literature)
# -------------------------------------------------------------------

comparison_tbl <- survey |> 
  dplyr::inner_join(
    lit_adults |> 
      dplyr::select(
        common.name, age.group, effect.type,
        opt.temp.min, opt.temp.mean, opt.temp.max,
        stress.temp.min, stress.temp.max,
        lethal.temp.min, lethal.temp.max
      ),
    by = "common.name"
  ) |> 
  dplyr::mutate(
    # Optimal temperature differences
    diff_opt_min_from_survey_min  = opt.temp.min  - tmin,
    diff_opt_max_from_survey_max  = opt.temp.max  - tmax,
    
    # Stress temperature differences
    diff_stress_min_from_survey_min = stress.temp.min - tmin,
    diff_stress_max_from_survey_max = stress.temp.max - tmax,
    
    # Lethal temperature differences
    diff_lethal_min_from_survey_min = lethal.temp.min - tmin,
    diff_lethal_max_from_survey_max = lethal.temp.max - tmax  
  ) |> 
  dplyr::select(
    common.name, age.group, effect.type,
    tmin, tmax,
    opt.temp.min, opt.temp.mean, opt.temp.max,
    stress.temp.min, stress.temp.max,
    lethal.temp.min, lethal.temp.max,
    starts_with("diff_")
  )

# Save comparison table as CSV for GitHub viewing
write_csv(
  comparison_tbl,
  here::here("data-raw", "survey_vs_lit_adult_thermal_niche_comparison.csv")
)


# -------------------------------------------------------------------
# 4. Load Raw Survey Data for Visualization
# -------------------------------------------------------------------

message("Loading raw survey data for temperature histograms...")

survdat_raw <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")$survdat
inshore_raw <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")$survdat

survdat <- dplyr::full_join(survdat_raw, inshore_raw, by = dplyr::join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

# Get species mapping to link SVSPP to COMNAME
species_list <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds") |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed)) |>
  dplyr::filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  dplyr::distinct(SVSPP, .keep_all = TRUE) |>
  dplyr::select(SVSPP, COMNAME)

# Filter raw data for positive catches with valid bottom temperatures
survdat_mgmt <- survdat |>
  dplyr::inner_join(species_list, by = "SVSPP") |>
  dplyr::filter(ABUNDANCE > 0, !is.na(BOTTEMP))


# -------------------------------------------------------------------
# 5. Generate Histograms
# -------------------------------------------------------------------

message("Generating histograms for ", length(unique(comparison_tbl$common.name)), " species...")

purrr::walk(unique(comparison_tbl$common.name), function(sp) {
  
  # Filter raw data for the specific species
  df_sp <- survdat_mgmt |> 
    dplyr::filter(COMNAME == sp) |> 
    dplyr::mutate(BOTTEMP = as.numeric(BOTTEMP))
  
  if(nrow(df_sp) == 0) return(NULL)
  
  # Extract ALL comparison limits for this species (could be multiple rows)
  sp_limits <- comparison_tbl |> dplyr::filter(common.name == sp)
  
  # Survey limits are identical across rows, so we only need to plot them once
  survey_bounds <- sp_limits |> dplyr::slice(1)
  
  p <- ggplot(df_sp, aes(x = BOTTEMP)) +
    geom_histogram(binwidth = 0.5, fill = "grey75", color = "grey30", alpha = 0.8) +
    
    # 10-90th Percentile (Survey Empirical) - Plotted once
    geom_vline(data = survey_bounds, aes(xintercept = tmin, color = "Survey 10-90th"), linewidth = 1.2, linetype = "solid") +
    geom_vline(data = survey_bounds, aes(xintercept = tmax, color = "Survey 10-90th"), linewidth = 1.2, linetype = "solid") +
    
    # Literature Optimal Limits - Maps all available estimates for the species
    geom_vline(data = sp_limits, aes(xintercept = opt.temp.min, color = "Lit Opt Low"), linewidth = 1, linetype = "dashed", na.rm = TRUE) +
    geom_vline(data = sp_limits, aes(xintercept = opt.temp.max, color = "Lit Opt High"), linewidth = 1, linetype = "dashed", na.rm = TRUE) +
    
    # Literature Lethal Limits - Maps all available estimates for the species
    geom_vline(data = sp_limits, aes(xintercept = lethal.temp.min, color = "Lit Lethal Low"), linewidth = 1, linetype = "dotted", na.rm = TRUE) +
    geom_vline(data = sp_limits, aes(xintercept = lethal.temp.max, color = "Lit Lethal High"), linewidth = 1, linetype = "dotted", na.rm = TRUE) +
    
    scale_color_manual(
      name = "Niche Boundaries",
      values = c(
        "Survey 10-90th"  = "#0072B2",  # Dark Blue
        "Lit Opt Low"     = "#56B4E9",  # Sky Blue
        "Lit Opt High"    = "#E69F00",  # Orange
        "Lit Lethal Low"  = "#CC79A7",  # Pink
        "Lit Lethal High" = "#D55E00"   # Vermillion
      )
    ) +
    
    labs(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Observed Bottom Temperatures"),
      subtitle = paste0("Total positive tows: ", nrow(df_sp), "\nLiterature estimates plotted: ", nrow(sp_limits)),
      x = "Bottom Temperature (°C)",
      y = "Frequency (Number of Tows)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  # Save the plot
  safe_name <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_plots, paste0(safe_name, "_thermal_histogram.png"))
  ggsave(file_name, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
})

message("Histograms saved to: ", dir_plots)


# -------------------------------------------------------------------
# 6. Create Final Thermal Niche Table for the Pipeline
# -------------------------------------------------------------------

# Exclusively use survey 10-90th percentiles for all species
final_thermal_niche <- survey |> 
  dplyr::mutate(source = "Survey") |> 
  dplyr::select(
    COMNAME = common.name,
    tmin,
    tmax,
    source
  )

# Save as CSV so it can be easily viewed on GitHub
write_csv(
  final_thermal_niche,
  here::here("data-raw", "final_thermal_niche_values.csv")
)

# Also save as RDS to ensure 01_get_perc_suitable_thermal_habitat_seasonally.R 
# can load it exactly as it expects
saveRDS(
  final_thermal_niche,
  here::here("data-raw", "thermal_niche.rds")
)

# -------------------------------------------------------------------
# 7. Prepare Spatial Data for Habitat Comparison Maps
# -------------------------------------------------------------------

message("Loading spatial layers for habitat comparison maps...")

library(sf)
library(rnaturalearth)
library(patchwork)

dir_comp_maps <- here::here("images/thermal_niche_explorations/habitat_comparisons")
if (!dir.exists(dir_comp_maps)) dir.create(dir_comp_maps, recursive = TRUE)

# Land polygons
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)

# Survey Strata (NOAA ArcGIS Hub)
arcgis_url <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/Bottom_Trawl_Survey/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
strata <- sf::st_read(arcgis_url, quiet = TRUE) |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  dplyr::mutate(strata_uid = dplyr::row_number())

# Calculate the 0-100th percentile (Full Range) for each species
full_range_niche <- survdat_mgmt |>
  dplyr::group_by(COMNAME) |>
  dplyr::summarize(
    tmin_full = min(as.numeric(BOTTEMP), na.rm = TRUE),
    tmax_full = max(as.numeric(BOTTEMP), na.rm = TRUE),
    .groups = "drop"
  )

# Combine 10-90th limits and Full Range limits into one table
niche_comparisons <- final_thermal_niche |>
  dplyr::inner_join(full_range_niche, by = "COMNAME")


# -------------------------------------------------------------------
# 8. Function: Build Thermal Habitat Footprint
# -------------------------------------------------------------------
# Filters survey points by a specific thermal range, then applies the 
# V6 logic (>= 3 observations in a single year) to define the footprint.

build_thermal_footprint <- function(sp_name, season_name, temp_min, temp_max) {
  
  pts <- survdat_mgmt |>
    dplyr::filter(
      COMNAME == sp_name,
      SEASON == season_name,
      as.numeric(BOTTEMP) >= temp_min,
      as.numeric(BOTTEMP) <= temp_max,
      !is.na(LAT), !is.na(LON), !is.na(YEAR)
    ) |>
    dplyr::mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), YEAR = as.numeric(YEAR)) |>
    dplyr::distinct(YEAR, CRUISE6, STATION, LAT, LON)
  
  if (nrow(pts) == 0) return(NULL)
  
  pts_sf <- sf::st_as_sf(pts, coords = c("LON", "LAT"), crs = 4326)
  
  # Spatially join to strata and count unique stations per year
  valid_strata_uids <- sf::st_join(pts_sf, strata) |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(strata_uid)) |>
    dplyr::group_by(strata_uid, YEAR) |>
    dplyr::summarise(n_stations = dplyr::n_distinct(paste(CRUISE6, STATION)), .groups = "drop") |>
    dplyr::filter(n_stations >= 3) |>
    dplyr::pull(strata_uid) |>
    unique()
  
  valid_strata <- strata |> dplyr::filter(strata_uid %in% valid_strata_uids)
  
  if (nrow(valid_strata) == 0) return(NULL)
  
  # Dissolve and subtract land
  habitat_sf <- sf::st_union(valid_strata) |> sf::st_make_valid()
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  marine_habitat <- suppressWarnings(sf::st_difference(habitat_sf, land_union)) |> 
    sf::st_make_valid()
  
  if (length(marine_habitat) == 0 || all(sf::st_is_empty(marine_habitat))) return(NULL)
  
  return(sf::st_as_sf(marine_habitat) |> dplyr::rename(geometry = x))
}

# Helper to plot a single panel
plot_habitat_panel <- function(poly, title_text, strata_bg, land_bg, fill_color) {
  if (is.null(poly)) {
    return(ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = paste("No valid habitat\n", title_text)))
  }
  
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(c(bbox["xmin"], bbox["xmax"])) * 0.15)
  ypad <- max(2, diff(c(bbox["ymin"], bbox["ymax"])) * 0.15)
  
  ggplot() +
    geom_sf(data = strata_bg, fill = NA, color = "grey80", linewidth = 0.2) +
    geom_sf(data = poly, fill = fill_color, color = colorspace::darken(fill_color, 0.3), alpha = 0.6, linewidth = 0.4) +
    geom_sf(data = land_bg, fill = "grey50", color = NA) +
    coord_sf(
      xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad), 
      ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad), 
      expand = FALSE
    ) +
    labs(title = title_text) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.text = element_blank()
    )
}


# -------------------------------------------------------------------
# 9. Generate 4-Panel Comparison Maps
# -------------------------------------------------------------------

message("Generating 4-panel habitat comparisons...")

purrr::walk(unique(niche_comparisons$COMNAME), function(sp) {
  
  lims <- niche_comparisons |> dplyr::filter(COMNAME == sp) |> dplyr::slice(1)
  
  # Build the 4 polygons
  poly_spring_1090 <- build_thermal_footprint(sp, "SPRING", lims$tmin, lims$tmax)
  poly_fall_1090   <- build_thermal_footprint(sp, "FALL", lims$tmin, lims$tmax)
  
  poly_spring_full <- build_thermal_footprint(sp, "SPRING", lims$tmin_full, lims$tmax_full)
  poly_fall_full   <- build_thermal_footprint(sp, "FALL", lims$tmin_full, lims$tmax_full)
  
  # Create the 4 plots
  p1 <- plot_habitat_panel(poly_spring_1090, paste("Spring (10-90th)\n", round(lims$tmin,1), "-", round(lims$tmax,1), "°C"), strata, land, "#0072B2")
  p2 <- plot_habitat_panel(poly_spring_full, paste("Spring (Full Range)\n", round(lims$tmin_full,1), "-", round(lims$tmax_full,1), "°C"), strata, land, "#56B4E9")
  p3 <- plot_habitat_panel(poly_fall_1090, paste("Fall (10-90th)\n", round(lims$tmin,1), "-", round(lims$tmax,1), "°C"), strata, land, "#D55E00")
  p4 <- plot_habitat_panel(poly_fall_full, paste("Fall (Full Range)\n", round(lims$tmin_full,1), "-", round(lims$tmax_full,1), "°C"), strata, land, "#E69F00")
  
  # Stitch together with patchwork
  comp_plot <- (p1 | p2) / (p3 | p4) + 
    plot_annotation(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Habitat Envelope Sensitivity"),
      subtitle = "Comparing footprints derived from the 10-90th thermal percentile vs. the full 0-100th observed range.",
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    )
  
  safe_name <- stringr::str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  file_name <- file.path(dir_comp_maps, paste0(safe_name, "_habitat_comparison.png"))
  
  ggsave(file_name, plot = comp_plot, width = 10, height = 10, dpi = 300, bg = "white")
})

message("All habitat comparison maps saved to: ", dir_comp_maps)
