# get_historic_habitat_V6_seasonal.R
#
# Purpose: Define and visualize seasonal historic habitat for NEFMC-managed species.
#          Habitat is defined as the dissolved union of all NEFSC bottom trawl 
#          survey strata in which the species has been observed 3 or more times 
#          IN A SINGLE YEAR, processed separately for SPRING and FALL.
#
# Output:
#   RDS  : data/historic_habitat_V6_seasonal/historic_habitat_V6_seasonal.rds
#   Maps : images/historic_habitat_V6_seasonal/<species>_<season>.png
#
# Dependencies: tidyverse, sf, terra, rnaturalearth, ggnewscale, marmap, here

# -------------------------------------------------------------------
# 0. Packages
# -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggnewscale)  
library(marmap)      
library(here)


# -------------------------------------------------------------------
# 1. Output directories
# -------------------------------------------------------------------

dir_data   <- here::here("data/historic_habitat_V6_seasonal")
dir_images <- here::here("images/historic_habitat_V6_seasonal")

if (!dir.exists(dir_data))   dir.create(dir_data,   recursive = TRUE)
if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)


# -------------------------------------------------------------------
# 2. Load survey data
# -------------------------------------------------------------------

survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")
survdat <- survdat$survdat

inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")
inshore <- inshore$survdat

survdat <- dplyr::full_join(survdat, inshore, by = join_by(
  CRUISE6, STATION, STRATUM, TOW, YEAR, SEASON, LAT, LON, DEPTH, 
  SURFTEMP, BOTTEMP, SVSPP, CATCHSEX, ABUNDANCE, BIOMASS
))

species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

# Windowpane is managed by NEFMC — correct the Fed.Managed field
species <- species |>
  dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))

ne_species <- species |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC") |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME, SCINAME, Fed.Managed)

survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP") |>
  # Standardize season names just in case
  mutate(SEASON = toupper(SEASON)) |>
  filter(SEASON %in% c("SPRING", "FALL"))

# Land polygons used in all maps
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_transform(4326)


# -------------------------------------------------------------------
# 3. Habitat parameters
# -------------------------------------------------------------------

habitat_params <- list(
  # Minimum number of unique observation events (stations) required 
  # within a single stratum IN A SINGLE YEAR for that stratum to be 
  # permanently added to the overall historic habitat envelope.
  min_observations = 3
)


# -------------------------------------------------------------------
# 4. Survey strata (Reproducible NOAA ArcGIS Hub Source)
# -------------------------------------------------------------------

message("Downloading NEFSC survey strata from NOAA ArcGIS Hub...")

# NOAA Bottom Trawl Survey Strata GeoJSON API Endpoint
arcgis_url <- "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/arcgis/rest/services/Bottom_Trawl_Survey/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

strata <- sf::st_read(arcgis_url, quiet = TRUE) |>
  sf::st_transform(4326) |>
  sf::st_make_valid() |>
  # Add a unique ID for grouping during spatial joins
  mutate(strata_uid = row_number())


# -------------------------------------------------------------------
# 5. Bathymetry (visualization only)
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
# 6. Core habitat polygon function
# -------------------------------------------------------------------

get_seasonal_habitat <- function(species_name,
                                 season_name,
                                 survdat_mgmt,
                                 strata,
                                 params = habitat_params) {
  
  message("Building habitat polygon: ", species_name, " (", season_name, ")")
  
  # --- a. Presence records ---
  pts_filtered <- survdat_mgmt |>
    filter(
      COMNAME   == species_name,
      SEASON    == season_name,
      ABUNDANCE >  0,
      !is.na(LAT),
      !is.na(LON),
      !is.na(YEAR)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON), YEAR = as.numeric(YEAR)) |>
    distinct(YEAR, SEASON, CRUISE6, STATION, LAT, LON)
  
  if (nrow(pts_filtered) == 0) {
    message("  No presence records — skipping.")
    return(NULL)
  }
  
  # Convert to spatial points
  pts_sf <- pts_filtered |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
  
  # --- b. Count observations per stratum PER YEAR ---
  # Spatially join points to strata to get the strata_uid for each point
  pts_with_strata <- sf::st_join(pts_sf, strata)
  
  strata_year_counts <- pts_with_strata |>
    sf::st_drop_geometry() |>
    filter(!is.na(strata_uid)) |>
    group_by(strata_uid, YEAR) |>
    # Count unique stations per stratum per year
    summarise(n_stations = n_distinct(paste(CRUISE6, STATION)), .groups = "drop")
  
  # --- c. Filter strata ---
  # Identify any stratum that hit the threshold in ANY given year
  qualifying_strata_uids <- strata_year_counts |>
    filter(n_stations >= params$min_observations) |>
    pull(strata_uid) |>
    unique()
  
  valid_strata <- strata |> 
    filter(strata_uid %in% qualifying_strata_uids)
  
  if (nrow(valid_strata) == 0) {
    message("  No strata met the minimum annual observation threshold (", params$min_observations, "/year) — skipping.")
    return(NULL)
  }
  
  message("  Included ", nrow(valid_strata), " strata based on annual threshold.")
  
  # --- d. Dissolve valid strata ---
  habitat_sf <- valid_strata |>
    sf::st_union() |>
    sf::st_make_valid()
  
  if (is.null(habitat_sf) || all(sf::st_is_empty(habitat_sf))) {
    message("  Empty polygon after dissolve — skipping.")
    return(NULL)
  }
  
  # --- e. Subtract land ---
  land_union <- sf::st_union(land) |> sf::st_make_valid()
  
  habitat_marine <- suppressWarnings(
    sf::st_difference(habitat_sf, land_union)
  ) |> sf::st_make_valid()
  
  if (is.null(habitat_marine) || length(habitat_marine) == 0 || all(sf::st_is_empty(habitat_marine))) {
    message("  Empty polygon after land subtraction — skipping.")
    return(NULL)
  }
  
  # --- f. Metadata ---
  habitat_marine <- sf::st_as_sf(habitat_marine) |> 
    rename(geometry = x) |>
    mutate(
      COMNAME          = species_name,
      SEASON           = season_name,
      n_stations_total = nrow(pts_filtered),
      n_strata         = nrow(valid_strata),
      min_obs_per_year = params$min_observations
    )
  
  return(habitat_marine)
}


# -------------------------------------------------------------------
# 7. Build habitat polygons for all NEFMC species (Seasonal)
# -------------------------------------------------------------------

# Create a grid of every species and both seasons
run_grid <- expand_grid(
  species = unique(ne_species$COMNAME),
  season  = c("SPRING", "FALL")
)

historic_habitat_seasonal <- map2(
  run_grid$species, 
  run_grid$season,
  ~get_seasonal_habitat(
    species_name = .x,
    season_name  = .y,
    survdat_mgmt = survdat_mgmt,
    strata       = strata,
    params       = habitat_params
  )
) |>
  # Name the list elements combining Species and Season (e.g., "ATLANTIC COD_SPRING")
  setNames(paste0(run_grid$species, "_", run_grid$season))

# Remove any empty combinations
historic_habitat_seasonal <- Filter(Negate(is.null), historic_habitat_seasonal)

message(length(historic_habitat_seasonal), " seasonal habitat polygons built.")


# -------------------------------------------------------------------
# 8. Save RDS
# -------------------------------------------------------------------

out_rds <- file.path(dir_data, "historic_habitat_V6_seasonal.rds")
saveRDS(historic_habitat_seasonal, out_rds)

message("RDS saved to: ", out_rds)


# -------------------------------------------------------------------
# 9. Visualization function
# -------------------------------------------------------------------

map_historic_habitat_v6_seasonal <- function(list_key,
                                             historic_habitat_list,
                                             survdat_mgmt,
                                             strata,
                                             bathy,
                                             params  = habitat_params,
                                             out_dir = dir_images) {
  
  poly <- historic_habitat_list[[list_key]]
  if (is.null(poly)) return(invisible(NULL))
  
  species_name <- poly$COMNAME[1]
  season_name  <- poly$SEASON[1]
  
  all_pts <- survdat_mgmt |>
    filter(
      COMNAME == species_name, 
      SEASON  == season_name, 
      ABUNDANCE > 0, 
      !is.na(LAT), 
      !is.na(LON)
    ) |>
    mutate(LAT = as.numeric(LAT), LON = as.numeric(LON)) |>
    distinct(CRUISE6, STATION, LAT, LON, DEPTH)
  
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(c(bbox["xmin"], bbox["xmax"])) * 0.15)
  ypad <- max(2, diff(c(bbox["ymin"], bbox["ymax"])) * 0.15)
  xlim <- c(bbox["xmin"] - xpad, bbox["xmax"] + xpad)
  ylim <- c(bbox["ymin"] - ypad, bbox["ymax"] + ypad)
  
  bathy_crop <- terra::crop(bathy, terra::ext(xlim[1], xlim[2], ylim[1], ylim[2]))
  bathy_df   <- as.data.frame(bathy_crop, xy = TRUE)
  colnames(bathy_df)[3] <- "depth"
  bathy_df <- bathy_df |>
    filter(depth < 0) |>
    mutate(depth_capped = pmax(depth, -2000))
  
  p <- ggplot() +
    
    geom_tile(
      data = bathy_df,
      aes(x = x, y = y, fill = depth_capped)
    ) +
    scale_fill_gradientn(
      colors   = c("grey15", "grey45", "grey75", "grey92"),
      values   = scales::rescale(c(-2000, -500, -200, 0)),
      limits   = c(-2000, 0),
      name     = "Depth (m)",
      na.value = "white",
      labels   = function(x) ifelse(x == -2000, "\u2264 -2000", as.character(x))
    ) +
    
    ggnewscale::new_scale_fill() +
    
    # Habitat polygon (Dissolved Strata)
    geom_sf(
      data      = poly,
      fill      = "steelblue",
      color     = "steelblue4",
      alpha     = 0.45,
      linewidth = 0.6
    ) +
    
    # Unselected survey strata boundaries for context
    geom_sf(
      data      = strata,
      fill      = NA,
      color     = "orange",
      linetype  = "solid",
      linewidth = 0.2,
      alpha     = 0.5
    ) +
    
    # All seasonal presence points
    geom_point(
      data  = all_pts,
      aes(x = LON, y = LAT, color = DEPTH),
      size  = 1.0,
      alpha = 0.8
    ) +
    scale_color_viridis_c(
      name      = "Obs depth (m)",
      option    = "plasma",
      direction = -1,
      na.value  = "grey50"
    ) +
    
    geom_sf(data = land, fill = "grey35", color = NA) +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    
    labs(
      title = paste0(
        tools::toTitleCase(tolower(species_name)),
        " \u2014 Historic Habitat Envelope (V6 - ", tools::toTitleCase(tolower(season_name)), ")"
      ),
      subtitle = paste0(
        "Total Stations: ", poly$n_stations_total,
        "  |  Strata Included: ", poly$n_strata,
        "  |  Threshold: \u2265 ", params$min_observations, " obs/stratum/year"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Blue: Union of NEFSC survey strata containing \u2265 ", params$min_observations, " unique presence stations in ANY single year. ",
        "Orange outlines: Full NEFSC bottom trawl survey grid. Bathymetry gradient capped at -2000 m."
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
    paste0(gsub(" ", "_", species_name), "_", season_name, "_historic_habitat_V6.png")
  )
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 300)
  
  message("  Saved: ", file_name)
  invisible(p)
}


# -------------------------------------------------------------------
# 10. Generate maps for all species/seasons
# -------------------------------------------------------------------

walk(
  names(historic_habitat_seasonal),
  ~map_historic_habitat_v6_seasonal(
    list_key              = .x,
    historic_habitat_list = historic_habitat_seasonal,
    survdat_mgmt          = survdat_mgmt,
    strata                = strata,
    bathy                 = bathy,
    params                = habitat_params
  )
)

# -------------------------------------------------------------------
# 11. Summary table
# -------------------------------------------------------------------

habitat_summary <- map_dfr(
  names(historic_habitat_seasonal),
  ~{
    poly <- historic_habitat_seasonal[[.x]]
    tibble(
      species          = poly$COMNAME[1],
      season           = poly$SEASON[1],
      n_stations_total = poly$n_stations_total[1],
      n_strata         = poly$n_strata[1],
      min_obs_per_year = poly$min_obs_per_year[1],
      area_km2         = as.numeric(
        sf::st_area(sf::st_transform(poly, 5070))
      ) / 1e6
    )
  }
)

print(habitat_summary, n = Inf)

# -------------------------------------------------------------------
# 12. Tabular Comparison: Spring vs. Fall
# -------------------------------------------------------------------

message("Generating tabular comparison...")

habitat_comparison <- habitat_summary |>
  select(species, season, n_stations_total, n_strata, area_km2) |>
  pivot_wider(
    names_from  = season,
    values_from = c(n_stations_total, n_strata, area_km2),
    names_glue  = "{.value}_{season}"
  ) |>
  # Handle cases where a species might be missing in one season entirely
  mutate(across(everything(), ~replace_na(.x, 0))) |>
  mutate(
    area_diff_km2 = area_km2_SPRING - area_km2_FALL,
    larger_season = case_when(
      area_diff_km2 > 0 ~ "SPRING",
      area_diff_km2 < 0 ~ "FALL",
      TRUE ~ "EQUAL"
    )
  )

out_csv <- file.path(dir_data, "habitat_comparison_spring_vs_fall.csv")
write_csv(habitat_comparison, out_csv)
message("Tabular comparison saved to: ", out_csv)


# -------------------------------------------------------------------
# 13. Map Comparison: Side-by-Side Plots
# -------------------------------------------------------------------

library(patchwork)

dir_comp_images <- file.path(dir_images, "comparisons")
if (!dir.exists(dir_comp_images)) dir.create(dir_comp_images, recursive = TRUE)

message("Generating side-by-side comparison maps...")

# Helper function to generate a simplified map for the patchwork grid
build_comp_map <- function(poly, season_name, strata_bg, land_bg) {
  if (is.null(poly)) {
    # Return an empty plot indicating no habitat met the threshold
    return(
      ggplot() + 
        theme_void() + 
        annotate("text", x = 0, y = 0, label = paste("No qualifying", season_name, "habitat"))
    )
  }
  
  bbox <- sf::st_bbox(poly)
  xpad <- max(2, diff(c(bbox["xmin"], bbox["xmax"])) * 0.15)
  ypad <- max(2, diff(c(bbox["ymin"], bbox["ymax"])) * 0.15)
  
  ggplot() +
    geom_sf(data = strata_bg, fill = NA, color = "grey80", linewidth = 0.2) +
    geom_sf(data = poly, fill = "steelblue", color = "steelblue4", alpha = 0.6, linewidth = 0.5) +
    geom_sf(data = land_bg, fill = "grey50", color = NA) +
    coord_sf(
      xlim = c(bbox["xmin"] - xpad, bbox["xmax"] + xpad), 
      ylim = c(bbox["ymin"] - ypad, bbox["ymax"] + ypad), 
      expand = FALSE
    ) +
    labs(
      title = tools::toTitleCase(tolower(season_name)),
      subtitle = paste0("Area: ", scales::comma(round(as.numeric(sf::st_area(sf::st_transform(poly, 5070))) / 1e6)), " km\u00b2")
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

# Iterate through species to build and save the stitched plots
walk(unique(habitat_summary$species), function(sp) {
  
  poly_spring <- historic_habitat_seasonal[[paste0(sp, "_SPRING")]]
  poly_fall   <- historic_habitat_seasonal[[paste0(sp, "_FALL")]]
  
  # Skip if species lacks habitat in both seasons
  if (is.null(poly_spring) && is.null(poly_fall)) return(NULL)
  
  p_spring <- build_comp_map(poly_spring, "SPRING", strata, land)
  p_fall   <- build_comp_map(poly_fall, "FALL", strata, land)
  
  # Stitch together using patchwork
  comp_plot <- p_spring + p_fall + 
    plot_annotation(
      title = paste0(tools::toTitleCase(tolower(sp)), " - Seasonal Habitat Comparison (V6)"),
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )
  
  file_name <- file.path(dir_comp_images, paste0(gsub(" ", "_", sp), "_seasonal_comparison.png"))
  
  ggsave(file_name, plot = comp_plot, width = 12, height = 6, dpi = 300, bg = "white")
  message("  Saved comparison: ", sp)
})

message("All side-by-side maps saved to: ", dir_comp_images)