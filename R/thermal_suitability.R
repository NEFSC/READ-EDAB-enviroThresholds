# Max Grezlik
# 09/19/2025
# 
# In our meeting on 09/12/2025, Megan Ware asked for a one-size-fits-all indicator
# that is also somewhat species-specific. She suggested using temperature and
# comparing it to thermal tolerance of species.
#
# Here I will generate indicators of:
# 1. percent of days within the thermal niche by species
# 2. thermal stress index by species (degree days) to capture both frequency and duration

# Thermal niche is 10th to 90th percentile of bottom temperatures where species was observed in survey data
# Supplemented by literature review data when available (not implemented yet)

# call in packages -------------------
library(tidyverse)
library(terra)
library(ecodata)


# get species strata --------------------
## focused on species managed by NEFMC

## Calling in data -----------------------

## survey data
survdat <- readRDS("~/EDAB_Datasets/Workflows/surveyNoLengthsData.rds")
survdat <- survdat$survdat

## Mass inshore survey
inshore <- readRDS("~/EDAB_Datasets/Workflows/massInshoreData.rds")
inshore <- inshore$survdat

## add inshore to survdat
survdat <- dplyr::full_join(survdat, inshore)

## species names
species <- readRDS("~/EDAB_Datasets/Workflows/SOE_species_list_24.rds")

# windowpane is managed by NEFMC
# fix that in species

species <- species |>
            dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE","NEFMC",Fed.Managed))


### filter for NEFMC managed species
ne_species <- species  |>
  filter(!is.na(Fed.Managed), Fed.Managed == "NEFMC")  |>
  distinct(SVSPP, .keep_all = TRUE) |>
  select(SVSPP, COMNAME, SCINAME, Fed.Managed)

### join survdat and species by SVSPP
survdat_mgmt <- survdat |>
  inner_join(ne_species, by = "SVSPP")

## bottom temp data
 nc_path <- "~/EDAB_Datasets/GLORYS/GLORYS_daily"
 nc_files <- list.files(nc_path, pattern = "GLORYS_daily_BottomTemp_\\d{4}\\.nc$", full.names = TRUE)

 first_file <- terra::rast(nc_files[1])
 first_file
# 
# # loop over years
#  for (f in nc_files) {
#    message("Processing file: ", f)
# 
#    bt <- terra::rast(f)   # daily bottom temps for one year
# 
#    bt_mean <- terra::mean(bt)
# 
#    # Save raster
#    out_name <- gsub(".nc", "_mean.tif", basename(f))
# 
#    # make sure output folder exists
#    if (!dir.exists("inputs")) dir.create("inputs")
# 
#    terra::writeRaster(bt_mean, file.path("inputs", out_name), overwrite = TRUE)
#  }



# thermal niche from survey ---------------
# thermal_niche <- survdat_mgmt  |> 
#   filter(!is.na(BOTTEMP), ABUNDANCE > 0)  |> 
#   group_by(SVSPP, COMNAME, SCINAME)  |> 
#   summarise(
#     tmin = quantile(BOTTEMP, 0.10, na.rm = TRUE),
#     tmax = quantile(BOTTEMP, 0.90, na.rm = TRUE),
#     n_obs = n(),
#     .groups = "drop"
#   )
 
# Thermal niche from survey was compared to lit review values
# some survey values were replaced where appropriate
 
thermal_niche <- readRDS(here::here("data-raw/thermal_niche.rds"))


## spot check for cod
# Example: histogram of bottom temps for Atlantic cod
# survdat_mgmt |> 
#   filter(COMNAME == "ATLANTIC COD", !is.na(BOTTEMP), ABUNDANCE > 0)  |> 
#   ggplot(aes(x = BOTTEMP)) +
#   geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
#   geom_vline(data = thermal_niche  |>  filter(COMNAME == "ATLANTIC COD"),
#              aes(xintercept = tmin), color = "red", linetype = "dashed") +
#   geom_vline(data = thermal_niche  |>  filter(COMNAME == "ATLANTIC COD"),
#              aes(xintercept = tmax), color = "red", linetype = "dashed") +
#   labs(title = "Thermal niche of Atlantic cod (10–90th percentile)",
#        x = "Bottom Temperature (°C)", y = "Frequency")

# link thermal niche with GLORYS data ---------------

## function to generate indicators
species_indicator <- function(species_name, thermal_niche, bt, survdat_mgmt, year = NULL) {
  # Filter thermal niche
  th <- thermal_niche |> filter(COMNAME == species_name)
  if (nrow(th) == 0) return(NULL)
  
  tmin <- th$tmin
  tmax <- th$tmax
  
  # Historical stations (all-time)
  hist_pts <- survdat_mgmt |> 
    filter(COMNAME == species_name) |> 
    distinct(STATION, LAT, LON) |> 
    mutate(LON = as.numeric(LON), LAT = as.numeric(LAT))
  
  # Year-specific stations
  if (!is.null(year)) {
    year_pts <- survdat_mgmt |> 
      filter(COMNAME == species_name, YEAR == year) |> 
      distinct(STATION, LAT, LON) |> 
      mutate(LON = as.numeric(LON), LAT = as.numeric(LAT))
  } else {
    year_pts <- hist_pts
  }
  
  # Helper function to calculate stress index and perc_within
  calc_metrics <- function(pts) {
    if (nrow(pts) == 0) return(tibble(
      perc_within = NA_real_,
      stress_index = NA_real_
    ))
    
    pts_df <- as.data.frame(pts)
    pts_vect <- vect(pts_df, geom = c("LON", "LAT"), crs = "EPSG:4326")
    if (!identical(crs(pts_vect), crs(bt))) {
      pts_vect <- project(pts_vect, bt)
    }
    
    vals <- terra::extract(bt, pts_vect)[, -1, drop = FALSE]
    if (all(is.na(vals))) return(tibble(
      perc_within = NA_real_,
      stress_index = NA_real_
    ))
    
    val_vec <- as.numeric(as.matrix(vals))
    
    tibble(
      perc_within = mean(val_vec >= tmin & val_vec <= tmax, na.rm = TRUE),
      stress_index = sum(pmax(val_vec - tmax, 0, na.rm = TRUE))
    )
  }
  
  # Calculate metrics
  hist_metrics <- calc_metrics(hist_pts)
  year_metrics <- calc_metrics(year_pts)
  
  tibble(
    species = species_name,
    perc_within_hist = hist_metrics$perc_within,
    stress_index_hist = hist_metrics$stress_index,
    perc_within_year = year_metrics$perc_within,
    stress_index_year = year_metrics$stress_index
  )
}

results <- list()

for (f in nc_files) {
  year <- as.numeric(stringr::str_extract(f, "\\d{4}"))
  bt <- terra::rast(f)
  
  out <- map_dfr(
    unique(ne_species$COMNAME),
    ~species_indicator(.x, thermal_niche, bt, survdat_mgmt, year = year)
  )
  
  out$year <- year
  results[[year]] <- out
  
  # Free memory
  rm(bt, out)
  gc()
  
  saveRDS(results[[year]], file.path("thresholds", paste0("indicators_", year, ".rds")))
}

indicators <- bind_rows(results)



# indicators |>
#   filter(species == "ATLANTIC COD") |>
#   ggplot(aes(x = as.numeric(year), y = perc_within_year)) +
#   geom_line() +
#   labs(title = "Percent of days within thermal niche", x = "Year", y = "Percent")
# 
# indicators |>
#   filter(species == "ATLANTIC COD") |>
#   ggplot(aes(x = as.numeric(year), y = perc_within_hist)) +
#   geom_line() +
#   labs(title = "Percent of days within thermal niche", x = "Year", y = "Percent")
# 
# 
# indicators |>
#   filter(species == "ATLANTIC COD") |>
#   ggplot(aes(x = as.numeric(year), y = stress_index_year)) +
#   geom_line() +
#   labs(title = "Stress Index", x = "Year", y = "Degree Days")
# 
# indicators |>
#   filter(species == "ATLANTIC COD") |>
#   ggplot(aes(x = as.numeric(year), y = stress_index_hist)) +
#   geom_line() +
#   labs(title = "Stress Index", x = "Year", y = "Degree Days")




# loop through plots of all species and indicators

# Ensure folder exists
if (!dir.exists("images")) dir.create("images")

# List of indicators to plot
indicator_vars <- c("perc_within_hist", "perc_within_year", "stress_index_hist", "stress_index_year")

# Loop through species and indicators
for (sp in unique(indicators$species)) {
  for (ind in indicator_vars) {
    p <- indicators |> 
      filter(species == sp) |> 
      ggplot(aes(x = as.numeric(year), y = .data[[ind]])) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      labs(
        title = paste(sp, "-", ind),
        x = "Year",
        y = ind
      ) +
      theme_minimal(base_size = 12)
    
    # Define file name
    file_name <- paste0("images/", gsub(" ", "_", sp), "_", ind, ".png")
    
    # Save plot
    ggsave(file_name, plot = p, width = 6, height = 4, dpi = 300)
  }
}

# Mapping suitable thermal habitat by species and year --------

if (!dir.exists("images/suitable_habitat")) {
  dir.create("images/suitable_habitat", recursive = TRUE)
}

library(rnaturalearth)


## mapping function ---------
map_suitable_habitat <- function(species_name,
                                 thermal_niche,
                                 nc_file,
                                 survdat_mgmt,
                                 out_dir = "images/suitable_habitat") {
  
  year <- as.numeric(stringr::str_extract(nc_file, "\\d{4}"))
  message("Mapping ", species_name, " - ", year)
  
  # Thermal limits
  th <- thermal_niche |> 
    dplyr::filter(COMNAME == species_name)
  
  if (nrow(th) == 0) {
    message("No thermal niche found for ", species_name)
    return(NULL)
  }
  
  tmin <- th$tmin
  tmax <- th$tmax
  
  # Load GLORYS
  bt <- terra::rast(nc_file)
  bt_mean <- terra::mean(bt)
  
  # Suitable habitat mask
  suitable <- bt_mean >= tmin & bt_mean <= tmax
  suitable <- terra::ifel(suitable, 1, NA)
  
  suitable_df <- as.data.frame(suitable, xy = TRUE, na.rm = FALSE)
  colnames(suitable_df)[3] <- "suitable"
  
  # Get raster extent
  r_ext <- terra::ext(bt_mean)
  xmin <- r_ext[1]
  xmax <- r_ext[2]
  ymin <- r_ext[3]
  ymax <- r_ext[4]
  
  # Land polygons
  land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  land <- sf::st_transform(land, terra::crs(bt_mean))
  
  # -----------------------------
  # Survey points for this species & year
  # -----------------------------
  year_pts <- survdat_mgmt |>
    dplyr::filter(
      COMNAME == species_name,
      YEAR == year,
      ABUNDANCE > 0,
      !is.na(LAT),
      !is.na(LON)
    ) |>
    dplyr::distinct(STATION, LAT, LON) |>
    dplyr::mutate(
      LAT = as.numeric(LAT),
      LON = as.numeric(LON)
    )
  
  # -----------------------------
  # Plot
  # -----------------------------
  
  p <- ggplot() +
    
    # Suitable raster
    geom_raster(
      data = suitable_df,
      aes(x = x, y = y, fill = suitable)
    ) +
    
    scale_fill_gradient(
      low = "white",
      high = "blue",
      na.value = "white",
      name = "Suitable"
    ) +
    
    # Land
    geom_sf(
      data = land,
      fill = "darkgreen",
      color = NA
    ) +
    
    # Observed stations
    geom_point(
      data = year_pts,
      aes(x = LON, y = LAT),
      color = "red",
      size = 1.5,
      alpha = 0.8
    ) +
    
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      expand = FALSE
    ) +
    
    labs(
      title = paste0(species_name, " Suitable Thermal Habitat"),
      subtitle = paste0("Annual Mean Bottom Temp - ", year,
                        " | Red dots = observed presence"),
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal()
  
  # Save
  file_name <- file.path(
    out_dir,
    paste0(gsub(" ", "_", species_name), "_", year, "_suitable.png")
  )
  
  ggsave(file_name, plot = p, width = 7, height = 6, dpi = 300)
  
  rm(bt, bt_mean, suitable)
  gc()
}



## call mapping function --------
### Cod as a test

# # Example: first GLORYS file
# test_file <- nc_files[1]
# 
# map_suitable_habitat(
#   species_name = "ATLANTIC COD",
#   thermal_niche = thermal_niche,
#   nc_file = test_file,
#   survdat_mgmt = survdat_mgmt
# )

# Got it working for cod
# Now looping over species and year

for (f in nc_files) {
  for (sp in unique(ne_species$COMNAME)) {
    map_suitable_habitat(
      species_name = sp,
      thermal_niche = thermal_niche,
      nc_file = f,
      survdat_mgmt = survdat_mgmt
    )
  }
}

# Mapping species habitats ----------

# ## mapping function ---------
# 
# library(sf)
# library(concaveman)
# 
# species_habitat_polygon <- function(species_name, survdat_mgmt){
#   
#   pts <- survdat_mgmt |>
#     filter(COMNAME == species_name,
#            !is.na(LAT),
#            !is.na(LON)) |>
#     distinct(STATION, LAT, LON)
#   
#   if(nrow(pts) < 3) return(NULL)
#   
#   pts_sf <- st_as_sf(
#     pts,
#     coords = c("LON","LAT"),
#     crs = 4326
#   )
#   
#   # concave hull around all points
#   habitat_poly <- pts_sf |>
#     concaveman()
#   
#   habitat_poly
# }
# 
# ## edited script which excludes land
# library(sf)
# library(concaveman)
# library(rnaturalearth)
# 
# species_habitat_polygon <- function(species_name, survdat_mgmt){
#   
#   pts <- survdat_mgmt |>
#     dplyr::filter(COMNAME == species_name,
#                   !is.na(LAT),
#                   !is.na(LON)) |>
#     dplyr::distinct(STATION, LAT, LON)
#   
#   if(nrow(pts) < 3) return(NULL)
#   
#   pts_sf <- sf::st_as_sf(
#     pts,
#     coords = c("LON","LAT"),
#     crs = 4326
#   )
#   
#   # Create concave hull
#   habitat_poly <- pts_sf |>
#     concaveman(concavity = 2)
#   
#   # Get land polygons
#   land <- rnaturalearth::ne_countries(
#     scale = "medium",
#     returnclass = "sf"
#   ) |>
#     sf::st_transform(sf::st_crs(habitat_poly))
#   
#   # Remove land from habitat polygon
#   habitat_poly <- sf::st_difference(
#     habitat_poly,
#     sf::st_union(land)
#   )
#   
#   habitat_poly
# }

## version robust to edges crossing
library(sf)
library(concaveman)
library(rnaturalearth)

species_habitat_polygon <- function(species_name, survdat_mgmt){
  
  pts <- survdat_mgmt |>
    dplyr::filter(COMNAME == species_name,
                  !is.na(LAT),
                  !is.na(LON)) |>
    dplyr::distinct(STATION, LAT, LON)
  
  if(nrow(pts) < 3) return(NULL)
  
  pts_sf <- sf::st_as_sf(
    pts,
    coords = c("LON","LAT"),
    crs = 4326
  )
  
  # concave hull
  habitat_poly <- pts_sf |>
    concaveman()
  
  # fix invalid geometry
  habitat_poly <- sf::st_make_valid(habitat_poly)
  
  # land polygons
  land <- rnaturalearth::ne_countries(
    scale = "medium",
    returnclass = "sf"
  ) |>
    sf::st_transform(sf::st_crs(habitat_poly))
  
  # remove land from polygon
  habitat_poly <- sf::st_difference(
    habitat_poly,
    sf::st_union(land)
  )
  
  habitat_poly
}

## call by species --------

### Cod ------
cod_habitat <- species_habitat_polygon(
  "ATLANTIC COD",
  survdat_mgmt
)

pts <- survdat_mgmt |>
  filter(COMNAME == "ATLANTIC COD")

ggplot() +
  geom_sf(data = cod_habitat, fill = "lightblue", alpha = 0.4) +
  geom_point(data = pts, aes(x = LON, y = LAT), size = 0.6) +
  theme_minimal() +
  labs(title = "Atlantic Cod Observed Habitat Envelope")

### Scallop ---------
scallop_habitat <- species_habitat_polygon(
  "SEA SCALLOP",
  survdat_mgmt
)

pts <- survdat_mgmt |>
  filter(COMNAME == "SEA SCALLOP")

ggplot() +
  geom_sf(data = scallop_habitat, fill = "lightblue", alpha = 0.4) +
  geom_point(data = pts, aes(x = LON, y = LAT), size = 0.6) +
  theme_minimal() +
  labs(title = "Scallop Observed Habitat Envelope")


# Comparing with Rob's data ----------

library(terra)

# Call in Rob's cod data as a raster
cod_raster_rob <- rast(here::here('DisMAP/Figures/Gadus morhua_Total.tif'))


# Convert my cod polygon to a raster
library(sf)

hab_vect <- vect(cod_habitat)

cod_raster_max <- rasterize(
  hab_vect,
  cod_raster_rob,
  field = 1,
  background = 0
)

# compare overlap

overlap <- global((cod_raster_max == 1) & (cod_raster_rob > 0), "sum", na.rm = TRUE)
hab_area <- global(cod_raster_max == 1, "sum", na.rm = TRUE)

percent_overlap <- overlap / hab_area
