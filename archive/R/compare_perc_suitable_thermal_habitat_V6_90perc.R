## MTG 03/16/2026
##
## Comparing time series of percent suitable thermal habitat
## Updated to include:
##   - V1 (legacy KDE) 
##   - V6 (Survey Strata - 3 obs absolute cutoff)
##   - V6_90perc (Survey Strata - dynamic 90% cumulative observation cutoff)
##   - Scott's regional indicators

library(tidyverse)
library(here)

# Call in data ------------

## Max's V1 indicators (Legacy KDE) ------------

rds_dir_v1 <- here::here('data/thresholds')

max_v1_data <- list.files(rds_dir_v1, pattern = "^indicators_\\d{4}\\.rds$", full.names = TRUE) |>
  map(readRDS) |>
  bind_rows() |>
  rename(percent_suitable_v1 = perc_within_hist) |>
  mutate(
    species             = str_to_upper(species),
    percent_suitable_v1 = percent_suitable_v1 * 100 # Scaling to 0-100
  ) |> 
  dplyr::select(species, year, percent_suitable_v1)


## Max's V6 indicators (Current Survey Strata - Absolute cutoff) ------------

rds_file_v6 <- here::here('data/indicators/perc_suitable_thermal_habitat.rds')

max_v6_data <- readRDS(rds_file_v6) |>
  rename(percent_suitable_v6 = perc_within_hist) |>
  mutate(species = str_to_upper(species)) |>
  dplyr::select(species, year, percent_suitable_v6)


## Max's V6_90perc indicators (Dynamic 90% cutoff) ------------

rds_file_v6_90 <- here::here('data/indicators/perc_suitable_thermal_habitat_V6_90perc.rds')

max_v6_90_data <- readRDS(rds_file_v6_90) |>
  rename(percent_suitable_v6_90 = perc_within_hist) |>
  mutate(species = str_to_upper(species)) |>
  dplyr::select(species, year, percent_suitable_v6_90)


## Scott's indicators ------------

scott_indicator <- read_csv("~/EDAB_Dev/grezlik/NE_Risk_Policy/neus_thermal_habitat_area.csv")

# Convert snake_case species names to ALL CAPS to match
scott_data <- scott_indicator |>
  mutate(species = str_replace_all(species, "_", " ") |> str_to_upper()) |>
  # Combine all regions (and seasons) by summing areas, then recalculate percent
  group_by(species, year, season) |>
  summarise(
    total_area_km2      = sum(total_area_km2,      na.rm = TRUE),
    thermal_habitat_km2 = sum(thermal_habitat_km2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(percent_suitable_scott = thermal_habitat_km2 / total_area_km2 * 100) |> 
  dplyr::select(species, year, season, percent_suitable_scott)


# Combine data sets -------------

# Join all four datasets together
combined <- full_join(max_v1_data, max_v6_data, by = c("species", "year")) |>
  full_join(max_v6_90_data, by = c("species", "year")) |>
  full_join(scott_data, by = c("species", "year"))


# Plot -------------- 

# Separate into individual plot frames and drop NAs
v1_plot <- combined |>
  dplyr::select(species, year, percent_suitable_v1) |>
  drop_na(percent_suitable_v1) |>
  distinct()

v6_plot <- combined |>
  dplyr::select(species, year, percent_suitable_v6) |>
  drop_na(percent_suitable_v6) |>
  distinct()

v6_90_plot <- combined |>
  dplyr::select(species, year, percent_suitable_v6_90) |>
  drop_na(percent_suitable_v6_90) |>
  distinct()

scott_plot <- combined |>
  dplyr::select(species, year, season, percent_suitable_scott) |>
  drop_na(percent_suitable_scott)

species_list <- sort(unique(combined$species))

plot_list <- map(species_list, function(sp) {
  
  v1_sp    <- v1_plot    |> filter(species == sp)
  v6_sp    <- v6_plot    |> filter(species == sp)
  v6_90_sp <- v6_90_plot |> filter(species == sp)
  scott_sp <- scott_plot |> filter(species == sp)
  
  ggplot() +
    # Scott's lines (one per season, colored)
    geom_line(
      data = scott_sp,
      aes(x = year, y = percent_suitable_scott, color = season, linetype = season),
      linewidth = 0.8, alpha = 0.6
    ) +
    geom_point(
      data = scott_sp,
      aes(x = year, y = percent_suitable_scott, color = season),
      size = 1.5, alpha = 0.6
    ) +
    
    # Max's V1 indicator (Legacy - dashed grey)
    geom_line(
      data = v1_sp,
      aes(x = year, y = percent_suitable_v1),
      color = "grey50", linetype = "dashed", linewidth = 0.8
    ) +
    
    # Max's V6 indicator (Absolute 3 obs - solid black)
    geom_line(
      data = v6_sp,
      aes(x = year, y = percent_suitable_v6),
      color = "black", linewidth = 1.0
    ) +
    
    # Max's V6_90perc indicator (Dynamic 90% obs target - solid seagreen)
    geom_line(
      data = v6_90_sp,
      aes(x = year, y = percent_suitable_v6_90),
      color = "seagreen", linewidth = 1.2
    ) +
    geom_point(
      data = v6_90_sp,
      aes(x = year, y = percent_suitable_v6_90),
      color = "seagreen", size = 2
    ) +
    
    scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = sp,
      x        = "Year",
      y        = "% Suitable Thermal Habitat",
      color    = "Scott (by season)",
      linetype = "Scott (by season)",
      caption  = "Solid Green: Max V6_90perc (Dynamic 90% Target)\nSolid Black: Max V6 (3 Obs Absolute)\nDashed Grey: Max V1 (Legacy KDE)\nColored: Scott's indicator (Combined Regions)"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "bottom"
    )
})

names(plot_list) <- species_list

# Save plots -----------------------

img_dir <- here::here('images/comparisons/perc_thermal_habitat_V1_V6_V690_Scott')
if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)

# Safely open PDF device
message("Building PDF...")
graphics.off() # Clears any hanging background devices to prevent corruption

pdf(file.path(img_dir, "thermal_habitat_comparison.pdf"), width = 8, height = 5)
walk(plot_list, print)
dev.off()

message("Saved: ", file.path(img_dir, "thermal_habitat_comparison.pdf"))

# Individual PNGs per species
iwalk(plot_list, function(p, sp) {
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  ggsave(
    filename = file.path(img_dir, paste0(safe_name, "_comparison.png")),
    plot     = p,
    width    = 8, height = 5, dpi = 300
  )
})

message("Saved individual PNGs to ", img_dir)