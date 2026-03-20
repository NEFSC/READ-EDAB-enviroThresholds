## MTG 03/16/2026
##
## Comparing time series of percent suitable thermal habitat
## I had been working on this as a possible indicator for the Risk Policy framework
## Scott shared that he has been working on a very similar indicator using different methods
## Comparing the time series here to help decide whether or not to adopt Scott's methods

library(tidyverse)

# Call in data ------------

## Max's indicators ------------

rds_dir <- here::here('data/thresholds')

max_data <- list.files(rds_dir, pattern = "^indicators_\\d{4}\\.rds$", full.names = TRUE) |>
  map(readRDS) |>
  bind_rows() |>
  # Rename to a common column name for merging
  rename(percent_suitable_max = perc_within_hist) |>
  mutate(
    species              = str_to_upper(species),
    percent_suitable_max = percent_suitable_max * 100
  ) |> 
  dplyr::select(species, percent_suitable_max, year)


## Scott's indicators ------------

scott_indicator <- read_csv("~/EDAB_Dev/grezlik/NE_Risk_Policy/neus_thermal_habitat_area.csv")

# Convert snake_case species names to ALL CAPS to match
scott_data <- scott_indicator |>
  mutate(species = str_replace_all(species, "_", " ") |> str_to_upper()) |>
  # Combine all regions (and seasons) by summing areas, then recalculate percent
  group_by(species, year, season) |>
  summarise(
    total_area_km2    = sum(total_area_km2,    na.rm = TRUE),
    thermal_habitat_km2 = sum(thermal_habitat_km2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(percent_suitable_scott = thermal_habitat_km2 / total_area_km2 * 100) |> 
  dplyr::select(species, percent_suitable_scott, season,year)

# combine data sets -------------

combined <- full_join(max_data, scott_data, by = c("species", "year"))


# Plot -------------- 

max_plot <- max_data |>
  dplyr::select(species, year, percent_suitable_max) |>
  drop_na(percent_suitable_max)

scott_plot <- combined |>
  dplyr::select(species, year, season, percent_suitable_scott) |>
  drop_na(percent_suitable_scott)

species_list <- sort(unique(combined$species))

plot_list <- map(species_list, function(sp) {
  
  max_sp   <- max_plot   |> filter(species == sp)
  scott_sp <- scott_plot |> filter(species == sp)
  
  ggplot() +
    # Scott's lines (one per season, colored)
    geom_line(
      data = scott_sp,
      aes(x = year, y = percent_suitable_scott, color = season, linetype = season),
      linewidth = 0.8
    ) +
    geom_point(
      data = scott_sp,
      aes(x = year, y = percent_suitable_scott, color = season),
      size = 1.5
    ) +
    # Max's indicator (solid black)
    geom_line(
      data = max_sp,
      aes(x = year, y = percent_suitable_max),
      color = "black", linewidth = 1
    ) +
    geom_point(
      data = max_sp,
      aes(x = year, y = percent_suitable_max),
      color = "black", size = 1.8
    ) +
    scale_y_continuous(labels = scales::label_percent(scale = 1),
                       limits = c(0, 100)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title    = sp,
      x        = "Year",
      y        = "% Suitable Thermal Habitat",
      color    = "Scott (by season)",
      linetype = "Scott (by season)",
      caption  = "Black line = Max's indicator\nColored lines = Scott's indicator, combined across regions"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold"),
      legend.position = "bottom"
    )
})

names(plot_list) <- species_list

# Save plots -----------------------

img_dir <- here::here('images')

# Multi-page PDF
pdf(file.path(img_dir, "thermal_habitat_comparison.pdf"), width = 8, height = 5)
walk(plot_list, print)
dev.off()

message("Saved: ", file.path(img_dir, "thermal_habitat_comparison.pdf"))

# Individual PNGs per species
iwalk(plot_list, function(p, sp) {
  safe_name <- str_replace_all(sp, "[^A-Za-z0-9]+", "_")
  ggsave(
    filename = file.path(img_dir, paste0(safe_name, ".png")),
    plot     = p,
    width    = 8, height = 5, dpi = 150
  )
})

message("Saved individual PNGs to ", img_dir)