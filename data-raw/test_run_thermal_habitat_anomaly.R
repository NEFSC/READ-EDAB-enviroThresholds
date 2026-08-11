# data-raw/test_thermal_habitat_anomaly.R
# 
# Purpose: Test the automated workflow for the thermal habitat anomaly indicator 
#          and verify the resulting data structure visually. Compares the 
#          heatmap and spaghetti visualization styles.

library(tidyverse)
library(here)
library(patchwork)
library(ecodata)

# -------------------------------------------------------------------
# 1. Setup Paths and Run Workflow
# -------------------------------------------------------------------

dir.create(here::here("data-raw/temp"), showWarnings = FALSE)
dir_out <- here::here("images/SOE_mockups")
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

indicator_path <- here::here("data/indicators/perc_suitable_thermal_habitat_seasonally.rds")
species_path   <- "~/EDAB_Datasets/Workflows/SOE_species_list_24.rds"
outputPath     <- here::here("data-raw/temp")

# Source the wrapper
source(here::here("data-raw/workflow_thermal_habitat_anomaly.R"))
source(here::here("R/thermal_habitat_anomaly.R"))

message("Running thermal habitat anomaly workflow...")
new_thermal_data <- workflow_thermal_habitat_anomaly(
  indicator_path = indicator_path,
  species_path = species_path,
  outputPath = outputPath
)

# Verify the data structure (Should be: Time, Var, Value, EPU, Units)
message("Data generated successfully. Preview:")
print(head(new_thermal_data))


# -------------------------------------------------------------------
# 2. Test Visualizations (Mocking the ecodata plot scripts)
# -------------------------------------------------------------------
message("Generating test plots (Heatmap vs. Spaghetti)...")

# Define test variables
recent_years <- c(2015, 2024)

# ===================================================================
# MID-ATLANTIC PLOTS
# ===================================================================
setup_ma <- ecodata::plot_setup(shadedRegion = recent_years, report = "MidAtlantic")
dat_ma <- new_thermal_data |> dplyr::filter(EPU == "MA")

# --- MA Heatmap ---
sort_ma <- dat_ma |> 
  dplyr::group_by(Var) |> 
  dplyr::summarize(mean_anom = mean(Value, na.rm = TRUE), .groups = "drop") |> 
  dplyr::arrange(mean_anom) |> 
  dplyr::pull(Var)
dat_ma$Var <- factor(dat_ma$Var, levels = sort_ma)

p_ma_heatmap <- dat_ma |>
  ggplot(aes(x = Time, y = forcats::fct_rev(Var), fill = Value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  annotate("rect", fill = setup_ma$shade.fill, alpha = setup_ma$shade.alpha,
           xmin = setup_ma$x.shade.min, xmax = setup_ma$x.shade.max,
           ymin = -Inf, ymax = Inf) +
  scale_fill_viridis_c(name = "Fall Habitat\nAnomaly (%)", na.value = "grey90") +
  scale_x_continuous(breaks = round(seq(min(dat_ma$Time, na.rm=T), max(dat_ma$Time, na.rm=T), by = 5)), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Fall Thermal Habitat Anomaly (Heatmap) \u2014 MidAtlantic Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title()

# --- MA Spaghetti (With Outliers) ---
dat_ma_mean <- dat_ma |>
  dplyr::group_by(Time) |>
  dplyr::summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop")

dat_ma_diff <- dat_ma |>
  dplyr::left_join(dat_ma_mean, by = "Time") |>
  dplyr::group_by(Var) |>
  dplyr::summarize(mean_abs_diff = mean(abs(Value - Mean_Value), na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mean_abs_diff))

top_outliers_ma <- head(dat_ma_diff$Var, 4)
dat_ma_bg <- dat_ma |> dplyr::filter(!Var %in% top_outliers_ma)
dat_ma_outliers <- dat_ma |> dplyr::filter(Var %in% top_outliers_ma)

p_ma_spag <- ggplot() +
  annotate("rect", fill = setup_ma$shade.fill, alpha = setup_ma$shade.alpha,
           xmin = setup_ma$x.shade.min, xmax = setup_ma$x.shade.max,
           ymin = -Inf, ymax = Inf) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(data = dat_ma_bg, aes(x = Time, y = Value, group = Var),
            color = "grey60", alpha = 0.3, linewidth = 0.5) +
  geom_line(data = dat_ma_outliers, aes(x = Time, y = Value, color = Var),
            alpha = 0.8, linewidth = 0.8) +
  geom_line(data = dat_ma_mean, aes(x = Time, y = Mean_Value),
            color = "black", linewidth = 1.2) +
  scale_x_continuous(breaks = round(seq(min(dat_ma$Time, na.rm=T), max(dat_ma$Time, na.rm=T), by = 5)), expand = c(0.01, 0.01)) +
  scale_color_viridis_d(name = "Highest Deviations:", option = "viridis", end = 0.9) +
  labs(y = "Fall Habitat Anomaly (%)", x = NULL) +
  theme_bw() +
  ggtitle("Fall Thermal Habitat Anomaly (Spaghetti) \u2014 MidAtlantic Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))


# ===================================================================
# NEW ENGLAND PLOTS
# ===================================================================
setup_ne <- ecodata::plot_setup(shadedRegion = recent_years, report = "NewEngland")
dat_ne <- new_thermal_data |> dplyr::filter(EPU == "NE")

# --- NE Heatmap ---
sort_ne <- dat_ne |> 
  dplyr::group_by(Var) |> 
  dplyr::summarize(mean_anom = mean(Value, na.rm = TRUE), .groups = "drop") |> 
  dplyr::arrange(mean_anom) |> 
  dplyr::pull(Var)
dat_ne$Var <- factor(dat_ne$Var, levels = sort_ne)

p_ne_heatmap <- dat_ne |>
  ggplot(aes(x = Time, y = forcats::fct_rev(Var), fill = Value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  annotate("rect", fill = setup_ne$shade.fill, alpha = setup_ne$shade.alpha,
           xmin = setup_ne$x.shade.min, xmax = setup_ne$x.shade.max,
           ymin = -Inf, ymax = Inf) +
  scale_fill_viridis_c(name = "Fall Habitat\nAnomaly (%)", na.value = "grey90") +
  scale_x_continuous(breaks = round(seq(min(dat_ne$Time, na.rm=T), max(dat_ne$Time, na.rm=T), by = 5)), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Fall Thermal Habitat Anomaly (Heatmap) \u2014 NewEngland Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title()

# --- NE Spaghetti (With Outliers) ---
dat_ne_mean <- dat_ne |>
  dplyr::group_by(Time) |>
  dplyr::summarize(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop")

dat_ne_diff <- dat_ne |>
  dplyr::left_join(dat_ne_mean, by = "Time") |>
  dplyr::group_by(Var) |>
  dplyr::summarize(mean_abs_diff = mean(abs(Value - Mean_Value), na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(mean_abs_diff))

top_outliers_ne <- head(dat_ne_diff$Var, 4)
dat_ne_bg <- dat_ne |> dplyr::filter(!Var %in% top_outliers_ne)
dat_ne_outliers <- dat_ne |> dplyr::filter(Var %in% top_outliers_ne)

p_ne_spag <- ggplot() +
  annotate("rect", fill = setup_ne$shade.fill, alpha = setup_ne$shade.alpha,
           xmin = setup_ne$x.shade.min, xmax = setup_ne$x.shade.max,
           ymin = -Inf, ymax = Inf) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_line(data = dat_ne_bg, aes(x = Time, y = Value, group = Var),
            color = "grey60", alpha = 0.3, linewidth = 0.5) +
  geom_line(data = dat_ne_outliers, aes(x = Time, y = Value, color = Var),
            alpha = 0.8, linewidth = 0.8) +
  geom_line(data = dat_ne_mean, aes(x = Time, y = Mean_Value),
            color = "black", linewidth = 1.2) +
  scale_x_continuous(breaks = round(seq(min(dat_ne$Time, na.rm=T), max(dat_ne$Time, na.rm=T), by = 5)), expand = c(0.01, 0.01)) +
  scale_color_viridis_d(name = "Highest Deviations:", option = "viridis", end = 0.9) +
  labs(y = "Fall Habitat Anomaly (%)", x = NULL) +
  theme_bw() +
  ggtitle("Fall Thermal Habitat Anomaly (Spaghetti) \u2014 NewEngland Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title() +
  theme(legend.position = "bottom", 
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8))


# -------------------------------------------------------------------
# 3. View and Save Results
# -------------------------------------------------------------------

# Display the plots
print(p_ma_heatmap)
print(p_ma_spag)
print(p_ne_heatmap)
print(p_ne_spag)

# Save the plots
ggsave(file.path(dir_out, "SOE_Mockup_MA_Heatmap.png"), plot = p_ma_heatmap, width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(dir_out, "SOE_Mockup_MA_Spaghetti.png"), plot = p_ma_spag, width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(dir_out, "SOE_Mockup_NE_Heatmap.png"), plot = p_ne_heatmap, width = 10, height = 7, dpi = 300, bg = "white")
ggsave(file.path(dir_out, "SOE_Mockup_NE_Spaghetti.png"), plot = p_ne_spag, width = 8, height = 5, dpi = 300, bg = "white")

message("\nTest complete! Plots saved to images/SOE_mockups/ for review.")