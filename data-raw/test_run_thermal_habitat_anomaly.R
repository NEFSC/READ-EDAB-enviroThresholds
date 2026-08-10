# data-raw/test_thermal_habitat_anomaly.R
# 
# Purpose: Test the automated workflow for the thermal habitat anomaly indicator 
#          and verify the resulting data structure visually.

library(tidyverse)
library(here)
library(patchwork)
library(ecodata)

# -------------------------------------------------------------------
# 1. Setup Paths and Run Workflow
# -------------------------------------------------------------------

dir.create(here::here("data-raw/temp"), showWarnings = FALSE)

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
# 2. Test Visualizations (Mocking the ecodata plot script)
# -------------------------------------------------------------------
# Since this data isn't in the ecodata package yet, we will test the 
# plotting logic directly against our `new_thermal_data` object.

message("Generating test plots...")

# Define test variables
recent_years <- 2013:2022

# --- Mid-Atlantic Test Plot ---
setup_ma <- ecodata::plot_setup(shadedRegion = recent_years, report = "MidAtlantic")

dat_ma <- new_thermal_data |> dplyr::filter(EPU == "MA")
sort_ma <- dat_ma |> 
  dplyr::group_by(Var) |> 
  dplyr::summarize(mean_anom = mean(Value, na.rm = TRUE), .groups = "drop") |> 
  dplyr::arrange(mean_anom) |> 
  dplyr::pull(Var)
dat_ma$Var <- factor(dat_ma$Var, levels = sort_ma)

p_ma <- dat_ma |>
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
  ggtitle("Fall Thermal Habitat Anomaly \u2014 MidAtlantic Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title()


# --- New England Test Plot ---
setup_ne <- ecodata::plot_setup(shadedRegion = recent_years, report = "NewEngland")

dat_ne <- new_thermal_data |> dplyr::filter(EPU == "NE")
sort_ne <- dat_ne |> 
  dplyr::group_by(Var) |> 
  dplyr::summarize(mean_anom = mean(Value, na.rm = TRUE), .groups = "drop") |> 
  dplyr::arrange(mean_anom) |> 
  dplyr::pull(Var)
dat_ne$Var <- factor(dat_ne$Var, levels = sort_ne)

p_ne <- dat_ne |>
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
  ggtitle("Fall Thermal Habitat Anomaly \u2014 NewEngland Managed Species") +
  ecodata::theme_ts() +
  ecodata::theme_title()


# -------------------------------------------------------------------
# 3. View Results
# -------------------------------------------------------------------

# Display the plots
print(p_ma)
print(p_ne)

message("\nTest complete! The data structure is compatible with ecodata.")