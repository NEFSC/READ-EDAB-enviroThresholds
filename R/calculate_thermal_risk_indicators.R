# calculate_thermal_risk_indicators.R
#
# Purpose: Derive two new risk indicators for NEFMC-managed species:
#          1. Thermal Constraint Inversion Year (Intersection of Hot and Cold lines)
#          2. Year of 100% Thermal Exclusion (Extrapolation of the "Too Hot" line)
#
# Input:   data/indicators/unsuitable_thermal_habitat.rds
#
# Output:
#   CSV : data/indicators/thermal_regime_risk_indicators.csv
#   PNG : images/species_climate_vulnerability_ranking.png
#
# Dependencies: tidyverse, here, ggplot2

# -------------------------------------------------------------------
# 0. Packages and Directories
# -------------------------------------------------------------------

library(tidyverse)
library(here)

dir_indicators <- here::here("data/indicators")
dir_images     <- here::here("images")

if (!dir.exists(dir_images)) dir.create(dir_images, recursive = TRUE)

# -------------------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------------------

input_file <- file.path(dir_indicators, "unsuitable_thermal_habitat.rds")

if (!file.exists(input_file)) {
  stop("Input data not found. Please run R/get_thermally_unsuitable_habitat.R first.")
}

data_raw <- readRDS(input_file)

# -------------------------------------------------------------------
# 2. Compute Risk Metrics via Linear Extrapolation
# -------------------------------------------------------------------

message("Calculating thermal risk metrics and projection models...")

risk_indicators <- data_raw |>
  group_by(species) |>
  filter(sum(!is.na(perc_too_hot)) > 5) |> # Ensure robust time series
  summarise(
    # Fit "Too Hot" Model
    mod_hot  = list(lm(perc_too_hot ~ year)),
    b_hot    = coef(mod_hot[[1]])[1], # Intercept
    m_hot    = coef(mod_hot[[1]])[2], # Slope
    p_hot    = summary(mod_hot[[1]])$coefficients[2,4],
    
    # Fit "Too Cold" Model
    mod_cold = list(lm(perc_too_cold ~ year)),
    b_cold   = coef(mod_cold[[1]])[1], # Intercept
    m_cold   = coef(mod_cold[[1]])[2], # Slope
    p_cold   = summary(mod_cold[[1]])$coefficients[2,4],
    .groups = "drop"
  ) |>
  mutate(
    # 1. Intersection Year (Thermal Constraint Inversion)
    # m_hot*Year + b_hot = m_cold*Year + b_cold  ==>  Year = (b_cold - b_hot) / (m_hot - m_cold)
    Inversion_Year = (b_cold - b_hot) / (m_hot - m_cold),
    Inversion_Year = round(Inversion_Year, 1),
    
    # 2. Year of 100% Habitat Exclusion
    # m_hot*Year + b_hot = 100  ==>  Year = (100 - b_hot) / m_hot
    Year_100_Percent_Exclusion = (100 - b_hot) / m_hot,
    Year_100_Percent_Exclusion = round(Year_100_Percent_Exclusion, 1),
    
    # Add an indicator of current trajectory steepness (% increase in hot exclusion per decade)
    Hot_Exclusion_Rate_Per_Decade = round(m_hot * 10, 2)
  ) |>
  # Clean up model objects for export
  select(
    species, 
    Hot_Exclusion_Rate_Per_Decade, 
    Inversion_Year, 
    Year_100_Percent_Exclusion,
    p_val_too_hot = p_hot,
    p_val_too_cold = p_cold
  ) |>
  arrange(Year_100_Percent_Exclusion)

# -------------------------------------------------------------------
# 3. Save Tabular Results
# -------------------------------------------------------------------

csv_out <- file.path(dir_indicators, "thermal_regime_risk_indicators.csv")
write_csv(risk_indicators, csv_out)
message("Tabular risk metrics successfully saved to: ", csv_out)

# -------------------------------------------------------------------
# 4. Generate Vulnerability Ranking Plot
# -------------------------------------------------------------------

message("Generating climate vulnerability ranking visualization...")


plot_data <- risk_indicators |>
  # Exclude mathematical errors (negative slopes), but cap distant futures at 2250
  filter(Year_100_Percent_Exclusion > 2000) |>
  mutate(
    Year_100_Percent_Exclusion = if_else(Year_100_Percent_Exclusion > 2250, 2250, Year_100_Percent_Exclusion),
    species = str_to_title(species),
    # Add a visual flag for capped species
    Is_Capped = if_else(Year_100_Percent_Exclusion == 2250, "Projected Beyond 2250", "Within 200 Years")
  )

ranking_plot <- ggplot(plot_data, aes(x = reorder(species, -Year_100_Percent_Exclusion), y = Year_100_Percent_Exclusion)) +
  geom_segment(aes(xend = species, yend = 2026), color = "grey70", linewidth = 1) +
  geom_point(aes(size = Hot_Exclusion_Rate_Per_Decade), color = "#d73027", alpha = 0.9) +
  geom_hline(yintercept = 2026, linetype = "solid", color = "darkblue", linewidth = 0.8) +
  annotate("text", x = 1.5, y = 2030, label = "Current Year (2026)", color = "darkblue", hjust = 0, fontface = "italic") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Species Vulnerability: Projected Horizon to 100% Thermal Exclusion",
    subtitle = "Based on linear extrapolation of daily baseline habitat loss trends (1959-2025)",
    x = "",
    y = "Projected Year of Complete Historical Footprint Exclusion",
    size = "% Lost Per Decade"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank()
  )

png_out <- file.path(dir_images, "species_climate_vulnerability_ranking.png")
ggsave(png_out, plot = ranking_plot, width = 10, height = 7, dpi = 300)
message("Vulnerability ranking plot saved to: ", png_out)