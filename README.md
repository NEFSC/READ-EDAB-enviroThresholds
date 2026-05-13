# NEFMC Thermal Habitat Indicators & Risk Scoring

## Overview
This repository contains the workflow for generating, evaluating, and scoring thermal habitat suitability indicators for species managed by the New England Fishery Management Council (NEFMC). These indicators are developed specifically to integrate into the NEFMC Risk Policy framework. 

The pipeline defines historic habitat footprints based on NEFSC bottom trawl survey strata, calculates the percentage of that habitat that is thermally suitable in a given year, and translates that timeseries into a discrete -4 to +4 risk score.

## Workflow & Key Scripts
The repository is structured to run sequentially:

1. **Habitat Footprint Generation**
   * `sensitivity_strata_obs.R`: Evaluates absolute and cumulative percentage observation thresholds to define core habitat.
   * `get_historic_habitat_V6.R`: Builds the spatial footprint (V6) based on survey strata containing $\ge$ 3 historical observations.
   * `get_historic_habitat_V6_90perc.R`: Builds an alternative dynamic footprint retaining 90% of a species' historical observations.
2. **Indicator Calculation**
   * `get_perc_suitable_thermal_habitat.R`: Intersects the habitat footprints with daily bottom temperatures to calculate the annual `% suitable thermal habitat`.
3. **Risk Policy Scoring (-4 to +4)**
   * `thermal_suitability_z_scoring.R`: Calculates scores using a true expanding-window Z-score (Standard Deviation) method.
   * `thermal_suitability_scoring_V6.R`: Calculates scores using a discrete State + Trend matrix method.
   * `R/scoring_functions_V6.R`: Helper functions for trend calculation and significance testing.
4. **Visualization**
   * `plot_risk_score_heatmap.R`: Generates summary heatmap tables of species' risk scores over time.

## Data Access: Bottom Temperature NetCDF
Due to file size constraints, the historical bottom temperature dataset (1959-2021) is **not** hosted in this repository. To run the indicator scripts, you must download the data locally.

The data is a combined GLORYS and bias-corrected ROMS time series hosted on the NEFSC ERDDAP server.

**Download Instructions:**
1. Navigate to the dataset on ERDDAP: [duPontavice_bottom_temp](https://comet.nefsc.noaa.gov/erddap/griddap/duPontavice_bottom_temp.html)
2. Adjust the time slider to encompass the full time series (1959-01-02 to 2021-01-01).
3. Under **File type**, select `.nc` (NetCDF-3 or NetCDF-4). *Do not download as a .csv or .htmlTable, as it will crash your R session.*
4. Save the downloaded file as `duPontavice_bottom_temp.nc` in your local directory at:
   `~/EDAB_Datasets/GLORYS/duPontavice_bottom_temp.nc` 
   *(Note: If you save it elsewhere, you must update the `nc_file` path in `get_perc_suitable_thermal_habitat.R`)*

## Dependencies
This project relies heavily on the spatial ecosystem within R. Ensure you have the following packages installed:
* `tidyverse`
* `sf`
* `terra` (for lazy-loading NetCDF files)
* `exactextractr` (for fast raster-polygon extraction)
* `here`
* `marmap`
* `broom`

---

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
