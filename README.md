---

editor_options: 
  markdown: 
    wrap: 72
---

# NEFMC Thermal Habitat Indicators & Risk Scoring

## Overview

This repository generates a thermal habitat suitability indicator for species managed by the New England Fishery Management Council (NEFMC), scored for the NEFMC Risk Policy Framework (-4 to +4).

For each species, the indicator is the percentage of its fall historic habitat footprint where daily bottom temperature (September-November) fell within the species' thermal niche. That percentage is standardized against a fixed 1982-2011 climatological baseline and binned into Risk Policy scores.

## Methodology

1.  **Thermal nice** 10th-90th percentile of bottom temperature at positive fall tows (NEFSC bottom trawl survey + MassDMF inshore survey), pooled across years. All niches come from survey observations; literature values are compared but never substituted. Species with fewer than `params$min_positive_tows` positive tows are excluded (e.g., Atlantic salmon).
2.  **Historic footprint** Union of NEFSC survey strata where the species was caught at 3 or more stations in a single fall survey, in any year. Species whose footprint contains less than 75% of their positive fall tows are poorly sampled by the survey and are not scored.
3.  **Indicator** Area-weighted percentage of footprint cell-days (Sep-Nov) within the niche. Bottom temperature: du Pontavice bias-corrected ROMS/GLORYS product (ERDDAP) through 2020, daily GLORYS from 2021.
4.  **Risk score** z-score against the 1982-2011 baseline mean and SD, binned at 0.5 SD to -4..+4. Lower suitability gives a positive (more precautionary) score.

## Workflow & Key Scripts

All parameters and file paths live in `R/utils_config.R`. Shared functions are in `R/utils_pipeline.R`. Run the scripts in order:

1.  **`00_get_thermal_niches.R`**: fall survey niches, bootstrap for the minimum number of tows, literature comparison, Figure 1 draft.
2.  **`01_get_historic_habitat.R`**: fall footprints, maps, and a check of how many MassDMF stations fall inside NEFSC strata.
3.  **`02_get_perc_suitable_thermal_habitat.R`**: annual fall suitability indicator (slowest step).
4.  **`03_calculate_risk_scores.R`**: baseline statistics, z-scores, Risk Policy scores, diagnostics, time-series plots.
5.  **`04_plot_risk_score_heatmap.R`**: all-species heatmap.

`sensitivity_footprint.R` (optional) tests the station threshold and full-series vs. baseline-only footprints.

## Final Output

The primary output is the all-species risk score heatmap (`images/scoring/risk_score_heatmap_fall.png`) and the score tables in `data/scoring/`.

## Data Access: Bottom Temperature NetCDF

Due to file size constraints, the historical bottom temperature dataset (1959-2021) is **not** hosted in this repository.

## 1959-2020: du Pontavice product (ERDDAP).
A combined GLORYS and bias-corrected ROMS time series hosted on the NEFSC ERDDAP server

1. Navigate to duPontavice_bottom_temp
2. Set the time slider to the full time series (1959-01-02 to 2021-01-01)
3. Under File type, select `.nc` (or update `path@bt_erdap` in `R/utils_config.R`)

## 2021 onward: daily GLORYS
One file per year named `GLORYS_daily_BottomTemp_YYYY.nc` in `data-raw/`. The switch year is set by `params$erddap_last_year`. Years with an incomplete fall are calculated but not scored.

## Dependencies

 - `tidyverse`
 - `sf`
 - `terra`
 - `exactextractr`
 - `rnaturalearth` (+ `rnaturalearthdata`)
 - `here`
 - `marmap`, `ggnewscale` (maps only)

------------------------------------------------------------------------

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
