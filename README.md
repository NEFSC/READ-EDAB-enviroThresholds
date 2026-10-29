---
editor_options: 
  markdown: 
    wrap: 72
---

# NEFMC Thermal Habitat Indicators & Risk Scoring

## Overview

This repository generates a thermal habitat suitability indicator for
species managed in the Northeast U.S. It has two products:

- **Risk Policy scores** (-4 to +4) for the New England Fishery
  Management Council (NEFMC) Risk Policy Framework.
- **State of the Ecosystem (SOE) export**: the indicator and anomalies
  in ecodata format for the New England and Mid-Atlantic SOE reports.
  Figures are produced in ecodata. For each species, the indicator is
  the percentage of its fall historic habitat footprint where daily
  bottom temperature (September-November) fell within the species'
  thermal niche. For the Risk Policy, that percentage is standardized
  against a fixed 1982-2011 climatological baseline and binned into
  scores.

## Methodology

1.  **Thermal niche.** 10th-90th percentile of in situ bottom
    temperature at positive tows from the spring and fall surveys over
    the full time series (NEFSC bottom trawl survey + MassDMF inshore
    survey; one record per tow per species). All niches come from survey
    observations; literature values (adult, Northeast US, field studies)
    are compared but never substituted. Species with fewer than
    `params$min_positive_tows` (50) positive tows are excluded. The
    niche is applied to fall bottom temperatures.
2.  **Historic footprint.** Union of NEFSC survey strata where the
    species was caught at 3 or more stations in a single fall survey, in
    any year (full time series). Species with no qualifying stratum, or
    whose footprint contains less than
    `params$min_pct_tows_in_footprint` (75%) of their positive fall
    tows, are poorly sampled by the survey and are not scored.
3.  **Indicator.** Area-weighted percentage of footprint cell-days
    (Sep-Nov) within the niche, calculated over the part of each
    footprint covered by both bottom temperature products. Bottom
    temperature: du Pontavice bias-corrected ROMS/GLORYS product
    (ERDDAP) through 2020, daily GLORYS from 2021.
4.  **Risk score.** z-score against the 1982-2011 baseline mean and SD,
    binned at 0.5 SD to -4..+4. Lower suitability gives a positive (more
    precautionary) score. A minimum baseline SD of
    `params$min_baseline_sd` (3 percentage points) is applied to
    near-ceiling species. Years with an incomplete fall are not scored.
5.  **SOE export.** Percent suitable habitat and anomalies (relative to
    the 1982-2011 mean) for federally managed species: New England =
    NEFMC + jointly managed, Mid-Atlantic = MAFMC + jointly managed.
    Includes the mean anomaly of managed species in each region. \##
    Workflow & Key Scripts

All parameters and file paths live in `R/utils_config.R`. Shared
functions are in `R/utils_pipeline.R`. Run the scripts in order:

1.  **`00_get_thermal_niches.R`**: spring + fall survey niches,
    bootstrap for the minimum number of tows, literature comparison,
    Figure 1 draft.
2.  **`01_get_historic_habitat.R`**: fall footprints, footprint coverage
    check, maps, and a check of how many MassDMF stations fall inside
    NEFSC strata.
3.  **`02_get_perc_suitable_thermal_habitat.R`**: annual fall
    suitability indicator (slowest step).
4.  **`03_calculate_risk_scores.R`**: baseline statistics, z-scores,
    Risk Policy scores, diagnostics, time-series plots.
5.  **`04_plot_risk_score_heatmap.R`**: all-species risk score heatmap.
6.  **`05_export_soe.R`**: SOE export (ecodata format) and draft SOE
    figures for the New England and Mid-Atlantic reports.
    `sensitivity_footprint.R` (optional) tests the station threshold and
    full-series vs. baseline-only footprints.

### Where each step runs

- **00-01** need the survey data and strata shapefile (NEFSC RStudio
  container). Commit and push `data-raw/thermal_niche.rds` and
  `data/historic_habitat/historic_habitat_fall.rds`.
- **02-05** need the bottom temperature NetCDF files (stored locally;
  see below). Pull the outputs of 00-01 before running. \### Annual
  update (SOE)

1.  Pull the latest survey data.
2.  Download the new year's GLORYS file to `data-raw/` (confirm the fall
    is complete through November 30).
3.  Run 00-01 on the container; commit and push.
4.  Run 02, 03, 04 and 05 locally. Niches and footprints are
    recalculated each year, so the full time series can change between
    reports. `data/soe/thermal_habitat_soe_run_info.txt` records the
    species and settings used for each run.

## Outputs

- **Risk Policy:** all-species heatmap
  (`images/scoring/risk_score_heatmap_fall.png`) and score tables
  (`data/scoring/`).
- **SOE:** `data/soe/thermal_habitat_soe.csv` / `.rds` (columns `Time`,
  `Var`, `Value`, `EPU`, `Units`) and draft figures in `data/soe/`.
- **Diagnostics:** niche tables (`data/thermal_niche/`), footprint
  summary and coverage (`data/historic_habitat/`), temperature-domain
  coverage (`data/indicators/footprint_temperature_domain.csv`),
  baseline diagnostics (`data/scoring/baseline_diagnostics.csv`). \##
  Data Access: Bottom Temperature NetCDF

Due to file size constraints, the bottom temperature files are **not**
hosted in this repository. Do not commit `.nc` files.

### 1959-2020: du Pontavice product (ERDDAP)

A combined GLORYS and bias-corrected ROMS time series hosted on the
NEFSC ERDDAP server.

1.  Navigate to
    [duPontavice_bottom_temp](https://comet.nefsc.noaa.gov/erddap/griddap/duPontavice_bottom_temp.html).
2.  Set the time slider to the full time series (1959-01-02 to
    2021-01-01).
3.  Under **File type**, select `.nc`. *Do not download as .csv or
    .htmlTable; it will crash your R session.*
4.  Save as `data-raw/duPontavice_bottom_temp.nc` (or update
    `paths$bt_erddap` in `R/utils_config.R`). \### 2021 onward: daily
    GLORYS

One file per year named `GLORYS_daily_BottomTemp_YYYY.nc` in `data-raw/`
(or update `paths$bt_glorys_dir`), from the Copernicus Marine Service
[add product ID and download steps]. The switch year is set by
`params$erddap_last_year`. Years with an incomplete fall are calculated
but not scored.

## Dependencies

- `tidyverse`

- `sf`

- `terra`

- `exactextractr`

- `rnaturalearth` (+ `rnaturalearthdata`)

- `here`

- `scales`, `viridisLite` (figures)

- `marmap`, `ggnewscale` (footprint maps only)

- 

  ## `ecodata` (optional; SOE themes and trend lines in the `05_export_soe.R` draft figures)

## Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
