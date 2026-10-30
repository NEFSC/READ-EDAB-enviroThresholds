# R/utils_config.R
#
# Shared parameters and file paths for the fall thermal habitat indicator.
# Every numbered script sources this file, so changing a value here changes it
# everywhere. Section numbers refer to the manuscript methods (issue #27).

# -------------------------------------------------------------------
# Parameters
# -------------------------------------------------------------------

on_windows <- .Platform$OS.type == "windows"

params <- list(
  
  # --- Season (2.1) ---------------------------------------------------
  season        = "FALL",
  season_months = 9:11,          # Sep-Nov daily bottom temperature window
  season_days   = 91,            # expected number of daily layers per fall
  
  # --- Species --------------------------------------------------------
  # Management bodies whose species are processed. Plots and tables can be
  # filtered to NEFMC/JOINT later; the pipeline builds everything.
  managers = c("NEFMC", "MAFMC", "JOINT", "ASMFC"),
  
  # --- Thermal niche (2.1.2) ------------------------------------------
  # Seasons whose positive tows define the niche. NULL = all seasons, full
  # time series (NEFSC spring, fall, and winter/summer where present; MassDMF
  # spring and fall). Set to "FALL" for the fall-only sensitivity run.
  niche_seasons     = NULL,
  niche_probs       = c(0.10, 0.90),
  # Minimum number of positive tows (all niche seasons, with bottom
  # temperature) required to estimate a niche. Confirm against the bootstrap
  # output of 00_get_thermal_niches.R after rerunning on all seasons.
  min_positive_tows    = 50,
  niche_boot_reps      = 500,
  niche_boot_n         = c(5, 10, 15, 20, 30, 40, 50, 75, 100, 150, 200, 300, 500, 750, 1000),
  niche_boot_ref_min_n = 1000,   # references well above the largest n tested
  
  # --- Historic footprint (2.2) ---------------------------------------
  min_stations_per_stratum_year = 3,
  # A footprint must contain at least this % of the species' positive fall
  # tows (all years). Species below it are poorly sampled by the survey and
  # are not scored.
  min_pct_tows_in_footprint = 75,
  # Years used to build footprints. NULL = full time series.
  # Set to 1982:2011 to build footprints from the baseline period only.
  footprint_years = NULL,
  
  # --- Bottom temperature (2.1.3, 2.3) --------------------------------
  erddap_last_year = 2020,       # du Pontavice ERDDAP product through this year;
  # GLORYS daily files: file name pattern (date taken from the name) and the
  # variable name if files hold more than one variable (NULL = single variable)
  glorys_pattern   = "BOTTOMT-\\d{8}\\.nc$",
  glorys_var       = NULL,
  
  # --- Scoring (2.4) --------------------------------------------------
  baseline_years  = 1982:2011,
  min_baseline_sd = 3,           # floor on baseline SD (percentage points); applies to
  # near-ceiling species whose baseline SD is compressed
  low_sd_flag     = 3,           # flag species whose baseline SD is below this
  
  # --- SOE export (issue #25) -----------------------------------------
  # Report assignment by manager (whole-footprint S; JOINT in both reports)
  soe_reports    = list(NE = c("NEFMC", "JOINT"), MA = c("MAFMC", "JOINT")),
  soe_mean_label = "Managed species mean",
  soe_var_sep    = " - ",
  
  # --- Output ---------------------------------------------------------
  make_maps = TRUE
)

# -------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------

paths <- list(
  survdat       = "~/EDAB_Datasets/Workflows/Survey_Data/survey_no_lengths_data.rds",
  inshore       = "~/EDAB_Datasets/Workflows/Survey_Data/mass_inshore_data.rds",
  species       = "~/EDAB_Resources/static_workflow_inputs/SOE_species_list_24.rds",
  strata        = "~/Maxwell.Grezlik/Rprojects/READ-PDB-StockEff/gis_files/survey_strata.shp",
  literature    = here::here("data/MS_screening_analysis_cleaned.csv"),
  bt_erddap     = here::here("data-raw/duPontavice_bottom_temp.nc"),
  bt_glorys_dir = if (on_windows) {
    "//nefscdata/EDAB_Datasets/GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT"
  } else {
    "~/EDAB_Datasets/GLORYS/V12/SOURCE/NES_9KM_DAILY/GLOBAL_MULTIYEAR_PHY_001_030/BOTTOMT"
  },
  soe_dir      = here::here("data/soe"),
  glorys_cache  = here::here("data-raw/glorys_cache"),   # local copies of GLORYS files (not committed)
  
  # Pipeline outputs
  niche        = here::here("data-raw/thermal_niche.rds"),
  niche_dir    = here::here("data/thermal_niche"),
  habitat_dir  = here::here("data/historic_habitat"),
  habitat      = here::here("data/historic_habitat/historic_habitat_fall.rds"),
  indicator    = here::here("data/indicators/perc_suitable_thermal_habitat_fall.rds"),
  scoring_dir  = here::here("data/scoring"),
  images_dir   = here::here("images")
)