#' Create data for ecodata::thermal_habitat_anomaly
#'
#' Processes the seasonal thermal habitat indicator to produce a
#' summary of Fall thermal habitat anomalies for the State of the Ecosystem report.
#'
#' @param indicator_path Character string. Path to perc_suitable_thermal_habitat_seasonally.rds
#' @param species_path Character string. Path to SOE_species_list_24.rds
#' @param outputPathDataSets Character string. Path to folder where data pull should be saved
#'
#' @return A single tibble containing all summarized anomaly data formatted for ecodata.
#'
#' @importFrom dplyr bind_rows case_when distinct filter group_by left_join mutate rename select summarise ungroup first arrange
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble
#' @importFrom tools toTitleCase
#'
#' @export
#'
create_thermal_habitat_anomaly <- function(
    indicator_path,
    species_path,
    outputPathDataSets = NULL
) {
  
  # Check if the input files exist ---------------------------
  if (!file.exists(indicator_path) || !file.exists(species_path)) {
    stop("One or more of the input files are not present in the location specified.")
  }
  
  # 1. Load Data ---------------------------------------------
  indicators_df <- readRDS(indicator_path) |> tibble::as_tibble()
  species_raw   <- readRDS(species_path) |> tibble::as_tibble()
  
  # 2. Calculate Fall Anomaly --------------------------------
  # Filter for Fall and calculate the anomaly relative to the first year in the time series
  anomaly_df <- indicators_df |>
    dplyr::filter(season == "FALL", !is.na(perc_within_hist)) |>
    dplyr::arrange(species, year) |>
    dplyr::group_by(species) |>
    dplyr::mutate(
      baseline_val = dplyr::first(perc_within_hist),
      fall_anomaly = perc_within_hist - baseline_val
    ) |>
    dplyr::ungroup()
  
  # 3. Process Species Management Info -----------------------
  species_raw <- species_raw |>
    dplyr::mutate(Fed.Managed = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed))
  
  # Keep ASMFC definitions available for when a decision is made on how to route them
  asmfc_species <- c(
    "STRIPED BASS", "ATLANTIC MENHADEN", "TAUTOG", "WEAKFISH",
    "ATLANTIC CROAKER", "SPOT", "AMERICAN EEL", "ATLANTIC STURGEON",
    "HORSESHOE CRAB", "AMERICAN SHAD", "ALEWIFE", "BLUEBACK HERRING"
  )
  
  species_mgmt <- species_raw |>
    dplyr::mutate(State.Managed = ifelse(COMNAME %in% asmfc_species, "ASMFC", NA_character_)) |>
    dplyr::distinct(COMNAME, Fed.Managed, State.Managed)
  
  plot_data <- anomaly_df |>
    dplyr::left_join(species_mgmt, by = c("species" = "COMNAME"))
  
  # 4. Map to SOE Reports (EPU column) -----------------------
  # EPU column acts as the report flag: 'MA' for Mid-Atlantic, 'NE' for New England.
  # JOINT species are duplicated so they appear in both reports.
  # Note: ASMFC-exclusive species are intentionally omitted for now.
  
  ma_data <- plot_data |>
    dplyr::filter(Fed.Managed %in% c("MAFMC", "JOINT")) |>
    dplyr::mutate(EPU = "MA")
  
  ne_data <- plot_data |>
    dplyr::filter(Fed.Managed %in% c("NEFMC", "JOINT")) |>
    dplyr::mutate(EPU = "NE")
  
  # 5. Format to strict ecodata standards --------------------
  final_data <- dplyr::bind_rows(ma_data, ne_data) |>
    dplyr::mutate(
      Time  = year,
      Var   = tools::toTitleCase(tolower(as.character(species))),
      Value = fall_anomaly,
      Units = "percent"
    ) |>
    dplyr::select(Time, Var, Value, EPU, Units) |>
    dplyr::arrange(EPU, Var, Time)
  
  # 6. Save Output -------------------------------------------
  if (!is.null(outputPathDataSets)) {
    if (!dir.exists(outputPathDataSets)) dir.create(outputPathDataSets, recursive = TRUE)
    saveRDS(final_data, file.path(outputPathDataSets, "thermal_habitat_anomaly.rds"))
  }
  
  return(final_data)
}