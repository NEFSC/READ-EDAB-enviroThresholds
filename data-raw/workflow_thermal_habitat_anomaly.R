#' Calculates thermal habitat anomaly data set for automated workflow
#'
#' This uses the output from the thermal habitat indicator pipeline to 
#' format the fall anomaly data for ecodata.
#'
#' @param indicator_path Character string. Full path to the perc_suitable_thermal_habitat_seasonally.rds file
#' @param species_path Character string. Full path to the species list data pull rds file (e.g., SOE_species_list_24.rds)
#' @param outputPath Character string. Path to folder where data pull should be saved
#'
#' @return tibble containing the `ecodata::thermal_habitat_anomaly` data frame
#'
#' @example
#' \dontrun{
#' workflow_thermal_habitat_anomaly(
#'    indicator_path = "path/to/perc_suitable_thermal_habitat_seasonally.rds",
#'    species_path = "path/to/SOE_species_list_24.rds",
#'    outputPath = "path/to/output/folder"
#'    )
#' }
#'
#' @section Dependencies:
#'
#' This assumes that the thermal habitat anomaly data has been processed by the 
#' environmental thresholds pipeline and resides in `indicator_path`.
#'
#' @export

workflow_thermal_habitat_anomaly <- function(
    indicator_path,
    species_path,
    outputPath
) {
  # Add check to skip running workflow if data not present
  
  tryCatch(
    {
      if (
        !all(
          !is.null(outputPath),
          file.exists(indicator_path),
          file.exists(species_path)
        )
      ) {
        stop("Incorrect file path or file missing")
      }
      
      # calculate indicator
      # Note: Once migrated, this may need the package namespace, e.g., SOEworkflows::create_thermal_habitat_anomaly
      indicatorData <- create_thermal_habitat_anomaly(
        indicator_path = indicator_path,
        species_path = species_path,
        outputPathDataSets = outputPath
      )
      
      # The create function handles saving to the outputPath, so we just return the object
      return(indicatorData)
    },
    error = function(e) {
      message("An error occurred: ", conditionMessage(e))
      return(NULL)
    }
  )
}