#' Pull survey data for envirnomental thresholds project
#'
#' Require temp, depth, salinity for each species at tow level to determine
#' observed species tolerace ranges
#'
#' @return survdat data pull list


channel <- dbutils::connect_to_database("NEFSC_USERS",username)

data <- survdat::get_survdat_data(channel = channel,
                                  filterByYear = NA,
                                  all.season = T,
                                  shg.check = T,
                                  conversion.factor = T,
                                  use.SAD = F,
                                  getBio = F,
                                  getLengths = F,
                                  getWeightLength = F)
