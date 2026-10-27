# R/utils_pipeline.R
#
# Helper functions shared by the numbered pipeline scripts.
# Source after utils_config.R.

# -------------------------------------------------------------------
# Survey data
# -------------------------------------------------------------------

#' Managed species table (SVSPP, COMNAME, SCINAME, manager)
load_managed_species <- function(paths, params) {
  
  asmfc_species <- c(
    "STRIPED BASS", "ATLANTIC MENHADEN", "TAUTOG", "WEAKFISH",
    "ATLANTIC CROAKER", "SPOT", "AMERICAN EEL", "ATLANTIC STURGEON",
    "HORSESHOE CRAB", "AMERICAN SHAD", "ALEWIFE", "BLUEBACK HERRING"
  )
  
  readRDS(path.expand(paths$species)) |>
    dplyr::as_tibble() |>
    # Windowpane is managed by NEFMC
    dplyr::mutate(
      Fed.Managed   = ifelse(COMNAME == "WINDOWPANE", "NEFMC", Fed.Managed),
      State.Managed = ifelse(COMNAME %in% asmfc_species, "ASMFC", NA_character_),
      manager       = dplyr::coalesce(Fed.Managed, State.Managed)
    ) |>
    dplyr::filter(Fed.Managed %in% params$managers | State.Managed %in% params$managers) |>
    dplyr::mutate(SVSPP = as.integer(as.character(SVSPP))) |>
    dplyr::distinct(SVSPP, .keep_all = TRUE) |>
    dplyr::select(SVSPP, COMNAME, SCINAME, manager)
}

#' Positive tows for managed species, one row per tow x species
#'
#' Combines the NEFSC and MassDMF surveys and collapses CATCHSEX rows so each
#' tow counts once per species.
#'
#' @param seasons Character vector of seasons to keep (e.g. "FALL"), or NULL
#'   for all seasons.
load_positive_tows <- function(paths, params, seasons = params$season) {
  
  standardize <- function(df, survey) {
    df |>
      dplyr::as_tibble() |>
      dplyr::transmute(
        SURVEY    = survey,
        CRUISE6   = as.character(CRUISE6),
        STATION   = as.character(STATION),
        STRATUM   = as.character(STRATUM),
        TOW       = as.character(TOW),
        YEAR      = as.integer(as.character(YEAR)),
        SEASON    = toupper(as.character(SEASON)),
        LAT       = as.numeric(LAT),
        LON       = as.numeric(LON),
        DEPTH     = as.numeric(DEPTH),
        BOTTEMP   = as.numeric(BOTTEMP),
        SVSPP     = as.integer(as.character(SVSPP)),
        ABUNDANCE = as.numeric(ABUNDANCE)
      )
  }
  
  nefsc   <- standardize(readRDS(path.expand(paths$survdat))$survdat, "NEFSC")
  massdmf <- standardize(readRDS(path.expand(paths$inshore))$survdat, "MassDMF")
  
  managed <- load_managed_species(paths, params)
  
  tows <- dplyr::bind_rows(nefsc, massdmf)
  if (!is.null(seasons)) tows <- dplyr::filter(tows, SEASON %in% toupper(seasons))
  
  tows |>
    dplyr::filter(ABUNDANCE > 0, !is.na(LAT), !is.na(LON), !is.na(YEAR)) |>
    dplyr::inner_join(managed, by = "SVSPP") |>
    dplyr::mutate(tow_id = paste(SURVEY, CRUISE6, STATION, TOW, sep = "_")) |>
    # CATCHSEX splits some species into several rows per tow
    dplyr::distinct(tow_id, SVSPP, .keep_all = TRUE) |>
    dplyr::select(-ABUNDANCE)
}

#' Positive tows in the indicator season (fall); used for footprints
load_fall_positive_tows <- function(paths, params) {
  load_positive_tows(paths, params, seasons = params$season)
}

# -------------------------------------------------------------------
# Spatial helpers
# -------------------------------------------------------------------

load_strata <- function(paths) {
  Sys.setenv(SHAPE_RESTORE_SHX = "YES")
  on.exit(Sys.unsetenv("SHAPE_RESTORE_SHX"))
  
  sf::st_read(path.expand(paths$strata), quiet = TRUE) |>
    sf::st_set_crs(4269) |>
    sf::st_transform(4326) |>
    sf::st_make_valid() |>
    dplyr::mutate(strata_uid = dplyr::row_number())
}

load_land <- function() {
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_transform(4326)
}

#' Attach the survey stratum (strata_uid) containing each tow
assign_strata <- function(tows, strata) {
  tows |>
    sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326, remove = FALSE) |>
    sf::st_join(strata["strata_uid"]) |>
    sf::st_drop_geometry() |>
    # A tow on a shared boundary can match two strata; keep one
    dplyr::distinct(tow_id, SVSPP, .keep_all = TRUE)
}

#' Build one species' historic footprint (Section 2.2)
#'
#' @param tows_sp Positive tows for one species, already passed through
#'   assign_strata().
#' @return An sf polygon with summary columns, or NULL if no stratum qualifies.
build_footprint <- function(tows_sp, strata, land_union, min_stations, years = NULL) {
  
  if (!is.null(years)) tows_sp <- dplyr::filter(tows_sp, YEAR %in% years)
  if (nrow(tows_sp) == 0) return(NULL)
  
  qualifying <- tows_sp |>
    dplyr::filter(!is.na(strata_uid)) |>
    dplyr::count(strata_uid, YEAR, name = "n_stations") |>
    dplyr::filter(n_stations >= min_stations) |>
    dplyr::pull(strata_uid) |>
    unique()
  
  if (length(qualifying) == 0) return(NULL)
  
  poly <- strata |>
    dplyr::filter(strata_uid %in% qualifying) |>
    sf::st_union() |>
    sf::st_make_valid()
  
  poly <- suppressWarnings(sf::st_difference(poly, land_union)) |>
    sf::st_make_valid()
  
  if (length(poly) == 0 || all(sf::st_is_empty(poly))) return(NULL)
  
  sf::st_sf(
    n_tows             = nrow(tows_sp),
    n_tows_in_strata   = sum(!is.na(tows_sp$strata_uid)),
    n_tows_in_footprint = sum(tows_sp$strata_uid %in% qualifying),
    n_strata           = length(qualifying),
    strata_uids        = I(list(sort(qualifying))),
    area_km2           = as.numeric(sf::st_area(poly)) / 1e6,
    geometry           = poly
  )
}

# -------------------------------------------------------------------
# Scoring
# -------------------------------------------------------------------

#' Map a z-score to the -4..+4 Risk Policy scale (Section 2.4.2, Table 2)
#'
#' Lower thermal suitability (negative z) gives a positive, more
#' precautionary score.
z_to_risk_score <- function(z) {
  dplyr::case_when(
    is.na(z)   ~ NA_real_,
    z >=  2.0  ~ -4,
    z >=  1.5  ~ -3,
    z >=  1.0  ~ -2,
    z >=  0.5  ~ -1,
    z >  -0.5  ~  0,
    z >  -1.0  ~  1,
    z >  -1.5  ~  2,
    z >  -2.0  ~  3,
    TRUE       ~  4
  )
}

#' Colour-blind-safe diverging palette for Risk Policy scores (-4..+4)
#' Blue = less risk averse (more suitable habitat); orange = more risk averse.
#' Blue arm from ColorBrewer RdBu, orange arm from PuOr.
risk_palette <- setNames(
  c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
    "#FEE0B6", "#FDB863", "#E08214", "#B35806"),
  -4:4
)

# -------------------------------------------------------------------
# Misc
# -------------------------------------------------------------------

species_label <- function(x) tools::toTitleCase(tolower(x))
safe_name     <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

ensure_dir <- function(...) {
  d <- file.path(...)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  invisible(d)
}