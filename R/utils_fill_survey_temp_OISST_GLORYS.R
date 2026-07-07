# Sarah G's code for mapping alternative bottom and surface temperatures to survey stations
# Uses date and nearest neighbor matching
# June 2024
# 
# Bottom temperature full workflow with comparisons is here
# https://noaa-edab.github.io/benthosindex/BottomTempFill.html
# Matches Hubert-Joe's ROMS-GLORYS reanalysis bottom temp to survey stations
#
# Surface temperature full workflow with comparisons is here
# https://sgaichas.github.io/bluefishdiet/SSTmethods.html
# Matches OISST dataset to survey stations
#
# NOTE THAT GLORYS BOTTOM TEMP AND OISST DATASETS HAVE DIFFERENT FORMATS
# Functions are not identical for bottom and surface temperature filling

# required R packages
# here, dplyr, tidync, sf, nngeo, terra, raster (for older OISST code)

#######################################################
#######################################################
# Bottom temp functions in this section
#######################################################

# I have already processed bottom temp files from .nc and saved them as .rds
# You should be able to use them directly, the code below is FYI how I got them

# https://github.com/NOAA-EDAB/benthosindex/tree/main/data-raw/bottomtemp/bt_data

# matching code assumes they live in a data-raw/bottomtemp/bt_data folder

#####################################################################
# SKIP THIS SECTION IF YOU DOWNLOAD THE ALREADY PROCESSED TEMPERATURE FILES ABOVE

# The original nc files up to 2020 are on google drive:
#   https://drive.google.com/drive/folders/1H4eMpdfo3EKnmu0EJFwZyHKt237Y3heW
# Navigate to EDABranch_Drive/ITD/ERDDAP/Data/Bottom Temp from Hubert Revised by Joe v1
# Copy the files to a local drive to run these functions
# The files are already split by year and clipped to the NE. 
# File name format is `bottom_temp_yyyy.nc`

# Functions for getting tibbles from bottom temp nc files

nctotibble <- function(ncfile = ncfile){
  # tunit for this file is days since 1950-1-1
  origin <- as.Date("1950-01-01") 
  
  bttib <- tidync::tidync(ncfile) |>
    tidync::hyper_tibble(force = TRUE) |>
    dplyr::mutate(date = as.Date(time-1, origin=origin),
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date)) |>
    dplyr::rename(mod_bt = sea_water_temperature_at_sea_floor)
  
  return(bttib)
}

# loop through years to make nc into tibbles and save
years <- 1968:2020
for(i in years) {
  name <- here::here("data-raw/bottomtemp/bt_revised_metadata_032024", paste0("bottom_temp_",i, ".nc"))
  filename <- here::here("data-raw","bottomtemp", "bt_data", paste0("bt", i, ".rds"))
  text <- knitr::knit_expand(text = "bt{{year}} <- nctotibble(ncfile = name)
                                     saveRDS(bt{{year}}, filename)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
}

# function to process the GLORYS files for 2020-2023
glorysnctotibble <- function(ncfile = ncfile){
  # time is seconds since 1970-01-01 00:00:00
  origin <- as.Date("1970-01-01") 
  
  bttib <- tidync::tidync(ncfile) |>
    tidync::hyper_tibble(force = TRUE) |>
    dplyr::mutate(date = as.Date((time/86400), origin=origin),
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date))|>
    dplyr::rename(mod_bt = bottomT)
  
  return(bttib)
}

# apply to nc files and split tibbles to year to get same format as above
glfiles <- list.files(path = here::here("data-raw/bottomtemp/GLORYS-20240515"), 
                      pattern = "^glo",
                      full.names = TRUE)

allgltib <- purrr::map(glfiles, glorysnctotibble) |>
  purrr::list_rbind() 

# thanks for your opinions tidyverse, I want list elements named
yrs <- unique(allgltib$year)

allgltibls <- allgltib |>
  dplyr::group_by(year) |>
  dplyr::group_split() 

#name the list elements by year
names(allgltibls) <- yrs

#save each year in list to rds  
purrr::imap(allgltibls, ~saveRDS(.x, file = here::here("data-raw","bottomtemp", "bt_data", paste0("bt", .y, ".rds"))))

# This is the end of code for processing from nc files

#######################################################
# Bottom temp matching code starts here
#######################################################

# Now Match local btYEAR.rds files to your survey data:

# Read in your survey data and ensure dates match those in the btYEAR.rds files
# this assumes you have a stationid (joins cruise number and station number)
# this assumes you have a year field
# this assumes you have one row per station, if you don't uncomment dplyr::distinct()
# check that your lat and lon column names match those in the sf::st_as_sf line

# WARNING!
# ensure this points to your survey data file
yoursurvdat <- readRDS("whereveritis/yoursurvdat.rds")

stations <- yoursurvdat %>%
  #dplyr::mutate(day = str_pad(day, 2, pad='0'),
  #              month = str_pad(month, 2, pad='0'),
  #              yrmody = as.numeric(paste0(year, month, day))) %>%
  # consider this instead, may not need to pad strings? already date fields in the bt data
  dplyr::mutate(date = as.Date(paste0(year,"-", month,"-", day)), numdate = as.numeric(date)) |>
  dplyr::select(stationid, lon, lat, year, numdate) %>%
  # dplyr::distinct() |>
  na.omit() %>%
  sf::st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)



#list of SST dataframes
BTdfs <- list.files(here("data-raw/bottomtemp/bt_data/"), pattern = "*.rds")

survstn_mod_bt <- tibble()


for(df in BTdfs){
  btdf <- readRDS(paste0(here("data-raw/bottomtemp/bt_data/", df)))
  
  if(unique(btdf$year) %in% unique(stations$year)){
    # keep only bluefish dates in SST year
    stationsyr <- stations %>%
      filter(year == unique(btdf$year))
    
    # keep only modeled bt days in survey dataset
    btdf_survdays <- btdf %>%
      dplyr::mutate(numdate = as.numeric(date))%>%
      dplyr::filter(numdate %in% unique(stationsyr$numdate)) %>%
      dplyr::mutate(year = as.numeric(year),
                    month = as.numeric(month),
                    day = as.numeric(day),
                    lon = longitude,
                    lat = latitude) %>%
      dplyr::select(-longitude, -latitude) %>%
      sf::st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)
    
    # now join by nearest neighbor and date
    
    #https://stackoverflow.com/questions/71959927/spatial-join-two-data-frames-by-nearest-feature-and-date-in-r
    
    yrsurvmodBT <- do.call('rbind', lapply(split(stationsyr, 1:nrow(stationsyr)), function(x) {
      sf::st_join(x, btdf_survdays[btdf_survdays$numdate == unique(x$ numdate),],
                  #join = st_nearest_feature
                  join = st_nn, k = 1, progress = FALSE
      )
    }))
    
    #   #datatable solution--works but doesnt seem faster?
    #    df1 <- data.table(stationsyr)
    #
    #  .nearest_samedate <- function(x) {
    #    st_join(st_as_sf(x), sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),], join = st_nearest_feature)
    #  }
    # #
    #  yrsurvmodBT <- df1[, .nearest_samedate(.SD), by = list(1:nrow(df1))]
    
    survstn_mod_bt <- rbind(survstn_mod_bt,  yrsurvmodBT)
  }
}

survstn_mod_bt_merge <- survstn_mod_bt %>%
  dplyr::rename(lon = lon.x,
                lat = lat.x,
                year = year.x) %>%
  dplyr::select(id, mod_bt) %>%
  sf::st_drop_geometry()

yoursurvdat_modBT <- left_join(yoursurvdat, survstn_mod_bt_merge)

# WARNING!
# ensure this points to where you want survey data-GLORYS bottom temp merge file
saveRDS(yoursurvdat_modBT, here::here("whereveritis/yoursurvdat_modBT.rds"))

# END BOTTOM TEMP
#######################################################

#######################################################
#######################################################
# Surface temp functions in this section
#######################################################

# I have already processed surface temp files from .nc and saved them as .rds
# You should be able to use them directly, the code below is FYI how I got them
#
# Note:
# OISST data only goes back to 1982 so these are 1982-2022
# I have not yet processed 2023

# https://github.com/NOAA-EDAB/forageindex/tree/main/data-raw/gridded/sst_data

# matching code assumes they live in a data-raw/gridded/sst_data folder

#####################################################################
# SKIP THIS SECTION IF YOU DOWNLOAD THE ALREADY PROCESSED TEMPERATURE FILES ABOVE
# USE/MODIFY THIS CODE if you want to make an OISST .rds file for 2023

# Bastille function from https://github.com/kimberly-bastille/ecopull/blob/main/R/utils.R

nc_to_raster <- function(nc,
                         varname,
                         extent = c(0, 360, -90, 90),
                         crop = raster::extent(280, 300, 30, 50),
                         show_images = FALSE) {
  
  message("Reading .nc as brick...")
  
  r <- raster::brick(nc, varname = varname)
  
  message("Setting CRS...")
  raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)
  
  if(show_images){
    par(mfrow = c(1,2))
    raster::plot(r, 1, sub = "Full dataset")
  }
  
  message("Cropping data...")
  ne_data <- raster::crop(r, crop)
  #ne_data <- raster::rotate(ne_data) add here for future pulls
  
  if(show_images){
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1,1))
  }
  
  message("Done!")
  
  return(ne_data)
}

# function to convert to dataframe based on
# https://towardsdatascience.com/transforming-spatial-data-to-tabular-data-in-r-4dab139f311f

raster_to_sstdf <- function(brick,
                            rotate=TRUE){
  
  if(rotate) brick_r <- raster::rotate(brick)
  brick_r <- raster::crop(brick_r, raster::extent(-77,-65,35,45))
  sstdf <- as.data.frame(raster::rasterToPoints(brick_r, spatial = TRUE))
  sstdf <- sstdf %>%
    dplyr::rename(Lon = x,
                  Lat = y) %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = c("year", "month", "day"),
                        names_prefix = "X",
                        names_sep = "\\.",
                        values_to = "sst",
    )
  return(sstdf)
}

# pull the OISST data as raster brick, modified from 
# https://github.com/kimberly-bastille/ecopull/blob/main/.github/workflows/pull_satellite_data.yml

varname <- "sst"

# 1985-2021 previously pulled, processed and stored. add 2022.
# add 1981-1984 to extend back in time. No OISST before 1981.
# 1981 is only Sept-Dec so don't use

years <- 1982:2022
  for(i in years) {
    name <- paste0(i, ".nc")
    dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
    filename <- here::here("data-raw","gridded", "sst_data", paste0("test_", i, ".grd"))
    url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", i, ".nc")
    download.file(url, destfile = name)
    
    text <- knitr::knit_expand(text = "test_{{year}} <- nc_to_raster(nc = name, varname = varname)
                                     raster::writeRaster(test_{{year}}, filename = filename, overwrite=TRUE)",
                               year = i)
    print(text)
    try(eval(parse(text = text)))
    unlink(name) # remove nc file to save space
    print(paste("finished",i))
  }


# convert raster to dataframe
years <- 1982:2022
for(i in years) {
  name <- get(paste0("test_",i))
  filename <- here::here("data-raw","gridded", "sst_data", paste0("sst", i, ".rds"))
  text <- knitr::knit_expand(text = "sst{{year}} <- raster_to_sstdf(brick = name)
                                     saveRDS(sst{{year}}, filename)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
}

# This is the end of code for processing .rds from OISST .nc files

#######################################################
# Surface temp OISST matching code starts here
#######################################################

# Now Match local sstYEAR.rds files to your survey data:

# Read in your survey data and ensure dates match those in the sstYEAR.rds files
# this assumes you have a stationid (joins cruise number and station number)
# this assumes you have a year field
# this assumes you have one row per station, if you don't uncomment dplyr::distinct()
# check that your lat and lon column names match those in the sf::st_as_sf line

yoursurvdat <- readRDS("whereveritis/yoursurvdat.rds")

stations <- yoursurvdat %>%
  dplyr::mutate(day = str_pad(day, 2, pad='0'),
                month = str_pad(month, 2, pad='0'),
                yrmody = as.numeric(paste0(year, month, day))) %>%
  dplyr::select(stationid, lon, lat, year, yrmody) %>%
  # dplyr::distinct() |>
  na.omit() %>%
  sf::st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE) 

#list of SST dataframes
SSTdfs <- list.files(here("data-raw/gridded/sst_data/"), pattern = "*.rds")

survstn_OISST <- tibble()


for(df in SSTdfs){
  #sstdf <- readRDS(paste0(here("data-raw/gridded/sst_data/", df)))
  sstdf <- readRDS(paste0(d.name, df))
  
  # keep only zooplankton dates in SST year
  stationsyr <- stations %>%
    filter(year == unique(sstdf$year))
  
  # keep only sst days in zooplankton dataset
  sstdf_survdays <- sstdf %>%
    dplyr::mutate(yrmody = as.numeric(paste0(year, month, day)) )%>%
    dplyr::filter(yrmody %in% unique(stationsyr$yrmody)) %>%
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  lon = Lon,
                  lat = Lat) %>%
    dplyr::select(-Lon, -Lat) %>%
    sf::st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)  
  
  # now join by nearest neighbor and date
  
  #https://stackoverflow.com/questions/71959927/spatial-join-two-data-frames-by-nearest-feature-and-date-in-r      
  
  yrsurvOISST <- do.call('rbind', lapply(split(stationsyr, 1:nrow(stationsyr)), function(x) {
    sf::st_join(x, sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),],
                #join = st_nearest_feature
                join = st_nn, k = 1, progress = FALSE
    )
  }))
  
  #   #datatable solution--works but doesnt seem faster?
  #    df1 <- data.table(stationsyr)
  #   
  #  .nearest_samedate <- function(x) {
  #    st_join(st_as_sf(x), sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),], join = st_nearest_feature)
  #  }
  # # 
  #  yrsurvOISST <- df1[, .nearest_samedate(.SD), by = list(1:nrow(df1))]
  
  survstn_OISST <- rbind(survstn_OISST, yrsurvOISST)
  
}

# Now join with OISST dataset

survstn_OISST_merge <- survstn_OISST %>%
  dplyr::rename(lon = lon.x,
                lat = lat.x,
                year = year.x,
                oisst = sst) %>%
  dplyr::select(stationid, oisst) %>%
  sf::st_drop_geometry()

yoursurvdat_OISST <- left_join(yoursurvdat, survstn_OISST_merge)

# WARNING!
# ensure this points to where you want survey data-OISST merge file
saveRDS(yoursurvdat_OISST, here::here("whereveritis/yoursurvdat_OISST.rds"))

