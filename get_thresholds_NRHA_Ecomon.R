# Use pulled survdat.rds file to get environmental thresholds for Atlantis groups
library(dplyr)
library(here)
library(geosphere)

# Temporal and spatial bounds

isWithinBounds <- function(CTD_loc,p1_loc,SPATIAL_BOUND) {
  
  distance <- distm(CTD_loc,p1_loc,fun=distGeo) * 0.000621371
  if (distance <= SPATIAL_BOUND) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

convertCTDdate <- function(date) {
  split.points <- c(4,6,8)
  converted_date <- substring(date,c(1,split.points + 1), c(split.points, nchar(date)))
  converted_date <- as.Date(paste(converted_date[1],"-",converted_date[2],"-",converted_date[3],sep=""))
  return(converted_date)
}

TEMPORAL_BOUND <- 7      # days
SPATIAL_BOUND <- 1       # miles
# Get output data
NRHA_dt <-read.csv(here("data","NRHA_data.csv"))
CTD_dt <-read.csv(here("data","CTD_data.csv"))

# create empty dts to be filled for later analysts
rows_CTD_dt <- nrow(CTD_dt)
surfbot_dt <- CTD_dt[-c(1:rows_CTD_dt),]
nearest_dt <- surfbot_dt

# create list of uniqueIDs for CTD casts
uniqueID_list <- unique(CTD_dt$UniqueID)[]


# Make temporary data structure with just first (surface) and last (bottom) records, 
# and add the first row to nearest_dt and the first and last to to surfbot_dt

numCasts <- length(uniqueID_list)

for(i in 1:numCasts) {
  print(i)
  temp_dt <- filter(CTD_dt, UniqueID == uniqueID_list[i])
  nearest_dt <- rbind(nearest_dt,temp_dt[1,])
  surfbot_dt <- rbind(surfbot_dt,temp_dt[1,])
  surfbot_dt <- rbind(surfbot_dt,temp_dt[nrow(temp_dt),])
}

surfbot_dt_old <- surfbot_dt
nearest_dt_old <- nearest_dt


nearest_dt <- mutate(nearest_dt, DATE = convertCTDdate(Date))
surfbot_dt$Date <- convertCTDdate(surfbot_dt$Date)

surfbot_numRows <- nrow(surfbot_dt)

newDates <- c(convertCTDdate(surfbot_dt$Date[1]))
for (i in 2:surfbot_numRows) {
  print(i)
  newDate <- convertCTDdate(surfbot_dt$Date[i])
  newDates <- append(newDates,newDate)
}

surfbot_dt$Date <- newDates
atlantis_groups_dt <- read.csv(here("inputs","atlantis_codes_svspp_survey_thresholds.csv"))

# Modify data tables for ease of use
colnames(atlantis_groups_dt)[2] <- "COMNAME"
atlantis_groups_dt <- group_by(atlantis_groups_dt,Code)

atlantis_NRHA_dt <- inner_join(NRHA_dt,atlantis_groups_dt,by="COMNAME")

atlantis_NRHA_tows_dt <- unique(select(atlantis_NRHA_dt,DATE,Start_Lat,Start_Lon))

atlantis_NRHA_dt$SurfTEMP <- NA
atlantis_NRHA_dt$BottTEMP <- NA
atlantis_NRHA_dt$SurfSALIN <- NA
atlantis_NRHA_dt$BottSALIN <- NA

# Convert date columns to date object
atlantis_NRHA_dt$DATE <- as.Date(atlantis_NRHA_dt$DATE)

numRecords <- nrow(atlantis_NRHA_dt)

for (n in 1:numRecords) {
  loc_tow <- c(atlantis_NRHA_dt$Start_Lat[n], atlantis_NRHA_dt$Start_Lon[n])
  date_tow <- as.Date(atlantis_NRHA_dt$DATE[n])
  
  min_date <- date_tow - TEMPORAL_BOUND
  max_date <- date_tow + TEMPORAL_BOUND
  
  min_lat <- atlantis_NRHA_dt$Start_Lat[n] - 0.2
  max_lat <- atlantis_NRHA_dt$Start_Lat[n] + 0.2
  min_lon <- atlantis_NRHA_dt$Start_Lon[n] - 0.2
  max_lon <- atlantis_NRHA_dt$Start_Lon[n] + 0.2
  
  surfbot_dt_filtered <- filter(surfbot_dt, Date >= min_date & Date <= max_date)
  surfbot_dt_filtered <- filter(surfbot_dt_filtered, Latitude >= min_lat & Latitude <= max_lat & Longitude >= min_lon & Longitude <= max_lon)
  print(n)
  numCasts <- nrow(surfbot_dt_filtered)
  
  if (numCasts > 0) {
    for (i in 1:numCasts) {
      loc_ctd <- c(surfbot_dt_filtered$Latitude[i], surfbot_dt_filtered$Longitude[i])
      if (isWithinBounds(loc_tow,loc_ctd,SPATIAL_BOUND)) {
        print("Within Bounds")
        atlantis_NRHA_dt$SurfTEMP[n] <- surfbot_dt_filtered$Temperature[i]
        atlantis_NRHA_dt$SurfSALIN[n] <- surfbot_dt_filtered$Salinity[i]
        i <- i + 1
        atlantis_NRHA_dt$BottTEMP[n] <- surfbot_dt_filtered$Temperature[i]
        atlantis_NRHA_dt$BottSALIN[n] <- surfbot_dt_filtered$Salinity[i]
      } else {
        i <- i + 1
      }
    }
  }
  
}

# The data includes a lot of 0's in the surface temperature data and some above 40 (104 F)
# for the surface temperature data.  This constricts the ranges to exclude 0's and temperatures above 40.

atlantis_NRHA_dt$BottTEMP[(atlantis_NRHA_dt$BottTEMP == 0) | (atlantis_NRHA_dt$BottTEMP > 40)] <- NA
atlantis_NRHA_dt$SurfTEMP[(atlantis_NRHA_dt$SurfTEMP == 0) | (atlantis_NRHA_dt$SurfTEMP > 40)] <- NA

atlantis_NRHA_summary_table <- group_by(atlantis_NRHA_dt,Code,SEASON)
atlantis_NRHA_summary_table_SEASON <- summarise(atlantis_NRHA_summary_table, min_bottom_temp = min(BottTEMP, na.rm=T), max_bottom_temp = max(BottTEMP, na.rm=T),
                                  min_surface_temp = min(SurfTEMP,na.rm=T), max_surface_temp = max(SurfTEMP,na.rm=T),
                                  min_bottom_sal = min(BottSALIN,na.rm=T), max_bottom_sal = max(BottSALIN,na.rm=T),
                                  min_surface_sal = min(SurfSALIN,na.rm=T), max_surface_sal = max(SurfSALIN,na.rm=T))


atlantis_NRHA_summary_table_SPECIES <- group_by(atlantis_NRHA_dt,Code)
atlantis_NRHA_summary_table_SPECIES <- summarise(atlantis_NRHA_summary_table_SPECIES, min_bottom_temp = min(BottTEMP, na.rm=T), max_bottom_temp = max(BottTEMP, na.rm=T),
                                   min_surface_temp = min(SurfTEMP,na.rm=T), max_surface_temp = max(SurfTEMP,na.rm=T),
                                   min_bottom_sal = min(BottSALIN,na.rm=T), max_bottom_sal = max(BottSALIN,na.rm=T),
                                   min_surface_sal = min(SurfSALIN,na.rm=T), max_surface_sal = max(SurfSALIN,na.rm=T))

# Removes infinity values from the outputs and sets to NA
atlantis_NRHA_summary_table_SEASON$min_bottom_sal[(atlantis_NRHA_summary_table_SEASON$min_bottom_sal == 'Inf') | (atlantis_NRHA_summary_table_SEASON$min_bottom_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SEASON$max_bottom_sal[(atlantis_NRHA_summary_table_SEASON$max_bottom_sal == 'Inf') | (atlantis_NRHA_summary_table_SEASON$max_bottom_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SEASON$min_surface_sal[(atlantis_NRHA_summary_table_SEASON$min_surface_sal == 'Inf') | (atlantis_NRHA_summary_table_SEASON$min_surface_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SEASON$max_surface_sal[(atlantis_NRHA_summary_table_SEASON$max_surface_sal == 'Inf') | (atlantis_NRHA_summary_table_SEASON$max_surface_sal == '-Inf')] <- 'NA'

atlantis_NRHA_summary_table_SPECIES$min_bottom_sal[(atlantis_NRHA_summary_table_SPECIES$min_bottom_sal == 'Inf') | (atlantis_NRHA_summary_table_SPECIES$min_bottom_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SPECIES$max_bottom_sal[(atlantis_NRHA_summary_table_SPECIES$max_bottom_sal == 'Inf') | (atlantis_NRHA_summary_table_SPECIES$max_bottom_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SPECIES$min_surface_sal[(atlantis_NRHA_summary_table_SPECIES$min_surface_sal == 'Inf') | (atlantis_NRHA_summary_table_SPECIES$min_surface_sal == '-Inf')] <- 'NA'
atlantis_NRHA_summary_table_SPECIES$max_surface_sal[(atlantis_NRHA_summary_table_SPECIES$max_surface_sal == 'Inf') | (atlantis_NRHA_summary_table_SPECIES$max_surface_sal == '-Inf')] <- 'NA'


write.csv(atlantis_NRHA_summary_table_SEASON,here("thresholds","seasonal_thresholds_NRHA_ecomon_t7_d1.csv"),row.names=FALSE)
write.csv(atlantis_NRHA_summary_table_SPECIES,here("thresholds","group_thresholds_NRHA_ecomon_t7_d1.csv"),row.names=FALSE)