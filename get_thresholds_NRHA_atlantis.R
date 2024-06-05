# Use pulled survdat.rds file to get environmental thresholds for Atlantis groups
library(dplyr)
library(here)

# Get output data
NRHA_dt <-read.csv(here("data","NRHA_data.csv"))
atlantis_groups_dt <- read.csv(here("inputs","atlantis_codes_svspp_survey_thresholds.csv"))

# Modify data tables for ease of use
colnames(atlantis_groups_dt)[2] <- "COMNAME"
atlantis_groups_dt <- group_by(atlantis_groups_dt,Code)

atlantis_NRHA_dt <- inner_join(NRHA_dt,atlantis_groups_dt,by="COMNAME")

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

write.csv(atlantis_NRHA_summary_table_SEASON,here("thresholds","atlantis_seasonal_thresholds_NRHA.csv"),row.names=FALSE)
write.csv(atlantis_NRHA_summary_table_SPECIES,here("thresholds","atlantis_group_thresholds_NRHA.csv"),row.names=FALSE)