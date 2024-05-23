# Use pulled survdat.rds file to get environmental thresholds for Atlantis groups
library(dplyr)
library(here)

survdat_dt <-readRDS(here("data","surveyPull.rds"))
summary_table <- group_by(survdat_dt$survdat,SVSPP,SEASON)
summary_table_SEASON <- summarise(summary_table, min_bottom_temp = min(BOTTEMP, na.rm=T), max_bottom_temp = max(BOTTEMP, na.rm=T),
                                min_surface_temp = min(SURFTEMP,na.rm=T), max_surface_temp = max(SURFTEMP,na.rm=T),
                                          min_bottom_sal = min(BOTSALIN,na.rm=T), max_bottom_sal = max(BOTSALIN,na.rm=T),
                                          min_surface_sal = min(SURFSALIN,na.rm=T), max_surface_sal = max(SURFSALIN,na.rm=T))

summary_table_SPECIES <- group_by(survdat_dt$survdat,SVSPP)
summary_table_SPECIES <- summarise(summary_table_SPECIES, min_bottom_temp = min(BOTTEMP, na.rm=T), max_bottom_temp = max(BOTTEMP, na.rm=T),
                                  min_surface_temp = min(SURFTEMP,na.rm=T), max_surface_temp = max(SURFTEMP,na.rm=T),
                                  min_bottom_sal = min(BOTSALIN,na.rm=T), max_bottom_sal = max(BOTSALIN,na.rm=T),
                                  min_surface_sal = min(SURFSALIN,na.rm=T), max_surface_sal = max(SURFSALIN,na.rm=T))

atlantis_groups <- read.csv(here("inputs","atlantis_codes_svspp_survey_thresholds.csv"))

atlantis_group_table <- group_by(atlantis_groups,Code)
combined_table_SEASON <- full_join(summary_table_SEASON, atlantis_group_table)
combined_table_SEASON <- filter(combined_table_SEASON, !is.na(Code))

combined_table_SEASON <- group_by(combined_table_SEASON,Code,SEASON)
summary_table_ATLANTIS_SEASON <- summarise(combined_table_SEASON,min_bottom_temp = min(min_bottom_temp), max_bottom_temp = max(max_bottom_temp),
                                           min_surface_temp = min(min_surface_temp), max_surface_temp = max(max_surface_temp),
                                           min_bottom_sal = min(min_bottom_sal), max_bottom_sal = max(max_bottom_sal),
                                           min_surface_sal = min(min_surface_sal), max_surface_sal = max(max_surface_sal))
summary_table_ATLANTIS_SEASON <- filter(summary_table_ATLANTIS_SEASON,!is.na(SEASON))
summary_table_ATLANTIS_SEASON$min_bottom_sal[(summary_table_ATLANTIS_SEASON$min_bottom_sal == 'Inf') | (summary_table_ATLANTIS_SEASON$min_bottom_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SEASON$max_bottom_sal[(summary_table_ATLANTIS_SEASON$max_bottom_sal == 'Inf') | (summary_table_ATLANTIS_SEASON$max_bottom_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SEASON$min_surface_sal[(summary_table_ATLANTIS_SEASON$min_surface_sal == 'Inf') | (summary_table_ATLANTIS_SEASON$min_surface_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SEASON$max_surface_sal[(summary_table_ATLANTIS_SEASON$max_surface_sal == 'Inf') | (summary_table_ATLANTIS_SEASON$max_surface_sal == '-Inf')] <- 'NA'

combined_table_SPECIES <- full_join(summary_table_SPECIES, atlantis_group_table)
combined_table_SPECIES <- filter(combined_table_SPECIES, !is.na(Code))
combined_table_SPECIES <- group_by(combined_table_SPECIES,Code)
summary_table_ATLANTIS_SPECIES <- summarise(combined_table_SPECIES,min_bottom_temp = min(min_bottom_temp), max_bottom_temp = max(max_bottom_temp),
                                           min_surface_temp = min(min_surface_temp), max_surface_temp = max(max_surface_temp),
                                           min_bottom_sal = min(min_bottom_sal), max_bottom_sal = max(max_bottom_sal),
                                           min_surface_sal = min(min_surface_sal), max_surface_sal = max(max_surface_sal))
summary_table_ATLANTIS_SPECIES <- mutate(summary_table_ATLANTIS_SPECIES, isNA_temp=min_bottom_temp+max_bottom_temp+
                                           min_surface_temp+max_surface_temp, isNA_sal = min_bottom_sal+max_bottom_sal+
                                           min_surface_sal+max_surface_sal)
summary_table_ATLANTIS_SPECIES <- filter(summary_table_ATLANTIS_SPECIES,(!is.na(isNA_temp) | !is.na(isNA_sal)))
summary_table_ATLANTIS_SPECIES <- select(summary_table_ATLANTIS_SPECIES,-isNA_temp,-isNA_sal)

summary_table_ATLANTIS_SPECIES$min_bottom_sal[(summary_table_ATLANTIS_SPECIES$min_bottom_sal == 'Inf') | (summary_table_ATLANTIS_SPECIES$min_bottom_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SPECIES$max_bottom_sal[(summary_table_ATLANTIS_SPECIES$max_bottom_sal == 'Inf') | (summary_table_ATLANTIS_SPECIES$max_bottom_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SPECIES$min_surface_sal[(summary_table_ATLANTIS_SPECIES$min_surface_sal == 'Inf') | (summary_table_ATLANTIS_SPECIES$min_surface_sal == '-Inf')] <- 'NA'
summary_table_ATLANTIS_SPECIES$max_surface_sal[(summary_table_ATLANTIS_SPECIES$max_surface_sal == 'Inf') | (summary_table_ATLANTIS_SPECIES$max_surface_sal == '-Inf')] <- 'NA'

write.csv(summary_table_ATLANTIS_SEASON,here("thresholds","atlantis_seasonal_thresholds.csv"),row.names=FALSE)
write.csv(summary_table_ATLANTIS_SPECIES,here("thresholds","atlantis_group_thresholds.csv"),row.names=FALSE)
