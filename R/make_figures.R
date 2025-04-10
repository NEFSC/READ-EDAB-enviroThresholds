library(here)
library(dplyr)
library(ggridges)
library(ggplot2)

remove_tails <- function(dt,CODES,pct) {
  
  
  numCodes <- length(CODES)
  
  new_dt <- filter(dt,Code==CODES[1])
  observations <- nrow(new_dt)
  toRemove <- round(observations * pct)
  tail_remove <- observations - toRemove + 1
  new_dt <- new_dt[-(tail_remove:observations)]
  new_dt <- new_dt[-(1:toRemove)]
  
  for (i in 2:numCodes) {
    temp_dt <- filter(dt,Code == CODES[i])
    observations <- nrow(temp_dt)
    toRemove <- round(observations * pct)
    tail_remove <- observations - toRemove + 1
    temp_dt <- temp_dt[-(tail_remove:observations)]
    temp_dt <- temp_dt[-(1:toRemove)]
    new_dt <- bind_rows(new_dt,temp_dt,id=NULL)
  }
  return(new_dt)
}

remove_high_tail <- function(dt,CODES,pct) {
  
  
  numCodes <- length(CODES)
  
  new_dt <- filter(dt,Code==CODES[1])
  observations <- nrow(new_dt)
  toRemove <- round(observations * pct)
  tail_remove <- observations - toRemove + 1
  new_dt <- new_dt[-(tail_remove:observations)]

  for (i in 2:numCodes) {
    temp_dt <- filter(dt,Code == CODES[i])
    observations <- nrow(temp_dt)
    toRemove <- round(observations * pct)
    tail_remove <- observations - toRemove + 1
    temp_dt <- temp_dt[-(tail_remove:observations)]
    new_dt <- bind_rows(new_dt,temp_dt,id=NULL)
  }
  return(new_dt)
}

create_min_max_table <- function(dt,CODES) {
  numCodes <- length(CODES)
  
  new_dt <- filter(dt,Code==CODES[1])
  numRows <- nrow(new_dt)
  new_dt <- new_dt[c(1,numRows)]
  
  for (i in 2:numCodes) {
    temp_dt <- filter(dt,Code == CODES[i])
    numRows <- nrow(temp_dt)
    temp_dt <- temp_dt[c(1,numRows)]
    new_dt <- bind_rows(new_dt,temp_dt,id=NULL)
  }
  return(new_dt)
}

atlantis_group_dt <- read.csv(here("inputs","functional_groups_atlantis.csv"))
atlantis_group_dt <- select(atlantis_group_dt,Code,LongName)

survDT <- readRDS(here("data","surveyPull.rds"))
survdat_dt <-survDT$survdat
survdat_dt$BOTTEMP[(survdat_dt$BOTTTEMP == 0) | (survdat_dt$BOTTTEMP > 40)] <- NA
survdat_dt$SURFTEMP[(survdat_dt$SURFTEMP == 0) | (survdat_dt$SURFTEMP > 40)] <- NA

atlantis_groups <- read.csv(here("inputs","atlantis_codes_svspp_survey_thresholds.csv"))

atlantis_group_table <- group_by(atlantis_groups,Code)

density_ridges_dt <- select(survdat_dt,SVSPP,SEASON,SURFTEMP,BOTTEMP,ABUNDANCE,BIOMASS)
density_ridges_dt <- mutate(density_ridges_dt,surftemp_Bin = trunc(SURFTEMP))
density_ridges_dt <- mutate(density_ridges_dt,bottemp_Bin = trunc(BOTTEMP))
density_ridges_dt <- mutate(density_ridges_dt, presence = 1)
density_ridges_dt <- full_join(density_ridges_dt, atlantis_group_table)
density_ridges_dt <-left_join(density_ridges_dt,atlantis_group_dt, by="Code")


density_ridges_bottemp_dt <-select(density_ridges_dt,SVSPP,SEASON,BOTTEMP,bottemp_Bin,ABUNDANCE,BIOMASS,presence,Code,Name,LongName)
density_ridges_surftemp_dt <-select(density_ridges_dt,SVSPP,SEASON,SURFTEMP,surftemp_Bin,ABUNDANCE,BIOMASS,presence,Code,Name,LongName)
density_ridges_bottemp_dt <- density_ridges_bottemp_dt[complete.cases(density_ridges_bottemp_dt$BOTTEMP),]
density_ridges_surftemp_dt <- density_ridges_surftemp_dt[complete.cases(density_ridges_surftemp_dt$SURFTEMP),]
density_ridges_bottemp_dt <- density_ridges_bottemp_dt[complete.cases(density_ridges_bottemp_dt$Code),]
density_ridges_surftemp_dt <- density_ridges_surftemp_dt[complete.cases(density_ridges_surftemp_dt$Code),]

density_ridges_surftemp_dt <- density_ridges_surftemp_dt[order(-LongName,SURFTEMP)]
density_ridges_bottemp_dt <- density_ridges_bottemp_dt[order(-LongName,BOTTEMP)]

CODES <- unique(density_ridges_surftemp_dt$Code)
PELAGICS <- c('ANC', 'BIL', 'BFT', 'BPF', 'BUT', 'FDE', 'HER', 'ISQ', 'LSQ', 'MAK', 'MEN', 'NSH', 'OSH', 'POR', 'PSH', 'TUN')
DEMERSALS <- CODES[!CODES %in% PELAGICS]

demersal_dt <- filter(density_ridges_bottemp_dt, Code %in% DEMERSALS)
pelagic_dt <- filter(density_ridges_surftemp_dt, Code %in% PELAGICS)

demersal_95pct <- remove_tails(demersal_dt,DEMERSALS,pct = 0.025)
pelagic_95pct <- remove_tails(pelagic_dt,PELAGICS,pct = 0.025)

demersal_90pct <- remove_tails(demersal_dt,DEMERSALS,pct = 0.05)
pelagic_90pct <- remove_tails(pelagic_dt,PELAGICS,pct = 0.05)

demersal_98pct_high <- remove_high_tail(demersal_dt,DEMERSALS,pct = 0.025)
demersal_95pct_high <- remove_high_tail(demersal_dt,DEMERSALS,pct = 0.05)
pelagic_98pct_high <- remove_high_tail(pelagic_dt,PELAGICS,pct = 0.025)
pelagic_95pct_high <- remove_high_tail(pelagic_dt,PELAGICS,pct = 0.05)

min_max_demersal <- create_min_max_table(demersal_95pct_high,DEMERSALS)
min_max_pelagic <- create_min_max_table(pelagic_95pct_high,PELAGICS)

write.csv(min_max_demersal,here("thresholds","atlantis_demersal_survdat_95pct_high.csv"))
write.csv(min_max_pelagic,here("thresholds","atlantis_pelagic_survdat_95pct_high.csv"))

demersal_98pct_mutated <- mutate (demersal_98pct_high, MED = )

ggplot(demersal_98pct_high, aes(x = bottemp_Bin, y = LongName)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

ggplot(pelagic_98pct_high, aes(x = surftemp_Bin, y = LongName)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges()

temp_dt <- filter(demersal_dt,Code == 'WTF')