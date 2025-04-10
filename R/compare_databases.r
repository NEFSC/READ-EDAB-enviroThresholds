library(tidyverse)
library(here)
library(ggplot2)

gf_mp_species <- c('COD','HAD','YTF','POL','PLA','WTF','WHK','WIF','RED','HAL','WPF','OPT','WOL')

lookup_table <- read.csv(here("inputs","atlantis_codes_spp_dem_pel.csv"))
atlantis_NRHA.dt <- read.csv(here("thresholds","atlantis_group_thresholds_NRHA.csv"))
atlantis_survdat.dt <- read.csv(here("thresholds","atlantis_group_thresholds_survdat.csv"))
atlantis_ecomon_t14_d5.dt <- read.csv(here("thresholds","seasonal_thresholds_NRHA_ecomon_t14_d5.csv"))
atlantis_ecomon_t14_d1.dt <- read.csv(here("thresholds","seasonal_thresholds_NRHA_ecomon_t14_d1.csv"))
atlantis_ecomon_t7_d1.dt <- read.csv(here("thresholds","seasonal_thresholds_NRHA_ecomon_t7_d1.csv"))
atlantis_lit_review.dt <- read.csv(here("thresholds","atlantis_group_thresholds_lit_review.csv"))

atlantis_lit_review.dt <- mutate(atlantis_lit_review.dt, across(where(is.character), ~na_if(., "na")))
atlantis_lit_review.dt <- transform(atlantis_lit_review.dt,min_bottom_temp=as.numeric(min_bottom_temp))
atlantis_lit_review.dt <- transform(atlantis_lit_review.dt,max_bottom_temp=as.numeric(max_bottom_temp))
atlantis_lit_review.dt <- transform(atlantis_lit_review.dt,min_surface_temp=as.numeric(min_surface_temp))
atlantis_lit_review.dt <- transform(atlantis_lit_review.dt,max_surface_temp=as.numeric(max_surface_temp))

atlantis_survdat_only.dt <- anti_join(atlantis_survdat.dt, atlantis_NRHA.dt, by="Code")
atlantis_survdat.dt <- semi_join(atlantis_survdat.dt, atlantis_NRHA.dt, by="Code")
atlantis_NRHA_only.dt <- anti_join(atlantis_NRHA.dt, atlantis_survdat.dt, by="Code")
atlantis_NRHA.dt <- semi_join(atlantis_NRHA.dt, atlantis_survdat.dt, by="Code")


atlantis_ecomon_t14_d5.dt <- subset(atlantis_ecomon_t14_d5.dt, select = -SEASON)
atlantis_ecomon_t14_d1.dt <- subset(atlantis_ecomon_t14_d1.dt, select = -SEASON)
atlantis_ecomon_t7_d1.dt <- subset(atlantis_ecomon_t7_d1.dt, select = -SEASON)

atlantis_ecomon_t14_d5.dt <- group_by(atlantis_ecomon_t14_d5.dt,Code)
atlantis_ecomon_t14_d5.dt <- summarise(atlantis_ecomon_t14_d5.dt, min_bottom_temp = min(min_bottom_temp, na.rm=T), max_bottom_temp = max(max_bottom_temp, na.rm=T),
                                   min_surface_temp = min(min_surface_temp,na.rm=T), max_surface_temp = max(max_surface_temp,na.rm=T),
                                   min_bottom_sal = min(min_bottom_sal,na.rm=T), max_bottom_sal = max(max_bottom_sal,na.rm=T),
                                   min_surface_sal = min(min_surface_sal,na.rm=T), max_surface_sal = max(max_surface_sal,na.rm=T))

atlantis_ecomon_t14_d5_only.dt <- anti_join(atlantis_ecomon_t14_d5.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t14_d5.dt <- semi_join(atlantis_ecomon_t14_d5.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t14_d5.dt <- filter(atlantis_ecomon_t14_d5.dt,Code != 'SAL')

atlantis_ecomon_t14_d1.dt <- group_by(atlantis_ecomon_t14_d1.dt,Code)
atlantis_ecomon_t14_d1.dt <- summarise(atlantis_ecomon_t14_d1.dt, min_bottom_temp = min(min_bottom_temp, na.rm=T), max_bottom_temp = max(max_bottom_temp, na.rm=T),
                                       min_surface_temp = min(min_surface_temp,na.rm=T), max_surface_temp = max(max_surface_temp,na.rm=T),
                                       min_bottom_sal = min(min_bottom_sal,na.rm=T), max_bottom_sal = max(max_bottom_sal,na.rm=T),
                                       min_surface_sal = min(min_surface_sal,na.rm=T), max_surface_sal = max(max_surface_sal,na.rm=T))

atlantis_ecomon_t14_d1_only.dt <- anti_join(atlantis_ecomon_t14_d1.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t14_d1.dt <- semi_join(atlantis_ecomon_t14_d1.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t14_d1.dt <- filter(atlantis_ecomon_t14_d1.dt,Code != 'SAL')



atlantis_ecomon_t7_d1.dt <- group_by(atlantis_ecomon_t7_d1.dt,Code)
atlantis_ecomon_t7_d1.dt <- summarise(atlantis_ecomon_t7_d1.dt, min_bottom_temp = min(min_bottom_temp, na.rm=T), max_bottom_temp = max(max_bottom_temp, na.rm=T),
                                       min_surface_temp = min(min_surface_temp,na.rm=T), max_surface_temp = max(max_surface_temp,na.rm=T),
                                       min_bottom_sal = min(min_bottom_sal,na.rm=T), max_bottom_sal = max(max_bottom_sal,na.rm=T),
                                       min_surface_sal = min(min_surface_sal,na.rm=T), max_surface_sal = max(max_surface_sal,na.rm=T))

atlantis_ecomon_t7_d1_only.dt <- anti_join(atlantis_ecomon_t7_d1.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t7_d1.dt <- semi_join(atlantis_ecomon_t7_d1.dt, atlantis_survdat.dt, by="Code")
atlantis_ecomon_t7_d1.dt <- filter(atlantis_ecomon_t7_d1.dt,Code != 'SAL')

demersal_species.dt <- (filter(lookup_table, PorD == "D"))
pelagic_species.dt <- (filter(lookup_table, PorD == "P"))

atlantis_survdat_pelagic.dt <- left_join(pelagic_species.dt, atlantis_survdat.dt)
atlantis_survdat_demersal.dt <- left_join(demersal_species.dt, atlantis_survdat.dt)

atlantis_survdat_demersal.dt <- select(atlantis_survdat_demersal.dt, Code, CommonName, min_bottom_temp, max_bottom_temp)
atlantis_survdat_pelagic.dt <- select(atlantis_survdat_pelagic.dt, Code, CommonName, min_surface_temp, max_surface_temp)

colnames(atlantis_survdat_demersal.dt) <- c("Code", "CommonName", "min_temp","max_temp")
colnames(atlantis_survdat_pelagic.dt) <- c("Code", "CommonName", "min_temp","max_temp")

atlantis_survdat_combined.dt <- full_join(atlantis_survdat_demersal.dt,atlantis_survdat_pelagic.dt)

atlantis_NRHA_pelagic.dt <- left_join(pelagic_species.dt, atlantis_NRHA.dt)
atlantis_NRHA_demersal.dt <- left_join(demersal_species.dt, atlantis_NRHA.dt)

atlantis_NRHA_demersal.dt <- select(atlantis_NRHA_demersal.dt, Code, CommonName, min_bottom_temp, max_bottom_temp)
atlantis_NRHA_pelagic.dt <- select(atlantis_NRHA_pelagic.dt, Code, CommonName, min_surface_temp, max_surface_temp)

colnames(atlantis_NRHA_demersal.dt) <- c("Code", "CommonName", "min_temp","max_temp")
colnames(atlantis_NRHA_pelagic.dt) <- c("Code", "CommonName", "min_temp","max_temp")

atlantis_NRHA_combined.dt <- full_join(atlantis_NRHA_demersal.dt,atlantis_NRHA_pelagic.dt)

atlantis_lit_review_demersal.dt <- left_join(demersal_species.dt, atlantis_lit_review.dt)
atlantis_lit_review_pelagic.dt <- left_join(pelagic_species.dt, atlantis_lit_review.dt)

atlantis_lit_review_demersal.dt <- select(atlantis_lit_review_demersal.dt, Code, CommonName, min_bottom_temp, max_bottom_temp)
atlantis_lit_review_pelagic.dt <- select(atlantis_lit_review_pelagic.dt, Code, CommonName, min_surface_temp, max_surface_temp)

colnames(atlantis_lit_review_demersal.dt) <- c("Code", "CommonName", "min_temp","max_temp")
colnames(atlantis_lit_review_pelagic.dt) <- c("Code", "CommonName", "min_temp","max_temp")

atlantis_lit_review_combined.dt <- full_join(atlantis_lit_review_demersal.dt,atlantis_lit_review_pelagic.dt)


#atlantis_NRHA.dt <- filter(atlantis_NRHA.dt, Code %in% gf_mp_species)
#atlantis_survdat.dt <- filter(atlantis_survdat.dt, Code %in% gf_mp_species)

#atlantis_ecomon_t7_d1.dt <- filter(atlantis_ecomon_t7_d1.dt, Code %in% gf_mp_species)
#atlantis_ecomon_t14_d1.dt <- filter(atlantis_ecomon_t14_d1.dt, Code %in% gf_mp_species)
#atlantis_ecomon_t14_d5.dt <- filter(atlantis_ecomon_t14_d5.dt, Code %in% gf_mp_species)



ggplot(atlantis_survdat_combined.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_temp,ymax=max_temp),linetype=0,color="red")+
  geom_point(aes(y=min_temp),size=3,color="red")+
  geom_point(aes(y=max_temp),size=3,color="red")+
  geom_point(data = atlantis_NRHA_combined.dt,aes(y=min_temp),size=3,color="blue")+
  geom_point(data = atlantis_NRHA_combined.dt,aes(y=max_temp),size=3,color="blue")+
  geom_point(data = atlantis_lit_review_combined.dt,aes(y=min_temp),size=3,color="green")+
  geom_point(data = atlantis_lit_review_combined.dt,aes(y=max_temp),size=3,color="green")+
  labs(title="Temperature Thresholds",x="Atlantis Code",y="Temperature") +
  theme_bw()

ggplot(atlantis_ecomon_t14_d5.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_bottom_temp,ymax=max_bottom_temp),linetype=0,color="red")+
  geom_point(aes(y=min_bottom_temp),size=3,color="red")+
  geom_point(aes(y=max_bottom_temp),size=3,color="red")+
  geom_point(data = atlantis_ecomon_t14_d1.dt,aes(y=min_bottom_temp),size=3,color="blue")+
  geom_point(data = atlantis_ecomon_t14_d1.dt,aes(y=max_bottom_temp),size=3,color="blue")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=min_bottom_temp),size=3,color="green")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=max_bottom_temp),size=3,color="green")+
  
  theme_bw()

ggplot(atlantis_survdat.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_bottom_temp,ymax=max_bottom_temp),linetype=0,color="red")+
  geom_point(aes(y=min_bottom_temp),size=3,color="red")+
  geom_point(aes(y=max_bottom_temp),size=3,color="red")+
  geom_point(data = atlantis_NRHA.dt,aes(y=min_bottom_temp),size=3,color="blue")+
  geom_point(data = atlantis_NRHA.dt,aes(y=max_bottom_temp),size=3,color="blue")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=min_bottom_temp),size=3,color="green")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=max_bottom_temp),size=3,color="green")+
  
  theme_bw()


ggplot(atlantis_survdat.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_bottom_sal,ymax=max_bottom_sal),linetype=0,color="red")+
  geom_point(aes(y=min_bottom_sal),size=3,color="red")+
  geom_point(aes(y=max_bottom_sal),size=3,color="red")+
  geom_point(data = atlantis_NRHA.dt,aes(y=min_bottom_sal),size=3,color="blue")+
  geom_point(data = atlantis_NRHA.dt,aes(y=max_bottom_sal),size=3,color="blue")+

  theme_bw()


ggplot(atlantis_survdat.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_surface_sal,ymax=max_surface_sal),linetype=0,color="red")+
  geom_point(aes(y=min_surface_sal),size=3,color="red")+
  geom_point(aes(y=max_surface_sal),size=3,color="red")+
  geom_point(data = atlantis_NRHA.dt,aes(y=min_surface_sal),size=3,color="blue")+
  geom_point(data = atlantis_NRHA.dt,aes(y=max_surface_sal),size=3,color="blue")+
  
  theme_bw()

ggplot(atlantis_ecomon_t14_d5.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_bottom_sal,ymax=max_bottom_sal),linetype=0,color="red")+
  geom_point(aes(y=min_bottom_sal),size=3,color="red")+
  geom_point(aes(y=max_bottom_sal),size=3,color="red")+
  geom_point(data = atlantis_ecomon_t14_d1.dt,aes(y=min_bottom_sal),size=3,color="blue")+
  geom_point(data = atlantis_ecomon_t14_d1.dt,aes(y=max_bottom_sal),size=3,color="blue")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=min_bottom_sal),size=3,color="green")+
  geom_point(data = atlantis_ecomon_t7_d1.dt,aes(y=max_bottom_sal),size=3,color="green")+
  
  theme_bw()

ggplot(atlantis_survdat.dt, aes(x=Code))+
  geom_linerange(aes(ymin=min_bottom_sal,ymax=max_bottom_sal),linetype=0,color="red")+
  geom_point(aes(y=min_bottom_sal),size=3,color="red")+
  geom_point(aes(y=max_bottom_sal),size=3,color="red")+
  geom_point(data = atlantis_NRHA.dt,aes(y=min_bottom_sal),size=3,color="blue")+
  geom_point(data = atlantis_NRHA.dt,aes(y=max_bottom_sal),size=3,color="blue")+
#  geom_point(data = atlantis_ecomon_t14_d5.dt,aes(y=min_bottom_sal),size=3,color="green")+
#  geom_point(data = atlantis_ecomon_t14_d5.dt,aes(y=max_bottom_sal),size=3,color="green")+
  
  theme_bw()
