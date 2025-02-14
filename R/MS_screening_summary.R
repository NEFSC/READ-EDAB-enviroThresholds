#script to show literature review screening summary
library(dplyr)

data = read.csv(here::here('data','MS_screening_analysis_cleaned.csv'))

data.long = data %>%
  select(-scientific.name)%>%
  tidyr::gather(variable,value,-study.type,-application, -effect.type,-common.name,-age.group,-location,-title)%>%
  mutate(env.var = ifelse(grepl('temp',variable),'temp','salt'),
         is.min = ifelse(grepl('min',variable),T,F),
         is.max = ifelse(grepl('max',variable),T,F),
         is.string = ifelse(grepl(',|-|_',variable),T,F))

data.spp.n = data.long %>%
  filter(!is.na(value))%>%
  mutate(age.group = ifelse(age.group %in% c('Egg','Juvenile','Larval'),'Juvenile','Adult'))%>%
  group_by(common.name, age.group)%>%
  summarise(n = n())%>%
  tidyr::spread(age.group,n)
  
ad.temp.US = data.long %>%
  filter(location %in% c('Northeast US','Southeast US', 'Other', 'Canada') & 
           effect.type == 'Survival' &
           age.group == 'Adult' &
           env.var == 'temp',
         !is.na(value))%>%
  distinct(title,common.name)%>%
  group_by(common.name)%>%
  summarise(n = n())

ad.temp.surv = data %>%
  filter(age.group == 'Adult' & effect.type == 'Survival')%>%
  group_by(common.name)%>%
  summarise(opt.temp.min.mean = mean(opt.temp.min,na.rm=T),
            opt.temp.min.sd = sd(opt.temp.min,na.rm=T),
            opt.temp.max.mean = mean(opt.temp.max,na.rm=T),
            opt.temp.max.sd = sd(opt.temp.max,na.rm=T),
            stress.temp.min.mean = mean(stress.temp.min,na.rm=T),
            stress.temp.min.sd = sd(stress.temp.min,na.rm=T),
            stress.temp.max.mean = mean(stress.temp.max,na.rm=T),
            stress.temp.max.sd = sd(stress.temp.max,na.rm=T),
            lethal.temp.min.mean = mean(lethal.temp.min,na.rm=T),
            lethal.temp.min.sd = sd(lethal.temp.min,na.rm=T),
            lethal.temp.max.mean =mean(lethal.temp.max,na.rm=T),
            lethal.temp.max.sd = sd(lethal.temp.max,na.rm=T))
