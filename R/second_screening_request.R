library(dplyr)
results = read.csv(here::here('data','second_screening_complete.csv'))%>%
  mutate(Common.Name = tolower(Common.Name),
         Priority = tolower(Priority))

results %>%
  group_by(Common.Name)%>%
  summarise(Count = n())

#This one w/o Salmon
spp.effect.NE = results %>%
  filter(Effect.Type %in% c('Survival','Habitat','Stress') &
           Priority != 'low' &
           Location %in% c("Northeast US","Canada","Other") &
           Common.Name != 'atlantic salmon')%>%
  group_by(Common.Name, Effect.Type)%>%
  summarise(Count = n())%>%
  tidyr::spread('Effect.Type','Count')

# Manually remove European ones under Location = 'Other'
salmon = results %>%
  filter(Effect.Type %in% c('Survival','Habitat','Stress')
         & Priority == 'high'
         & Location %in% c("Northeast US","Canada","Other")
         & Common.Name == 'atlantic salmon')%>%
  group_by(Common.Name, Effect.Type)%>%
  summarise(Count = n())%>%
  tidyr::spread('Effect.Type','Count')
  

# spp.effect.noaqua = results %>%
#   filter(Effect.Type %in% c('Survival','Growth','Habitat','Reproduction','Stress') & Priority != 'low' & Application != "Aquaculture")%>%
#   group_by(Common.Name, Effect.Type)%>%
#   summarise(Count = n())%>%
#   tidyr::spread('Effect.Type','Count')
# 
# spp.priority = results %>%
#   group_by(Common.Name,Priority)%>%
#   summarise(Count = n())%>%
#   tidyr::spread('Priority','Count')
