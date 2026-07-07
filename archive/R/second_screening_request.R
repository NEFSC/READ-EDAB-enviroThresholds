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
           Common.Name != 'atlantic salmon')
  # group_by(Common.Name, Effect.Type)%>%
  # summarise(Count = n())%>%
  # tidyr::spread('Effect.Type','Count')

# Manually remove European ones under Location = 'Other'
salmon = results %>%
  filter(Effect.Type %in% c('Survival','Habitat','Stress')
         & Priority == 'high'
         & Location %in% c("Northeast US","Canada","Other")
         & Common.Name == 'atlantic salmon')
  # group_by(Common.Name, Effect.Type)%>%
  # summarise(Count = n())%>%
  # tidyr::spread('Effect.Type','Count')
  
second.screen.out = bind_rows(spp.effect.NE,salmon)

write.csv(second.screen.out,here::here('data','second_screening_request_fulltext.csv'),row.names = F)
