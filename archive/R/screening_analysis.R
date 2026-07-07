#Cursory analysis of literature review screening data
library(dplyr)

data.orig = read.csv(here::here('data','screening_results.csv')) %>%
  mutate(Answers = gsub("\"","",Answers,fixed = T),
         Question.N = paste0('Q',Question.N))%>%
  select(-Question)%>%
  tidyr::spread(Question.N, Answers)

data.include = data.orig %>%
  filter(Q2 == 'Yes, include the reference')

data.var = data.include %>%
  group_by(Q2,Q1)%>%
  summarise(N = n(),
            pct = N/nrow(data.include))

data.spp = data.include %>%
  group_by(Q2,Q3)%>%
  summarise(N = n(),
            pct = N/nrow(data.include))

data.low.N = data.spp %>%
  filter(N < 20)

data.high.N = data.spp %>%
  filter(N >= 20)

data.request.now = data.orig %>%
  filter(Q2 == 'Yes, include the reference' & Q3 %in% data.low.N$Q3)

data.request.later = data.orig %>%
  filter(Q2 == 'Yes, include the reference' & Q3 %in% data.high.N$Q3)

write.csv(data.request.now,here::here('data','fulltext_request_preliminary.csv'),row.names =F)
write.csv(data.request.later,here::here('data','second_screening_request.csv'),row.names =F)
