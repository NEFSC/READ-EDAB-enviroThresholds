#Cursory analysis of literature review screening data
library(dplyr)

data.orig = read.csv(here::here('data','screening_results.csv')) %>%
  mutate(Answers = gsub("\"","",Answers,fixed = T),
         Question.N = paste0('Q',Question.N))%>%
  select(Title, Question.N,Answers)%>%
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

