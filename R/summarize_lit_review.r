library(dplyr)
library(here)

lit_dt <- read.csv(here("thresholds","MS_screening_analysis.csv"))

lit_dt <- select(lit_dt, Title,Scientific.Name,Age.Group,Location,contains("Temp"),contains("Sal"))
species_dt <- read.csv(here::here("data","Species_list.csv"))
colnames(species_dt)[2] <- "Scientific.Name"
lit_dt <- union(lit_dt,species_dt)
for (n in 5:19) {
  lit_dt[,n] <- as.numeric(lit_dt[,n])
}
lit_dt_pa <- lit_dt
for (r in 1:309) {
  for (c in 5:19) {
    if (!(is.na(lit_dt[r,c]))) {
      lit_dt_pa[r,c] <- 1
    } else {
      lit_dt_pa[r,c] <- 0
    } 
    
  }
}

lit_dt_pa <- mutate(lit_dt_pa,temp_threshold = lit_dt_pa$Temp_Obs_Max + lit_dt_pa$Temp_Obs_Mean + lit_dt_pa$Temp_Obs_Min + lit_dt_pa$Temp_Stress_Max + lit_dt_pa$Temp_Stress_Min + lit_dt_pa$Temp_Lethal_Min + lit_dt_pa$Temp_Lethal_Mean + lit_dt_pa$Temp_Lethal_Max)
lit_dt_pa <- mutate(lit_dt_pa,sal_threshold = lit_dt_pa$Salinity_Obs_Max + lit_dt_pa$Salinity_Obs_Mean + lit_dt_pa$Salinity_Obs_Min + lit_dt_pa$Salinity_Stress_Max + lit_dt_pa$Salinity_Stress_Min + lit_dt_pa$Salinity_Lethal_Min + lit_dt_pa$Salinity_Lethal_Max)

for (r in 1:309) {
  if (lit_dt_pa$temp_threshold[r] > 0) {
      lit_dt_pa$temp_threshold[r] <- 1
  }
  if (lit_dt_pa$sal_threshold[r] > 0) {
    lit_dt_pa$sal_threshold[r] <- 1
  }
}

lit_dt_pa_total <- select(lit_dt_pa,Title,Scientific.Name,contains("Temp"),contains("Sal"))
lit_dt_pa_region <- select(lit_dt_pa,Title,Scientific.Name,Location,contains("Temp"),contains("Sal"))
lit_dt_pa_age <- select(lit_dt_pa,Title,Scientific.Name,Age.Group,contains("Temp"),contains("Sal"))

lit_dt_pa_cnt <- group_by(lit_dt_pa,Scientific.Name)
lit_dt_pa_total_cnt <- group_by(lit_dt_pa_total,Scientific.Name)
lit_dt_pa_region_cnt <- group_by(lit_dt_pa_region,Scientific.Name)
lit_dt_pa_age_cnt <- group_by(lit_dt_pa_age,Scientific.Name)

lit_dt_pa_total_cnt <- select(lit_dt_pa,Scientific.Name,contains("Temp"),contains("Sal"))
lit_dt_pa_total_cnt <- lit_dt_pa_total_cnt %>% group_by(Scientific.Name) %>% summarise_all(sum) %>% ungroup()

lit_dt_pa_region_cnt <- select(lit_dt_pa_region,Scientific.Name,Location,contains("Temp"),contains("Sal"))
lit_dt_pa_region_cnt <- lit_dt_pa_region_cnt %>% group_by(Scientific.Name,Location) %>% summarise_all(sum) %>% ungroup()

lit_dt_pa_age_cnt <- select(lit_dt_pa_age,Scientific.Name,Age.Group,contains("Temp"),contains("Sal"))
lit_dt_pa_age_cnt <- lit_dt_pa_age_cnt %>% group_by(Scientific.Name,Age.Group) %>% summarise_all(sum) %>% ungroup()

lit_dt_pa_all_cnt <- select(lit_dt_pa,Scientific.Name,Location,Age.Group,contains("Temp"),contains("Sal"))
lit_dt_pa_all_cnt <- lit_dt_pa_cnt %>% group_by(Scientific.Name,Location,Age.Group) %>% summarise_all(sum) %>% ungroup()

write.csv(lit_dt_pa_total_cnt,here("thresholds","literature_review_summary_total.csv"))
write.csv(lit_dt_pa_age_cnt,here("thresholds","literature_review_summary_age.csv"))
write.csv(lit_dt_pa_region_cnt,here("thresholds","literature_review_summary_region.csv"))
write.csv(lit_dt_pa_all_cnt,here("thresholds","literature_review_summary_all.csv"))
