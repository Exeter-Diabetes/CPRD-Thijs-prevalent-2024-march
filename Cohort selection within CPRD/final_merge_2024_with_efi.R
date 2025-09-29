############################################################################################

# Setup
library(tidyverse)
library(aurum)
library(EHRBiomarkr)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024", cprdConf = "~/.aurum.yaml")

index_date <- as.Date("2024-03-01")

analysis = cprd$analysis("all")
diabetes_cohort <- diabetes_cohort %>% analysis$cached("diabetes_cohort")


# add biomarkers, medications, ckd_stages tables
analysis_prefix <- "thijs_ckd"
analysis = cprd$analysis(analysis_prefix)

medications_2024_march <- medications %>% 
  analysis$cached("medications_2024_march", unique_indexes="patid")

biomarkers_2024_march <- biomarkers_2024_march %>%
  analysis$cached("biomarkers_2024_march", unique_indexes="patid")

ckd_stages_2024_march <- ckd_stages_2024_march %>% 
  analysis$cached("ckd_stages_2024_march", unique_indexes="patid")

comorbidities_2024_march <- comorbidities_2024_march %>% 
  analysis$cached("comorbidities_2024_march", unique_indexes="patid")

efi_2024_march <- efi_2024_march %>% 
  analysis$cached("efi_2024_march", unique_indexes="patid")

# Define prevalent cohort and add in variables from other tables plus age and diabetes duration at index date
## Prevalent cohort: registered on index_date and with diagnosis at/before then and with linked HES records (and n_patid_hes<=20).

cohort_ids <- diabetes_cohort %>%
  filter(dm_diag_date_all<=index_date & 
           regstartdate<=index_date & 
           gp_record_end>=index_date & 
           (is.na(death_date) | death_date>=index_date) #& with_hes==1
         ) %>%
  select(patid) %>%
  analysis$cached("cohort_ids", unique_indexes="patid")

cohort_ids %>% count()



final_merge <- cohort_ids %>%
  left_join(diabetes_cohort, by="patid") %>%
  left_join(biomarkers_2024_march, by="patid") %>%
  left_join(ckd_stages_2024_march, by="patid") %>%
  left_join(medications_2024_march, by="patid") %>%
  left_join(comorbidities_2024_march, by ="patid") %>%
  left_join(efi_2024_march, by ="patid") %>%
  mutate(index_date_age=datediff(index_date, dob)/365.25,
         index_date_dm_dur_all=datediff(index_date, dm_diag_date_all)/365.25) %>%
  relocate(c(index_date_age, index_date_dm_dur_all), .before=gender)

## Join with main dataset

final_merge_2024_march <- final_merge %>%
  analysis$cached("cross_section_2024_march_with_efi", unique_indexes="patid")

cross_section_2024_march <- collect(final_merge_2024_march %>% mutate(patid = as.character(patid)))
glimpse(cross_section_2024_march)

setwd("C:/Users/tj358/OneDrive - University of Exeter/CPRD/2024/Raw data/")
today <- as.character(Sys.Date(), format="%Y%m%d")
save(cross_section_2024_march, file=paste0(today, "_cross_section_2024_march.Rda"))
