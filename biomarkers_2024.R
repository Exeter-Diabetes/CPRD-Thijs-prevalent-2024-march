
############################################################################################

# Setup
library(tidyverse)
library(aurum)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024", cprdConf = "C:/Users/tj358/OneDrive - University of Exeter/CPRD/aurum.yaml")
codesets = cprd$codesets()
codes_2024 = codesets$getAllCodeSetVersion(v = "01/06/2024")

# check codelists:
current_list <- codesets$listCodeSets()

analysis_prefix <-"thijs_ckd"

analysis = cprd$analysis(analysis_prefix)

# Get index date
index_date <- as.Date("2024-03-01")


############################################################################################

# Define biomarkers
## Keep HbA1c separate as processed differently
## If you add biomarker to the end of this list, code should run fine to incorporate new biomarker, as long as you delete final 'baseline_biomarkers' table

biomarkers <- c("weight", "height", "bmi", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", 
                "totalcholesterol", "dbp", "sbp", "acr", "albumin_urine", "creatinine_urine", "potassium"
                )


############################################################################################

# Pull out all raw biomarker values and cache

analysis = cprd$analysis("all_patid")

for (i in biomarkers) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_medcodes")
  
  if (i != "potassium") {
    data <- cprd$tables$observation %>%
      inner_join(codes_2024[[i]], by="medcodeid") %>%
      analysis$cached(raw_tablename, indexes=c("patid", "obsdate", "testvalue", "numunitid"))
  } else {
    data <- cprd$tables$observation %>% inner_join(
      readr::read_tsv(
      here::here("C:/Users/tj358/OneDrive - University of Exeter/CPRD/Aurum codelists/medcodes/exeter_medcodelist_potassium.tsv"),
      col_types = cols(.default=col_character())) %>%
        rename(medcodeid=MedCodeId) %>%
        select(medcodeid) %>%
        mutate(potassium_cat = NA), 
      by="medcodeid", copy = T) %>%
      analysis$cached(raw_tablename, indexes=c("patid", "obsdate", "testvalue", "numunitid"))
    
  }
  assign(raw_tablename, data)
  
}


# HbA1c

raw_hba1c_medcodes <- cprd$tables$observation %>%
  inner_join(codes$hba1c, by="medcodeid") %>%
  analysis$cached("raw_hba1c_medcodes", indexes=c("patid", "obsdate", "testvalue", "numunitid"))


############################################################################################

# Clean biomarkers:
## Only keep those within acceptable value limits
## Only keep those with valid unit codes (numunitid)
## If multiple values on the same day, take mean
## Remove those with invalid dates (before DOB or after LCD/death/deregistration)
### HbA1c only: remove if <1990 and assume in % and convert to mmol/mol if <=20 (https://github.com/Exeter-Diabetes/CPRD-Codelists#hba1c)


analysis = cprd$analysis("all_patid")


for (i in biomarkers) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_medcodes")
  clean_tablename <- paste0("clean_", i, "_medcodes")
  
  if (i == "potassium") {
    # potassium limits and units are not defined in the EHRBiomarkr package - define manually
    data <- get(raw_tablename) %>%
      dplyr::filter(testvalue >= 2.5 & testvalue <= 6.5) %>%
      dplyr::inner_join(
        data.frame(numunitid = c(218, 425, NA)),
        by = "numunitid",
        na_matches = "na",
        copy = TRUE
      ) %>%      
      group_by(patid,obsdate) %>%
      summarise(testvalue=mean(testvalue, na.rm=TRUE)) %>%
      ungroup() %>%
      
      inner_join(cprd$tables$validDateLookup, by="patid") %>%
      # filter(obsdate>=min_dob & obsdate<=gp_ons_end_date) %>%
      filter(obsdate>=min_dob & obsdate<=gp_end_date) %>%
      
      select(patid, date=obsdate, testvalue) %>%
      
      analysis$cached(clean_tablename, indexes=c("patid", "date", "testvalue"))
    
  } else {
  
  data <- get(raw_tablename) %>%
    clean_biomarker_values(testvalue, i) %>%
    clean_biomarker_units(numunitid, i) %>%
    
    group_by(patid,obsdate) %>%
    summarise(testvalue=mean(testvalue, na.rm=TRUE)) %>%
    ungroup() %>%
    
    inner_join(cprd$tables$validDateLookup, by="patid") %>%
#    filter(obsdate>=min_dob & obsdate<=gp_ons_end_date) %>%
    filter(obsdate>=min_dob & obsdate<=gp_end_date) %>%
    
    select(patid, date=obsdate, testvalue) %>%
    
    analysis$cached(clean_tablename, indexes=c("patid", "date", "testvalue"))
  
  }
  
  assign(clean_tablename, data)
  
}


# HbA1c

clean_hba1c_medcodes <- raw_hba1c_medcodes %>%
  
  mutate(testvalue=ifelse(testvalue<=20,((testvalue-2.152)/0.09148),testvalue)) %>%
  
  clean_biomarker_values(testvalue, "hba1c") %>%
  clean_biomarker_units(numunitid, "hba1c") %>%
  
  group_by(patid,obsdate) %>%
  summarise(testvalue=mean(testvalue, na.rm=TRUE)) %>%
  ungroup() %>%
  
  inner_join(cprd$tables$validDateLookup, by="patid") %>%
#  filter(obsdate>=min_dob & obsdate<=gp_ons_end_date & year(obsdate)>=1990) %>%
  filter(obsdate>=min_dob & obsdate<=gp_end_date & year(obsdate)>=1990) %>%
  
  select(patid, date=obsdate, testvalue) %>%
  
  analysis$cached("clean_hba1c_medcodes", indexes=c("patid", "date", "testvalue"))

# Make ACR from separate urine albumin and urine creatinine measurements on the same day
# Then clean values

clean_acr_from_separate_medcodes <- clean_albumin_urine_medcodes %>%
  inner_join((clean_creatinine_urine_medcodes %>% select(patid, creat_date=date, creat_value=testvalue)), by="patid") %>%
  filter(date==creat_date) %>%
  mutate(new_testvalue=testvalue/creat_value) %>%
  select(patid, date, testvalue=new_testvalue) %>%
  clean_biomarker_values(testvalue, "acr") %>%
  analysis$cached("clean_acr_from_separate_medcodes", indexes=c("patid", "date", "testvalue"))

biomarkers <- setdiff(biomarkers, c("albumin_urine", "creatinine_urine"))
biomarkers <- c("acr_from_separate", biomarkers)



# Make eGFR table from creatinine readings and add to list of biomarkers
## Use DOBs produced in all_t1t2_cohort script to calculate age (uses yob, mob and also earliest medcode in yob to get dob, as per https://github.com/Exeter-Diabetes/CPRD-Codelists/blob/main/readme.md#general-notes-on-implementation)
## Also need gender from Patient table for eGFR

analysis = cprd$analysis("diabetes_cohort")

dob <- dob %>% analysis$cached("dob")

analysis = cprd$analysis("all_patid")

clean_egfr_medcodes <- clean_creatinine_blood_medcodes %>%
  
  inner_join((dob %>% select(patid, dob)), by="patid") %>%
  inner_join((cprd$tables$patient %>% select(patid, gender)), by="patid") %>%
  mutate(age_at_creat=(datediff(date, dob))/365.25,
         sex=ifelse(gender==1, "male", ifelse(gender==2, "female", NA))) %>%
  select(-c(dob, gender)) %>%
  
  ckd_epi_2021_egfr(creatinine=testvalue, sex=sex, age_at_creatinine=age_at_creat) %>%
  select(-c(testvalue, sex, age_at_creat)) %>%
  
  rename(testvalue=ckd_epi_2021_egfr) %>%
  filter(!is.na(testvalue)) %>%
  analysis$cached("clean_egfr_medcodes", indexes=c("patid", "date", "testvalue"))

biomarkers <- c("egfr", biomarkers)


############################################################################################

# Combine each biomarker with index date

## Get index date

analysis = cprd$analysis(analysis_prefix)


## Merge with biomarkers and calculate date difference between biomarker and index date

for (i in biomarkers) {
  
  print(i)
  
  clean_tablename <- paste0("clean_", i, "_medcodes")
  index_date_merge_tablename <- paste0("full_", i, "_index_date_2024_merge")
  
  data <- get(clean_tablename) %>%
    mutate(datediff=datediff(date, index_date))
  
  assign(index_date_merge_tablename, data)
  
}


# HbA1c

full_hba1c_index_date_2024_merge <- clean_hba1c_medcodes %>%
  mutate(datediff=datediff(date, index_date))


############################################################################################

# Find baseline values
## Within period defined above (-2 years to +7 days for all except height)
## Then use closest date to index date
## May be multiple values; use minimum test result, except for eGFR - use maximum
## Can get duplicates where person has identical results on the same day/days equidistant from the index date - choose first row when ordered by datediff

biomarkers_2024_march <- cprd$tables$patient %>%
  select(patid)


## For all except HbA1c and height: between 2 years prior and 7 days after index date

biomarkers_no_height <- setdiff(biomarkers, "height")

for (i in biomarkers_no_height) {
  
  print(i)
  
  index_date_merge_tablename <- paste0("full_", i, "_index_date_2024_merge")
  interim_2024_biomarker_table <- paste0("biomarkers_2024_march_interim_", i)
  pre_biomarker_variable <- paste0("pre", i)
  pre_biomarker_date_variable <- paste0("pre", i, "date")
  pre_biomarker_datediff_variable <- paste0("pre", i, "datediff")
  
  
  data <- get(index_date_merge_tablename) %>%
    filter(datediff<=7 & datediff>=-730) %>%
    
    group_by(patid) %>%
    
    mutate(min_timediff=min(abs(datediff), na.rm=TRUE)) %>%
    filter(abs(datediff)==min_timediff) %>%
    
    mutate(pre_biomarker=ifelse(i=="egfr", max(testvalue, na.rm=TRUE), min(testvalue, na.rm=TRUE))) %>%
    filter(pre_biomarker==testvalue) %>%
    
    dbplyr::window_order(datediff) %>%
    filter(row_number()==1) %>%
    
    ungroup() %>%
    
    relocate(pre_biomarker, .after=patid) %>%
    relocate(date, .after=pre_biomarker) %>%
    relocate(datediff, .after=date) %>%
    
    rename({{pre_biomarker_variable}}:=pre_biomarker,
           {{pre_biomarker_date_variable}}:=date,
           {{pre_biomarker_datediff_variable}}:=datediff) %>%
    
    select(-c(testvalue, min_timediff))
  
  
  biomarkers_2024_march <- biomarkers_2024_march %>%
    left_join(data, by="patid") %>%
    analysis$cached(interim_2024_biomarker_table, unique_indexes="patid")
  
}


## Height - only keep readings at/post-index date, and find mean

height_2024 <- full_height_index_date_2024_merge %>%
  filter(datediff>=0) %>%
  group_by(patid) %>%
  summarise(height=mean(testvalue, na.rm=TRUE)) %>%
  ungroup()

biomarkers_2024_march <- biomarkers_2024_march %>%
  left_join(height_2024, by="patid")

## add previous acr value to confirm uacr result

analysis = cprd$analysis("all_patid")

# acr_temp <- acr_temp %>% analysis$cached("clean_acr_from_separate_medcodes")
# acr_temp2 <- acr_temp2 %>% analysis$cached("clean_acr_medcodes")
# 
# acr_long <- acr_temp %>% union_all(acr_temp2) %>% analysis$cached("acr_long")
# 
# analysis = cprd$analysis("thijs_ckd")
# 
# prev_acr <- biomarkers_2024_march %>%
#   select(patid, preacrdate) %>%
#   left_join(acr_long, by="patid", copy = T) %>%
#   mutate(indexdatediff=datediff(index_date, preacrdate)) %>%
#   filter(indexdatediff<=7 & indexdatediff>=-730 & indexdatediff != 0) %>% # take a second uACR value within 2 years prior / 7 days after other one; avoid one from the same date (as may be duplicate)
#   group_by(patid) %>%
#   dbplyr::window_order(indexdatediff) %>%
#   filter(row_number() ==2) %>%
#   mutate(preacr_secondvalue=testvalue) %>%
#   select(patid, preacr_secondvalue) %>%
#   analysis$cached("acr_secondvalue_2024", indexes=c("patid"))


## HbA1c: only between 6 months prior and 7 days after index date
### NB: in treatment response cohort, baseline HbA1c set to missing if occurs before previous treatment change

hba1c_2024 <- full_hba1c_index_date_2024_merge %>%
  
  filter(datediff<=7 & datediff>=-730) %>%
  
  group_by(patid) %>%
  
  mutate(min_timediff=min(abs(datediff), na.rm=TRUE)) %>%
  filter(abs(datediff)==min_timediff) %>%
  
  mutate(prehba1c=min(testvalue, na.rm=TRUE)) %>%
  filter(prehba1c==testvalue) %>%
  
  dbplyr::window_order(datediff) %>%
  filter(row_number()==1) %>%
  
  ungroup() %>%
  
  relocate(prehba1c, .after=patid) %>%
  relocate(date, .after=prehba1c) %>%
  relocate(datediff, .after=date) %>%
  
  rename(prehba1cdate=date,
         prehba1cdatediff=datediff) %>%
  
  select(-c(testvalue, min_timediff))


## Join HbA1c and prior acr to main table

analysis = cprd$analysis(analysis_prefix)

biomarkers_2024_march <- biomarkers_2024_march %>%
  left_join(hba1c_2024, by="patid") %>% 
  analysis$cached("biomarkers_2024_march", unique_indexes="patid")
