
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

# Get pointer to longitudinal CKD stage table

analysis = cprd$analysis("all_patid")

ckd_stages_from_algorithm <- ckd_stages_from_algorithm %>% analysis$cached("ckd_stages_from_algorithm")


################################################################################################################################

# Merge with index date to get CKD stages at index date


## Get index date

analysis = cprd$analysis(analysis_prefix)


# Merge with CKD stages (1 row per patid)

ckd_stage_2024_march <- cprd$tables$patient %>%
  select(patid) %>%
  left_join(ckd_stages_from_algorithm, by="patid") %>%
  mutate(preckdstage=ifelse(!is.na(stage_5) & datediff(stage_5, index_date)<=7, "stage_5",
                            ifelse(!is.na(stage_4) & datediff(stage_4, index_date)<=7, "stage_4",
                                   ifelse(!is.na(stage_3b) & datediff(stage_3b, index_date)<=7, "stage_3b",
                                          ifelse(!is.na(stage_3a) & datediff(stage_3a, index_date)<=7, "stage_3a",
                                                 ifelse(!is.na(stage_2) & datediff(stage_2, index_date)<=7, "stage_2",
                                                        ifelse(!is.na(stage_1) & datediff(stage_1, index_date)<=7, "stage_1", NA)))))),
         
         preckdstagedate=ifelse(preckdstage=="stage_5", stage_5,
                                ifelse(preckdstage=="stage_4", stage_4,
                                       ifelse(preckdstage=="stage_3b", stage_3b,
                                              ifelse(preckdstage=="stage_3a", stage_3a,
                                                     ifelse(preckdstage=="stage_2", stage_2,
                                                            ifelse(preckdstage=="stage_1", stage_1, NA)))))),
         
         preckdstagedatediff=datediff(preckdstagedate, index_date)) %>%
  
  select(patid, preckdstage, preckdstagedate, preckdstagedatediff) %>%
  
  analysis$cached("ckd_stages_2024_march", unique_indexes="patid")

