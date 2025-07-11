
# Extracts dates for diabetes medication and BP medication scripts in GP records

# Merges with index date

# Then finds earliest pre-index date, latest pre-index date, and earliest post-index date script for each drug

############################################################################################

# Setup
library(tidyverse)
library(aurum)
rm(list=ls())

cprd = CPRDData$new(cprdEnv = "diabetes-jun2024", cprdConf = "C:/Users/tj358/OneDrive - University of Exeter/CPRD/aurum.yaml")
codesets = cprd$codesets()
codes_2020 = codesets$getAllCodeSetVersion(v = "31/10/2021")
codes_2024 = codesets$getAllCodeSetVersion(v = "01/06/2024")

# set MYSQL table prefix
analysis_prefix <-"thijs_ckd"
analysis = cprd$analysis(analysis_prefix)

# Get index date
index_date <- as.Date("2024-03-01")
index_date_string <- "2024_march"

### NEED TO DELETE LAST TABLE AT BOTTOM THEN RERUN FULL SCRIPT ###

############################################################################################

# Define medications

meds <- c("ace_inhibitors",
          "arb",
          # "beta_blockers",
          # "calcium_channel_blockers",
          # "thiazide_diuretics",
          # "loop_diuretics",
          "ksparing_diuretics" ,
          "finerenone",
          "statins"
)

# OHA and insulin processed separately as want to keep extra variables


############################################################################################

# Pull out raw script instances and cache with 'all_patid' prefix
## Some of these already exist from previous analyses

analysis = cprd$analysis("all_patid")

for (i in meds) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_prodcodes")
  
  data <- cprd$tables$drugIssue %>%
    inner_join(codes_2020[[i]], by="prodcodeid") %>%
    select(patid, date=issuedate) %>%
    analysis$cached(raw_tablename, indexes=c("patid", "date"))
  
  assign(raw_tablename, data)
  
}


raw_oha_prodcodes <- cprd$tables$drugIssue %>%
  inner_join(cprd$tables$ohaLookup, by="prodcodeid") %>%
  analysis$cached("raw_oha_prodcodes", indexes=c("patid", "issuedate"))

raw_insulin_prodcodes <- cprd$tables$drugIssue %>%
  inner_join(codes_2024$insulin, by="prodcodeid") %>%
  analysis$cached("raw_insulin_prodcodes", indexes=c("patid", "issuedate"))


############################################################################################

# Clean scripts (remove if before DOB or after lcd/deregistration/death), then merge with drug start dates
## NB: for biomarkers, cleaning and combining with drug start dates is 2 separate steps with caching between, but as there are fewer cleaning steps for meds I have made this one step here

# Add OHA classes and insulin to meds
oha <- c("acarbose", "dpp4", "glinide", "glp1", "mfn", "sglt2", "su", "tzd", "insulin", "gipglp1")
sema_subclass <- c("ldsema", "hdsema", "osema", "sema_query")
oha <- c(oha, sema_subclass)
meds <- c(oha, meds)

# For OHAs: clean and then separate by drug class first

clean_oha_prodcodes <- raw_oha_prodcodes %>%
  inner_join(cprd$tables$validDateLookup, by="patid") %>%
  filter(issuedate>=min_dob & issuedate<=gp_end_date) %>%
  rename(date=issuedate) %>%
  analysis$cached("clean_oha_prodcodes", indexes=c("patid", "issuedate"))

for (i in oha) {
  
  print (i)
  clean_tablename <- paste0("clean_", i, "_prodcodes")
  
  if (i %in% sema_subclass) {
    clean_prodcodes <- clean_oha_prodcodes %>%
      filter(drug_substance_2 == i) %>%
      select(patid, date)
  } else {
  clean_prodcodes <- clean_oha_prodcodes %>%
    filter(drug_class_1 == i) %>%
    select(patid, date)
  }
  
  assign(clean_tablename, clean_prodcodes)
  rm(clean_prodcodes)
}

# For insulin: clean and cache, then combine with insulin from oha scripts (in combo with GLP1s) first

clean_insulin_prodcodes <- raw_insulin_prodcodes %>%
  inner_join(cprd$tables$validDateLookup, by="patid") %>%
  filter(issuedate>=min_dob & issuedate<=gp_ons_end_date) %>%
  rename(date=issuedate) %>%
  analysis$cached("clean_insulin_prodcodes", indexes=c("patid", "issuedate"))

# clean_insulin_prodcodes <- clean_insulin_prodcodes %>%
#   select(patid, date) %>%
#   union_all(clean_oha_prodcodes %>%
#               filter(INS==1) %>%
#               select(patid, date))




analysis = cprd$analysis(analysis_prefix)

# Clean scripts and combine with index date

for (i in meds) {
  
  print(i)
  
  if (i %in% oha) {
    
    clean_tablename <- paste0("clean_", i, "_prodcodes")
    index_date_merge_tablename <- paste0("full_", i, "_", index_date_string, "_merge")
    
    data <- get(clean_tablename) %>%
      mutate(datediff=datediff(date, index_date)) %>%
      analysis$cached(index_date_merge_tablename, indexes="patid")
    
    assign(index_date_merge_tablename, data)
    
  } else {
    
    raw_tablename <- paste0("raw_", i, "_prodcodes")
    index_date_merge_tablename <- paste0("full_", i, "_", index_date_string, "_merge")
    
    data <- get(raw_tablename) %>%
      
      inner_join(cprd$tables$validDateLookup, by="patid") %>%
 #     filter(date>=min_dob & date<=gp_ons_end_date) %>% # ONS and HES linkage still awaited
      filter(date>=min_dob & date<=gp_end_date) %>%
      select(patid, date) %>%
      
      mutate(datediff=datediff(date, index_date)) %>%
      
      analysis$cached(index_date_merge_tablename, indexes="patid")
    
    assign(index_date_merge_tablename, data)
    
  }
}


############################################################################################

# Find earliest pre-index date, latest pre-index date and first post-index date dates


medications <- cprd$tables$patient %>%
  select(patid)


for (i in meds) {
  
  print(paste("working out pre- and post- index date code occurrences for", i))
  
  index_date_merge_tablename <- paste0("full_", i, "_", index_date_string, "_merge")
  interim_medications_table <- paste0("medications_im_", i)
  pre_index_date_earliest_date_variable <- paste0("pre_", index_date_string, "_earliest_", i, "")
  pre_index_date_latest_date_variable <- paste0("pre_", index_date_string, "_latest_", i, "")
  post_index_date_date_variable <- paste0("post_", index_date_string, "_first_", i, "")
  
  pre_index_date <- get(index_date_merge_tablename) %>%
    filter(date<=index_date) %>%
    group_by(patid) %>%
    summarise({{pre_index_date_earliest_date_variable}}:=min(date, na.rm=TRUE),
              {{pre_index_date_latest_date_variable}}:=max(date, na.rm=TRUE)) %>%
    ungroup()
  
  post_index_date <- get(index_date_merge_tablename) %>%
    filter(date>index_date) %>%
    group_by(patid) %>%
    summarise({{post_index_date_date_variable}}:=min(date, na.rm=TRUE)) %>%
    ungroup()
  
  medications <- medications %>%
    left_join(pre_index_date, by="patid") %>%
    left_join(post_index_date, by="patid") %>%
    analysis$cached(interim_medications_table, unique_indexes="patid")
  
}


# Cache final version

medications_2024_march <- medications %>% analysis$cached("medications_2024_march", unique_indexes="patid")