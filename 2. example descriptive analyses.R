#load necessary packages
library(tidyverse)
library(tableone)

#clear current global environment
rm(list=ls())

#set working directory - this should be the folder name the data are saved in
setwd("C:/Users/tj358/OneDrive - University of Exeter/CPRD/2024/Raw data/")
today = "2025-07-10" #version of dataset
load(paste0(today, "_cross_section_2024_march_for_Ebi.Rda"))

#create variables for treatment eligibility
data <- data %>% mutate(
  #those eligible for ACE/ARB treatment: anyone with uACR >=3mg/mmol who "tolerates" treatment. Pragmatically, I've used a threshold of SBP 110mmHg (at this level I would feel comfortable adding low-dose ACEi/ARB)
  eligible_aceorarb = ifelse(uacr >= 3 & presbp >= 110, T, F),
  #anyone with T2D and CKD should be considered for a statin
  eligible_statin = T,
  #SGLT2: there are different grades of recommendations. Those with uacr >=30 should be offered an SGLT2 for kidney protection. Those with uacr >=3 should be considered for an SGLT2 for kidney protection, and for those with a eGFR <90 it is an option.
  macroalbuminuria = ifelse(uacr>=30, T, F),
  eligible_sglt2_kidney = ifelse(preegfr <90 | uacr >=3, T, F),
  #separately, people should be offered SGLT2 treatment for heart failure protection if they have a diagnosis of atheroslerotic cardiovascular disease or heart failure.
  eligible_sglt2_hf = ifelse(pre_index_date_ascvd == T | pre_index_date_heartfailure == T, T, F), #At present, I am ignoring the recommendation to consider SGLT2 treatment for QRISK 10% or higher.
  #finerenone: CKD stage 3/4 with eGFR >=25 and uACR >=3. Another requirement is that potassium should be below 5. If NA, assume high (5.5 mmol/L as random number)
  prepotassium_noNA = ifelse(is.na(prepotassium), 5.5, prepotassium), 
  eligible_finerenone = ifelse((preegfr >=25 & preegfr <60 | preckdstage %in% c("stage_3a", "stage_3b", "stage_4")) & uacr >=3 & (prepotassium_noNA <= 5 | pre_index_date_finerenone == T), T, F),
) 


#define the variables that should be displayed in the table
vars = c("index_date_age", "malesex", "ethnicity_5cat", "index_date_dm_dur_all", "imd_decile", "prebmi", "presbp", "predbp", "prepotassium", "prehba1c",
         "preckdstage", "preegfr", "uacr", "pre_index_date_ascvd", "pre_index_date_heartfailure", "pre_index_date_genital_infection", "pre_index_date_ace_or_arb", "pre_index_date_statin", "pre_index_date_sglt2", "pre_index_date_glp1", "pre_index_date_finerenone", "pre_index_date_mra")

#specify which variables are categorical
factors = c("malesex", "ethnicity_5cat", "imd_decile", "pre_index_date_ace_or_arb", "pre_index_date_statin", "pre_index_date_sglt2", "pre_index_date_glp1", "pre_index_date_finerenone", "pre_index_date_mra", "preckdstage", "pre_index_date_ascvd", "pre_index_date_heartfailure", "pre_index_date_genital_infection")

#specify which variables are non-normally distributed
nonnormal = c("uacr", "index_date_dm_dur_all")

#create table
table <- CreateTableOne(vars = vars, 
                        strata = "macroalbuminuria", #variable to stratify data by
                        data = data,  
                        factorVars = factors,
                        test = F)

#print table in R
tabforprint <- print(table, nonnormal = nonnormal, quote = FALSE, noSpaces = TRUE, printToggle = T)
