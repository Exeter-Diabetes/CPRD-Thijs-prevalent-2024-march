library(tidyverse)
library(tableone)

rm(list=ls())

setwd("C:/Users/tj358/OneDrive - University of Exeter/CPRD/2024/Raw data/")
# UPDATE DATE OF DATA DOWNLOAD
today = "2025-07-10"
load(paste0(today, "_cross_section_2024_march.Rda"))


# remove create logical variables for drugs of interest
data <- cross_section_2024_march %>% 
  mutate(
    pre_index_date_glp1=!is.na(pre_2024_march_earliest_glp1),
    pre_index_date_sglt2=!is.na(pre_2024_march_earliest_sglt2),
    pre_index_date_finerenone=!is.na(pre_2024_march_latest_finerenone),
    pre_index_date_statin=!is.na(pre_2024_march_latest_statins),
    pre_index_date_ace=!is.na(pre_2024_march_latest_ace_inhibitors),
    pre_index_date_arb=!is.na(pre_2024_march_latest_arb),
    pre_index_date_ace_or_arb=ifelse(pre_index_date_ace == T | pre_index_date_arb == T, T, F),
    pre_index_date_mra=!is.na(pre_2024_march_latest_ksparing_diuretics),
    pre_index_date_ascvd=ifelse(pre_index_date_angina==1 | pre_index_date_ihd==1 | pre_index_date_myocardialinfarction==1 | pre_index_date_pad==1 | pre_index_date_revasc==1 | pre_index_date_stroke==1 | pre_index_date_tia==1, T, F),
    pre_index_date_heartfailure=as.logical(pre_index_date_heartfailure),
    pre_index_date_genital_infection=as.logical(pre_index_date_genital_infection),
    malesex=ifelse(gender==1, T, F),
    ethnicity_5cat=case_when(ethnicity_5cat==0 ~"White",
                                     ethnicity_5cat==1 ~"South Asian",
                                     ethnicity_5cat==2 ~"Black",
                                     ethnicity_5cat==3 ~"Other",
                                     ethnicity_5cat==4 ~"Mixed"), 
    uacr=ifelse(!is.na(preacr), preacr, ifelse(!is.na(preacr_from_separate), preacr_from_separate, NA)),
    pre_index_date_ins=!is.na(pre_2024_march_latest_insulin),
    pre_index_date_dpp4=!is.na(pre_2024_march_latest_dpp4),
    pre_index_date_mfn=!is.na(pre_2024_march_latest_mfn),
    pre_index_date_su=!is.na(pre_2024_march_latest_su),
    pre_index_date_tzd=!is.na(pre_2024_march_earliest_tzd),
    ncurrtx=pre_index_date_dpp4+pre_index_date_glp1+pre_index_date_mfn+pre_index_date_su+pre_index_date_sglt2+pre_index_date_tzd+pre_index_date_ins, 
    oha=(pre_index_date_dpp4+pre_index_date_glp1+pre_index_date_mfn+pre_index_date_su+pre_index_date_sglt2+pre_index_date_tzd>0)
  ) %>% 
  # retain those with type 2 diabetes only on treatment
  filter(diabetes_type == "type 2" & ncurrtx > 0) %>% 
  # remove those with ckd stage 5
  filter(pre_index_date_ckd5_code == 0 & preckdstage != "stage_5")

# percentage missing uACR:
(data %>% filter(is.na(uacr)) %>% nrow() / data %>% nrow() * 100) %>% round()

# percentage missing eGFR:
(data %>% filter(is.na(preegfr)) %>% nrow() / data %>% nrow() * 100) %>% round()

# store separate cohort of people with missing uACR
missing_uacr <- data %>% 
  filter(!is.na(preegfr) & is.na(uacr))

# for main analyses, retain those with non-missing eGFR and uACR only
data <- data %>% 
  filter(!is.na(preegfr) & !is.na(uacr)) %>%
  # code those without CKD (eGFR >60 and uACR < 3) as ckd stage 0
  mutate(
    preckdstage = ifelse(preckdstage %in% c("stage_1", "stage_2") & uacr < 3, "stage_0", as.character(preckdstage)),
    any_ckd = ifelse(preckdstage != "stage_0", T, F)
  ) %>% filter(any_ckd == T)

# omit variables giving limited information
data <- data %>% select(
  -contains("pre_2024_march"), -contains("post_2024_march"), -contains("post_index_date"), -contains("datediff"), -contains("pre_index_date_latest"),
  -contains("pre_index_date_earliest"), -contains("code_count"), -contains("dm_diag_"), 
  -c("dob", "ins_in_1_year", "has_insulin", "with_hes", "current_oha"), 
  -ends_with("date")
)

setwd("C:/Users/tj358/OneDrive - University of Exeter/CPRD/2024/Raw data/")
save(data, file=paste0(today, "_cross_section_2024_march_for_Ebi.Rda"))
