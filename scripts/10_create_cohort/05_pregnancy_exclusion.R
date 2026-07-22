# -------------------------------------
# Script: 05_pregnancy_exclusion
# Author: Anton Hung
# Purpose: Exclude those who had any pregnancy related ICD, CPT, or Revenue center codes in the washout period.
# Notes: 
# -------------------------------------
library(tidyverse)
library(fst)
library(lubridate)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# codes come from the Maternal and Infant Health (MIH) Reference Codes list. Accessed July 2026.
codes <- read.csv("~/medicaid/low-back-therapies/data/public/mih_pregnancy_codes.csv")

# 1. ICD-10 diagnosis and procedure codes (Applies to IP file)
pregnancy_ip_dgns <- codes |>  filter(Type == "ICD-10-CM") |>  pull(Code)

pregnancy_ip_pcs <- codes |>  filter(Type == "ICD-10-PCS") |>  pull(Code)

# 2. CPT and HCPCS Procedure Codes (Applies to line-level OT file)
pregnancy_ot <- codes |>  filter(Type %in% c("CPT", "HCPCS Level II")) |>  pull(Code)

# 3. Revenue Center Codes
pregnancy_rev <- codes |>  filter(Type == "UBREV") |>  pull(Code)

# TAF inpatient header file
iph <- open_iph() |>
  inner_join(cohort, by = "BENE_ID") |>
  mutate(SRVC_BGN_DT = ifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |>
  filter(SRVC_BGN_DT <= washout_end_dt, SRVC_BGN_DT >= washout_start_dt)|>
  collect() #|>
  # group_by(SUBMTG_STATE_CD, CLM_ID) %>%  # Using state and claim ID
  # slice_max(order_by = as.numeric(IP_VRSN), n = 1, with_ties = FALSE) %>%
  # ungroup() |>
  # filter(!CLM_TYPE_CD %in% c("2", "4", "B", "D", "V", "X"))
  
# TAF outpatient line file
otl <- open_otl() |>
  inner_join(cohort, by = "BENE_ID") |>
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)) |>
  filter(LINE_SRVC_BGN_DT <= washout_end_dt, LINE_SRVC_BGN_DT >= washout_start_dt)|>
  collect() #|>
  # group_by(SUBMTG_STATE_CD, CLM_ID) %>%  # Using state and claim ID
  # slice_max(order_by = as.numeric(OT_VRSN), n = 1, with_ties = FALSE) %>%
  # ungroup() |>
  # filter(!CLM_TYPE_CD %in% c("2", "4", "B", "D", "V", "X"))

# TAF outpatient header file
oth <- open_oth() |>
  inner_join(cohort, by = "BENE_ID") |>
  mutate(SRVC_BGN_DT = ifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |>
  filter(SRVC_BGN_DT <= washout_end_dt, SRVC_BGN_DT >= washout_start_dt)|>
  collect()

iph_vars <- c("BENE_ID", "ADMTG_DGNS_CD", paste0("DGNS_CD_",1:12),paste0("PRCDR_CD_",1:6))
              
otl_vars <- c("BENE_ID", "LINE_PRCDR_CD", "REV_CNTR_CD")

oth_vars <- c("BENE_ID", "DGNS_CD_1", "DGNS_CD_2")

# identify inpatient claims on the basis of ICD-CM and ICD-PCS codes
iph_pregnancy <- select(iph, all_of(iph_vars)) |>
  filter(if_any(contains("DGNS_CD"), ~ .x %in% pregnancy_ip_dgns) |
           if_any(contains("PRCDR_CD"), ~ .x %in% pregnancy_ip_pcs)) |>
  mutate(exclusion_pregnancy = 1) |>
  select(BENE_ID, exclusion_pregnancy)
print(nrow(iph_pregnancy))

# identify outpatient claims on the basis of CPT codes and revenue codes
otl_pregnancy <- select(otl, all_of(otl_vars)) %>%
  filter(if_any(contains("LINE_PRCDR_CD"), ~ .x %in% pregnancy_ot) |
           REV_CNTR_CD %in% pregnancy_rev) |>
  mutate(exclusion_pregnancy = 1) %>%
  select(BENE_ID, exclusion_pregnancy)
print(nrow(otl_pregnancy))

# identify outpatient claims on the basis of ICD-CM codes
oth_pregnancy <- select(oth, all_of(oth_vars)) |>
  filter(if_any(contains("DGNS_CD"), ~ .x %in% pregnancy_ip_dgns)) |>
  mutate(exclusion_pregnancy = 1) %>%
  select(BENE_ID, exclusion_pregnancy)
print(nrow(oth_pregnancy))

pregnancy_exclusion <- bind_rows(iph_pregnancy, otl_pregnancy, oth_pregnancy) %>% distinct()

pregnancy_exclusion <- cohort |>
  left_join(pregnancy_exclusion) |>
  mutate(exclusion_pregnancy = replace_na(exclusion_pregnancy,0)) |>
  select(BENE_ID, exclusion_pregnancy)

write_data(pregnancy_exclusion, "pregnancy_exclusion.fst", file.path(drv_root, "exclusion"))
