# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(dplyr)

dts_cohorts <- readRDS("/mnt/general-data/disability/create_cohort/final/analysis_cohort.rds")

low_back_pain_icds <- read.csv("~/medicaid/pain-severity/input/chronic_pain_icd10_20230216.csv") |>
  filter(grepl("low back", ICD_DESC, ignore.case = TRUE) |
         grepl("lumb", ICD_DESC, ignore.case = TRUE) |
         grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")

pain_all <- readRDS("/mnt/general-data/disability/disenrollment/tmp/pain_all.rds")

# add in pain categories
pain_all_adj <- 
  pain_all |>
  left_join(low_back_pain_icds |> select(pain_cat = PAIN_CAT,
                                        dgcd = ICD9_OR_10)) |>
  filter(!is.na(pain_cat)) |>
  select(BENE_ID, pain_cat, dgcd_dt, period) |>
  distinct()

pain_df <-
  pain_all_adj |>
  left_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_cal_end_dt)) 

pain_df <- pain_df |>
  group_by(BENE_ID) |>
  summarise(low_back_washout_cal = as.numeric(any(dgcd_dt >= washout_start_dt & dgcd_dt <= washout_cal_end_dt)))

pain_df <- pain_df |>
  filter(low_back_washout_cal == 1)

pain_df <- pain_df |>
  left_join(dts_cohorts)

saveRDS(pain_df, "/mnt/general-data/disability/low-back-therapies/exclusion/low_back_cohort.rds")
