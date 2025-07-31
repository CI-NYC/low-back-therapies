# -------------------------------------
# Script: 02_cohort_mme_join.R
# Author: Nick Williams
# Purpose: Calculate MME/strength per day for opioids in exposure period
# Notes: Modified from: 
#   - https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/01_00_treatment_dose_mme.R
#   - https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/01_01_treatment_dose_mme.R
# -------------------------------------

library(tidyverse)
library(fst)
library(yaml)
library(lubridate)
library(data.table)
library(arrow)

source("~/medicaid/low-back-therapies/R/helpers.R")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

#  Read in OTL (Other services line) 
otl <- open_otl()

# load cohort
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |> as.data.table()
cohort[, let(exposure_end_dt = pain_diagnosis_dt + days(90))] # because diagnosis dt is included in exposure period, total length = 91 days

mme <- readRDS("~/medicaid/low-back-therapies/data/public/opioids_mme.rds")
bup_list <- read_fst("~/medicaid/low-back-therapies/data/public/bup_list.fst")

rxl_opioids <-
  rxl |>
  filter(NDC %in% mme$NDC) |>
  select(BENE_ID, CLM_ID, RX_FILL_DT, contains("ndc"), DAYS_SUPPLY) |>
  collect() |>
  left_join(mme)

rxl_opioids <- left_join(cohort, rxl_opioids)

otl_opioids <- 
  otl |>
  filter(NDC %in% mme$NDC) |>
  mutate(LINE_SRVC_BGN_DT = ifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)) |>
  select(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, contains("NDC")) |>
  collect() |>
  left_join(mme)

otl_opioids <- left_join(cohort, otl_opioids)

rxl_opioids <- 
  rxl_opioids |> 
  filter((RX_FILL_DT >= pain_diagnosis_dt) & 
           (RX_FILL_DT <= exposure_end_dt))

otl_opioids <- 
  otl_opioids |> 
  filter((LINE_SRVC_BGN_DT >= pain_diagnosis_dt) & 
           (LINE_SRVC_BGN_DT <= exposure_end_dt))

# calculate strength per day in Milligram Morphine Equivalent (MME) units
# no caps on number of pills, days supply, and pills per day
rxl_opioids <-
  rxl_opioids |>
  drop_na(BENE_ID) |>
  mutate(number_pills = case_when(!is.na(NDC_QTY) ~ abs(NDC_QTY),
                                  TRUE ~ 1),
         days_supply = case_when(!is.na(DAYS_SUPPLY) ~ DAYS_SUPPLY,
                                 TRUE ~ 1), # best assumption we can make if missing a days supply var
         pills_per_day = number_pills / days_supply,
         strength = parse_number(numeratorValue),
         strength_per_day = strength * pills_per_day,
         mme_strength_per_day = strength_per_day * conversion#, 
         # mme_strength_per_day = pmin(mme_strength_per_day, quantile(mme_strength_per_day, 0.99))
         )

# keep only relevant vars for RXL opioids
rxl_opioids <-
  rxl_opioids |>
  select(BENE_ID,
         CLM_ID,
         pain_diagnosis_dt, 
         # last_treatment_dt,
         exposure_end_dt,
         opioid,
         NDC,
         dose_form,
         days_supply,
         pills_per_day,
         strength,
         strength_per_day,
         mme_strength_per_day,
         days_supply,
         rx_start_dt = RX_FILL_DT) |>
  mutate(rx_end_dt = rx_start_dt + days_supply - 1) |>
  arrange(BENE_ID, rx_start_dt, opioid)

# filter to opioids for pain, calculate strength per day in Milligram Morphine Equivalent (MME) units
otl_opioids <-
  otl_opioids |>
  drop_na(BENE_ID) |>
  mutate(strength = parse_number(numeratorValue),
         # we assume all OTL opioids are one day supply (outpatient)
         mme_strength_per_day = strength * conversion) 

# keep only relevant vars for OTL opioids
otl_opioids <-
  otl_opioids |>
  select(BENE_ID,
         CLM_ID,
         pain_diagnosis_dt, 
         # last_treatment_dt,
         exposure_end_dt,
         NDC,
         dose_form,
         opioid,
         strength,
         mme_strength_per_day,
         rx_start_dt = LINE_SRVC_BGN_DT) |>
  mutate(rx_end_dt = rx_start_dt) |> # 1 day supply assumption
  arrange(BENE_ID, rx_start_dt, opioid)

opioids <- rxl_opioids |>
  bind_rows(rxl_opioids, otl_opioids) |>
  unique() |> 
  mutate(days_supply = replace_na(days_supply, 1),
         treatment_name = "Opioid")

# remove opioids (buprenorphine) if they belong to our list of buprenorphine codes that are specifically used to treat MOUD
opioids_wo_moud_bup <- opioids |>
  filter(!NDC %in% bup_list$ndc)

write_data(opioids_wo_moud_bup, "exposure_period_opioids.fst", file.path(drv_root, "treatment"))
