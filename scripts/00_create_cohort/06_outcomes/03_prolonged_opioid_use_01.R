# -------------------------------------
# Script: 04_filter_opioid_ndc.R
# Author: Nick Williams
# Purpose: find opioid fills for 1 year after follow-up
# Notes:
# -------------------------------------
# 

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/low-back-therapies/R/helpers.R")

ndc <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("pain_washout_continuous_enrollment_with_exposures.fst", file.path(drv_root, "treatment")) |>
  mutate(followup_start_dt = exposure_period_end_dt + days(1), # start of follow-up
         followup_12mos_dt = exposure_period_end_dt + days(366)) |> # 1 year follow-up
  select(BENE_ID, followup_start_dt, followup_12mos_dt)

# filter rxl and otl files ------------------------------------------------

ndc_opioids <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_opioids.rds")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find opioids in OTL following diagnosis
otl <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT)
  ) |> 
  filter((LINE_SRVC_BGN_DT >= followup_start_dt) & 
           (LINE_SRVC_BGN_DT <= followup_12mos_dt), 
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  select(BENE_ID, rx_start_dt = LINE_SRVC_BGN_DT, rx_end_dt = LINE_SRVC_BGN_DT) |>
  distinct()

otl <- collect(otl) |> as.data.table()

# Find opioids in RXL following diagnosis
rxl <- 
  rxl |>
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= followup_start_dt) & 
           (RX_FILL_DT <= followup_12mos_dt),
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  distinct()

rxl <- collect(rxl) |> 
  mutate(rx_end_dt = RX_FILL_DT + days(DAYS_SUPPLY - 1)) |>
  select(BENE_ID, rx_start_dt = RX_FILL_DT, rx_end_dt) |>
  as.data.table()


opioid_claims <- unique(rbind(otl, rxl)) |>
  mutate(treatment_name = "opioid")


write_data(opioid_claims, "opioid_dts_12mos.fst", file.path(drv_root,"treatment"))
