# -------------------------------------
# Script: 04_filter_opioid_ndc.R
# Author: Nick Williams
# Purpose: Filter to observations with no opioids in washout but opioids in the exposure period
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/low-back-therapies/R/helpers.R")

# load initial continuous enrollment cohort
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

ndc_opioids <- readRDS("~/medicaid/low-back-therapies/data/public/opioids_mme.rds")

# filter rxl and otl files ------------------------------------------------

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find beneficiaries with an opioid in the washout period in OTL
otl <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT)
  ) |> 
  filter((LINE_SRVC_BGN_DT >= washout_start_dt) & 
           (LINE_SRVC_BGN_DT < pain_diagnosis_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid in the washout period in RXL
rxl <- 
  select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= washout_start_dt) & 
           (RX_FILL_DT < pain_diagnosis_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  select(BENE_ID) |> 
  distinct()

rxl <- collect(rxl) |> as.data.table()

# remove observations with opioid in washout period
remove <- rbind(otl, rxl) |> unique()

# number of patients with opioids in washout
remove |> nrow()

# cohort <- anti_join(cohort, remove)

# # Read in RXL (pharmacy line)
# rxl <- open_rxl()

# # Read in OTL (Other services line)
# otl <- open_otl()

# # Find beneficiaries with an opioid in the exposure period in OTL
# otl <-
#   select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |>
#   inner_join(cohort, by = "BENE_ID") |>
#   mutate(LINE_SRVC_BGN_DT = ifelse(
#     is.na(LINE_SRVC_BGN_DT),
#     LINE_SRVC_END_DT,
#     LINE_SRVC_BGN_DT
#   )) |>
#   filter((LINE_SRVC_BGN_DT > pain_diagnosis_dt) &
#            (LINE_SRVC_BGN_DT <= exposure_end_dt),
#          NDC %in% ndc_opioids$NDC) |>
#   select(BENE_ID) |>
#   distinct()

# otl <- collect(otl) |> as.data.table()

# # Find beneficiaries with an opioid in the exposure period in RXL
# rxl <-
#   select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, NDC) |>
#   inner_join(cohort, by = "BENE_ID") |>
#   filter((RX_FILL_DT > pain_diagnosis_dt) &
#            (RX_FILL_DT <= exposure_end_dt),
#          NDC %in% ndc_opioids$NDC) |>
#   select(BENE_ID) |>
#   distinct()

# rxl <- collect(rxl) |> as.data.table()

# # Combine and export
# keep <- unique(rbind(otl, rxl))
# # keep <- unique(rxl)
# cohort <- unique(left_join(keep, cohort))

cohort <- cohort |>
  mutate(exclusion_opioid_naive = ifelse(BENE_ID %in% remove$BENE_ID, 1, 0)) |>
  select(BENE_ID, exclusion_opioid_naive)

write_data(cohort, "pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
