# -------------------------------------
# Script: 06_mediator_prescribers_per_month.R
# Author: Sarah Forrest
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(data.table)
library(fst)

source("~/medicaid/low-back-therapies/R/helpers.R")

rxh <- open_rxh()

# RXL
opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatments"))

rxl_claims <- open_rxl() |>
  filter(CLM_ID %in% opioids$CLM_ID) |>
  select(CLM_ID) |>
  collect()

rxl <- opioids |>
  filter(CLM_ID %in% rxl_claims$CLM_ID)

# Read in cohort and dates
cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, pain_diagnosis_dt) |>
  as.data.table()

# Opioids
op <- readRDS("~/medicaid/low-back-therapies/data/public/opioids_mme.rds")

# Create prescribers dataset ---------------------------------------------------

rxl_claims <- unique(rxl$CLM_ID)

rhl_vars <- c("BENE_ID", "CLM_ID", "RX_FILL_DT", "PRSCRBNG_PRVDR_ID")

prescribers <- select(rxh, all_of(rhl_vars)) |>
  filter(CLM_ID %in% rxl_claims) |>
  rename(rx_start_dt = RX_FILL_DT) |>
  collect()
  

# Merge ------------------------------------------------------------------------

rxl <- rxl |>
  select(BENE_ID, CLM_ID, rx_start_dt, NDC)

prescribers <- 
  merge(prescribers, rxl, by = c("BENE_ID", "CLM_ID", "rx_start_dt")) |> 
  unique()

prescribers <- merge(cohort, prescribers) |> as.data.table()

# Create prescriber count variables  -------------------------------------------

# Set up month variables
prescribers[, c("month1", "month2", "month3") := 
              .(rx_start_dt %within% interval(pain_diagnosis_dt, pain_diagnosis_dt + days(30)),
                rx_start_dt %within% interval(pain_diagnosis_dt + days(31), pain_diagnosis_dt + days(60)),
                rx_start_dt %within% interval(pain_diagnosis_dt + days(61), pain_diagnosis_dt + days(90)))
              ]

# Calculate the sum of unique prescriber IDs for each month and sum variable for the 6-month period
prescribers_per_month <- 
  prescribers[, .(mediator_prescribers_month1 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month1])),
                  mediator_prescribers_month2 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month2])),
                  mediator_prescribers_month3 = sum(uniqueN(PRSCRBNG_PRVDR_ID[month3])),
                  exposure_distinct_prescribers = uniqueN(c(PRSCRBNG_PRVDR_ID[month1],
                                                           PRSCRBNG_PRVDR_ID[month2],
                                                           PRSCRBNG_PRVDR_ID[month3]
                                                           ))),
              by = BENE_ID]

# Merge with analysis cohort  --------------------------------------------------

# Right join with the analysis cohort
exposure_distinct_prescribers <- 
  merge(prescribers_per_month, cohort[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID") |>
  mutate(exposure_distinct_prescribers = ifelse(is.na(exposure_distinct_prescribers), 0, exposure_distinct_prescribers)) |>
  select(BENE_ID, exposure_distinct_prescribers)


# Save final dataset -----------------------------------------------------------
write_data(exposure_distinct_prescribers, "exposure_distinct_prescribers.fst", file.path(drv_root, "treatments"))
