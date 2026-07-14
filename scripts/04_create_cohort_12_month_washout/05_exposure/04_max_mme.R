# -------------------------------------
# Script: 03_max_mme.R
# Author: Nick Williams
# Purpose: Calculate max daily dose (MME) for opioids prescribed during the exposure period
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/02_06mo/10_mme/02_treatment_max_daily_dose_mme.R
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)

source("~/medicaid/low-back-therapies/R/helpers.R")

# load cohort and opioid data
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root_12_month_washout, "modified_variables"))

opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(-washout_start_dt) |>
  right_join(cohort) |> # keep people who passed enrollment criteria in the cohort
  filter(treatment_start_dt >= day0_dt,
         treatment_start_dt <= exposure_end_dt) |>
  mutate(treatment_end_dt = as.Date(pmin(treatment_end_dt, exposure_end_dt)))

setDT(opioids)
setkey(opioids, BENE_ID)

opioids <- opioids[, .(BENE_ID, day0_dt, 
                       treatment_start_dt, treatment_end_dt, NDC, opioid, mme_strength_per_day)]

# Calculate max daily dose -----------------------------------------------------

opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]
# opioids <- opioids[1:100000,]

calculate_max_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  to_modify[, .(date = seq(treatment_start_dt, treatment_end_dt, by = "1 day"), NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
  ][, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), 
    by = .(date)
  ][, .(exposure_max_daily_dose_mme = max(total_mme_strength))]
}

plan(multisession, workers = 8)

# Apply function
out <- foreach(data = opioids$data, 
               id = opioids$BENE_ID, 
               .combine = "rbind",
               .options.future = list(chunk.size = 5e4)) %dofuture% {
                 out <- calculate_max_daily_dose(data)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

testthat::test_that(
  "All observations have a max daily MME",
  testthat::expect_false(any(is.na(out$exposure_max_daily_dose_mme)))
)

write_data(out, "exposure_max_daily_dose_mme.fst", file.path(drv_root_12_month_washout, "modified_variables"))