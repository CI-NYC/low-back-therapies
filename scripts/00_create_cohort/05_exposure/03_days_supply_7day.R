# -------------------------------------
# Script: 04_days_supply.R
# Author: Nick Williams
# Purpose: Calculate the nonoverlapping total days supply of opioids (i.e., a value bounded between 1 and 91)
# Notes: Modified from https://github.com/CI-NYC/medicaid-treatments-oud-risk/blob/main/scripts/01_create_treatments/02_06mo/09_treatment_proportion_days_covered.R
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)

source("~/medicaid/low-back-therapies/R/helpers.R")

# load cohort and opioid data
cohort <- load_data("pain_washout_continuous_enrollment_dts_7day_gap.fst", file.path(drv_root, "exclusion"))
opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  left_join(cohort) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  arrange(BENE_ID, treatment_start_dt) |> 
  mutate(treatment_end_dt = pmin(treatment_end_dt + 1, exposure_end_dt)) |>
  select(BENE_ID, rx_start=treatment_start_dt, rx_end=treatment_end_dt)



days_supply <- function(data) {
  dur <- 0
  rx_int <- with(data, interval(rx_start, rx_end))
  current_int <- rx_int[1]
  for (i in 1:nrow(data)) {
    check <- intersect(current_int, rx_int[i + 1])
    if (is.na(check)) {
      # if they don't intersect, add the duration of the first interval
      dur <- dur + as.duration(current_int)
      current_int <- rx_int[i + 1]
    } else {
      # if they do intersect, then update current interval as the union
      current_int <- union(current_int, rx_int[i + 1])
    }
  }
  time_length(dur, "days")
}


plan(multisession, workers = 10)

days <- foreach(id = unique(opioids$BENE_ID), 
                .combine = "c",
                .options.future = list(chunk.size = 1e4)) %dofuture% {
                  fsubset(opioids, BENE_ID %==% id) |> 
                    days_supply()
                }

plan(sequential)

opioids <- 
  fselect(opioids, BENE_ID) |> 
  funique() |>
  fmutate(exposure_days_supply = days)

write_data(opioids, "exposure_days_supply_7day_gap.fst", file.path(drv_root, "treatment"))
