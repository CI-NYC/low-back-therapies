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
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))
opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  left_join(cohort) |>
  filter(rx_start_dt <= last_treatment_dt) |>
  arrange(BENE_ID, rx_start_dt) |> 
  mutate(rx_end_dt = pmin(rx_end_dt + 1, exposure_end_dt)) |>
  select(BENE_ID, rx_start=rx_start_dt, rx_end=rx_end_dt)
  

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

# opioids <- 
#   opioids |> 
#   mutate(rx_int = interval(rx_start_dt, pmin(rx_end_dt+1, exposure_end_dt))) |> # +1 because "as.duration", a function used below, counts the time elapsed between two dates. A prescription that starts and ends on the same day should have 1 day supply, but as.duration will return 0.
#   select(BENE_ID, NDC, opioid, rx_int) |> 
#   as_tibble() |> 
#   mutate(interval_days_supply = as.numeric(as.duration(rx_int), "days")) |> 
#   group_by(BENE_ID) |> 
#   arrange(BENE_ID, int_start(rx_int)) |> 
#   ungroup()
# 
# opioids <- 
#   mutate(opioids, 
#          rx_start = int_start(rx_int), 
#          rx_end = int_end(rx_int)) |> 
#   select(-rx_int)

# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH447777ddB") |> 
#       days_supply()
#   }, 76)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH447AddkCB") |> 
#       days_supply()
#   }, 25)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH44Ak7AnnH") |> 
#       days_supply()
#   }, 28)
# )
# 
# testthat::test_that(
#   "Test days_supply function works as expected",
#   testthat::expect_equal({
#     fsubset(opioids, BENE_ID %==% "HHHHHH4477d4Bnd") |> 
#       days_supply()
#   }, 30)
# )

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
  fmutate(exposure_days_supply = pmax(days, 1)) # sometimes, the only opioid is prescribed on the last day of the period. as.duration would return a days supply of 0, so I am manually converting these to 1.

write_data(opioids, "exposure_days_supply.fst", file.path(drv_root, "treatment"))
