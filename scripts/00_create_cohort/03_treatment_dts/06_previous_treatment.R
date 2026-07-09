# -------------------------------------
# Script: previous_treatment
# Author: Anton Hung
# Updated:
# Purpose: tracking which treatments show up in the washout period, so that we don't
#           include them in the set of new treatments following aLBP diagnosis.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(lubridate)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion"))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt, treatment_end_dt, treatment_name) |> distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment")) |>
  filter()

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  mutate(treatment_name = ifelse(treatment_name %in% c("Other analgesic", "Acupuncture"), "Other treatment", treatment_name)) |>
  as.data.table()


# Collect all treatments -------------------------------------------------------

washout_tx <- cohort |>
  right_join(treatments) |>
  filter(treatment_start_dt >= washout_start_dt,
         treatment_start_dt < diagnosis_dt) |>
  select(BENE_ID, previous_treatment = treatment_name)

write_data(washout_tx, "previous_treatment.fst", file.path(drv_root, "exclusion"))
