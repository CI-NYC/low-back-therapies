# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(fst)
library(dplyr)

source("~/medicaid/low-back-therapies/R/helpers.R")
save_dir <- "/mnt/general-data/disability/low-back-therapies/treatments"

mme <- load_data("exposure_max_daily_dose_mme.fst", save_dir)
days_supply <- load_data("exposure_days_supply.fst", save_dir)
distinct_prescribers <- load_data("exposure_distinct_prescribers.fst", file.path(save_dir))
coprescriptions <- readRDS(file.path(save_dir, "opioid_coprescriptions.rds")) |> select(-pain_diagnosis_dt, -exposure_end_dt)
nonopioid_rx <- readRDS(file.path(save_dir, "nop_binary_refactor.rds"))
nonpharma <- readRDS(file.path(save_dir, "nonpharma_bin.rds"))

exposures <- coprescriptions |>
  left_join(nonopioid_rx) |>
  left_join(nonpharma) |>
  left_join(mme) |>
  left_join(days_supply) |>
  left_join(distinct_prescribers)
  # fmutate(subset_B1 = exposure_max_daily_dose_mme >= 50, 
  #         subset_B2 = exposure_days_supply > 7, 
  #         subset_B3 = subset_B1 & subset_B2)

write_data(exposures, "exposures.fst", save_dir)
