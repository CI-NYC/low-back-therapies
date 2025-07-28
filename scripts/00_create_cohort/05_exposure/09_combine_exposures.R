# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(tidyverse)
library(fst)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

mme <- load_data("exposure_max_daily_dose_mme.fst", file.path(drv_root, "treatment"))
days_supply <- load_data("exposure_days_supply.fst", file.path(drv_root, "treatment"))
# distinct_prescribers <- load_data("exposure_distinct_prescribers.fst", file.path(drv_root, "treatment"))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt, treatment_name) |>
  distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment")) |>
  rename(treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt)
nonpharma_dts <- load_data("nonpharma_dts_with_scs.fst", file.path(drv_root, "treatment"))
treatment_end_dt <- load_data("exposure_end_dt_30_days.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, last_treatment_dt)
treatment_end_dt_7_days <- load_data("exposure_end_dt_7_days.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, last_treatment_dt)

# Exposure: Which treatments were present during the exposure period? -----------
treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  left_join(treatment_end_dt) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  as.data.table()

combinations_wide <- cohort %>%
  left_join(treatments) %>%
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  rename_with(~paste0("exposure_", tolower(.)), -1)
# ------------------------------------------------------------------------------

exposures <- combinations_wide |>
  left_join(mme) |>
  left_join(days_supply)

write_data(exposures, "exposures_with_scs.fst", file.path(drv_root, "treatment"))


cohort <- cohort |>
  left_join(exposures) |>
  mutate(exposure_period_end_dt = first_treatment_dt + 90) # 91 day exposure period

write_data(cohort, "pain_washout_continuous_enrollment_with_exposures.fst", file.path(drv_root, "treatment"))


## 7 day gap
rm(cohort)
rm(mme)
rm(days_supply)

cohort <- load_data("pain_washout_continuous_enrollment_dts_7day_gap.fst", file.path(drv_root, "exclusion"))
mme <- load_data("exposure_max_daily_dose_mme_7day_gap.fst", file.path(drv_root, "treatment"))
days_supply <- load_data("exposure_days_supply_7day_gap.fst", file.path(drv_root, "treatment"))

# Exposure: Which treatments were present during the exposure period? -----------

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  left_join(treatment_end_dt_7_days) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  as.data.table()

combinations_wide <- cohort %>%
  left_join(treatments) %>%
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  rename_with(~paste0("exposure_", tolower(.)), -1)

# ------------------------------------------------------------------------------

exposures <- combinations_wide |>
  left_join(mme) |>
  left_join(days_supply)

write_data(exposures, "exposures_with_scs_7day_gap.fst", file.path(drv_root, "treatment"))

cohort <- cohort |>
  left_join(exposures) |>
  mutate(exposure_period_end_dt = first_treatment_dt + 90) # 91 day exposure period

write_data(cohort, "pain_washout_continuous_enrollment_with_exposures_7day_gap.fst", file.path(drv_root, "treatment"))