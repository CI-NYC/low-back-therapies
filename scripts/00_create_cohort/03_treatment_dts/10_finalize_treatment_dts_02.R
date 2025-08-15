# -------------------------------------
# Script: 08_non_pharmaceuticals.R
# Author: Anton Hung
# Updated:
# Purpose: Looks through compiled treatments after low back pain diagnosis and 
#           Keeps treatments only if they are within a 30 day (or 7 day) gap
#           of the previous treatment.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)


source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(treatment_start_dt_possible_latest = pain_diagnosis_dt + days(30))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt, treatment_name) |>
  distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment")) |>
  rename(treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt)
nonpharma_dts <- load_data("nonpharma_dts_with_scs.fst", file.path(drv_root, "treatment"))
treatment_end_dt <- load_data("exposure_end_dt_30_days.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, last_treatment_dt) |>
  filter(!is.na(last_treatment_dt))
treatment_end_dt_7day_gap <- load_data("exposure_end_dt_7_days.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, last_treatment_dt) |>
  filter(!is.na(last_treatment_dt),
         BENE_ID %in% treatment_end_dt$BENE_ID)

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  right_join(treatment_end_dt) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  as.data.table()


# Collect all treatments -------------------------------------------------------
treatments <- treatments |>
  arrange(treatment_start_dt) |>
  group_by(BENE_ID) |>
  distinct(treatment_name, .keep_all = TRUE)

# number of people with at least one claim for each treatment
write.csv(table(treatments$treatment_name), "~/medicaid/low-back-therapies/data/private/table_treatment_counts.csv", row.names = F)

cohort_dts <- cohort |>
  right_join(treatments) |> # XXXX unique BENE_ID
  group_by(BENE_ID) |>
  mutate(first_treatment_dt = min(treatment_start_dt)) |>
  as.data.table() |>
  select(BENE_ID, pain_diagnosis_dt, first_treatment_dt, last_treatment_dt) |>
  distinct()

write_data(cohort_dts, "low_back_cohort_treatment_dts.fst", file.path(drv_root, "exclusion"))



### Cohort with a 7 day gap between treatments

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  left_join(treatment_end_dt_7day_gap) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  as.data.table()

cohort_dts <- cohort |>
  right_join(treatments) |> # XXXX unique BENE_ID
  group_by(BENE_ID) |>
  mutate(first_treatment_dt = min(treatment_start_dt)) |>
  as.data.table() |>
  select(BENE_ID, pain_diagnosis_dt, first_treatment_dt, last_treatment_dt) |>
  distinct()

write_data(cohort_dts, "low_back_cohort_treatment_dts_7day_gap.fst", file.path(drv_root, "exclusion"))
