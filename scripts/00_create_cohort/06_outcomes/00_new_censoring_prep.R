# -------------------------------------
# Script: 00_getting_enrollment_dates.R (part 1)
# Author: Anton Hung
# Purpose: collecting enrollment data for beneficiaries in our cohort
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(dplyr)

source("~/medicaid/low-back-therapies/R/helpers.R")

# Load washout dates
washout <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |> 
  mutate(study_end_dt = first_treatment_dt + days(545)) |>
  as.data.table()

# Load all dates
dates <- open_dedts()

dates <- 
  filter(dates, !is.na(BENE_ID)) |> 
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  inner_join(washout, by = "BENE_ID") |> 
  arrange(BENE_ID, ENRLMT_START_DT) |>
  collect() |>
  distinct()

censoring_df <- 
    dates |>
    group_by(BENE_ID) |>
    filter(ENRLMT_START_DT %within% interval(first_treatment_dt, study_end_dt)) |>
    filter(row_number() == n()) |>
    mutate(study_complete_dt = as.integer(ENRLMT_END_DT >= study_end_dt))

censoring_df <- dates[
  ENRLMT_START_DT >= first_treatment_dt & ENRLMT_START_DT <= study_end_dt, 
  .(study_complete_dt = as.integer(ENRLMT_END_DT >= study_end_dt)), 
  by = BENE_ID
][, .SD[.N], by = BENE_ID] # Get the last row per ID

censoring_df <- 
    censoring_df_tmp  |>
    mutate(censoring_dts_cal_dt = case_when(study_complete_dt == 0 ~ ENRLMT_END_DT)) |>
    select(BENE_ID, study_complete_dt, censoring_dts_cal_dt)
