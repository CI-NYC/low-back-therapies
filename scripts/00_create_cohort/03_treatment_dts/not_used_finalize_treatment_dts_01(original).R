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
  mutate(treatment_start_dt_possible_latest = pain_diagnosis_dt + days(90))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt, treatment_name) |>
  distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment")) |>
  rename(treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt)
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> select(-treatment_name) |> as.data.table()

# Collect all treatments
cohort <- cohort |>
  right_join(treatments) |>
  select(BENE_ID, treatment_start_dt_possible_latest, treatment_start_dt, treatment_end_dt) |>
  arrange(BENE_ID, treatment_start_dt, treatment_end_dt) |>
  as.data.table()

cohort <- cohort |>
  slice(1:10000)

cohort <- cohort[, list(data = list(data.table(.SD))), by = BENE_ID]


get_duration <- function(data, gap = 30) {
# returns the last possible start date for a treatment where the previous treatment is no more than 30 days before.
  
  to_modify <- copy(data) |>
    as.data.table()
  
  data <- data |>
    as.data.table()
  
  # all dates with some opioid prescription
  tx_dates <- to_modify[, .(date = seq(treatment_start_dt, treatment_end_dt, by = "1 day"), 
                            treatment_start_dt_possible_latest), 
                            by = .(seq_len(nrow(to_modify)))
  ][date <= treatment_start_dt_possible_latest, 
  ][, treatment_start_dt_possible_latest := NULL][, seq_len := 1] |> distinct()

  
  # all dates in hypothetical exposure period
  all_dates_exposure_period <- data[, .(date = seq(first(treatment_start_dt), treatment_start_dt_possible_latest, by = "1 day")), 
                                    by = .(seq_len(nrow(data)))
  ][seq_len == 1][, seq_len := NULL]
  
  # join opioid dates with all possible dates in exposure period
  all_dates_exposure_period <- merge(all_dates_exposure_period, tx_dates, by = "date", all.x = TRUE)[, seq_len := ifelse(is.na(seq_len), 0, seq_len)]
  
  # get cumulative day sum
  all_dates_exposure_period[, treatment_days := cumsum(seq_len)]
  
  # group by instance to identify gaps (anything > 0 indicates a gap of X days)
  all_dates_exposure_period[, num_days_in_gap := .N - 1, by = treatment_days]
  
  # find FIRST instance of X+ day gap -- this is the last day of exposure
  all_dates_exposure_period[, indicator_day_plus_day_gap := as.integer(.I == min(.I[num_days_in_gap > gap])), by = treatment_days]
  
  # if all instances of indicator_day_plus_day_gap are 0, then the last row is returned
  if (all(all_dates_exposure_period[, indicator_day_plus_day_gap] == 0)) {
    # find the final exposure date
    final_exposure <- all_dates_exposure_period[seq_len == 1, max(date)]
    all_dates_exposure_period <- all_dates_exposure_period[date <= final_exposure]
    all_dates_exposure_period[.N, indicator_day_plus_day_gap := 1]
  }
  
  # return last exposure date + number of days supplied
  all_dates_exposure_period <- all_dates_exposure_period[indicator_day_plus_day_gap == 1]
  
  # get only first instance of X+ day gap (if multiple)
  all_dates_exposure_period <- all_dates_exposure_period[date == min(date)]
  
  result <- data.table(all_dates_exposure_period[,date])
  
  names(result) <- c("last_treatment_dt")
  return(result)
}


plan(multisession, workers = 10)
# Apply function
out_compare <- foreach(data = cohort$data,
               id = cohort$BENE_ID,
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- get_duration(data, gap = 30)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)
# 
# write_data(out, "exposure_end_dt_30_days.fst", file.path(drv_root, "treatment"))

tic()
plan(multisession, workers = 10)

# Apply function
out_seven <- foreach(data = cohort$data,
                     id = cohort$BENE_ID,
                     .combine = "rbind",
                     .options.future = list(chunk.size = 1e4)) %dofuture% {
                       out <- get_duration(data, gap = 7)
                       out$BENE_ID <- id
                       setcolorder(out, "BENE_ID")
                       out
                     }

plan(sequential)
toc()

write_data(out_seven, "exposure_end_dt_7_days.fst", file.path(drv_root, "treatment"))