
library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)


source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |> as.data.table()
cohort[, let(exposure_end_dt = pain_diagnosis_dt + days(90))] # because diagnosis dt is included in exposure period, total length = 91 days

opioid_dts <- load_data("opioid_dts.fst", file.path(drv_root, "exclusion"))
nop_rx_dts <- readRDS(file.path(drv_root, "exclusion/nop_rx_dts.rds")) |>
  filter(!treatment=="nonopioid_uncategorized") |>
  mutate(treatment_end_dt = as.Date(ifelse(is.na(treatment_end_dt), treatment_start_dt, treatment_end_dt)))
nonpharma_dts <- readRDS(file.path(drv_root, "exclusion/treatments_dts.rds")) |>
  mutate(treatment_end_dt = treatment_start_dt)

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> as.data.table()

# Collect all treatments
cohort <- cohort |>
  right_join(treatments) |> # 1 634 900 unique BENE_ID
  group_by(BENE_ID) |>
  mutate(first_treatment_dt = min(treatment_start_dt)) |>
  as.data.table()
  
cohort <- cohort[, list(data = list(data.table(.SD))), by = BENE_ID]

# cohort <- cohort[1:100,]

get_duration <- function(data, gap = 30) {
  #data <- (opioids |> filter(BENE_ID == "HHHHHHddnCennB7"))$data |> as.data.table() #for testing
  # gap <- 30
  to_modify <- copy(data) |>
    as.data.table()
  
  data <- data |>
    as.data.table()
  
  # all dates with some opioid prescription
  opioid_dates <- to_modify[, .(date = seq(treatment_start_dt, treatment_end_dt, by = "1 day"), 
                                exposure_end_dt), 
                            by = .(seq_len(nrow(to_modify)))
  ][date <= exposure_end_dt, 
  ][, exposure_end_dt := NULL][, seq_len := 1] |> distinct()
  
  # all dates in hypothetical exposure period
  all_dates_exposure_period <- data[, .(date = seq(first_treatment_dt, exposure_end_dt, by = "1 day")), 
                                    by = .(seq_len(nrow(data)))
  ][seq_len == 1][, seq_len := NULL]
  
  # join opioid dates with all possible dates in exposure period
  all_dates_exposure_period <- merge(all_dates_exposure_period, opioid_dates, by = "date", all.x = TRUE)[, seq_len := ifelse(is.na(seq_len), 0, seq_len)]
  
  # get cumulative day sum
  all_dates_exposure_period[, opioid_days := cumsum(seq_len)]
  
  # group by instance to identify gaps (anything > 0 indicates a gap of X days)
  all_dates_exposure_period[, num_days_in_gap := .N - 1, by = opioid_days]
  
  # find FIRST instance of X+ day gap -- this is the last day of exposure
  all_dates_exposure_period[, indicator_day_plus_day_gap := as.integer(.I == min(.I[num_days_in_gap > gap])), by = opioid_days]
  
  # if all instances of indicator_day_plus_day_gap are 0, then the last row is returned
  if (all(all_dates_exposure_period[, indicator_day_plus_day_gap] == 0)) {
    # find the final exposure date
    final_exposure <- all_dates_exposure_period[seq_len == 1, max(date)]
    all_dates_exposure_period <- all_dates_exposure_period[date <= final_exposure]
    all_dates_exposure_period[.N, indicator_day_plus_day_gap := 1]
  }
  
  # changing column names
  setnames(all_dates_exposure_period, old = c("date", "opioid_days"), new = c("last_treatment_dt", "number_treatment_days"))
  
  # return last exposure date + number of days supplied
  all_dates_exposure_period <- all_dates_exposure_period[indicator_day_plus_day_gap == 1]
  
  # get only first instance of X+ day gap (if multiple)
  all_dates_exposure_period <- all_dates_exposure_period[last_treatment_dt == min(last_treatment_dt)]
  
  # keeping only exposure end date and days' supply
  all_dates_exposure_period <- all_dates_exposure_period[, .(last_treatment_dt, number_treatment_days)]
  
  all_dates_exposure_period
}


plan(multisession, workers = 10)

# Apply function
out <- foreach(data = cohort$data,
               id = cohort$BENE_ID,
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- get_duration(data, gap = 30)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

write_data(out, "exposure_end_dt_30_days.fst", file.path(drv_root, "treatments"))

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

write_data(out_seven, "exposure_end_dt_7_days.fst", file.path(drv_root, "treatments"))