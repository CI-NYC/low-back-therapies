# -------------------------------------
# Script: 09_mediator_proportion_days_covered.R
# Author: Nick Williams
# Updated:
# Purpose: Calculate proportion of days during the mediator window an observation was prescribed opioids
# Notes:
# -------------------------------------

library(lubridate)
library(foreach)
library(doFuture)
library(tidyverse)

src_root <- "/mnt/processed-data/disability"
low_back_dir <- "/mnt/general-data/disability/low-back-therapies"

dts_cohorts <- readRDS(file.path(low_back_dir, "exclusion/low_back_cohort.rds"))
otl <- readRDS(file.path(low_back_dir, "treatments/mediator_otl_opioid_pain_rx.rds"))
rxl <- readRDS(file.path(low_back_dir, "treatments/mediator_rxl_opioid_pain_rx.rds"))

prop_days_covered <- function(data) {
  dur <- 0
  current_int <- data$rx_int[1]
  for (i in 1:nrow(data)) {
    check <- intersect(current_int, data$rx_int[i + 1])
    if (is.na(check)) {
      # if they don't intersect, add the duration of the first interval
      dur <- dur + as.duration(current_int)
      current_int <- data$rx_int[i + 1]
    } else {
      # if they do intersect, then update current interval as the union
      current_int <- union(current_int, data$rx_int[i + 1])
    }
  }
  
  max(time_length(dur, "days"), 1)
}

opioids <- otl |> 
  mutate(rx_int = interval(LINE_SRVC_BGN_DT, LINE_SRVC_BGN_DT + days(1)), 
         rx_int = intersect(rx_int, interval(washout_cal_end_dt, 
                                             trt_end_dt))) |> 
  select(BENE_ID, rx_int) |> 
  as_tibble() |> 
  bind_rows({
    rxl |> 
      mutate(DAYS_SUPPLY = replace_na(DAYS_SUPPLY, 1), 
             rx_int = interval(RX_FILL_DT, RX_FILL_DT + days(DAYS_SUPPLY)), 
             rx_int = intersect(rx_int, interval(washout_cal_end_dt, 
                                                 trt_end_dt))) |> 
      select(BENE_ID, rx_int) |> 
      as_tibble()
  })

opioids <- group_by(opioids, BENE_ID) |> 
  arrange(BENE_ID, int_start(rx_int)) |> 
  nest()

plan(multisession, workers = 10)

opioids$treatment_opioid_days_supply <- 
  foreach(x = opioids$data, 
          .combine = "c",
          .options.future = list(chunk.size = 1e4)) %dofuture% {
            prop_days_covered(x)
          }

plan(sequential)

opioids <- select(dts_cohorts, BENE_ID) |> 
  left_join(select(opioids, -data)) |> 
  mutate(treatment_opioid_days_supply = replace_na(treatment_opioid_days_supply, 0))

saveRDS(opioids, file.path(low_back_dir, "treatments/treatment_opioid_days_supply.rds"))