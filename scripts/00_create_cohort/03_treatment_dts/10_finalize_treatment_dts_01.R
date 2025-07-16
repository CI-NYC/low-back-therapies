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

opioid_dts <- load_data("opioid_dts.fst", file.path(drv_root, "treatment")) |>
  rename(treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt)
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment")) |>
  rename(treatment_start_dt = rx_start_dt, treatment_end_dt = rx_end_dt)
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> select(-treatment_name) |> as.data.table()

# Collect all treatments
cohort <- cohort |>
  right_join(treatments) |> # 1 634 900 unique BENE_ID
  group_by(BENE_ID) |>
  select(BENE_ID, treatment_start_dt_possible_latest, treatment_start_dt, treatment_end_dt) |>
  as.data.table()

# cohort <- cohort |>
#   slice(1:10000)

cohort <- cohort[, list(data = list(data.table(.SD))), by = BENE_ID]


get_duration <- function(data, gap = 30) {
# returns the last possible start date for a treatment where the previous treatment is no more than 7 days before.
  
  observation_start_dt <- data$treatment_start_dt
  observation_end_dt   <- data$treatment_end_dt
  
  next_observation_start <- c(observation_start_dt[-1], NA)
  
  gaps <- as.numeric(next_observation_start - observation_end_dt - 1)
  
  elig_treatment_dates <- observation_start_dt[!is.na(gaps) & gaps <= gap]
  last_elig_treatment_date <- if (length(elig_treatment_dates)) {
    tail(elig_treatment_dates, 1) 
  } else {
    data$treatment_end_dt[1]
  }
  
  result <- data.table(min(last_elig_treatment_date, data$treatment_start_dt_possible_latest[1]))
  
  names(result) <- c("last_treatment_dt")
  return(result)
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

write_data(out, "exposure_end_dt_30_days.fst", file.path(drv_root, "treatment"))

# plan(multisession, workers = 10)
# 
# # Apply function
# out_seven <- foreach(data = cohort$data,
#                      id = cohort$BENE_ID,
#                      .combine = "rbind",
#                      .options.future = list(chunk.size = 1e4)) %dofuture% {
#                        out <- get_duration(data, gap = 7)
#                        out$BENE_ID <- id
#                        setcolorder(out, "BENE_ID")
#                        out
#                      }
# 
# plan(sequential)
# 
# write_data(out_seven, "exposure_end_dt_7_days.fst", file.path(drv_root, "treatment"))