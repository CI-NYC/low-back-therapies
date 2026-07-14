# -------------------------------------
# Script: 00_bup.R
# Author: Nick Williams
# Purpose: Identify MOUD buprenorphine periods
# Notes:
#   - 3 week (21 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(collapse)
library(yaml)

source("~/medicaid/low-back-therapies/R/helpers.R")

# drv_root <- "/mnt/general-data/disability/low-back-therapies/exclusion"

# Load cohort
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root_12_month_washout, "exclusion"))

bup_list <- read_fst("~/medicaid/low-back-therapies/data/public/bup_list.fst")
hcpcs <- read_yaml("~/medicaid/low-back-therapies/data/public/hcpcs_codes.yml")$buprenorphine


bup <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_bup_intervals.fst", file.path(drv_root, "exclusion"))

moud_bup <- 
  roworder(bup, BENE_ID, moud_start_dt) |> 
  join(cohort, how = "left") |> 
  fmutate(moud_bup_washout = int_overlaps(
    interval(moud_start_dt, moud_end_dt),
    interval(washout_start_dt, washout_end_dt)
  )) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(moud_bup_washout = as.numeric(sum(moud_bup_washout) > 0))

# - Rejoin entire initial cohort and save
moud_bup <- 
  join(cohort, moud_bup, how = "left") |> 
  fmutate(moud_bup_washout = replace_na(moud_bup_washout, 0)) |>
  select(BENE_ID, moud_bup_washout)

write_data(moud_bup, "pain_washout_continuous_enrollment_opioid_requirements_moud_bup_washout.fst", file.path(drv_root_12_month_washout,"exclusion"))
