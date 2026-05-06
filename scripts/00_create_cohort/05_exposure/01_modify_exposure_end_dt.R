# -------------------------------------
# Script: modify_exposure_end_dt
# Author: Anton Hung
# Purpose: Changing the exposure end date to be at day 30
# -------------------------------------
library(tidyverse)
library(lubridate)

source("~/medicaid/low-back-therapies/R/helpers.R")

# load cohort and opioid data
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  mutate(exposure_end_dt = day0_dt + days(30)) |>
  select(-last_treatment_dt)

write_data(cohort, "pain_washout_continuous_enrollment_dts.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
