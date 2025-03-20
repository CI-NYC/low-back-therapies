# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(fst)

source("~/medicaid/undertreated-pain/R/helpers.R")
save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exposures"

mme <- load_data("exposure_max_daily_dose_mme.fst", save_dir)
days_supply <- load_data("exposure_days_supply.fst", save_dir)

exposures <- 
  join(mme, days_supply, how = "full") |> 
  fmutate(subset_B1 = exposure_max_daily_dose_mme >= 50, 
          subset_B2 = exposure_days_supply > 7, 
          subset_B3 = subset_B1 & subset_B2)

write_data(exposures, "exposures_with_subsets.fst", save_dir)