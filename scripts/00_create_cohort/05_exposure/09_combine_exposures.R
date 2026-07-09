# -------------------------------------
# Script: 05_combine_exposures.R
# Author: Nick Williams
# Purpose: Combine two exposures and define exposure subsets
# Notes:
# -------------------------------------

library(collapse)
library(tidyverse)
library(fst)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
previous_treatment <- load_data("previous_treatment.fst", file.path(drv_root, "exclusion"))

nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))
mme <- load_data("exposure_max_daily_dose_mme.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
days_supply <- load_data("exposure_days_supply.fst", file.path(drv_root_30_day_treatment, "modified_variables"))

opioid <- left_join(mme, days_supply) |>
  fmutate(treatment_name = case_when(
    exposure_max_daily_dose_mme <= 50 & exposure_days_supply <= 7   ~ "Opioid <=7days <=50mme",
    exposure_max_daily_dose_mme <= 50 & exposure_days_supply > 7    ~ "Opioid >7days <=50mme",
    exposure_max_daily_dose_mme > 50                                ~ "Opioid >50mme",
    TRUE ~ NA
  )) |>
  select(BENE_ID, treatment_name)



# Exposure: Which treatments were present during the exposure period? -----------

treatments <- rbind(nop_rx_dts, nonpharma_dts) |> 
  mutate(treatment_name = ifelse(treatment_name %in% c("Other analgesic", "Acupuncture"), "Other treatment", treatment_name)) |>
  right_join(cohort) |>
  fsubset(treatment_start_dt >= day0_dt &
          treatment_start_dt <= exposure_end_dt) |>
  as.data.table()

exposures <- cohort %>%
  right_join(treatments) %>%
  select(BENE_ID, treatment_name) %>%
  anti_join(previous_treatment, by = c("BENE_ID" = "BENE_ID", "treatment_name" = "previous_treatment")) %>%
  rbind(opioid) |>
  distinct() %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  rename_with(~paste0("exposure_", tolower(.)), -1)

# replace spaces and dashes with underscore, because in the analysis, ranger has an issue with these characters.
names(exposures) <- gsub("[ -]", "_", names(exposures))

write_data(exposures, "exposures.fst", file.path(drv_root_30_day_treatment, "modified_variables"))


