# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Nick Williams
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

for (i in c("", "_7day_gap")){
  
  # opioid naive exclusion
  opioid_naive <- load_data("pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
  # base cohort
  cohort <- load_data(paste0("pain_washout_continuous_enrollment_dts.fst"), file.path(drv_root, "exclusion"))
  # washout pain exclusion
  washout_pain <- load_data("pain_washout_continuous_enrollment_washout_pain.fst", file.path(drv_root, "exclusion"))
  # debse exclusions
  debse_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))
  # iph exclusions
  iph_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst", file.path(drv_root, "exclusion"))
  # oth exclusions
  oth_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
  # oud exclusions
  oud_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst", file.path(drv_root, "exclusion"))
  # exposures
  exposures <- load_data(paste0("exposures",i,".fst"), file.path(drv_root, "treatment"))
  # censoring
  cens <- load_data("pain_washout_continuous_enrollment_censoring.fst", file.path(drv_root, "outcome"))
  # outcomes
  oud <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst", file.path(drv_root, "outcome"))
  hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_outcomes.fst", file.path(drv_root, "outcome"))
  chronic_pain <- load_data("outcome_chronic_pain.fst", file.path(drv_root, "outcome"))
  prolonged_opioid_use <- load_data("outcome_prolonged_opioid_use.fst", file.path(drv_root,"outcome")) |> select(BENE_ID, outcome_prolonged_opioid_use) |> distinct()
  chronic_opioid_therapy <- load_data("outcome_chronic_opioid_therapy.fst", file.path(drv_root, "outcome"))
  
  
  cohort <- list(
    cohort, 
    opioid_naive,    
    # oud_exclusions,
    washout_pain,
    debse_exclusions, 
    iph_exclusions, 
    oth_exclusions
  ) |> 
    reduce(join, how = "left") |>
    mutate(across(everything(), ~ replace_na(., 0)))
  
  # Remove observations with exclusions
  cohort <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))
  
  # Add in exposure, outcome, and censoring data
  cohort <- 
    join(cohort, exposures, how = "left") |>
    join(oud, how = "left") |>
    join(hillary, how = "left") |>
    join(cens, how = "left") |>
    join(chronic_pain, how = "left") |>
    join(prolonged_opioid_use, how = "left") |>
    join(chronic_opioid_therapy, how = "left") 
  
  cohort <- cohort |>
    left_join(opioid_naive)|>
    left_join(oud_exclusions) |>
    mutate(subset_oud = ifelse(exclusion_opioid_naive == 0 & exclusion_oud == 0, 0,
                               ifelse(exclusion_oud_hillary == 1, 1, NA))) |>
    filter(!is.na(subset_oud))
  
  cohort <- cohort |>
    mutate(oud_period_1 = case_when(cens_period_1 == 0 ~ as.numeric(NA),
                                    TRUE ~ oud_period_1),
           oud_period_2 = case_when(cens_period_2 == 0 ~ as.numeric(NA),
                                    TRUE ~ oud_period_2),
           oud_hillary_period_1 = case_when(cens_period_1 == 0 ~ as.numeric(NA),
                                            TRUE ~ oud_hillary_period_1),
           oud_hillary_period_2 = case_when(cens_period_2 == 0 ~ as.numeric(NA),
                                            TRUE ~ oud_hillary_period_2)
    ) |>
    select(-starts_with("exclusion"),
           -ends_with("exposure")) 
  
  write_data(cohort, paste0("inclusion_exclusion_cohort_with_exposure_outcomes",i,".fst"), file.path(drv_root, "exclusion"))

}