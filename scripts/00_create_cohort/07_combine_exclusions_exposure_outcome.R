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
# drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

for (i in c("", "_7day_gap")){
    
  # opioid naive exclusion
  opioid_naive_exclusion <- load_data("pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
  # base cohort
  cohort <- load_data(paste0("pain_washout_continuous_enrollment_dts",i,".fst"), file.path(drv_root, "exclusion")) #|>
    # filter(BENE_ID %in% opioid_naive_exclusion$BENE_ID)
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
  exposures <- load_data(paste0("exposures_with_scs",i,".fst"), file.path(drv_root, "treatment"))
  # censoring
  cens <- load_data("pain_washout_continuous_enrollment_censoring.fst", file.path(drv_root, "outcome"))
  # outcomes
  outcomes <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst", file.path(drv_root, "outcome"))
  hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_outcomes.fst", file.path(drv_root, "outcome"))
  chronic_pain <- load_data("outcome_chronic_pain.fst", file.path(drv_root, "outcome"))
  prolonged_opioid_use <- load_data("outcome_prolonged_opioid_use.fst", file.path(drv_root,"outcome")) |> select(BENE_ID, outcome_prolonged_opioid_use)
  
  ### if OUD is observed in the exposure period, then flag those beneficiaries as having OUD in period 1
  # OUD (Composite)
  outcomes <- outcomes |>
    mutate(oud_period_1 = pmax(oud_period_exposure, oud_period_1))
  
  # OUD (ICD only)
  hillary <- hillary |>
    mutate(oud_hillary_period_1 = pmax(oud_hillary_period_exposure, oud_hillary_period_1))
  
  
  cohort <- list(
    cohort, 
    # opioid_naive_exclusion,
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
    join(outcomes, how = "left") |>
    join(hillary, how = "left") |>
    join(cens, how = "left") |>
    join(chronic_pain, how = "left") |>
    join(prolonged_opioid_use, how = "left") |>
    mutate(across(
        c(
          outcome_chronic_pain_period_2,
          outcome_chronic_pain_period_4,
          outcome_prolonged_opioid_use
        ),
        ~ replace_na(.x, 0)
      )
    ) |> # TEMPORARY. proper solution would be to fix scripts so vectors are saved with this step already completed
    mutate(cens_hillary_period_1 = cens_period_1,
           cens_hillary_period_2 = cens_period_2,
           cens_hillary_period_3 = cens_period_3,
           cens_hillary_period_4 = cens_period_4,
           # cens_hillary_period_5 = cens_period_5,
           cens_chronic_pain_period_2 = cens_period_2,
           cens_chronic_pain_period_4 = cens_period_4,
           cens_prolonged_opioid_period_4 = cens_period_4,
    )
  
  convert_cens_to_na <- function (data, outcomes, cens) {
    DT <- as.data.table(data)
    tau <- length(outcomes)
    for (j in 1:(tau)) {
      modify <- setdiff(cens[match(cens[j], cens):tau], cens[j])
      outcome_j <- outcomes[j]
      DT[get(outcome_j) == 1, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
    }
    DT[]
    DT
  }
  
  convert_outcome_to_na <- function (data, outcomes, cens) {
    DT <- as.data.table(data)
    tau <- length(outcomes)
    for (j in 1:(tau - 1)) {
      modify <- outcomes[match(outcomes[j], outcomes):tau]
      cens_j <- cens[j]
      DT[get(cens_j) == 0, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
      
      if(j > 1){ # if previously experienced outcome but then censored at later point, considered to have had outcome at subsequent timepoints
        outcome_j_1 <- outcomes[j-1]
        DT[get(outcome_j_1) == 1, `:=`((modify), lapply(.SD, function(x) 1)), .SDcols = modify]
      }
      
      
    }
    DT[]
    DT
  }
  
  cohort <- cohort |>
    left_join(opioid_naive_exclusion)|>
    left_join(oud_exclusions) |>
    mutate(subset_oud = ifelse(exclusion_opioid_naive == 0 & exclusion_oud == 0, 0,
                               ifelse(exclusion_oud_hillary == 1, 1, NA))) |>
    filter(!is.na(subset_oud))
  
  cohort <- cohort |>
    convert_outcome_to_na(paste0("oud_period_", 1:4), paste0("cens_period_", 1:4)) |>
    convert_cens_to_na(paste0("oud_period_", 1:4), paste0("cens_period_", 1:4)) |>
    convert_outcome_to_na(paste0("oud_hillary_period_", 1:4), paste0("cens_hillary_period_", 1:4)) |>
    convert_cens_to_na(paste0("oud_hillary_period_", 1:4), paste0("cens_hillary_period_", 1:4)) |>
    convert_outcome_to_na(paste0("outcome_chronic_pain_period_", c(2,4)), paste0("cens_chronic_pain_period_", c(2,4))) |>
    convert_cens_to_na(paste0("outcome_chronic_pain_period_", c(2,4)), paste0("cens_chronic_pain_period_", c(2,4))) |>
    # convert_outcome_to_na("outcome_prolonged_opioid_use", "cens_prolonged_opioid_period_4") |>
    # convert_cens_to_na("outcome_prolonged_opioid_use", "cens_prolonged_opioid_period_4") |>
    select(BENE_ID, 
           ends_with("dt"),
           starts_with("exposure"),
           starts_with("subset"),
           starts_with("cens"),
           starts_with("oud"),
           starts_with("outcome"),
           -cens_period_5,
           -oud_period_exposure,
           -oud_hillary_period_exposure,
           -oud_period_5,
           -oud_hillary_period_5
           ) |>
    mutate(oud_period_4 = case_when(oud_period_3 == 1 ~ 1,
                                    cens_period_4 == 0 ~ as.numeric(NA),
                                    TRUE ~ oud_period_4),
           oud_hillary_period_4 = case_when(oud_hillary_period_3 == 1 ~ 1,
                                    cens_hillary_period_4 == 0 ~ as.numeric(NA),
                                    TRUE ~ oud_hillary_period_4),
           outcome_chronic_pain_period_4 = case_when(outcome_chronic_pain_period_2 == 1 ~ 1,
                                            cens_chronic_pain_period_4 == 0 ~ as.numeric(NA),
                                            TRUE ~ outcome_chronic_pain_period_4),
           outcome_prolonged_opioid_use = case_when(cens_prolonged_opioid_period_4 == 0 ~ as.numeric(NA),
                                                  TRUE ~ outcome_prolonged_opioid_use),
           )
  
  write_data(cohort, paste0("inclusion_exclusion_cohort_with_exposure_outcomes",i,".fst"), file.path(drv_root, "exclusion"))

}