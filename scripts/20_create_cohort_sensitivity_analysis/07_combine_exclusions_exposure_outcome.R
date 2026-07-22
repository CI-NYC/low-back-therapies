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
  print(i)
  
  # opioid naive exclusion
  opioid_naive_exclusion <- load_data("pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
  # base cohort
  cohort <- load_data(paste0("pain_washout_continuous_enrollment_dts.fst"), file.path(drv_root, "exclusion"))
  # washout pain exclusion
  washout_pain <- load_data("pain_washout_continuous_enrollment_washout_pain.fst", file.path(drv_root, "exclusion"))
  pregnancy_exclusion <- load_data("pregnancy_exclusion.fst", file.path(drv_root, "exclusion"))
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
  # chronic_pain <- load_data("outcome_chronic_pain.fst", file.path(drv_root, "outcome"))
  # prolonged_opioid_use <- load_data("outcome_prolonged_opioid_use.fst", file.path(drv_root,"outcome")) |> select(BENE_ID, outcome_prolonged_opioid_use) |> distinct()
  # chronic_opioid_therapy <- load_data("outcome_chronic_opioid_therapy.fst", file.path(drv_root, "outcome"))
  
  ### if OUD is observed in the exposure period, then flag those beneficiaries as having OUD in period 1
  # # OUD (Composite)
  # oud2 <- oud |>
  #   mutate(oud_period_1 = pmax(oud_period_exposure, oud_period_1))
  # 
  # # OUD (ICD only)
  # hillary <- hillary |>
  #   mutate(oud_hillary_period_1 = pmax(oud_hillary_period_exposure, oud_hillary_period_1))
  
  
  cohort <- list(
    cohort, 
    opioid_naive_exclusion,
    # oud_exclusions,
    washout_pain,
    pregnancy_exclusion,
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
    join(cens, how = "left") #|>
    # join(chronic_pain, how = "left") |>
    # join(prolonged_opioid_use, how = "left") |>
    # join(chronic_opioid_therapy, how = "left") 
  
  # convert_cens_to_na <- function (data, outcomes, cens) {
  #   DT <- as.data.table(data)
  #   tau <- length(outcomes)
  #   for (j in 1:(tau)) {
  #     modify <- setdiff(cens[match(cens[j], cens):tau], cens[j])
  #     outcome_j <- outcomes[j]
  #     DT[get(outcome_j) == 1, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  #   }
  #   DT[]
  #   DT
  # }
  # 
  # convert_outcome_to_na <- function (data, outcomes, cens) {
  #   DT <- as.data.table(data)
  #   tau <- length(outcomes)
  #   for (j in 1:(tau - 1)) {
  #     modify <- outcomes[match(outcomes[j], outcomes):tau]
  #     # cens_j <- cens[j]
  #     # DT[get(cens_j) == 0, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  #     
  #     if(j > 1){ # if previously experienced outcome but then censored at later point, considered to have had outcome at subsequent timepoints
  #       outcome_j_1 <- outcomes[j-1]
  #       DT[get(outcome_j_1) == 1, `:=`((modify), lapply(.SD, function(x) 1)), .SDcols = modify]
  #     }
  #   }
  #   DT[]
  #   DT
  # }
  
  cohort <- cohort |>
    left_join(opioid_naive_exclusion)|>
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