# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Nick Williams
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)

source("~/medicaid/low-back-therapies/R/helpers.R")




# opioid naive exclusion
opioid_naive_exclusion <- load_data("pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
# base cohort
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))
debse_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))
iph_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst", file.path(drv_root, "exclusion"))
oth_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
oud_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst", file.path(drv_root, "exclusion"))
pain_washout <- load_data("pain_washout_continuous_enrollment_washout_pain.fst", file.path(drv_root, "exclusion"))
pregnancy_exclusion <- load_data("pregnancy_exclusion.fst", file.path(drv_root, "exclusion"))



cohort <- list(
  cohort, 
  opioid_naive_exclusion,
  debse_exclusions, 
  iph_exclusions, 
  oth_exclusions, 
  oud_exclusions,
  pain_washout,
  pregnancy_exclusion
) |> 
  reduce(join, how = "left") |>
  mutate(across(everything(), ~ replace_na(., 0)))

print(paste("Has diagnosis:", nrow(load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")))))
print(paste("Has treatment:", nrow(load_data("low_back_cohort_treatment_dts.fst", file.path(drv_root, "exclusion")))))
print(paste("Washout continuously enrolled:", nrow(cohort)))


# exclusion oud
print(paste("Not opioid naive:", sum(cohort$exclusion_opioid_naive)))

cohort <- cohort |>
  filter(exclusion_opioid_naive == 0)

# exclusion oud
print(paste("Has OUD:", sum(cohort$exclusion_oud)))

cohort <- cohort |>
  filter(exclusion_oud == 0)

# exclusion prior pain
print(paste("Has other pain within 3 months:", sum(cohort$exclusion_washout_pain)))

cohort <- cohort |>
  filter(exclusion_washout_pain == 0)

print(paste("Remaining after OUD and other key exclusions:", nrow(cohort)))

# exclusion MD
print(paste("Maryland:", sum(cohort$exclusion_maryland)))

cohort <- cohort |>
  filter(exclusion_maryland == 0)

# exclusion age
print(paste("Age:", nrow(filter(cohort, exclusion_age == 1 | exclusion_double_bdays == 1))))

cohort <- cohort |>
  filter(exclusion_double_bdays == 0,
         exclusion_age == 0)

# exclusion pregnancy
print(paste("Pregnant:", nrow(filter(cohort, exclusion_pregnancy == 1 | exclusion_pregnancy_eligibility == 1))))

cohort <- cohort |>
  filter(exclusion_pregnancy == 0,
         exclusion_pregnancy_eligibility == 0)

# exclusion unknown sex
print(paste("Missing sex:", sum(cohort$exclusion_missing_sex)))

cohort <- cohort |>
  filter(exclusion_missing_sex == 0)

# exclusion comprehensive benefits
print(paste("Restricted benefits:", sum(cohort$exclusion_comp_bnfts)))

cohort <- cohort |>
  filter(exclusion_comp_bnfts == 0)

# exclusion dual eligible
print(paste("Dual eligible:", sum(cohort$exclusion_dual_eligible)))

cohort <- cohort |>
  filter(exclusion_dual_eligible == 0)


# exclusion cancer
print(paste("Cancer:", nrow(filter(cohort, exclusion_cancer == 1 | exclusion_cancer_oth == 1 | exclusion_cancer_iph == 1))))

cohort <- cohort |>
  filter(exclusion_cancer == 0,
         exclusion_cancer_oth == 0,
         exclusion_cancer_iph == 0)

# exclusion palliative
print(paste("Palliative:", nrow(filter(cohort, exclusion_pall_iph == 1 | exclusion_pall_oth == 1))))

cohort <- cohort |>
  filter(exclusion_pall_iph == 0,
         exclusion_pall_oth == 0)

# exclusion institution or palliative
print(paste("Institutionalized:", nrow(filter(cohort, exclusion_institution == 1))))

cohort <- cohort |>
  filter(exclusion_institution == 0)

# exclusion inpatient
print(paste("Inpatient:", nrow(filter(cohort, exclusion_monthprior_hospitalization == 1 | exclusion_monthprior_otherinpatient == 1))))

cohort <- cohort |>
  filter(exclusion_monthprior_hospitalization == 0,
         exclusion_monthprior_otherinpatient == 0)

# exclusion managed care
print(paste("Managed care in CO or AR:", nrow(filter(cohort, exclusion_managed_care == 1))))

cohort <- cohort |>
  filter(exclusion_managed_care == 0)


print(paste("Final remaining:", nrow(cohort)))



