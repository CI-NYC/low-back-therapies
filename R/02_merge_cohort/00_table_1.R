# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)

low_back_dir <- "/mnt/general-data/disability/low-back-therapies/final"

df <- readRDS(file.path(low_back_dir, "low_back_analysis_df_clean.rds"))

df <- df |>
  select(# baseline covariates
         dem_age, # CONTINUOUS
         dem_sex_m,
         dem_race_aian,
         dem_race_asian,
         dem_race_black,
         dem_race_hawaiian,
         dem_race_hispanic,
         dem_race_multiracial,
         missing_dem_race,
# race missing
         dem_primary_language_english,
         missing_dem_primary_language_english,
         dem_married_or_partnered,
         missing_dem_married_or_partnered,
         dem_household_size,
         dem_household_size_2,
         dem_household_size_2plus,
         missing_dem_household_size,
         dem_veteran,
         missing_dem_veteran,
         dem_probable_high_income,
         dem_tanf_benefits,
         missing_dem_tanf_benefits,
         dem_ssi_benefits_mandatory_optional,
         dem_ssi_benefits,
         missing_dem_ssi_benefits,
# other SSI categories
         bipolar_washout_cal,
         anxiety_washout_cal,
         adhd_washout_cal,
         depression_washout_cal,
         mental_ill_washout_cal,
         baseline_has_counseling,
         # post-exposure confounders
         anxiety_post_exposure_cal,
         depression_post_exposure_cal,
         bipolar_post_exposure_cal,
         mediator_has_counseling,
         # mediators/treatments
         mediator_max_daily_dose_mme, # CONTINUOUS
         mediator_opioid_days_covered, # CONTINUOUS
         mediator_prescribers_6mo_sum, # CONTINUOUS
         mediator_has_tapering,
         mediator_opioid_benzo_copresc,
         mediator_opioid_stimulant_copresc,
         mediator_opioid_mrelax_copresc,
         mediator_opioid_gaba_copresc,
         mediator_nonopioid_pain_rx,
         # mediator_nonopioid_gabapentin_rx,
         mediator_nonopioid_other_analgesic_rx,
         mediator_nonopioid_antidepressant_rx,
         # mediator_nonopioid_muscle_relaxant_rx,
         mediator_nonopioid_antiinflammatory_rx,
         mediator_nonopioid_topical_rx,
         # mediator_nonopioid_benzodiazepine_rx,
         mediator_has_physical_therapy,
         mediator_has_multimodal_pain_treatment_restrict, # split up to examine further
         mediator_has_ablative_techniques,
         mediator_has_acupuncture,
         mediator_has_blocks,
         mediator_has_botulinum_toxin,
         mediator_has_chiropractic,
         mediator_has_electrical_nerve_stimulation,
         mediator_has_epidural_steroid,
         mediator_has_intrathecal_drug_therapy,
         mediator_has_massage_therapy,
         mediator_has_trigger_point_injection,
         # outcomes
         oud_24mo,
         oud_24mo_icd,
         # censoring
         uncens_24mo
         )

df <- df |>
  mutate(baseline_covariates = NA, .before = dem_age) |>
  mutate(sex = NA, .before = dem_sex_m) |>
  mutate(dem_sex_f = 1 - dem_sex_m, .after=dem_sex_m) |>
  mutate(race = NA, .before = dem_race_aian) |>
  # mutate(english = NA, .before = dem_primary_language_english) |>
  # mutate(married = NA, .before = missing_dem_married_or_partnered) |>
  mutate(household_size = NA, .before = dem_household_size) |>
  mutate(ssi_benefits = NA, .before = dem_ssi_benefits_mandatory_optional) |>
  mutate(psychiatric_conditions = NA, .before = bipolar_washout_cal) |>
  mutate(post_exposure = NA, .before = anxiety_post_exposure_cal) |>
  mutate(treatments = NA, after = mediator_has_counseling) |>
  mutate(opioid_coprescribing = NA, .before = mediator_opioid_benzo_copresc) |>
  mutate(nonopioid = NA, .before = mediator_nonopioid_pain_rx) |>
  mutate(multimodal = NA, .before = mediator_has_multimodal_pain_treatment_restrict) |>
  mutate(outcomes = NA, .before = oud_24mo) |>
  mutate(censoring = NA, .before = uncens_24mo) |>
  as.data.table()


demographics <- c("\\textbf{Baseline covariates (months 1-6)}",
                  "Age",
                  "Sex",
                  "\\hspace{0.5cm}Male",
                  "\\hspace{0.5cm}Female",
                  "Race/ethnicity:",
                  "\\hspace{0.5cm}AIAN, non-Hispanic",
                  "\\hspace{0.5cm}Asian, non-Hispanic",
                  "\\hspace{0.5cm}Black, non-Hispanic",
                  "\\hspace{0.5cm}Hawaiian/Pacific Islander",
                  "\\hspace{0.5cm}Hispanic, all races",
                  "\\hspace{0.5cm}Multiracial, non-Hispanic",
                  "\\hspace{0.5cm}Unknown",
                  "Primary language English",
                  "\\hspace{0.5cm}Unknown",
                  "Married/partnered",
                  "\\hspace{0.5cm}Unknown",
                  "Household size:",
                  "\\hspace{0.5cm}1",
                  "\\hspace{0.5cm}2",
                  "\\hspace{0.5cm}$\\geq$ 3",
                  "\\hspace{0.5cm}Unknown",
                  "Veteran",
                  "\\hspace{0.5cm}Unknown",
                  "High income (> 138FPL)",
                  "TANF Benefits",
                  "\\hspace{0.5cm}Unknown",
                  "SSI benefits:",
                  "\\hspace{0.5cm}Mandatory or optional",
                  "\\hspace{0.5cm}Not applicable",
                  "\\hspace{0.5cm}Unknown",
                  "Psychiatric conditions \\& counseling (months 1-6):",
                  "\\hspace{0.5cm}Bipolar",
                  "\\hspace{0.5cm}Anxiety",
                  "\\hspace{0.5cm}ADHD",
                  "\\hspace{0.5cm}Depression",
                  "\\hspace{0.5cm}Other mental illness",
                  "\\hspace{0.5cm}Mental health counseling",
                  "Psychiatric conditions \\& counseling (months 7-12):",
                  "\\hspace{0.5cm}Anxiety",
                  "\\hspace{0.5cm}Depression",
                  "\\hspace{0.5cm}Bipolar",
                  "\\hspace{0.5cm}Mental health counseling",
                  "\\textbf{Treatments (months 7-12)}",
                  "Max daily dose (MME)",
                  "Proportion of opioid days",
                  "Distinct Prescribers",
                  "Tapering",
                  "Co-prescription:",
                  "\\hspace{0.5cm}Benzodiazepine",
                  "\\hspace{0.5cm}Stimulant",
                  "\\hspace{0.5cm}Muscle relaxant",
                  "\\hspace{0.5cm}Gabapentinoid",
                  "Non-opioid prescription for pain:",
                  "\\hspace{0.5cm}Any",
                  # "\\hspace{0.5cm}Gabapenteinoid",
                  "\\hspace{0.5cm}Other analgesic and antipyretic",
                  "\\hspace{0.5cm}Antidepressant",
                  # "\\hspace{0.5cm}Muscle relaxant",
                  "\\hspace{0.5cm}Antiinflammatory and antirheumatic",
                  "\\hspace{0.5cm}Topical",
                  # "\\hspace{0.5cm}Benzodiazepines",
                  "Physical therapy",
                  "Multimodal pain treatment:",
                  "\\hspace{0.5cm}Any",
                  "\\hspace{0.5cm}Ablative techniques",
                  "\\hspace{0.5cm}Acupuncture",
                  "\\hspace{0.5cm}Blocks",
                  "\\hspace{0.5cm}Botulinum toxin injection",
                  "\\hspace{0.5cm}Chiropractic",
                  "\\hspace{0.5cm}Electrical nerve stimulation",
                  "\\hspace{0.5cm}Epidural steroid",
                  "\\hspace{0.5cm}Intrathecal drug therapy",
                  "\\hspace{0.5cm}Massage therapy",
                  "\\hspace{0.5cm}Trigger point injection",
                  "\\textbf{Outcomes (months 13-24)}",
                  "OUD by 24 months",
                  "OUD (ICD only) by 24 months",
                  "\\textbf{Censoring:}",
                  "Uncensored throughout entire study period"
)

############# Preparing continuous variables
continuous_vars <- c("dem_age", "mediator_max_daily_dose_mme", "mediator_opioid_days_covered", "mediator_prescribers_6mo_sum")
continuous_names <- c("Age", "Max daily dose (MME)", "Proportion of opioid days", "Distinct Prescribers")

summarise_continuous_variable <- function(data, variable){
  if (variable != "dem_age"){
    data <- data[mediator_opioid_days_covered > 0,]
  }
  
  return(paste0(round(median(data[[variable]]),2)," (",
         round(quantile(data[[variable]], 0.25),2),", ",
         round(quantile(data[[variable]], 0.75),2),")"))
}

continuous_values <- as.vector(sapply(continuous_vars, function(variable) summarise_continuous_variable(df, variable)))




############# Preparing binary variables
number <- sapply(df, function(x) sum(x, na.rm=T))
proportion <- sapply(df, function(x) prop.table(table(x))[2])
number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")



############# Putting everything together

table_one <- data.table(
  Characteristic = demographics,
  `Number (\\%)` = number_proportion
)

table_one[Characteristic %in% continuous_names, "Number (\\%)"] <- continuous_values

table_one_part1 <- table_one[1:43,]
table_one_part2 <- table_one[44:77,]

write.csv(table_one, file = "~/medicaid/low-back-therapies/R/02_merge_cohort/table_one.csv", row.names = FALSE)
write.csv(table_one_part1, file = "~/medicaid/low-back-therapies/R/02_merge_cohort/table_one_part1.csv", row.names = FALSE)
write.csv(table_one_part2, file = "~/medicaid/low-back-therapies/R/02_merge_cohort/table_one_part2.csv", row.names = FALSE)

