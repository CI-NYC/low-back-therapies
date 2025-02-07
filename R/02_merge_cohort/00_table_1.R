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
  select(# outcomes
         oud_24mo,
         oud_24mo_icd,
         # baseline covariates
         dem_age, # CONTINUOUS
         dem_sex_m,
         dem_race_aian,
         dem_race_asian,
         dem_race_black,
         dem_race_hawaiian,
         dem_race_hispanic,
         dem_race_multiracial,
         dem_primary_language_english,
         dem_married_or_partnered,
         dem_household_size_2,
         dem_household_size_2plus,
         dem_veteran,
         dem_probable_high_income,
         dem_tanf_benefits,
         dem_ssi_benefits_mandatory_optional,
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
         # mediators/treatments
         mediator_has_counseling,
         mediator_max_daily_dose_mme, # CONTINUOUS
         mediator_opioid_days_covered, # CONTINUOUS
         mediator_prescribers_6mo_sum, # CONTINUOUS
         mediator_has_tapering,
         mediator_opioid_benzo_copresc,
         mediator_opioid_stimulant_copresc,
         mediator_opioid_mrelax_copresc,
         mediator_opioid_gaba_copresc,
         mediator_nonopioid_pain_rx,
         mediator_nonopioid_gabapentin_rx,
         mediator_nonopioid_other_analgesic_rx,
         mediator_nonopioid_antidepressant_rx,
         mediator_nonopioid_muscle_relaxant_rx,
         mediator_nonopioid_antiinflammatory_rx,
         mediator_nonopioid_topical_rx,
         mediator_nonopioid_benzodiazepine_rx,
         mediator_has_physical_therapy,
         mediator_has_multimodal_pain_treatment_restrict, # split up to examine further
         # censoring
         uncens_24mo
         )

df <- df |>
  mutate(outcomes = NA, .before = oud_24mo) |>
  mutate(baseline_covariates = NA, .before = dem_age) |>
  mutate(sex = NA, .before = dem_sex_m) |>
  mutate(dem_sex_f = 1 - dem_sex_m, .after=dem_sex_m) |>
  mutate(race = NA, .before = dem_race_aian) |>
  mutate(household_size = NA, .before = dem_household_size_2) |>
  mutate(psychiatric_conditions = NA, .before = bipolar_washout_cal) |>
  mutate(post_exposure = NA, .before = anxiety_post_exposure_cal) |>
  mutate(treatments = NA, .before = mediator_has_counseling) |>
  mutate(opioid_coprescribing = NA, .before = mediator_opioid_benzo_copresc) |>
  mutate(nonopioid = NA, .before = mediator_nonopioid_pain_rx) |>
  mutate(censoring = NA, .before = uncens_24mo) |>
  as.data.table()


demographics <- c("\\textbf{Outcomes:}",
                  "OUD (comprehensive)",
                  "OUD (ICD-10 only)",
                  "\\textbf{Baseline covariates:}",
                  "Age",
                  "Sex",
                  "\\hspace{0.5cm}Male",
                  "\\hspace{0.5cm}Female",
                  "Race/ethnicity:",
                  "\\hspace{0.5cm}American Indian or Alaska Native",
                  "\\hspace{0.5cm}Asian, non-Hispanic",
                  "\\hspace{0.5cm}Black, non-Hispanic",
                  "\\hspace{0.5cm}Hawaiian or Pacific Islander",
                  "\\hspace{0.5cm}Hispanic, all races",
                  "\\hspace{0.5cm}Multiracial, non-Hispanic",
                  "English primary language",
                  "Married or partnered",
                  "Household size:",
                  "\\hspace{0.5cm}2",
                  "\\hspace{0.5cm}More than 2",
                  "Veteran status",
                  "Probable high income",
                  "Tanf benefits",
                  "SSI benefits",
                  "Psychiatric Conditions:",
                  "\\hspace{0.5cm}Bipolar",
                  "\\hspace{0.5cm}Anxiety",
                  "\\hspace{0.5cm}ADHD",
                  "\\hspace{0.5cm}Depression",
                  "\\hspace{0.5cm}Mental illness",
                  "\\hspace{0.5cm}Mental health counseling ",
                  "\\textbf{Post-exposure confounders:}",
                  "Anxiety",
                  "Depression",
                  "Bipolar",
                  "\\textbf{Treatments:}",
                  "Mental health counseling",
                  "Maximum daily MME",
                  "Opioid proportion of days covered",
                  "Unique prescribers",
                  "Tapering",
                  "Co-prescribing:",
                  "\\hspace{0.5cm}Benzodiazepine",
                  "\\hspace{0.5cm}Stimulant",
                  "\\hspace{0.5cm}Muscle relaxant",
                  "\\hspace{0.5cm}Gabapentin",
                  "Non-opioid prescription for pain:",
                  "\\hspace{0.5cm}Any",
                  "\\hspace{0.5cm}Gabapentein",
                  "\\hspace{0.5cm}Other analgesics",
                  "\\hspace{0.5cm}Antidepressants",
                  "\\hspace{0.5cm}Muscle relaxants",
                  "\\hspace{0.5cm}Anti-inflammatory",
                  "\\hspace{0.5cm}Topical",
                  "\\hspace{0.5cm}Benzodiazepines",
                  "Physical therapy",
                  "Multimodal pain treatment",
                  "\\textbf{Censoring:}",
                  "Uncensored throughout entire study period"
)

############# Preparing continuous variables
continuous_vars <- c("dem_age", "mediator_max_daily_dose_mme", "mediator_opioid_days_covered", "mediator_prescribers_6mo_sum")
continuous_names <- c("Age", "Maximum daily MME", "Opioid proportion of days covered", "Unique prescribers")

summarise_continuous_variable <- function(data, variable){
  if (variable != "dem_age"){
    data <- data[mediator_opioid_days_covered > 0,]
  }
  
  return(paste0(round(median(data[[variable]]),2)," (",
         round(quantile(data[[variable]], 0.25),2),", ",
         round(quantile(data[[variable]], 0.75),2),")"))
}

continuous_values <- sapply(continuous_vars, function(variable) summarise_continuous_variable(df, variable))


# age <- summarise_continuous_variable(df, "dem_age")
# mme <- summarise_continuous_variable(df, "mediator_max_daily_dose_mme", "yes")
# days <- summarise_continuous_variable(df, "mediator_opioid_days_covered", "yes")
# prescribers <- summarise_continuous_variable(df, "mediator_prescribers_6mo_sum", "yes")




############# Preparing binary variables
number <- sapply(df, function(x) sum(x, na.rm=T))
proportion <- sapply(df, function(x) prop.table(table(x))[2])
number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")



############# Putting everything together

table_one <- data.table(
  demographics = demographics,
  number_proportion = number_proportion
)

table_one[demographics %in% continuous_names,] <- continuous_values

write.csv(table_one, file = "~/medicaid/low-back-therapies/R/02_merge_cohort/table_one.csv", row.names = FALSE)
