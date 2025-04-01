# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

df <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final"))

df <- df |>
  mutate(exposure_nonopioid_pain_rx = exposure_nonopioid_other_analgesic |
           exposure_nonopioid_antidep |
           exposure_nonopioid_antiinflam |
           exposure_nonopioid_topical,
         dem_race_aian = ifelse(missing_dem_race==0, dem_race_aian, NA),
         dem_race_asian = ifelse(missing_dem_race==0, dem_race_asian, NA),
         dem_race_black = ifelse(missing_dem_race==0, dem_race_black, NA),
         dem_race_hawaiian = ifelse(missing_dem_race==0, dem_race_hawaiian, NA),
         dem_race_hispanic = ifelse(missing_dem_race==0, dem_race_hispanic, NA),
         dem_race_multiracial = ifelse(missing_dem_race==0, dem_race_multiracial, NA),
         dem_race = ifelse(dem_race == "White, non-Hispanic", 1, 0),
         dem_race = ifelse(missing_dem_race == 0, dem_race, NA),
         dem_primary_language_english = ifelse(missing_dem_primary_language_english==0, dem_primary_language_english, NA),
         dem_married_or_partnered = ifelse(missing_dem_married_or_partnered==0, dem_married_or_partnered, NA),
         dem_household_size = ifelse(dem_household_size=="1", 1, 0),
         dem_household_size = ifelse(missing_dem_household_size==0, dem_household_size, NA),
         dem_household_size_2 = ifelse(missing_dem_household_size==0, dem_household_size_2, NA),
         dem_household_size_2plus = ifelse(missing_dem_household_size==0, dem_household_size_2plus, NA),
         dem_veteran = ifelse(missing_dem_veteran==0, dem_veteran, NA),
         dem_tanf_benefits = ifelse(missing_dem_tanf_benefits==0, dem_tanf_benefits, NA),
         dem_ssi_benefits_mandatory_optional = ifelse(missing_dem_ssi_benefits==0, dem_ssi_benefits_mandatory_optional, NA),
         dem_ssi_benefits = ifelse(dem_ssi_benefits=="Not Applicable", 1, 0),
         dem_ssi_benefits = ifelse(missing_dem_ssi_benefits==0, dem_ssi_benefits, NA)
  ) |>
  select(# baseline covariates
    dem_age, # CONTINUOUS
    dem_sex_m,
    dem_race_aian,
    dem_race_asian,
    dem_race_black,
    dem_race_hawaiian,
    dem_race_hispanic,
    dem_race_multiracial,
    dem_race, # White, non-Hispanic
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
    counseling_washout_cal,
    # post-exposure confounders
    anxiety_post_exposure_cal,
    depression_post_exposure_cal,
    bipolar_post_exposure_cal,
    exposure_nonopioid_counseling,
    # mediators/treatments
    exposure_max_daily_dose_mme, # CONTINUOUS
    exposure_days_supply, # CONTINUOUS
    exposure_distinct_prescribers, # CONTINUOUS
    exposure_opioid_benzo_copresc,
    # mediator_opioid_stimulant_copresc,
    exposure_opioid_mrelax_copresc,
    exposure_opioid_gaba_copresc,
    exposure_nonopioid_pain_rx,
    # mediator_nonopioid_gabapentin_rx,
    exposure_nonopioid_other_analgesic,
    exposure_nonopioid_antidep,
    # mediator_nonopioid_muscle_relaxant_rx,
    exposure_nonopioid_antiinflam,
    exposure_nonopioid_topical,
    # mediator_nonopioid_benzodiazepine_rx,
    exposure_nonopioid_physical_therapy,
    exposure_nonopioid_acupuncture,
    exposure_nonopioid_blocks,
    exposure_nonopioid_chiropractic,
    exposure_nonopioid_epidural_steroid,
    exposure_nonopioid_massage_therapy,
    exposure_nonopioid_trigger_point_injection,
    exposure_nonopioid_other_nonpharma,
    # outcomes
    oud_period_5,
    oud_hillary_period_5,
    # censoring
    cens_period_5,
    cens_hillary_period_5
  ) #|>
  # mutate(mediator_opioid_days_covered = mediator_opioid_days_covered*182)

df <- df |>
  mutate(baseline_covariates = NA, .before = dem_age) |>
  mutate(sex = NA, .before = dem_sex_m) |>
  mutate(dem_sex_f = 1 - dem_sex_m, .after=dem_sex_m) |>
  mutate(race = NA, .before = dem_race_aian) |>
  mutate(household_size = NA, .before = dem_household_size) |>
  mutate(ssi_benefits = NA, .before = dem_ssi_benefits_mandatory_optional) |>
  mutate(psychiatric_conditions = NA, .before = bipolar_washout_cal) |>
  mutate(post_exposure = NA, .before = anxiety_post_exposure_cal) |>
  mutate(treatments = NA, .after = exposure_nonopioid_counseling) |>
  mutate(opioid_coprescribing = NA, .before = exposure_opioid_benzo_copresc) |>
  mutate(nonopioid = NA, .before = exposure_nonopioid_pain_rx) |>
  mutate(outcomes = NA, .before = oud_period_5) |>
  mutate(censoring = NA, .before = cens_period_5) |>
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
                  "\\hspace{0.5cm}White, non-Hispanic",
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
                  "High income ($>$ 138FPL)",
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
                  "Days supply",
                  "Distinct Prescribers",
                  "Co-prescription:",
                  "\\hspace{0.5cm}Benzodiazepine",
                  # "\\hspace{0.5cm}Stimulant",
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
                  "Acupuncture",
                  "Chiropractic",
                  "Epidural steroid",
                  "Massage therapy",
                  "Trigger point injection",
                  "Blocks",
                  "Other non-pharmaceutical pain treatment",
                  "\\textbf{Outcomes (months 13-24)}",
                  "OUD by 24 months",
                  "OUD (ICD only) by 24 months",
                  "\\textbf{Censoring}",
                  "Uncensored (OUD) throughout entire study period",
                  "Uncensored (OUD ICD-only) throughout entire study period"
)

############# Preparing continuous variables
continuous_vars <- c("dem_age", "exposure_max_daily_dose_mme", "exposure_days_supply", "exposure_distinct_prescribers")
continuous_names <- c("Age", "Max daily dose (MME)", "Days supply", "Distinct Prescribers")

summarise_continuous_variable <- function(data, variable){
  if (variable != "dem_age"){
    data <- data[exposure_days_supply > 0,]
  }
  
  return(paste0(round(median(data[[variable]]),2)," (",
                round(quantile(data[[variable]], 0.25),2),", ",
                round(quantile(data[[variable]], 0.75),2),")"))
}

continuous_values <- as.vector(sapply(continuous_vars, function(variable) summarise_continuous_variable(df, variable)))




############# Preparing binary variables
number <- sapply(df, function(x) sum(x, na.rm=T))
proportion <- sapply(df, function(x) mean(x, na.rm=T))
number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")



############# Putting everything together

table_one <- data.table(
  Characteristic = demographics,
  `Number (\\%)` = number_proportion
)

table_one[Characteristic %in% continuous_names, "Number (\\%)"] <- continuous_values

# table_one[["Number (\\%)"]] <- gsub("0 (NA\\%)", "", table_one[["Number (\\%)"]])

table_one_part1 <- table_one[1:44,]
table_one_part2 <- table_one[45:72,]

write.csv(table_one, file = "~/medicaid/low-back-therapies/scripts/03_merge_cohort/table_one.csv", row.names = FALSE)
write.csv(table_one_part1, file = "~/medicaid/low-back-therapies/scripts/03_merge_cohort/table_one_part1.csv", row.names = FALSE)
write.csv(table_one_part2, file = "~/medicaid/low-back-therapies/scripts/03_merge_cohort/table_one_part2.csv", row.names = FALSE)

