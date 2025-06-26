# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(dplyr)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

df <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  filter(subset_oud == 0)

df <- df |>
  mutate(
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
    starts_with("dem_race"),
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
    ends_with("cal"),
    # treatments
    starts_with("exposure")#,
    # outcomes
    # oud_period_5,
    # oud_hillary_period_5,
    # # censoring
    # cens_period_5,
    # cens_hillary_period_5,
    # ed_visit_period_exposure,
    # ed_visit_period_1,
    # ed_visit_period_2,
    # ed_visit_period_3,
    # ed_visit_period_4,
    # ed_visit_period_5
  ) #|>
# mutate(mediator_opioid_days_covered = mediator_opioid_days_covered*182)

df <- df |>
  mutate(baseline_covariates = NA, .before = dem_age) |>
  mutate(sex = NA, .before = dem_sex_m) |>
  mutate(dem_sex_f = 1 - dem_sex_m, .after=dem_sex_m) |>
  mutate(race = NA, .before = dem_race) |>
  mutate(household_size = NA, .before = dem_household_size) |>
  mutate(ssi_benefits = NA, .before = dem_ssi_benefits_mandatory_optional) |>
  mutate(psychiatric_conditions = NA, .before = adhd_washout_cal) |>
  mutate(treatments = NA, .before = exposure_opioid) |>
  # mutate(outcomes = NA, .before = oud_period_5) |>
  # mutate(censoring = NA, .before = cens_period_5) |>
  # mutate(ed_visits = NA, .before = ed_visit_period_exposure) |>
  as.data.table()


demographics <- c("\\textbf{Baseline covariates (months 1-6)}",
                  "Age",
                  "Sex",
                  "\\hspace{0.5cm}Male",
                  "\\hspace{0.5cm}Female",
                  "Race/ethnicity:",
                  "\\hspace{0.5cm}White, non-Hispanic",
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
                  "High income ($>$ 138FPL)",
                  "TANF Benefits",
                  "\\hspace{0.5cm}Unknown",
                  "SSI benefits:",
                  "\\hspace{0.5cm}Mandatory or optional",
                  "\\hspace{0.5cm}Not applicable",
                  "\\hspace{0.5cm}Unknown",
                  "Psychiatric conditions \\& counseling (months 1-6):",
                  "\\hspace{0.5cm}ADHD",
                  "\\hspace{0.5cm}Anxiety",
                  "\\hspace{0.5cm}Bipolar",
                  "\\hspace{0.5cm}Depression",
                  "\\hspace{0.5cm}Other mental illness",
                  "\\textbf{Treatments (months 7-12)}",
                  "Opioid",
                  "Duloxetine",
                  "Anti-inflammatory",
                  "Muscle relaxant",
                  "Physical therapy",
                  "Intervention",
                  "Benzodiazepine",
                  "Chiropractic",
                  "Acupuncture",
                  "Blocks",
                  "Gabapentin",
                  "Max MME",
                  "Days supply"
                  # "\\textbf{Outcomes (months 13-24)}",
                  # "OUD by 24 months",
                  # "OUD (ICD only) by 24 months",
                  # "\\textbf{Censoring}",
                  # "Uncensored (OUD) throughout entire study period",
                  # "Uncensored (OUD ICD-only) throughout entire study period",
                  # "\\textbf{At least 1 ED visit at:}",
                  # "\\hspace{0.5cm}Exposure",
                  # "\\hspace{0.5cm}0-3 months",
                  # "\\hspace{0.5cm}3-6 months",
                  # "\\hspace{0.5cm}6-9 months",
                  # "\\hspace{0.5cm}9-12 months",
                  # "\\hspace{0.5cm}12-15 months"
)

############# Preparing continuous variables
continuous_vars <- c("dem_age",
                     "exposure_max_daily_dose_mme",
                     "exposure_days_supply"
                     # "ed_visit_period_exposure", 
                     # "ed_visit_period_1", 
                     # "ed_visit_period_2", 
                     # "ed_visit_period_3", 
                     # "ed_visit_period_4",
                     # "ed_visit_period_5"
                     )
continuous_names <- c("Age",
                      "Max MME",
                      "Days supply"
                      # "\\hspace{0.5cm}Exposure",
                      # "\\hspace{0.5cm}0-3 months",
                      # "\\hspace{0.5cm}3-6 months",
                      # "\\hspace{0.5cm}6-9 months",
                      # "\\hspace{0.5cm}9-12 months",
                      # "\\hspace{0.5cm}12-15 months"
                      )

summarise_continuous_variable <- function(data, variable){
  if (variable != "dem_age"){
    # data <- data[exposure_days_supply > 0,]
    return(paste0(sum(data[[variable]]>1),
                  " (", round(mean(data[[variable]]>1)*100,2), "\\%)")
    )
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

table_one_part1 <- table_one[1:38,]
table_one_part2 <- table_one[39:52,]

# write.csv(table_one, file = "~/medicaid/low-back-therapies/scripts/12_tables/table_one.csv", row.names = FALSE)
write.csv(table_one_part1, file = "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables/table_one_part1_oud_no.csv", row.names = FALSE)
write.csv(table_one_part2, file = "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables/table_one_part2_oud_no.csv", row.names = FALSE)
