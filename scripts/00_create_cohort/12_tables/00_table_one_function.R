
library(data.table)
library(tidyverse)

source("~/medicaid/low-back-therapies/R/helpers.R")

data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root_30_day_treatment, "modified_final"))

# opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
#   select(BENE_ID, treatment_start_dt, opioid, dose_form)

table_one_function <- function(df){
  
  # df_opioids <- df |>
  #   left_join(opioids) |>
  #   filter(treatment_start_dt <= last_treatment_dt)
  
  # selected_opioids <- c(
  #   "hydrocodone", "tramadol",      "oxycodone"#,
  #   # "codeine",
  #   # "morphine",      "hydromorphone", 
  #   # "fentanyl",    "buprenorphine", "methadone"
  # )
  
  # df_opioids_wide <- df_opioids %>%
  #   select(BENE_ID, opioid) %>%
  #   mutate(opioid = ifelse(!opioid %in% selected_opioids, "other_opioid", opioid)) |>
  #   distinct() %>%
  #   mutate(present = 1) %>%
  #   pivot_wider(
  #     id_cols       = BENE_ID,
  #     names_from    = opioid,
  #     values_from   = present,
  #     values_fill   = list(present = 0),
  #     names_prefix  = "exposure_"
  #   ) %>%
  #   select(
  #     BENE_ID,
  #     all_of(paste0("exposure_", c(selected_opioids, "other_opioid")))
  #   )
  
  df <- df |>
    # left_join(df_opioids_wide) |>
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
      dem_ssi_benefits = ifelse(missing_dem_ssi_benefits==0, dem_ssi_benefits, NA),
      iph_0_washout_cal_bin = as.numeric(num_iph_washout_cal == 0),
      iph_1_washout_cal_bin = as.numeric(num_iph_washout_cal == 1),
      # iph_2_washout_cal_bin = as.numeric(num_iph_washout_cal %in% c(3,4)),
      # iph_3_washout_cal_bin = as.numeric(num_iph_washout_cal >= 5),
      oth_0_washout_cal_bin = as.numeric(num_oth_washout_cal == 0),
      oth_20_washout_cal_bin = as.numeric(num_oth_washout_cal > 0 & num_oth_washout_cal <= 25),
      oth_40_washout_cal_bin = as.numeric(num_oth_washout_cal > 25),
      # rxl_0_washout_cal_bin = as.numeric(num_rxl_washout_cal == 0),
      # rxl_10_washout_cal_bin = as.numeric(num_rxl_washout_cal > 0 & num_rxl_washout_cal <= 10),
      # rxl_20_washout_cal_bin = as.numeric(num_rxl_washout_cal > 10 & num_rxl_washout_cal <= 20),
      # rxl_30_washout_cal_bin = as.numeric(num_rxl_washout_cal > 20 & num_rxl_washout_cal <= 30),
      # rxl_40_washout_cal_bin = as.numeric(num_rxl_washout_cal > 30)
      # ed_0_washout_cal_bin = as.numeric(n_ED_visits_washout_cal == 0),
      # ed_2_washout_cal_bin = as.numeric(n_ED_visits_washout_cal %in% c(1,2)),
      # ed_4_washout_cal_bin = as.numeric(n_ED_visits_washout_cal %in% c(3,4)),
      # ed_6_washout_cal_bin = as.numeric(n_ED_visits_washout_cal >= 5)
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
      -"num_iph_washout_cal", -"num_oth_washout_cal",
      ends_with("cal_bin"),
      # treatments
      exposure_acetaminophen,
      # exposure_acupuncture,
      `exposure_anti_inflammatory`,
      exposure_benzodiazepine,
      exposure_chiropractic,
      exposure_duloxetine,
      exposure_gabapentin,
      exposure_intervention,
      `exposure_muscle_relaxant`,
      `exposure_massage_therapy`,
      `exposure_physical_therapy`,
      `exposure_spinal_cord_stimulation`,
      exposure_steroid,
      # exposure_opioid,
      # all_of(paste0("exposure_", c(selected_opioids, "other_opioid"))),
      `exposure_opioid_<=7days_<=50mme`,
      `exposure_opioid_>7days_<=50mme`,
      `exposure_opioid_>50mme`,
      # exposure_max_daily_dose_mme,
      # exposure_days_supply,
      # outcomes
      oud_period_1,
      oud_period_2,
      # oud_hillary_period_1,
      # oud_hillary_period_2,
      # outcome_prolonged_opioid_use,
      # outcome_chronic_opioid_therapy,
      # outcome_chronic_pain_period_2,
      # outcome_chronic_pain_period_4,
      # censoring
      cens_period_1,
      cens_period_2
      # cens_hillary_period_4,
      # cens_prolonged_opioid_period_4,
      # cens_chronic_opioid_period_4,
      # cens_chronic_pain_period_4
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
    mutate(dem_sex_f = 1 - dem_sex_m, .after = dem_sex_m) |>
    mutate(race = NA, .before = dem_race) |>
    mutate(household_size = NA, .before = dem_household_size) |>
    mutate(ssi_benefits = NA, .before = dem_ssi_benefits_mandatory_optional) |>
    mutate(psychiatric_conditions = NA, .before = adhd_washout_cal) |>
    mutate(healthcare_utilization = NA, .before = n_ED_visits_0_washout_cal) |>
    mutate(emergency = NA, .before = n_ED_visits_0_washout_cal) |>
    mutate(inpatient = NA, .before = iph_0_washout_cal_bin) |>
    mutate(outpatient = NA, .before = oth_0_washout_cal_bin) |>
    # mutate(prescription = NA, .before = rxl_0_washout_cal_bin) |>
    mutate(treatments = NA, .before = exposure_acetaminophen) |>
    mutate(do.call(pmax, c(across(starts_with("exposure_opioid")), na.rm = TRUE)), .before = `exposure_opioid_<=7days_<=50mme`) |>
    mutate(outcomes = NA, .before = oud_period_1) |>
    mutate(censoring = NA, .before = cens_period_1) |>
    # mutate(ed_visits = NA, .before = ed_visit_period_exposure) |>
    as.data.table()
  
  
  demographics <- c("\\textbf{Baseline covariates (months -6 to 0)}",
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
                    "Psychiatric conditions \\& counseling:",
                    "\\hspace{0.5cm}ADHD",
                    "\\hspace{0.5cm}Anxiety",
                    "\\hspace{0.5cm}Bipolar",
                    "\\hspace{0.5cm}Depression",
                    "\\hspace{0.5cm}Other mental illness",
                    "\\hspace{0.5cm}Mental health counseling",
                    "Alcohol use disorder",
                    "Other drug use disorder (non-opioid)",
                    "\\textbf{Healthcare Utilization}",
                    "Emergency department",
                    paste0("\\hspace{0.5cm}", c("0", "1-2", "3+")),
                    "Inpatient hospitalizations",
                    paste0("\\hspace{0.5cm}", c("0", "1+")),
                    "Outpatient visits",
                    paste0("\\hspace{0.5cm}", c("0", "1-25", "26+")),
                    # "Prescriptions",
                    # paste0("\\hspace{0.5cm}", c("0", "1-10", "11-20", "21-30", "31+")),
                    "\\textbf{Treatments (month 1)}",
                    "Acetaminophen",
                    # "Acupuncture",
                    "Anti-inflammatory",
                    "Benzodiazepine",
                    "Chiropractic",
                    "Duloxetine",
                    "Gabapentin",
                    "Intervention",
                    "Muscle relaxant",
                    "Massage therapy",
                    "Physical therapy",
                    "Spinal cord stimulation",
                    "Steroid",
                    "Opioid",
                    # paste0("\\hspace{0.5cm}", c(selected_opioids, "other opioid"), "†"),
                    "\\hspace{0.5cm}$\\le7$ days, $\\le50$ MME",
                    "\\hspace{0.5cm}$>7$ days, $\\le50$ MME",
                    "\\hspace{0.5cm}$>50$ MME",
                    # "\\hspace{0.5cm}Max daily MME",
                    # "\\hspace{0.5cm}Days supply",
                    "\\textbf{Outcomes (months 2-13)}",
                    "OUD or overdose by 7 months",
                    "OUD or overdose by 13 months",
                    # "OUD (ICD only) by 9 months",
                    # "OUD (ICD only) by 15 months",
                    # "At least monthly opioid prescribing",
                    # "$\\ge$90 days supply for opioids",
                    # "Chronic LBP by 9 months",
                    # "Chronic LBP by 15 months",
                    "\\textbf{Censoring}",
                    "Uncensored through 7 months",
                    "Uncensored through 13 months"
                    # "Uncensored (OUD ICD-only) throughout entire study period",
                    # "Uncensored (POU) throughout entire study period",
                    # "Uncensored (COT) throughout entire study period",
                    # "Uncensored (Chronic LBP) throughout entire study period"
                    # "\\textbf{At least 1 ED visit at:}",
                    # "\\hspace{0.5cm}Exposure",
                    # "\\hspace{0.5cm}0-3 months",
                    # "\\hspace{0.5cm}3-6 months",
                    # "\\hspace{0.5cm}6-9 months",
                    # "\\hspace{0.5cm}9-12 months",
                    # "\\hspace{0.5cm}12-15 months"
  )
  
  ############# Preparing continuous variables
  continuous_vars <- c("dem_age"
                       # "num_iph_washout_cal",
                       # "num_oth_washout_cal",
                       # "num_rxl_washout_cal",
                       # "n_ED_visits_washout_cal",
                       # "exposure_max_daily_dose_mme",
                       # "exposure_days_supply"
                       # "ed_visit_period_exposure", 
                       # "ed_visit_period_1", 
                       # "ed_visit_period_2", 
                       # "ed_visit_period_3", 
                       # "ed_visit_period_4",
                       # "ed_visit_period_5"
  )
  continuous_names <- c("Age"
                        # "\\hspace{0.5cm}Inpatient",
                        # "\\hspace{0.5cm}Outpatient",
                        # "\\hspace{0.5cm}Prescriptions",
                        # "\\hspace{0.5cm}Emergency department",
                        # "\\hspace{0.5cm}Max daily MME",
                        # "\\hspace{0.5cm}Days supply"
                        # "\\hspace{0.5cm}Exposure",
                        # "\\hspace{0.5cm}0-3 months",
                        # "\\hspace{0.5cm}3-6 months",
                        # "\\hspace{0.5cm}6-9 months",
                        # "\\hspace{0.5cm}9-12 months",
                        # "\\hspace{0.5cm}12-15 months"
  )
  
  summarise_continuous_variable <- function(data, variable){
    data <- data[data[[variable]] > 0, ]
    return(paste0(round(median(data[[variable]], na.rm=T),1)," (",
                  round(quantile(data[[variable]], 0.25, na.rm=T),1),", ",
                  round(quantile(data[[variable]], 0.75, na.rm=T),1),")"))
  }
  
  continuous_values <- as.vector(sapply(continuous_vars, function(variable) summarise_continuous_variable(df, variable)))
  
  
  
  
  ############# Preparing binary variables
  number <- sapply(df, function(x) sum(x, na.rm=T))
  proportion <- sapply(df, function(x) mean(x, na.rm=T))
  number_proportion <- paste0(number, " (", round(proportion*100,1), "\\%)")
  
  
  
  ############# Putting everything together
  
  table_one <- data.table(
    Characteristic = demographics,
    `Number (\\%)` = number_proportion
  )
  
  table_one[Characteristic %in% continuous_names, "Number (\\%)"] <- continuous_values
  
  # table_one[["Number (\\%)"]] <- gsub("0 (NA\\%)", "", table_one[["Number (\\%)"]])
  
  # table_one_part1 <- table_one[1:38,]
  # table_one_part2 <- table_one[39:63,]
  table_one
  
}
