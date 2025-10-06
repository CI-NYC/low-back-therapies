# -------------------------------------
# Script: 00_main.R
# Author: Nick Williams
# Purpose: Estimate joint MTP effects for treatments on OUD at 18 months
# Notes: Run using `callr::rscript_process` with 01_run_main.R
# -------------------------------------

.libPaths(c("~/libs", .libPaths()))
library(data.table)
library(lmtp)
library(mlr3extralearners)
library(glue)
library(dplyr)

source("~/medicaid/low-back-therapies/R/helpers.R")
data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final"))

# data <- data[1:50000,]
  
args <- commandArgs(TRUE)

# options: [1, 2]
subset <- args[[1]]
# options: [1:12]
treatment <- as.numeric(args[[2]])
# options: see 01_run_main.R
body <- args[[3]]
eval(parse(text = paste('f <- function(x) ', body, sep='')))
# options: ["oud_period", "oud_hillary_period", "outcome_prolonged_opioid_use", "outcome_chronic_opioid_therapy", "outcome_chronic_pain"]
Y <- args[[4]]
cens <- args[[5]]
# options: [2, 5]
folds <- as.numeric(args[[6]])

# paramaters to modify
# learners
version <- "6_learners"
sl <- c("SL.glm", "SL.xgboost", 
        "SL.ranger",
        "SL.nnet",
        "SL.mean", "SL.earth")

SL_folds <- 2
print(paste0("CF_folds: ", folds, ", Version: ", version, ", ", paste(Y)))

use <- data |> filter(subset_oud == subset)

# Shift function function factory 
factory <- function(treatment, func) {
  fs <- lapply(1:14, function(x) function(x) x)
  fs[[treatment]] <- func
  
  function(data, m) {
    out <- list(
      fs[[1]](data[[m[1]]]),  # "exposure_acetaminophen"
      # fs[[2]](data[[m[2]]]),  # "exposure_acupuncture"
      fs[[2]](data[[m[2]]]),  # "exposure_anti-inflammatory"
      fs[[3]](data[[m[3]]]),  # "exposure_benzodiazepine"
      fs[[4]](data[[m[4]]]),  # "exposure_chiropractic"
      fs[[5]](data[[m[5]]]),  # "exposure_duloxetine"
      fs[[6]](data[[m[6]]]),  # "exposure_gabapentin"
      fs[[7]](data[[m[7]]]),  # "exposure_intervention"
      fs[[8]](data[[m[8]]]),  # "exposure_muscle relaxant"
      fs[[9]](data[[m[9]]]),  # "exposure_massage therapy"
      fs[[10]](data[[m[10]]]),   # "exposure_physical therapy"
      fs[[11]](data[[m[11]]]),   # "exposure_steroid"
      fs[[12]](data[[m[12]]]),   # "exposure_opioid"
      fs[[13]](data[[m[13]]]),   # "exposure_max_daily_dose_mme"
      fs[[14]](data[[m[14]]])   # "exposure_days_supply"
    )
    setNames(out, m)
  }
}

d <- factory(treatment, f)

W <- c(
  "dem_age",
  "dem_sex_m",
  "dem_race_aian",
  "dem_race_asian",
  "dem_race_black",
  "dem_race_hawaiian",
  "dem_race_hispanic",
  "dem_race_multiracial",
  "dem_primary_language_english", 
  "dem_married_or_partnered",
  "dem_household_size_2",
  "dem_household_size_2plus",
  "dem_veteran", 
  "dem_probable_high_income",
  "dem_tanf_benefits", 
  "dem_ssi_benefits_mandatory_optional",
  "bipolar_washout_cal",
  "anxiety_washout_cal",
  "adhd_washout_cal",
  "depression_washout_cal",
  "mental_ill_washout_cal",
  # "baseline_has_counseling",
  "missing_dem_race",
  "missing_dem_primary_language_english",
  "missing_dem_married_or_partnered",
  "missing_dem_household_size",
  "missing_dem_veteran",
  "missing_dem_tanf_benefits",
  "missing_dem_ssi_benefits"
)

A <- list(c("exposure_acetaminophen",
            # "exposure_acupuncture",
            "exposure_anti_inflammatory",
            "exposure_benzodiazepine",
            "exposure_chiropractic",
            "exposure_duloxetine",
            "exposure_gabapentin",
            "exposure_intervention",
            "exposure_muscle_relaxant",
            "exposure_massage_therapy",
            "exposure_physical_therapy",
            "exposure_steroid",
            "exposure_opioid",
            "exposure_max_daily_dose_mme",
            "exposure_days_supply"
))


fit <- lmtp_tmle(
  use,
  trt = A,
  outcome = Y,
  baseline = W,
  cens = cens,
  mtp = T,
  outcome_type = "binomial",
  learners_outcome = sl,
  learners_trt = sl,
  shift = d,
  folds = folds, 
  control = lmtp_control(.learners_outcome_folds = SL_folds,
                         .learners_trt_folds = SL_folds,
                         .discrete = F)
)

print("worked")

saveRDS(fit, file.path(drv_root, "analysis", version,  
                       glue("fit_{gsub(' ', '_', subset)}_{Y}_outcome_fix_treatment_{A[[1]][treatment]}.rds")))
