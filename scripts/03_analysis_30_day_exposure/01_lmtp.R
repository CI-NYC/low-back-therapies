# -------------------------------------
# Script: 00_main.R
# Author: Nick Williams
# Purpose: Estimate joint MTP effects for treatments on OUD at 18 months
# Notes: Run using `callr::rscript_process` with 01_run_main.R
# -------------------------------------

.libPaths(c("~/libs", .libPaths()))
library(data.table)
library(lmtp)
library(mlr3superlearner)
library(mlr3extralearners)
library(glue)
library(dplyr)

set.seed(1)
source("~/medicaid/low-back-therapies/R/helpers.R")

### Uncomment whichever cohort is relevant to your current run -----------------

# # sensitivity analysis cohort with 30-day gap between treatments
# data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final"))
# version <- "opioid_categorized"

# # sensitivity analysis cohort with 7-day gap between treatments
# data <- load_data("pain_cohort_clean_imputed_7day_gap.fst", file.path(drv_root, "final")) 
# version <- "sensitivity"

# cohort with 30 day exposure
data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root_30_day_treatment, "modified_final"))
version <- "30_day_exposure"

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
intervention <- args[[7]]

# paramaters to modify
# learners
sl <- list("glm", "lightgbm",
        # "ranger",
        # "nnet",
        "mean","earth",
        list("cv_glmnet", alpha = 1)
        )

SL_folds <- 2
print(paste0("CF_folds: ", folds, ", Version: ", version, ", ", paste(Y)))

use <- data |> filter(subset_oud == subset)

# Shift function function factory 
factory <- function(treatment, func) {
  # modification which is only applicable for opioid categories, due to being mutually exclusive
  fs <- lapply(1:14, function(x) function(x) x)
  if (treatment %in% c(12,13,14)) {
    remainder <- setdiff(c(12, 13, 14), treatment)
    fs[[treatment]] <- func
    fs[remainder] <- list(function(x) x*0, function(x) x*0)
  } else {
    fs[[treatment]] <- func
  }
  
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
      fs[[12]](data[[m[12]]]),   # "exposure_opioid_le7days_le50mme"
      fs[[13]](data[[m[13]]]),   # "exposure_opioid_g7days_le50mme"
      fs[[14]](data[[m[14]]])   # "exposure_opioid_g50mme"
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
  "counseling_washout_cal",
  "sud_alcohol_washout_cal",
  "sud_other_washout_cal",
  "num_iph_washout_cal",
  "num_oth_washout_cal",
  # "num_rxl_washout_cal",
  "n_ED_visits_0_washout_cal",
  "n_ED_visits_1_washout_cal",
  "n_ED_visits_3_washout_cal",
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
            # "exposure_opioid",
            # "exposure_max_daily_dose_mme",
            # "exposure_days_supply"
            "exposure_opioid_<=7days_<=50mme",
            "exposure_opioid_>7days_<=50mme",
            "exposure_opioid_>50mme"
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
                         .discrete = F,
                         .trim=0.995)
)

print("worked")

saveRDS(fit, file.path(drv_root, "analysis", version,  
                       glue("fit_{intervention}_outcome_{Y}_treatment_{A[[1]][treatment]}.rds")))
