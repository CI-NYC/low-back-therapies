# -------------------------------------
# Script: 00_main.R
# Author: Nick Williams
# Purpose: Estimate joint MTP effects for treatments on OUD at 18 months
# Notes: Run using `callr::rscript_process` with 01_run_main.R
# -------------------------------------

library(data.table)
library(lmtp)
library(mlr3superlearner)
library(mlr3extralearners)
library(glue)
library(dplyr)

set.seed(1)
source("~/medicaid/low-back-therapies/R/helpers.R")

### Uncomment whichever cohort is relevant to your current run -----------------

# # regular cohort and results directory
# data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) 
# version <- "opioid_categorized"

# # sensitivity analysis cohort and results directory
# data <- load_data("pain_cohort_clean_imputed_7day_gap.fst", file.path(drv_root, "final")) 
# version <- "sensitivity"

# cohort with 30 day exposure and results dir
data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root_30_day_treatment, "modified_final"))
version <- "30_day_exposure"

# paramaters to modify
run_index <- 1

message(sprintf("Executing 01_lmtp.R for run_index = %d", run_index))

sl <- list("glm", "lightgbm",
        # "ranger",
        # "nnet",
        "mean", "earth",
        list("cv_glmnet", alpha = 1))

SL_folds <- 2

Y <- c("oud_period_1", 
       "oud_period_2", 
       "oud_hillary_period_1", 
       "oud_hillary_period_2")[run_index]
cens <- c("cens_period_1", 
          "cens_period_2", 
          "cens_period_1", 
          "cens_period_2")[run_index]

print(paste0("no_cens; ", ", Version: ", version, ", ", paste(Y)))

data_n_oud <- data |> filter(subset_oud == 0)


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
            "exposure_other_treatment",
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

# --------
# No cens
# --------
fit <- lmtp_tmle(
  data_n_oud,
  trt = A,
  outcome = Y,
  baseline = W,
  cens = cens,
  outcome_type = "binomial",
  learners_outcome = sl,
  learners_trt = sl,
  shift = NULL,
  folds = 2,
  control = lmtp_control(.learners_outcome_folds = SL_folds,
                         .learners_trt_folds = SL_folds,
                         .discrete = F,
                         .trim=0.995)
)


saveRDS(fit, file.path(drv_root, "analysis", version,
                       glue("fit_outcome_{Y}_no_cens.rds")))


# fit <- lmtp_tmle(
#   data_y_oud,
#   trt = A,
#   outcome = Y,
#   baseline = W,
#   cens = cens,
#   outcome_type = "binomial",
#   learners_outcome = sl,
#   learners_trt = sl,
#   shift = NULL,
#   folds = 5,
#   control = lmtp_control(.learners_outcome_folds = SL_folds,
#                          .learners_trt_folds = SL_folds,
#                          .discrete = F,
#                          .trim=0.995)
# )
# 
# 
# saveRDS(fit, file.path(drv_root, "analysis", version,
#                        glue("fit_1_{Y}_outcome_fix_no_cens.rds")))

