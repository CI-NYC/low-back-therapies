# -------------------------------------
# Script: 01_run_main.R
# Author: Anton Hung
# Purpose: Like 02_run_main.R, this script runs another script (in this case, 02_run_main.R), 
#           in a loop, while passing through parameters to specify the outcome for this run
#           There are 8 outcomes in total, so this removes the need for 8 run_main.R scripts 
#           (Likewise, it removes the need for 8 no_cens.R scripts)
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script_no_cens <- "/home/amh2389/medicaid/low-back-therapies/scripts/01_analysis/03_no_cens.R"
script_run_main <- "/home/amh2389/medicaid/low-back-therapies/scripts/01_analysis/02_run_main.R"


# setting up the pairs of outcome-censoring variable names to pass through to 02_run_main.R
outcome_cens_params <- tribble(~Y, ~cens, 
                               "oud_period_2", "cens_period_2", # 1
                               "oud_period_4", "cens_period_4", # 2 
                               "oud_hillary_period_2", "cens_hillary_period_2", # 3
                               "oud_hillary_period_4", "cens_hillary_period_4", # 4
                               "outcome_chronic_pain_period_2", "cens_chronic_pain_period_2", # 5
                               "outcome_chronic_pain_period_4", "cens_chronic_pain_period_4", # 6
                               "outcome_prolonged_opioid_use", "cens_prolonged_opioid_period_4",
                               "outcome_chronic_opioid_therapy", "cens_chronic_opioid_period_4"
)

exposures <- 1:15 # which exposures still remain to be analyzed
outcomes <- c(1,3,4,5,7,8) # which outcome to use for this run

for (i in outcomes) {

  # Run no_cens script
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script_no_cens, 
      cmdargs = c(outcome_cens_params$Y[i], outcome_cens_params$cens[i])
    )
  )
  print(i)
  # # Run run_main script
  # Rprocess <- rscript_process$new(
  #   rscript_process_options(
  #     script = script_run_main, 
  #     cmdargs = c(outcome_cens_params$Y[i], outcome_cens_params$cens[i], exposures)
  #   )
  # )
  Rprocess$wait()
  
}