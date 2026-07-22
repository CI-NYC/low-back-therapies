# -------------------------------------
# Script: 01_run_main.R
# Author: Nick Williams
# Purpose: Run 00_main.R using `callr`
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script <- "/home/amh2389/medicaid/low-back-therapies/scripts/21_run_sensitivity_analysis_3_month_exposure/01_lmtp_30_day_gap.R"

### Parameters to modify -------------------------

run_index <- 1 # rerun for 1,2,3,4

### ----------------------------------------------

# args <- commandArgs(TRUE)

# subset 1 is non-OUD group. subset 2 is OUD group
n_oud_param <- tribble(~subset, ~mediator, ~func, 
                       0, "1",  "(x*0)", # "exposure_acetaminophen"
                       0, "2", "(x*0)", # "exposure_anti-inflammatory"
                       0, "3" , "(x*0)", # "exposure_benzodiazepine"
                       0, "4" , "(x*0)", # "exposure_chiropractic"
                       0, "5" , "(x*0)", # "exposure_other_treatment"
                       0, "6" , "(x*0)", # "exposure_gabapentin"
                       0, "7" , "(x*0)", # "exposure_intervention"
                       0, "8" , "(x*0)", # "exposure_muscle relaxant"
                       0, "9" , "(x*0)", # "exposure_massage therapy"
                       0, "10" , "(x*0)", # "exposure_physical therapy"
                       0, "11" , "(x*0)", # "exposure_steroid"
                       0, "12" , "(x*0)", # "exposure_opioid_le7days_le50mme"
                       0, "13" , "(x*0)", # "exposure_opioid_g7days_le50mme"
                       0, "14" , "(x*0)", # "exposure_opioid_g50mme"
                       # 0, "13" , "((x*1.2) > 115)*x + ((x*1.2) <= 115)*(x*1.2)", # 115 max MME is the highest in the cohort
                       # 0, "14" , "((x*1.2) > 90)*x + ((x*1.2) <= 90)*(x*1.2)" # 90 days is the highest in the cohort
)

y_oud_param <- n_oud_param
y_oud_param$subset <- 1

(Y <- c("oud_period_1", 
       "oud_period_2", 
       "oud_hillary_period_1", 
       "oud_hillary_period_2")[run_index])
cens <- c("cens_period_1", 
          "cens_period_2", 
          "cens_period_1", 
          "cens_period_2")[run_index]

# Execute for non-OUD subgroup ---------------------------------

log_dir <- "~/medicaid/low-back-therapies/scripts/lmtp_logs"

is <- c(1:14)
processes <- vector("list", nrow(n_oud_param))

# Crossfit with 2-folds
# launch all processes without waiting
for (i in is) {
  processes[[i]] <- rscript_process$new(
    rscript_process_options(
      script  = script,
      cmdargs = c(n_oud_param$subset[i], n_oud_param$mediator[i], n_oud_param$func[i], Y, cens, 2, "off")
    )
  )
  # processes[[i]]$wait()
}

# wait on each process, then capture and write its stderr
for (i in is) {
  proc <- processes[[i]]
  proc$wait()
  log_error <- proc$read_error()
  writeLines(
    log_error,
    file.path(log_dir, Y, paste0("error_n_oud", i, ".log"))
  )
}



# # Execute for OUD subgroup -------------------------------------------
# 
# processes <- vector("list", nrow(y_oud_param))
# 
# # Crossfit with 5-folds
# # launch all processes without waiting
# for (i in is) {
#   processes[[i]] <- rscript_process$new(
#     rscript_process_options(
#       script = script,
#       cmdargs = c(y_oud_param$subset[i], y_oud_param$mediator[i], y_oud_param$func[i], Y, cens, 5)
#     )
#   )
#   # processes[[i]]$wait()
# }
# 
# # wait on each process, then capture and write its stderr
# for (i in is) {
#   proc <- processes[[i]]
#   proc$wait()
#   log_error <- proc$read_error()
#   writeLines(
#     log_error,
#     file.path(log_dir, Y, paste0("error_y_oud", i, ".log"))
#   )
# }
