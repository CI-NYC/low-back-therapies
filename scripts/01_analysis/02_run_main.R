# -------------------------------------
# Script: 01_run_main.R
# Author: Nick Williams
# Purpose: Run 00_main.R using `callr`
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script <- "/home/amh2389/medicaid/low-back-therapies/scripts/01_analysis/01_lmtp.R"

# args <- commandArgs(TRUE)

# subset 1 is non-OUD group. subset 2 is OUD group
n_oud_param <- tribble(~subset, ~mediator, ~func, 
                       0, "1",  "(x*0) + 1",
                       0, "2", "(x*0) + 1",
                       0, "3" , "(x*0) + 1",
                       0, "4" , "(x*0) + 1",
                       0, "5" , "(x*0) + 1",
                       0, "6" , "(x*0) + 1",
                       0, "7" , "(x*0) + 1",
                       0, "8" , "(x*0) + 1",
                       0, "9" , "(x*0) + 1",
                       0, "10" , "(x*0) + 1",
                       0, "11" , "(x*0) + 1",
                       0, "12" , "(x*0) + 1",
                       0, "13" , "((x*1.2) > 115)*x + ((x*1.2) <= 115)*(x*1.2)", # 115 max MME is the highest in the cohort
                       0, "14" , "((x*1.2) > 90)*x + ((x*1.2) <= 90)*(x*1.2)" # 90 days is the highest in the cohort
                       )

y_oud_param <- n_oud_param
y_oud_param$subset <- 1

Y <- "oud_hillary_period_2"
cens <- "cens_period_2"
# "oud_period_2", "cens_period_2", # 1
# "oud_period_4", "cens_period_4", # 2 
# "oud_hillary_period_2", "cens_hillary_period_2", # 3
# "oud_hillary_period_4", "cens_hillary_period_4", # 4
# "outcome_chronic_pain_period_2", "cens_chronic_pain_period_2", # 5
# "outcome_chronic_pain_period_4", "cens_chronic_pain_period_4", # 6
# "outcome_prolonged_opioid_use", "cens_prolonged_opioid_period_4",
# "outcome_chronic_opioid_therapy", "cens_chronic_opioid_period_4"

# Execute for non-OUD subgroup ---------------------------------

log_dir <- "~/medicaid/low-back-therapies/scripts/lmtp_logs"

is <- c(12:14)
processes <- vector("list", nrow(n_oud_param))

# Crossfit with 2-folds
# launch all processes without waiting
for (i in is) {
  processes[[i]] <- rscript_process$new(
    rscript_process_options(
      script  = script,
      cmdargs = c(n_oud_param$subset[i], n_oud_param$mediator[i], n_oud_param$func[i], Y, cens, 2)
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
