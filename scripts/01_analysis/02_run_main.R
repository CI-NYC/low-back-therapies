# -------------------------------------
# Script: 01_run_main.R
# Author: Nick Williams
# Purpose: Run 00_main.R using `callr`
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script <- "~/medicaid/low-back-therapies/scripts/01_analysis/01_lmtp.R"

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
                       0, "13" , "(x*0) + 1",
                       0, "14" , "((x*1.2) > 1)*x + ((x*1.2) <= 1)*(x*1.2)",
                       0, "15" , "((x*1.2) > 1)*x + ((x*1.2) <= 1)*(x*1.2)"
                       )

y_oud_param <- n_oud_param
y_oud_param$subset <- 1

y <- "oud_period"

# Execute for chronic pain and disability ---------------------------------

# Crossfit with 2-folds
for (i in 1:nrow(n_oud_param)) {
  # if (i == 4) next
  
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script, 
      cmdargs = c(n_oud_param$subset[i], n_oud_param$mediator[i], n_oud_param$func[i], y, 2)
    )
  )
  Rprocess$wait()
}

# Execute for chronic pain only -------------------------------------------

# Crossfit with 5-folds
for (i in 1:nrow(y_oud_param)) {
  # if (i == 4) next
  
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script, 
      cmdargs = c(y_oud_param$subset[i], y_oud_param$mediator[i], y_oud_param$func[i], y, 5)
    )
  )
  Rprocess$wait()
}