# -------------------------------------
# Script: 01_run_main.R
# Author: Nick Williams
# Purpose: Run 00_main.R using `callr`
# Notes:
# -------------------------------------

library(callr)
library(tibble)

script <- "scripts/06_analysis/06mo/00_main.R"

dcp_param <- tribble(~subset, ~mediator, ~func, 
                     "low back pain", 1,  "(x*0) + 1", # "mediator_has_minimally_invasive_spinal_procedure.rds"
                     "low back pain", 2,  "(x*0) + 1", # "mediator_nonopioid_pain_rx"
                     "low back pain", 3, "(x*0) + 1", # "mediator_has_physical_therapy"
                     "low back pain", 4, "(x*0) + 1") # "mediator_has_multimodal_pain_treatment_restrict"

cp_param <- dcp_param
cp_param$subset <- "chronic pain only"

y <- "oud_24mo_icd"

# Execute for chronic pain and disability ---------------------------------

# Crossfit with 5-folds
for (i in 1:nrow(dcp_param)) {
  if (i == 4) next
  
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script, 
      cmdargs = c(dcp_param$subset[i], dcp_param$mediator[i], dcp_param$func[i], y, 5)
    )
  )
  Rprocess$wait()
}

# Execute for chronic pain only -------------------------------------------

# Crossfit with 2-folds
for (i in 1:nrow(dcp_param)) {
  # if (i == 4) next
  
  Rprocess <- rscript_process$new(
    rscript_process_options(
      script = script, 
      cmdargs = c(cp_param$subset[i], cp_param$mediator[i], cp_param$func[i], y, 2)
    )
  )
  Rprocess$wait()
}