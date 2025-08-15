library(callr)
library(future)
library(future.apply)

setwd("~/medicaid/low-back-therapies/scripts/00_create_cohort")


job_groups <- list(
  # group1 = "03_treatment_dts/10_finalize_treatment_dts_02.R",
  # group2 = "04_01_filter_continuous_enrollment.R",
  # group3 = "04_02_filter_continuous_enrollment.R",
  group4 = paste0("06_outcomes/", c("03_prolonged_opioid_use_01.R","04_chronic_opioid_therapy.R")),
  group5 = c("07_combine_exclusions_exposure_outcome.R"),
  group6 = c("08_baseline_covariates.R"),
  group7 = c("10_combine_cohort.R"),
  group8 = c("11_clean_impute_analysis_data.R", "11_clean_impute_analysis_data_7day_gap.R"),
  group9 = "12_tables/01_finalize_table_one.R"
)

# still need to do 00_getting_enrollment_dates_02.R


plan(multisession, workers = 4)

run_jobs_future <- function(groups, log_dir = "../logs") {
  # make sure log directory exists
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  for (grp in names(groups)) {
    scripts <- groups[[grp]]
    message("Launching group: ", grp)
    
    # launch all scripts in parallel futures
    futures <- future_lapply(scripts, function(file) {
      # build a log-file name, e.g. "logs/group2_07_finalize_treatment_dts.R.log"
      script_name <- tools::file_path_sans_ext(basename(file))
      log_file     <- file.path(log_dir, paste0(grp, "_", script_name, ".log"))
      
      callr::rscript(
        script = file,
        stdout = log_file,
        stderr = log_file
      )
      TRUE  # return TRUE on success
    }, future.seed = TRUE)
    
    message("Group ", grp, " completed successfully")
  }
}

tryCatch(
  run_jobs_future(job_groups),
  error = function(e) {
    message("Workflow halted: ", conditionMessage(e))
    quit(save = "no", status = 1, runLast = FALSE)
  }
)