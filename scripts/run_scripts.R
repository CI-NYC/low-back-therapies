library(callr)
library(future)
library(future.apply)

setwd("~/medicaid/low-back-therapies/scripts/00_create_cohort")


job_groups <- list(
  # group1 = paste0("03_treatment_dts/", c("03_cohort_mme_join.R","04_nonopioid_pain_rx.R","05_non_pharmacologic.R")),
  # group2 = paste0("03_treatment_dts/", c("06_compute_treatment_dts.R","06_compute_treatment_dts_7_day_gap.R")),
  # group3 = "03_treatment_dts/07_finalize_treatment_dts.R"
  # group4 = "04_01_filter_continuous_enrollment.R",
  # group5 = "04_02_filter_continuous_enrollment.R",
  # group6 = paste0("05_exposure/", c("03_days_supply.R", "03_days_supply_7day.R")),
  # group6 = "05_exposure/04_max_mme.R",
  group7 = "05_exposure/04_max_mme_7day.R",
  group8 = "05_exposure/09_combine_exposures.R",
  # group9 = paste0("06_outcomes/", c("03_prolonged_opioid_use_01.R","04_chronic_opioid_therapy.R")),
  group10 = c("07_combine_exclusions_exposure_outcome.R"),
  group11 = c("08_baseline_covariates.R"),
  group12 = c("10_combine_cohort.R"),
  group13 = c("11_clean_impute_analysis_data.R", "11_clean_impute_analysis_data_7day_gap.R"),
  group14 = "12_tables/01_finalize_table_one.R"
)


# 1a. Flatten all paths
all_paths <- unlist(job_groups, use.names = FALSE)

# 1b. Test existence
exists_vec <- file.exists(all_paths)

# 1c. Report
if (all(exists_vec)) {
  message("✅ All files exist.")
} else {
  missing <- all_paths[!exists_vec]
  warning("❌ Missing files:\n", paste(missing, collapse = "\n"))
}



run_jobs_future <- function(groups, log_dir = "../logs") {
  on.exit(future::plan(sequential), add = TRUE)
  
  # make sure log directory exists
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  for (grp in names(groups)) {
    scripts <- groups[[grp]]
    message("Launching group: ", grp)
    
    # launch all scripts in parallel futures
    futures <- future_lapply(scripts, function(file) {
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


plan(multisession, workers = 2)

tryCatch(
  run_jobs_future(job_groups),
  error = function(e) {
    message("Workflow halted: ", conditionMessage(e))
    quit(save = "no", status = 1, runLast = FALSE)
  }
)

plan(sequential)
