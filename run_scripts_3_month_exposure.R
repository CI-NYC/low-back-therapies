library(callr)
library(future)
library(future.apply)

print(renv::paths$library())
print(.libPaths())

setwd("~/medicaid/low-back-therapies/scripts/20_create_cohort_sensitivity_analysis")


job_groups <- list(
  # group00 = "00_filter_diagnosis_claims.R",
  # group0 = paste0("05_exposure/", c("03_days_supply_7day.R", "03_days_supply.R")),
  # group1 = paste0("05_exposure/", c("04_max_mme_7day.R", "04_max_mme.R")),
  # group2 = paste0("05_exposure/", c("09_combine_exposures.R")),
  # group3 = paste0("06_outcomes/", c("05_getting_enrollment_dates.R", "01_oud.R")),
  # group4 = paste0("06_outcomes/", c("06_censoring_enrollment.R")),
  # group5 = "06_outcomes/07_censoring_combined.R",
  group21 = c("07_combine_exclusions_exposure_outcome.R"),
  group22 = c("08_baseline_covariates.R"),

  group26 = c("10_combine_cohort.R"),
  group27 = c("11_clean_impute_analysis_data.R"),
  group28 = "12_tables/01_finalize_table_one.R"
  # group30 = "02_1_run_main_on.R",
  # group31 = "02_2_run_main_on.R",
  # group32 = "03_1_run_main_off.R",
  # group33 = "03_2_run_main_off.R",
  # group34 = "04_1_no_cens.R",
  # group35 = "04_2_no_cens.R"
)


# 1a. Flatten all paths
all_paths <- normalizePath(unlist(job_groups, use.names = FALSE), mustWork = FALSE)

# 1b. Test existence
exists_vec <- file.exists(all_paths)

# 1c. Report
if (all(exists_vec)) {
  message("✅ All files exist.")
} else {
  missing <- all_paths[!exists_vec]
  stop("❌ Missing files:\n", paste(missing, collapse = "\n"), call. = FALSE)}



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


# plan(multisession, workers = 4)

tryCatch(
  run_jobs_future(job_groups),
  error = function(e) {
    message("Workflow halted: ", conditionMessage(e))
    quit(save = "no", status = 1, runLast = FALSE)
  }
)

# plan(sequential)
