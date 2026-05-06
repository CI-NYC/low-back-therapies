library(callr)
library(future)
library(future.apply)

print(renv::paths$library())
print(.libPaths())

setwd("~/medicaid/low-back-therapies/scripts/00_create_cohort")


job_groups <- list(
  # group00 = "00_filter_diagnosis_claims.R",
  # group0 = "01_filter_study_ndc.R",
  # group1 = paste0("03_treatment_dts/", c("03_cohort_mme_join.R","04_nonopioid_pain_rx.R","05_non_pharmacologic.R")),
  # group2 = paste0("03_treatment_dts/", c("06_compute_treatment_dts.R","06_compute_treatment_dts_7_day_gap.R")),
  # group3 = "03_treatment_dts/07_finalize_treatment_dts.R",
  # group4 = "04_01_filter_continuous_enrollment.R",
  # group5 = "04_02_filter_continuous_enrollment.R",
  # group6 = "05_exposure/01_modify_exposure_end_dt.R",
  # group7 = paste0("05_exposure/", c("03_days_supply.R", "04_max_mme.R")),
  # group8 = "05_exposure/09_combine_exposures.R",
  # group9 = c("05_opioid_naive_exclusions.R","05_other_pain_exclusions.R"),
  # group10 = paste0("06_oud/", c("00_bup.R", "00_hillary.R", "00_methadone.R", "00_misuse/00_study_pain_opioids.R")),
  # group11 = paste0("06_oud/", c("00_naltrexone.R", "00_poison.R", "00_misuse/01_washout_misuse.R")),
  # group13 = c("06_oud/01_oud_washout.R", "06_outcomes/05_getting_enrollment_dates.R"),
  # group14 = paste0("06_outcomes/", c("06_censoring_enrollment.R", "01_oud.R")),
  # group15 = "06_outcomes/07_censoring_combined.R",
  # group20 = c("06_tafdebse_exclusions.R", "06_tafiph_exclusions.R", "06_tafoth_exclusions.R"),
  # group21 = c("07_combine_exclusions_exposure_outcome.R")
  group22 = c("08_baseline_covariates.R"),
  group23 = paste0("10_comorbidity/", c("02_anxiety.R","04_depression.R", "06_ed_visits/04_ED_visits_find_all.R", "06_ed_visits/05_surgeries_and_IP_admissions.R")),
  group24 = paste0("10_comorbidity/", c("01_adhd.R", "03_bipolar.R", "05_mental_illness.R", "06_ed_visits/06_ED_visits_clean.R")),
  group25 = paste0("10_comorbidity/", c("07_baseline_ip_op.R", "08_counseling.R", "09_drug_use_disorder.R", "06_ed_visits/07_count_ED_visits.R")),
  group26 = c("10_combine_cohort.R"),
  group27 = c("11_clean_impute_analysis_data.R"),
  group28 = "12_tables/01_finalize_table_one.R"
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


plan(multisession, workers = 4)

tryCatch(
  run_jobs_future(job_groups),
  error = function(e) {
    message("Workflow halted: ", conditionMessage(e))
    quit(save = "no", status = 1, runLast = FALSE)
  }
)

plan(sequential)
