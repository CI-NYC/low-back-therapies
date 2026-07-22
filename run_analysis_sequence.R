setwd("~/medicaid/low-back-therapies/scripts")

scripts <- c(
  # "11_run_analysis_30_day_exposure/02_1_run_main_on.R",
  # "11_run_analysis_30_day_exposure/02_2_run_main_on.R",
  # "11_run_analysis_30_day_exposure/03_1_run_main_off.R",
  # "11_run_analysis_30_day_exposure/03_2_run_main_off.R",
  # "11_run_analysis_30_day_exposure/04_1_no_cens.R",
  # "11_run_analysis_30_day_exposure/04_2_no_cens.R"
  "21_run_sensitivity_analysis_3_month_exposure/02_1_run_main_on_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/02_2_run_main_on_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/03_1_run_main_off_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/03_2_run_main_off_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/02_1_run_main_on_30_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/02_2_run_main_on_30_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/03_1_run_main_off_30_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/03_2_run_main_off_30_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/04_1_no_cens_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/04_1_no_cens_30_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/04_2_no_cens_7_day_gap.R",
  "21_run_sensitivity_analysis_3_month_exposure/04_2_no_cens_30_day_gap.R"
)

for (script in scripts) {
  cat("==========================================\n")
  cat("STARTING:", script, "at", as.character(Sys.time()), "\n")
  cat("==========================================\n")
  flush.console()
  
  status <- system2(
    "Rscript",
    args = c("--vanilla", script)
  )
  
  if (status != 0) {
    stop(sprintf("FAILED: %s (exit status %d)", script, status))
  }
  
  cat("FINISHED:", script, "at", as.character(Sys.time()), "\n\n")
  flush.console()
}

cat("Pipeline completed successfully.\n")

# # plan(sequential)
# nohup bash -c '
# scripts=(
#   "11_run_analysis_30_day_exposure/02_1_run_main_on.R"
#   "11_run_analysis_30_day_exposure/02_2_run_main_on.R"
#   "11_run_analysis_30_day_exposure/03_1_run_main_off.R"
#   "11_run_analysis_30_day_exposure/03_2_run_main_off.R"
#   "11_run_analysis_30_day_exposure/04_1_no_cens.R"
#   "11_run_analysis_30_day_exposure/04_2_no_cens.R"
#   "31_run_sensitivity_analysis_12_month_washout/02_1_run_main_on.R"
#   "31_run_sensitivity_analysis_12_month_washout/02_2_run_main_on.R"
#   "31_run_sensitivity_analysis_12_month_washout/03_1_run_main_off.R"
#   "31_run_sensitivity_analysis_12_month_washout/03_2_run_main_off.R"
#   "31_run_sensitivity_analysis_12_month_washout/04_1_no_cens.R"
#   "31_run_sensitivity_analysis_12_month_washout/04_2_no_cens.R"
# )
# 
# for s in "${scripts[@]}"; do
#   echo "=========================================="
#   echo "STARTING: $s at $(date)"
#   echo "=========================================="
#   Rscript --vanilla "$s" || { echo "FAILED: $s"; exit 1; }
# done
# ' > full_pipeline.log 2>&1 &
#   
#   
# nohup bash -c 'for s in 11_run_analysis_30_day_exposure/02_1_run_main_on.R 11_run_analysis_30_day_exposure/02_2_run_main_on.R 11_run_analysis_30_day_exposure/03_1_run_main_off.R 11_run_analysis_30_day_exposure/03_2_run_main_off.R 11_run_analysis_30_day_exposure/04_1_no_cens.R 11_run_analysis_30_day_exposure/04_2_no_cens.R; do echo "STARTING: $s at $(date)"; Rscript --vanilla "$s" || { echo "FAILED: $s"; exit 1; }; echo "FINISHED: $s at $(date)"; done' > full_pipeline.log 2>&1 &
#   
# 
#   status <- system2("Rscript", c("--vanilla", script))
#   if (status != 0) stop("Failed: ", script)