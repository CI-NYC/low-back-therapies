# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_cohort.fst", file.path(drv_root, "final"))
adhd <- load_data("adhd.rds", file.path(drv_root, "baseline_covariates")) 
anxiety <- load_data("anxiety.rds", file.path(drv_root, "baseline_covariates"))
bipolar <- load_data("bipolar.rds", file.path(drv_root, "baseline_covariates"))
depression <- load_data("depression.rds", file.path(drv_root, "baseline_covariates"))
mental_ill <- load_data("mental_ill.rds", file.path(drv_root, "baseline_covariates"))
baseline_counseling <- load_data("counseling.fst", file.path(drv_root, "baseline_covariates"))
substance_use_disorder <- load_data("substance_use_disorder_washout_cal.fst", file.path(drv_root, "baseline_covariates"))
num_inpatient_outpatient <- load_data("baseline_ip_op_rx.fst", file.path(drv_root, "baseline_covariates")) |>
  mutate(num_iph_washout_cal = as.numeric(num_iph_washout_cal >= 1))
num_ed_visits <- load_data("cohort_num_ED_visits.fst", file.path(drv_root, "baseline_covariates")) |>
  mutate(n_ED_visits_0_washout_cal = as.numeric(n_ED_visits_washout_cal == 0),
         n_ED_visits_1_washout_cal = as.numeric(n_ED_visits_washout_cal %in% c(1,2)),
         n_ED_visits_3_washout_cal = as.numeric(n_ED_visits_washout_cal >= 3)) |>
  select(-n_ED_visits_washout_cal)

cohort_MH_joined <- cohort |>
  left_join(adhd |> select(BENE_ID, adhd_washout_cal)) |> 
  left_join(anxiety |> select(BENE_ID, anxiety_washout_cal)) |> 
  left_join(bipolar |> select(BENE_ID, bipolar_washout_cal)) |> 
  left_join(depression |> select(BENE_ID, depression_washout_cal)) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_cal)) |>
  left_join(baseline_counseling |> select(BENE_ID, counseling_washout_cal)) |>
  left_join(substance_use_disorder) |>
  left_join(num_inpatient_outpatient) |>
  left_join(num_ed_visits) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
         starts_with("exposure"),
         starts_with("subset"),
         starts_with("cens"),
         starts_with("oud"),
         starts_with("outcome")
  )

write_data(cohort_MH_joined, "pain_cohort_with_MH.fst", file.path(drv_root, "final"))






### 7 days
rm(cohort)
rm(cohort_MH_joined)

cohort <- load_data("pain_cohort_7day_gap.fst", file.path(drv_root, "final"))

cohort_MH_joined <- cohort |>
  left_join(adhd |> select(BENE_ID, adhd_washout_cal)) |> 
  left_join(anxiety |> select(BENE_ID, anxiety_washout_cal)) |> 
  left_join(bipolar |> select(BENE_ID, bipolar_washout_cal)) |> 
  left_join(depression |> select(BENE_ID, depression_washout_cal)) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_cal)) |>
  left_join(baseline_counseling |> select(BENE_ID, counseling_washout_cal)) |>
  left_join(substance_use_disorder) |>
  left_join(num_inpatient_outpatient) |>
  left_join(num_ed_visits) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
         starts_with("exposure"),
         starts_with("subset"),
         starts_with("cens"),
         starts_with("oud"),
         starts_with("outcome")
  )

write_data(cohort_MH_joined, "pain_cohort_with_MH_7day_gap.fst", file.path(drv_root, "final"))
