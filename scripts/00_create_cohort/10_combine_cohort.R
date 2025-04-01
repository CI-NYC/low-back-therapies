# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------
library(tidyverse)
source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_cohort.fst", file.path(drv_root, "final"))
baseline_has_counseling <- readRDS(file.path(drv_root, "baseline_covariates/baseline_has_counseling.rds"))
adhd <- load_data("adhd.rds", file.path(drv_root, "baseline_covariates")) 
anxiety <- load_data("anxiety.rds", file.path(drv_root, "baseline_covariates"))
bipolar <- load_data("bipolar.rds", file.path(drv_root, "baseline_covariates"))
depression <- load_data("depression.rds", file.path(drv_root, "baseline_covariates"))
mental_ill <- load_data("mental_ill.rds", file.path(drv_root, "baseline_covariates"))
anxiety_post_exposure <- load_data("anxiety_post_exposure.rds", file.path(drv_root, "baseline_covariates"))
bipolar_post_exposure <- load_data("bipolar_post_exposure.rds", file.path(drv_root, "baseline_covariates"))
depression_post_exposure <- load_data("depression_post_exposure.rds", file.path(drv_root, "baseline_covariates"))

# ED_visits <- readRDS(file.path("/mnt/general-data/disability/everything-local-lmtp", "confounder_num_ED_visit.rds"))

cohort_MH_joined <- cohort |>
  left_join(baseline_has_counseling |> select(BENE_ID, counseling_washout_cal)) |>
  left_join(adhd |> select(BENE_ID, adhd_washout_cal)) |> 
  left_join(anxiety |> select(BENE_ID, anxiety_washout_cal)) |> 
  left_join(bipolar |> select(BENE_ID, bipolar_washout_cal)) |> 
  left_join(depression |> select(BENE_ID, depression_washout_cal)) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_cal)) |>
  left_join(anxiety_post_exposure |> select(BENE_ID, anxiety_post_exposure_cal)) |>
  left_join(bipolar_post_exposure |> select(BENE_ID, bipolar_post_exposure_cal)) |>
  left_join(depression_post_exposure |> select(BENE_ID, depression_post_exposure_cal)) |>
  
  # left_join(ED_visits |> select(BENE_ID, has_2plus_ED_visit_exposure)) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
         ends_with("_post_exposure_cal"),
         # "has_2plus_ED_visit_exposure",
         starts_with("exposure"), 
         starts_with("subset"), 
         cens_period_1, oud_period_1, 
         cens_period_2, oud_period_2, 
         cens_period_3, oud_period_3, 
         cens_period_4, oud_period_4, 
         cens_period_5, oud_period_5,
         cens_hillary_period_1, oud_hillary_period_1, 
         cens_hillary_period_2, oud_hillary_period_2, 
         cens_hillary_period_3, oud_hillary_period_3, 
         cens_hillary_period_4, oud_hillary_period_4, 
         cens_hillary_period_5, oud_hillary_period_5
  )

write_data(cohort_MH_joined, "pain_cohort_with_MH.fst", file.path(drv_root, "final"))
