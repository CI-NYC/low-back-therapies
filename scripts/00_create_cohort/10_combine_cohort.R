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

# ED_visits <- readRDS(file.path("/mnt/general-data/disability/everything-local-lmtp", "confounder_num_ED_visit.rds"))

cohort_MH_joined <- cohort |>
  left_join(adhd |> select(BENE_ID, adhd_washout_cal)) |> 
  left_join(anxiety |> select(BENE_ID, anxiety_washout_cal)) |> 
  left_join(bipolar |> select(BENE_ID, bipolar_washout_cal)) |> 
  left_join(depression |> select(BENE_ID, depression_washout_cal)) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_cal)) |>
  # left_join(ED_visits |> select(BENE_ID, has_2plus_ED_visit_exposure)) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
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
