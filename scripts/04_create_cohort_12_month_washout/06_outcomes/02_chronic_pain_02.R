library(tidyverse)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

chronic_pain <- readRDS(file.path(drv_root_30_day_treatment, "modified_variables/chronic_pain_wide.rds"))

# chronic_pain <- chronic_pain |>
#   group_by(BENE_ID) |>
#   summarise(outcome_chronic_pain_6mos  = as.integer(rowSums(across(all_of(paste0("chronic_pain_any_month_",  1:6))), na.rm = TRUE) > 0),
#             outcome_chronic_pain_12mos = as.integer(rowSums(across(all_of(paste0("chronic_pain_any_month_",  7:12))), na.rm = TRUE) > 0),
#             outcome_chronic_pain_18mos = as.integer(rowSums(across(all_of(paste0("chronic_pain_any_month_", 13:18))), na.rm = TRUE) > 0),
#             outcome_chronic_pain_24mos = as.integer(rowSums(across(all_of(paste0("chronic_pain_any_month_", 19:24))), na.rm = TRUE) > 0)
#   )

chronic_pain <- cohort |>
  left_join(chronic_pain) |>
  mutate(outcome_chronic_pain_period_1  = replace_na(chronic_pain_any_month_0,0),
         outcome_chronic_pain_period_2 = replace_na(chronic_pain_any_month_6,0),
         outcome_chronic_pain_period_2 = pmax(outcome_chronic_pain_period_1,outcome_chronic_pain_period_2)
  ) |>
  select(BENE_ID, outcome_chronic_pain_period_1, outcome_chronic_pain_period_2)



write_data(chronic_pain, "outcome_chronic_pain.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
