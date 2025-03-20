# 08_urbanicity

library(dplyr)
library(data.table)
library(arrow)

source("~/medicaid/undertreated-pain/R/helpers.R")

drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

cohort <- load_data("inclusion_exclusion_cohort_with_exposure_outcomes.fst", file.path(drv_root, "final"))

drv <- "/mnt/general-data/disability/disenrollment/tafdebse"

urbanicity <- open_dataset(paste0(drv, "/dem_df.parquet")) |>
  select(BENE_ID, RUCC_2013) |>
  filter(!is.na(RUCC_2013)) |>
  collect()

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


cohort <- cohort |>
  left_join(urbanicity) |>
  mutate(dem_RUCC_missing = as.numeric(is.na(RUCC_2013)),
         dem_RUCC_category = case_when(
           RUCC_2013 %in% c(1,2,3) ~ "Urban",
           RUCC_2013 %in% c(4,5,6) ~ "Suburban",
           RUCC_2013 %in% c(7,8,9) ~ "Rural",
           TRUE ~ NA
         ),
         dem_RUCC_category = fifelse(dem_RUCC_missing == 1, Mode(dem_RUCC_category), dem_RUCC_category)) |>
  select(BENE_ID, dem_RUCC_category, dem_RUCC_missing)

write_data(cohort, "covariate_urbanicity.fst", file.path(drv_root, "baseline_covariates"))
