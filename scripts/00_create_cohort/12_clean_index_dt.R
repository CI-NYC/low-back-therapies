library(tidyverse)
library(data.table)
library(stringr)
library(readr)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

cohort <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |> as.data.table()

opioids <- load_data("all_opioids.fst", file.path(drv_root, "ED_visits"))

# Load ED visits
# ED_visits <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned.rds")

cohort <- cohort |>
  inner_join(opioids) |>
  mutate(index_dt = RX_FILL_DT)
  # select(BENE_ID, index_dt)

# merged_cohort <- cohort |>
#   left_join(ED_visits) |>
#   as.data.table()

write_data(cohort, "pain_cohort_cleaned_with_opioids.fst", file.path(drv_root, "final"))
