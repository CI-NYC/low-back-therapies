library(tidyverse)
library(fst)
library(data.table)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

# base cohort
cohort <- load_data("inclusion_exclusion_cohort_with_exposure_outcomes.fst", file.path(drv_root, "final"))

# Load opioids and filter to the first one for each beneficiary?
opioids <- load_data("all_opioids.fst", file.path(drv_root, "ED_visits"))

# Load ED visits
ED_visits <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned.rds")

cohort <- cohort |>
  inner_join(opioids) |>
  mutate(index_dt = RX_FILL_DT) |>
  select(BENE_ID, index_dt)

merged_cohort <- cohort |>
  left_join(ED_visits) |>
  as.data.table()

ED_visit_1mos <- merged_cohort[start_dt >= index_dt & 
                          start_dt <= index_dt %m+% months(1), 
                        .(ED_visit_1mos = .N), 
                        by=BENE_ID] 

ED_visit_3mos <- merged_cohort[start_dt >= index_dt & 
                          start_dt <= index_dt %m+% months(3), 
                        .(ED_visit_3mos = .N), 
                        by=BENE_ID] 

ED_visit_6mos <- merged_cohort[start_dt >= index_dt & 
                          start_dt <= index_dt %m+% months(6), 
                        .(ED_visit_6mos = .N), 
                        by=BENE_ID] 

cohort <- cohort |>
  left_join(ED_visit_1mos) |>
  left_join(ED_visit_3mos) |>
  left_join(ED_visit_6mos) |>
  mutate(ED_visit_1mos = fifelse(is.na(ED_visit_1mos), 0, ED_visit_1mos),
         ED_visit_3mos = fifelse(is.na(ED_visit_3mos), 0, ED_visit_3mos),
         ED_visit_6mos = fifelse(is.na(ED_visit_6mos), 0, ED_visit_6mos))

write_data(cohort, "num_visits_by_month.rds", file.path(drv_root, "ED_visits"))

paste0(table(cohort$ED_visit_1mos==0)[2], " (",
       round(prop.table(table(cohort$ED_visit_1mos==0))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_1mos==1)[2], " (",
       round(prop.table(table(cohort$ED_visit_1mos==1))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_1mos>=2)[2], " (",
       round(prop.table(table(cohort$ED_visit_1mos>=2))[2]*100,1), "%)")


paste0(table(cohort$ED_visit_3mos==0)[2], " (",
       round(prop.table(table(cohort$ED_visit_3mos==0))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_3mos==1)[2], " (",
       round(prop.table(table(cohort$ED_visit_3mos==1))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_3mos>=2)[2], " (",
       round(prop.table(table(cohort$ED_visit_3mos>=2))[2]*100,1), "%)")


paste0(table(cohort$ED_visit_6mos==0)[2], " (",
       round(prop.table(table(cohort$ED_visit_6mos==0))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_6mos==1)[2], " (",
       round(prop.table(table(cohort$ED_visit_6mos==1))[2]*100,1), "%)")
paste0(table(cohort$ED_visit_6mos>=2)[2], " (",
       round(prop.table(table(cohort$ED_visit_6mos>=2))[2]*100,1), "%)")
