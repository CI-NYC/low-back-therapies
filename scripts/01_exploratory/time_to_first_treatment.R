library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)


source("~/medicaid/low-back-therapies/R/helpers.R")

# dates <- load_data("low_back_cohort_treatment_dts_7day_gap.fst", file.path(drv_root, "exclusion"))

cohort <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  select(pain_diagnosis_dt, first_treatment_dt)

median(time_length(as.duration(interval(cohort$first_treatment_dt,cohort$pain_diagnosis_dt)),"days"))
quantile(time_length(as.duration(interval(cohort$first_treatment_dt,cohort$pain_diagnosis_dt)),"days"), probs = 0.25)
quantile(time_length(as.duration(interval(cohort$first_treatment_dt,cohort$pain_diagnosis_dt)),"days"), 0.75)

cohort <- cohort |>
  mutate(time_to_treatment = time_length(as.duration(interval(cohort$pain_diagnosis_dt,cohort$first_treatment_dt)),"days"))

ggplot(cohort, aes(x=time_to_treatment)) +
  geom_histogram()

paste0("0: ", 
       sum(cohort$time_to_treatment==0),
       " (",
       round(sum(cohort$time_to_treatment==0)/nrow(cohort)*100,1),
       "%)")
paste0("1-7: ", sum(cohort$time_to_treatment>=1 & cohort$time_to_treatment <=7),
       " (",
       round(sum(cohort$time_to_treatment>=1 & cohort$time_to_treatment <=7)/nrow(cohort)*100,1),
       "%)")
paste0("8-30: ", sum(cohort$time_to_treatment>=8 & cohort$time_to_treatment <=30),
       " (",
       round(sum(cohort$time_to_treatment>=8 & cohort$time_to_treatment <=30)/nrow(cohort)*100,1),
       "%)")
paste0("1month-2months: ", sum(cohort$time_to_treatment>=31 & cohort$time_to_treatment <=60),
       " (",
       round(sum(cohort$time_to_treatment>=31 & cohort$time_to_treatment <=60)/nrow(cohort)*100,1),
       "%)")
paste0("2months-3months: ", sum(cohort$time_to_treatment>=61 & cohort$time_to_treatment <=90),
       " (",
       round(sum(cohort$time_to_treatment>=61 & cohort$time_to_treatment <=90)/nrow(cohort)*100,1),
       "%)")
