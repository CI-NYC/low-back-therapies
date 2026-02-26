# -------------------------------------
# Script: 00_oud.R
# Author: Nick Williams
# Updated: Anton Hung (Feb 2026)
# Purpose: Create composite OUD survival outcome
# Notes: Eliminated hard-coded periods (period 1, period 2, etc.), to be re-runnable for different period lengths and period numbers
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(lubridate)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, day0_dt, exposure_end_dt)

# load component files ----------------------------------------------------

poison <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_poison_dts.fst", file.path(drv_root, "exclusion"))
hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_dts.fst", file.path(drv_root, "exclusion"))
opioids <- load_data("pain_washout_continuous_enrollment_opioid_requirements_pain_opioids_dts.fst", file.path(drv_root, "exclusion"))
bup <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_bup_intervals.fst", file.path(drv_root, "exclusion"))
methadone <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_methadone_intervals.fst", file.path(drv_root, "exclusion"))
nal <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_nal_intervals.fst", file.path(drv_root, "exclusion"))

# set up period intervals for evaluating outcome ----------------------------------------

cohort <- as_tibble(cohort)

start_periods <- (0:(num_periods - 1)) * (follow_up_period_length) + 1 # +1 because the first day is exposure end_dt, and we want non-overlapping periods.
end_periods   <- start_periods + follow_up_period_length - 1 # -1 because the bookends should both be included in the period length

# Build interval list
periods <- lapply(seq_len(num_periods), function(i) {
  interval(
    cohort$exposure_end_dt + days(start_periods[i]),
    cohort$exposure_end_dt + days(end_periods[i])
  )
})

# Add periods to cohort
names(periods) <- paste0("period_", seq_len(num_periods))
cohort <- mutate(cohort, !!!periods)



in_period <- function(data, date_col, period_col, overlap = FALSE, prefix) {
  if (isFALSE(overlap)) {
    return(mutate(data, "{prefix}_{{ period_col }}" := as.numeric({{ date_col }} %within% {{ period_col }})))
  }
  mutate(data, "{prefix}_{{ period_col }}" := as.numeric(int_overlaps({{ date_col }}, {{ period_col }})))
}


add_all_periods <- function(x, y, date_col, overlap, prefix) {
  left_join(x, y) |> 
    in_period({{ date_col }}, period_1, overlap, prefix) |> 
    in_period({{ date_col }}, period_2, overlap, prefix)
}

oud_hillary <- 
  fselect(hillary, BENE_ID, oud_hillary_dt) |> 
  left_join(cohort) |> 
  filter(oud_hillary_dt %within% interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length))) |> 
  roworder(BENE_ID, oud_hillary_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, oud_hillary_dt) |> 
  add_all_periods(cohort, y = _, oud_hillary_dt, FALSE, "hillary") |> 
  fmutate(hillary_period_exposure = 
            fifelse(oud_hillary_dt %within% interval(day0_dt, exposure_end_dt), 1, 0)) |> 
  select(BENE_ID, hillary_period_exposure, starts_with("hillary_period")) |> 
  fmutate(across(paste0("hillary_period_", c("exposure", seq_len(num_periods))), replace_na))

oud_poison <- 
  fselect(poison, BENE_ID, oud_poison_dt) |> 
  left_join(cohort) |> 
  filter(oud_poison_dt %within% interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length))) |> 
  roworder(BENE_ID, oud_poison_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, oud_poison_dt) |> 
  add_all_periods(cohort, y = _, oud_poison_dt, FALSE, "poison") |> 
  fmutate(poison_period_exposure = 
            fifelse(oud_poison_dt %within% interval(day0_dt, exposure_end_dt), 1, 0)) |> 
  select(BENE_ID, poison_period_exposure, starts_with("poison_period")) |> 
  fmutate(across(paste0("poison_period_", c("exposure", seq_len(num_periods))), replace_na))

oud_bup <- 
  mutate(bup, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "bup") |> 
  fmutate(bup_period_exposure = 
            fifelse(moud_period %within% interval(day0_dt, exposure_end_dt), 1, 0)) |> 
  select(BENE_ID, bup_period_exposure, starts_with("bup_period")) |> 
  fmutate(across(paste0("bup_period_", c("exposure", seq_len(num_periods))), replace_na))

oud_methadone <- 
  mutate(methadone, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "methadone") |> 
  fmutate(methadone_period_exposure = 
            fifelse(moud_period %within% interval(day0_dt, exposure_end_dt), 1, 0)) |> 
  select(BENE_ID, methadone_period_exposure, starts_with("methadone_period")) |> 
  fmutate(across(paste0("methadone_period_", c("exposure", seq_len(num_periods))), replace_na))

oud_nal <- 
  mutate(nal, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "nal") |> 
  fmutate(nal_period_exposure = 
            fifelse(moud_period %within% interval(day0_dt, exposure_end_dt), 1, 0)) |> 
  select(BENE_ID, nal_period_exposure, starts_with("nal_period")) |> 
  fmutate(across(paste0("nal_period_", c("exposure", seq_len(num_periods))), replace_na))

in_period_misuse <- function(period) {
  fselect(opioids, BENE_ID, RX_FILL_DT, PRSCRBNG_PRVDR_NPI, DSPNSNG_PRVDR_NPI, DAYS_SUPPLY) |> 
    left_join(select(cohort, BENE_ID, day0_dt, exposure_end_dt, {{ period }})) |> 
    filter(RX_FILL_DT %within% {{ period }}) |> 
    fgroup_by(BENE_ID) |> 
    fsummarise(distinct_providers = n_distinct(PRSCRBNG_PRVDR_NPI), 
               distinct_dispensers = n_distinct(DSPNSNG_PRVDR_NPI),
               total_days_supply = sum(DAYS_SUPPLY)) |> 
    mutate(
      score_providers = case_when(
        distinct_providers <= 2 ~ 0,
        distinct_providers <= 4 ~ 1,
        distinct_providers >= 5 ~ 2
      ),
      score_dispensers = case_when(
        distinct_dispensers <= 2 ~ 0,
        distinct_dispensers <= 4 ~ 1,
        distinct_dispensers >= 5 ~ 2
      ),
      score_days_supply = case_when(
        total_days_supply <= 185 ~ 0,
        total_days_supply <= 240 ~ 1,
        total_days_supply > 240 ~ 2,
        is.na(total_days_supply) ~ 0
      ), 
      "misuse_{{ period }}" := as.numeric((score_providers +  score_dispensers + score_days_supply) >= 5)
    ) |> 
    select(BENE_ID, starts_with("misuse"))
}

cohort <- mutate(cohort, period_exposure = interval(day0_dt, exposure_end_dt))


oud_misuse <- 
  list(in_period_misuse(period_exposure), 
       in_period_misuse(period_1),
       in_period_misuse(period_2)) |> 
  reduce(left_join) |> 
  right_join(select(cohort, BENE_ID)) |> 
  fmutate(across(paste0("misuse_period_", c("exposure", 1:num_periods)), replace_na))


oud <- 
  list(
    oud_hillary, 
    oud_poison, 
    oud_bup,
    oud_methadone,
    oud_nal#,
    #oud_misuse
  ) |> 
  reduce(left_join) |> 
  mutate(oud_period_exposure = if_any(.cols = ends_with("period_exposure"), \(x) x == 1),
         oud_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         oud_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1),
         across(starts_with("oud_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("oud_period")) |> 
  lmtp::event_locf(paste0("oud_period_", c("exposure", 1:num_periods)))

oud_hillary <- 
  list(
    oud_hillary
  ) |> 
  reduce(left_join) |> 
  mutate(oud_hillary_period_exposure = if_any(.cols = ends_with("period_exposure"), \(x) x == 1),
         oud_hillary_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         oud_hillary_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1),
         across(starts_with("oud_hillary_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("oud_hillary_period")) |> 
  lmtp::event_locf(paste0("oud_hillary_period_", c("exposure", 1:num_periods)))

write_data(oud, "pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst",file.path(drv_root, "outcome"))
write_data(oud_hillary, "pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_outcomes.fst",file.path(drv_root, "outcome"))
