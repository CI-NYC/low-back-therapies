# -------------------------------------
# Script: 00_censoring.R
# Author: Nick Williams
# Purpose: Create censoring indicators
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(lubridate)
library(arrow)
library(yaml)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data(paste0("pain_washout_continuous_enrollment_dts.fst"), file.path(drv_root, "exclusion"))

# codes for dual eligibility
codes <- read_yaml(file.path(home_dir, "data/public/eligibility_codes.yml"))

start_periods <- (0:(num_periods - 1)) * (follow_up_period_length) + 1 # +1 because the first day is exposure end_dt, and we want non-overlapping periods.
end_periods   <- start_periods + follow_up_period_length - 1 # -1 because the bookends should both be included in the period length

# Create outcome periods
periods <- lapply(seq_len(num_periods), function(i) {
  interval(
    cohort$exposure_end_dt + days(start_periods[i]),
    cohort$exposure_end_dt + days(end_periods[i])
  )
})

# Add periods to cohort
names(periods) <- paste0("period_", seq_len(num_periods))
cohort <- mutate(cohort, !!!periods)

# Load demographics dataset
demo <- open_demo()

demo <- 
  filter(demo, BENE_ID %in% cohort$BENE_ID) |> 
  collect()

in_period <- function(data, date_col, period_col, prefix) {
  mutate(data, "{prefix}_{{ period_col }}" := as.numeric({{ date_col }} %within% {{ period_col }}))
}

add_all_periods <- function(data, date_col, prefix) {
  in_period(data, {{ date_col }}, period_1, prefix) |> 
    in_period({{ date_col }}, period_2, prefix)
}

# age censoring -----------------------------------------------------------

age_cens <- 
  fselect(demo, BENE_ID, BIRTH_DT) |> 
  funique() |> 
  drop_na() |> 
  fmutate(bday_65 = BIRTH_DT + years(65)) |> 
  inner_join(cohort) |> 
  group_by(BENE_ID) |> 
  add_tally() |> 
  ungroup() |> 
  filter(n == 1) |> 
  add_all_periods(bday_65, "age_cens") |> 
  select(BENE_ID, starts_with("age_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("age_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("age_cens"))

# date censoring ----------------------------------------------------------

dec_cens <- 
  fmutate(cohort, cens_date = as.Date("2020-01-01")) |> 
  add_all_periods(cens_date, "dec_cens") |> 
  select(BENE_ID, starts_with("dec_cens"))

# dual eligibility --------------------------------------------------------

dual_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("DUAL_ELGBL_CD"), -DUAL_ELGBL_CD_LTST) |> 
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")#,
        # na.rm=T
  ) |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  arrange(elig_dt) |>
  group_by(BENE_ID) |>
  fill(code, .direction = "up") |>
  mutate(code = replace_na(code, "99")) |> # can be anything other than 00. we just want to exclude people who have all NAs for dual eligibility code
  ungroup() |>
  fsubset(code %!=% "00") |> 
  fselect(BENE_ID, code, elig_dt)

dual_cens <- 
  inner_join(cohort, dual_codes) |> 
  filter(elig_dt %within% interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length))) |> 
  group_by(BENE_ID) |>
  arrange(elig_dt) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  add_all_periods(elig_dt, "dual_elig_cens") |> 
  select(BENE_ID, starts_with("dual_elig_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("dual_elig_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("dual_elig_cens"))

dual_codes2 <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("ELGBLTY_GRP_CD"), -ELGBLTY_GRP_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  drop_na() |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner", multiple = TRUE) |> 
  fselect(BENE_ID, washout_start_dt, day0_dt, code, elig_dt) |> 
  fsubset(code %in% codes$dual_eligibility)

dual_cens2 <- 
  inner_join(cohort, dual_codes2) |> 
  filter(elig_dt %within% interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length))) |> 
  group_by(BENE_ID) |>
  arrange(elig_dt) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  add_all_periods(elig_dt, "dual_elig2_cens") |> 
  select(BENE_ID, starts_with("dual_elig2_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("dual_elig2_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("dual_elig2_cens"))

# Loss of comprehensive or full-scope benefits ------------------------------

benefits_cens <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("RSTRCTD_BNFTS_CD"), -RSTRCTD_BNFTS_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  mutate(code = replace_na(code, "0")) |>
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner", multiple = TRUE) |> 
  fselect(BENE_ID, washout_start_dt, washout_end_dt, code, elig_dt) |>
  fsubset(!code %in% c("1","7","A","D"))

benefits_cens <- 
  inner_join(cohort, benefits_cens) |> 
  filter(elig_dt %within% interval(day0_dt, exposure_end_dt + days(num_periods*follow_up_period_length))) |> 
  group_by(BENE_ID) |>
  arrange(elig_dt) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  add_all_periods(elig_dt, "benefits_cens") |> 
  select(BENE_ID, starts_with("benefits_cens")) |> 
  right_join(cohort) |> 
  mutate(across(starts_with("benefits_cens"), replace_na)) |> 
  select(BENE_ID, starts_with("benefits_cens"))

enrollment_cens <- load_data("cens_enrollment_by_period.fst", file.path(drv_root, "outcome"))

cens <- 
  list(
    age_cens,
    enrollment_cens,
    dec_cens, 
    dual_cens, 
    dual_cens2,
    benefits_cens
  ) |> 
  reduce(left_join) |> 
  mutate(cens_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         cens_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1),
         across(starts_with("cens_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("cens_period")) |> 
  lmtp::event_locf(paste0("cens_period_", 1:num_periods))

# flip censoring indicators (in lmtp, 1 indicates still observed)
cens <- mutate(cens, across(starts_with("cens"), \(x) ifelse(x == 0, 1, 0)))

cens[is.na(cens)] <- 0
write_data(cens, paste0("pain_washout_continuous_enrollment_censoring.fst"), file.path(drv_root, "outcome"))
