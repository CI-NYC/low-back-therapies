# -------------------------------------
# Script: 06_tafdebse_exclusions.R
# Author: Nick Williams
# Purpose:
# Notes: Modified from https://github.com/CI-NYC/disability-chronic-pain/blob/main/scripts/02_clean_tafdebse.R
# -------------------------------------

library(collapse)
library(fst)
library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(yaml)
library(purrr)
library(stringr)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

codes <- read_yaml(file.path(home_dir, "data/public/eligibility_codes.yml"))

# Load demographics dataset
demo <- open_demo()

demo <- right_join(demo, cohort) |> 
  collect()

# exclude maryland --------------------------------------------------------
exclusion_md <-
  fselect(demo, BENE_ID, washout_start_dt, RFRNC_YR, STATE_CD) |>
  fsubset(year(washout_start_dt) == as.numeric(RFRNC_YR)) |>
  fmutate(exclusion_maryland = as.integer("MD" == STATE_CD)) |>
  fsubset(exclusion_maryland == 1) |>
  fselect(BENE_ID, exclusion_maryland) |>
  funique()

# age ---------------------------------------------------------------------

# Remove observations with more than 1 birthdate? 
exclusion_age <- 
  fselect(demo, BENE_ID, washout_start_dt, BIRTH_DT) |> 
  distinct() |> 
  drop_na() |> 
  fmutate(age_enrollment = floor(time_length(interval(BIRTH_DT, washout_start_dt), "years")), 
          exclusion_age = fcase(age_enrollment < 19, 1, 
                                age_enrollment >= 64, 1, 
                                default = 0)) |> 
  group_by(BENE_ID) |> 
  add_tally() |> 
  fmutate(exclusion_double_bdays = ifelse(n > 1, 1, 0)) |> 
  summarise(exclusion_age = max(exclusion_age),
            exclusion_double_bdays = max(exclusion_double_bdays)) |>
  ungroup() |>
  fselect(BENE_ID, exclusion_age, exclusion_double_bdays)

# sex ---------------------------------------------------------------------

exclusion_sex <- 
  fselect(demo, BENE_ID, SEX_CD) |> 
  fgroup_by(BENE_ID)|>
  fsummarise(exclusion_missing_sex = as.integer(all(is.na(SEX_CD)))) |>
  fungroup() |>
  fselect(BENE_ID, exclusion_missing_sex)

# eligibility codes -------------------------------------------------------

eligibility_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("ELGBLTY_GRP_CD"), -ELGBLTY_GRP_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  drop_na() |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner", multiple = TRUE) |> 
  fselect(BENE_ID, washout_start_dt, washout_end_dt, code, elig_dt)

eligibility_codes <- 
  fsubset(eligibility_codes, elig_dt %within% interval(washout_start_dt, washout_end_dt))

# Filter to last eligiblity code in washout time period
wo_eligibility_codes <- 
  roworder(eligibility_codes, elig_dt) |> 
  group_by(BENE_ID) |> 
  filter(elig_dt == max(elig_dt)) |> 
  ungroup()

exclusion_codes <- wo_eligibility_codes |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(
    exclusion_pregnancy_eligibility       = any(code %in% codes$pregnant, na.rm = TRUE),
    exclusion_institution     = any(code %in% codes$institution, na.rm = TRUE),
    exclusion_cancer          = any(code %in% codes$cancer, na.rm = TRUE),
    exclusion_dual_eligible_1 = any(code %in% codes$dual_eligibility, na.rm = TRUE),
    probable_high_income_cal  = any(code %in% codes$income, na.rm = TRUE)
  )

exclusion_codes <- 
  fselect(cohort, BENE_ID) |> 
  join(exclusion_codes, how = "left") |> 
  mutate(across(c(starts_with("exclusion"), probable_high_income_cal) , ~ replace_na(.x))) 

income_codes <- exclusion_codes |>
  fselect(BENE_ID, probable_high_income_cal)

write_data(income_codes, "probable_high_income_cal.fst", file.path(drv_root, "exclusion"))

exclusion_codes <- exclusion_codes |> 
  fselect(BENE_ID, exclusion_pregnancy_eligibility, exclusion_institution, exclusion_cancer, exclusion_dual_eligible_1)

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
# > tmp <- dual_codes2 |> group_by(BENE_ID)|> summarise(res = all(is.na(code)))

exclusion_dual_eligible <- 
  join(cohort, dual_codes, how = "inner", multiple = TRUE) |> 
  fmutate(exclusion_dual_eligible = as.integer(elig_dt %within% interval(washout_start_dt, washout_end_dt))) |> 
  fsubset(exclusion_dual_eligible==1) |> 
  fselect(BENE_ID, exclusion_dual_eligible) |> 
  funique() |> 
  join(fselect(cohort, BENE_ID), how = "right") |> 
  fmutate(exclusion_dual_eligible = replace_na(as.numeric(exclusion_dual_eligible),0))

# Managed care beneficiaries in Colorado and Arkansas -------------------------

mco_codes <- demo |>
  filter(STATE_CD %in% c("CO", "AR")) |>
  select(BENE_ID, RFRNC_YR, starts_with("MC")) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code"),
        na.rm=T
  ) |> 
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          mc_dt = as.Date(paste0(year, "-", month, "-01"))) |>
  fsubset(code %in% c("01","04")) |>  # these are the MC codes we want to exclude. 04 doesn't appear in our data, but just in case, it is left in the code.
  fselect(BENE_ID, code, mc_dt)

exclusion_mco <- 
  join(cohort, mco_codes, how = "inner", multiple = TRUE) |> 
  fmutate(exclusion_managed_care = as.integer(mc_dt %within% interval(washout_start_dt, washout_end_dt))) |> 
  fsubset(exclusion_managed_care==1) |> 
  fselect(BENE_ID, exclusion_managed_care) |> 
  distinct() |> 
  join(fselect(cohort, BENE_ID), how = "right") |> 
  fmutate(exclusion_managed_care = replace_na(as.numeric(exclusion_managed_care),0))

# Restricted benefits -----------------------------------------------------

benefits_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("RSTRCTD_BNFTS_CD"), -RSTRCTD_BNFTS_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  mutate(code = replace_na(code, "0")) |>
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner", multiple = TRUE) |> 
  fselect(BENE_ID, washout_start_dt, washout_end_dt, code, elig_dt)

benefits_codes <- 
  fsubset(benefits_codes, elig_dt %within% interval(washout_start_dt, washout_end_dt))

exclusion_benefits <- benefits_codes |>
  fgroup_by(BENE_ID) |>
  fsummarise(exclusion_comp_bnfts = as.integer(any(!code %in% c("1","7","A","D"))))

# join --------------------------------------------------------------------

exclusions <- 
  list(exclusion_md,
       exclusion_age, 
       exclusion_sex, 
       exclusion_codes, 
       exclusion_dual_eligible,
       exclusion_mco,
       exclusion_benefits) |> 
  reduce(join, on = "BENE_ID", how = "full")|>
  mutate(across(everything(), ~ replace_na(., 0)))

exclusions <- 
  fmutate(exclusions, 
          exclusion_dual_eligible = 
            as.numeric((exclusion_dual_eligible_1 + exclusion_dual_eligible) >= 1)) |> 
  fselect(-exclusion_dual_eligible_1)


# TEST -----------------------------------------------------------------------

# test1 <- exclusions |>
#   arrange(BENE_ID)
# 
# test2 <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion")) |>
#   arrange(BENE_ID)
# 
# print(sum(test1$BENE_ID!=test2$BENE_ID))
# 
# # testing how many values are different in each column
# for (i in 2:ncol(test1)){
#   out <- sum(test1[,i]!=test2[,i])
#   print(paste0(names(test1)[i],": ",out))
# }

write_data(exclusions, "pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))


