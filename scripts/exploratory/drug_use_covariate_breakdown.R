# -------------------------------------
# Script: drug_use_disorder
# Author: Anton Hung 
# Purpose: Identify substance use disorder during the washout period
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(arrow)
library(data.table)
library(yaml)

# claims data
source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_cohort.fst", file.path(drv_root_30_day_treatment, "modified_final"))

# Read in OTH
oth <- open_oth()

# Read in IPH
iph <- open_iph()

codebook <- read_yaml(file.path(home_dir, "data/public/mediator_codes.yml"))
codebook_2 <- names(codebook$`Other substances`$ICD10)

find_substance <- function(which_substance, new_column_name) {
  # codes <- (names(codebook[[which_substance]]$ICD10))
  # pattern <- paste(codes, collapse = "|")
  pattern <- codebook_2[which_substance]
  substance_oth <- oth |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~ grepl(pattern, .))) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  substance_iph <- iph |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~ grepl(pattern, .))) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  has_substance_abuse <- rbind(substance_oth, substance_iph) |>
    right_join(cohort) |>
    filter(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt)) |>
    group_by(BENE_ID) |>
    mutate(!!new_column_name := 1) |>
    ungroup() |>
    select(BENE_ID, !!new_column_name) |>
    distinct()
}

### ALCOHOL
sud_alcohol_washout_cal <- find_substance("Alcohol", "sud_alcohol_washout_cal")

### OTHER SUBSTANCES
sud_other_washout_cal <- find_substance("Other substances", "sud_other_washout_cal")

sud_cannabis <- find_substance(1, "cannabis")
sud_sedatives <- find_substance(2, "sedatives")
sud_cocaine <- find_substance(3, "cocaine")
sud_other_stimulants <- find_substance(4, "other_stimulants")
sud_hallucinogen <- find_substance(5, "hallucinogen")
sud_nicotine <- find_substance(6, "nicotine")

sud_inhalant <- find_substance(7, "inhalant")
sud_other_psychoactive <- find_substance(8, "other_psychoactive")

nrow(sud_cannabis)
nrow(sud_sedatives)
nrow(sud_cocaine)
nrow(sud_other_stimulants)
nrow(sud_hallucinogen)
nrow(sud_nicotine)
nrow(sud_inhalant)
nrow(sud_other_psychoactive)

nrow(sud_cannabis)/nrow(cohort)*100
nrow(sud_sedatives)/nrow(cohort)*100
nrow(sud_cocaine)/nrow(cohort)*100
nrow(sud_other_stimulants)/nrow(cohort)*100
nrow(sud_hallucinogen)/nrow(cohort)*100
nrow(sud_nicotine)/nrow(cohort)*100
nrow(sud_inhalant)/nrow(cohort)*100
nrow(sud_other_psychoactive)/nrow(cohort)*100










# 12 Month Washout ---------------------------------------------------------

cohort <- load_data("pain_cohort.fst", file.path(drv_root_12_month_washout, "modified_final"))


sud_alcohol_washout_cal <- find_substance("Alcohol", "sud_alcohol_washout_cal")

### OTHER SUBSTANCES
sud_other_washout_cal <- find_substance("Other substances", "sud_other_washout_cal")

sud_cannabis <- find_substance(1, "cannabis")
sud_sedatives <- find_substance(2, "sedatives")
sud_cocaine <- find_substance(3, "cocaine")
sud_other_stimulants <- find_substance(4, "other_stimulants")
sud_hallucinogen <- find_substance(5, "hallucinogen")
sud_nicotine <- find_substance(6, "nicotine")

sud_inhalant <- find_substance(7, "inhalant")
sud_other_psychoactive <- find_substance(8, "other_psychoactive")

nrow(sud_cannabis)
nrow(sud_sedatives)
nrow(sud_cocaine)
nrow(sud_other_stimulants)
nrow(sud_hallucinogen)
nrow(sud_nicotine)
nrow(sud_inhalant)
nrow(sud_other_psychoactive)

nrow(sud_cannabis)/nrow(cohort)*100
nrow(sud_sedatives)/nrow(cohort)*100
nrow(sud_cocaine)/nrow(cohort)*100
nrow(sud_other_stimulants)/nrow(cohort)*100
nrow(sud_hallucinogen)/nrow(cohort)*100
nrow(sud_nicotine)/nrow(cohort)*100
nrow(sud_inhalant)/nrow(cohort)*100
nrow(sud_other_psychoactive)/nrow(cohort)*100
