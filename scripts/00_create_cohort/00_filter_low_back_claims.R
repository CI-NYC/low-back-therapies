# -------------------------------------
# Script: 00_filter_low_back_claims.R
# Author: Anton Hung
# Purpose: Find all low back pain claims within the specified date range 
#   in the Other Services and Inpatient files
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)
library(fst)

source("~/medicaid/low-back-therapies/R/helpers.R")

# Load necessary datasets
oth <- open_oth()
iph <- open_iph()

# codes for low back pain, based on search terms
low_back_pain_icds <- read.csv("~/medicaid/low-back-therapies/data/public/chronic_pain_icd10_20230216.csv") |>
  filter(grepl("low back", ICD_DESC, ignore.case = TRUE) |
           grepl("lumb", ICD_DESC, ignore.case = TRUE) |
           grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")


codes <- low_back_pain_icds$ICD9_OR_10

start_dt <- as.Date("2016-07-01")
end_dt <- as.Date("2019-10-01")

keep <- c("BENE_ID", 
          "CLM_ID", 
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_pain <- 
  select(oth, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |>
  collect() |> 
  as.data.table()

iph_pain <-
  select(iph, any_of(keep)) |>
  filter(DGNS_CD_1 %in% codes |
           DGNS_CD_2 %in% codes |
           DGNS_CD_3 %in% codes |
           DGNS_CD_4 %in% codes |
           DGNS_CD_5 %in% codes |
           DGNS_CD_6 %in% codes |
           DGNS_CD_7 %in% codes |
           DGNS_CD_8 %in% codes |
           DGNS_CD_9 %in% codes |
           DGNS_CD_10 %in% codes) |>
  collect() |>
  as.data.table()

oth_pain[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)]

oth_pain <- oth_pain[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, SRVC_BGN_DT, DGNS_CD_1, DGNS_CD_2)]

iph_pain <- iph_pain[SRVC_BGN_DT %within% interval(start_dt, end_dt),
                   .SD,
                   .SDcols = c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", paste0("DGNS_CD_", 1:10))]

setkey(oth_pain, BENE_ID, CLM_ID)
oth_pain <- unique(oth_pain, by = c(1, 2))

setkey(iph_pain, BENE_ID, CLM_ID)
iph_pain <- unique(iph_pain, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_pain <- oth_pain[!is.na(BENE_ID)]
iph_pain <- iph_pain[!is.na(BENE_ID)]

# Sort by date and take first claim for each BENE_ID to calculate washout start date
setorder(oth_pain, BENE_ID, SRVC_BGN_DT)
oth_pain[, let(washout_start_dt = min(SRVC_BGN_DT)), BENE_ID]
oth_pain <- oth_pain[SRVC_BGN_DT == washout_start_dt]
oth_pain[, let(washout_start_dt = washout_start_dt - days(182))]

# temporary - recording what the pain diagnosis code is so we can join with pain categories later
oth_pain[, let(dgns_cd = ifelse(DGNS_CD_1 %in% codes, DGNS_CD_1, DGNS_CD_2))]

setorder(iph_pain, BENE_ID, SRVC_BGN_DT)
iph_pain[, let(washout_start_dt = min(SRVC_BGN_DT)), BENE_ID]
iph_pain <- iph_pain[SRVC_BGN_DT == washout_start_dt]
iph_pain[, let(washout_start_dt = washout_start_dt - days(182))]

oth_pain <- unique(oth_pain[, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)], by = 1)
iph_pain <- unique(iph_pain[, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)], by = 1)

low_back <- rbindlist(list(oth_pain, iph_pain))
low_back[, let(washout_start_dt_comb = min(washout_start_dt)), BENE_ID]
low_back <- low_back[washout_start_dt == washout_start_dt_comb, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)]

setnames(low_back, "SRVC_BGN_DT", "pain_diagnosis_dt")

# temporary - adding pain categories for exploratory purposes
# oth_pain <- oth_pain |>
#   distinct() |>
#   left_join(chronic_pain_df, by = c("dgns_cd" = "ICD9_OR_10"))

write_data(distinct(low_back), "low_back_washout_dts.fst", file.path(drv_root, "exclusion"))

# number of people with MSK pain claims
oth_pain |> nrow()
