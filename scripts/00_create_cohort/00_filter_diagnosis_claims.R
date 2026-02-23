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

codes_inpatient <- c(13, 21, 32, 24, 55, 31, 09, 41)

# codes for low back pain, based on search terms
diagnosis_icds <- read.csv(file.path(home_dir, "data/public/chronic_pain_icd10_20230216.csv")) |>
  filter(grepl("low back", ICD_DESC, ignore.case = TRUE) |
           grepl("lumb", ICD_DESC, ignore.case = TRUE) |
           grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")


codes <- diagnosis_icds$ICD9_OR_10

start_dt <- as.Date("2016-07-01")
end_dt <- as.Date("2019-10-01")

keep <- c("BENE_ID", 
          "CLM_ID", 
          "POS_CD",
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_claims <- 
  select(oth, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |>
  filter(!POS_CD %in% codes_inpatient) |>
  collect() |> 
  as.data.table()


oth_claims[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)]

oth_claims <- oth_claims[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, SRVC_BGN_DT, DGNS_CD_1, DGNS_CD_2)]

setkey(oth_claims, BENE_ID, CLM_ID)
oth_claims <- unique(oth_claims, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_claims <- oth_claims[!is.na(BENE_ID)]

# Sort by date and take first claim for each BENE_ID to calculate washout start date
setorder(oth_claims, BENE_ID, SRVC_BGN_DT)
oth_claims[, let(washout_start_dt = min(SRVC_BGN_DT)), BENE_ID]
oth_claims <- oth_claims[SRVC_BGN_DT == washout_start_dt]
oth_claims[, let(washout_start_dt = washout_start_dt - days(182))]

# temporary - recording what the pain diagnosis code is so we can join with pain categories later
oth_claims[, let(dgns_cd = ifelse(DGNS_CD_1 %in% codes, DGNS_CD_1, DGNS_CD_2))]

oth_claims <- unique(oth_claims[, .(BENE_ID, washout_start_dt, SRVC_BGN_DT)], by = 1)

setnames(oth_claims, "SRVC_BGN_DT", "diagnosis_dt")

# temporary - adding pain categories for exploratory purposes
# oth_pain <- oth_pain |>
#   distinct() |>
#   left_join(chronic_pain_df, by = c("dgns_cd" = "ICD9_OR_10"))

write_data(distinct(oth_claims), "low_back_washout_dts.fst", file.path(drv_root, "exclusion"))

# number of people with MSK pain claims
oth_claims |> nrow()

# 2659773
