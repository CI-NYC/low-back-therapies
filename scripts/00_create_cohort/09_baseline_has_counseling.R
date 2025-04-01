# -------------------------------------
# Script:
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/low-back-therapies/R/helpers.R")

mediator <- "Counseling"

# Read in OTL (Other services line) 
otl <- open_otl()

# Read in cohort and dates
cohort <- load_data("pain_cohort.fst", file.path(drv_root, "final")) |>
  select(BENE_ID, washout_start_dt, pain_diagnosis_dt) |>
  as.data.table()

# Read in CPT, HCPC, and ICD10 codes for mediator claims
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/mediator_codes.yml")
codes <- c(names(codes[[mediator]]$CPT), 
           names(codes[[mediator]]$HCPC), 
           names(codes[[mediator]]$ICD10))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
  filter(LINE_PRCDR_CD %in% codes) |>
  collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(washout_start_dt, pain_diagnosis_dt - 1), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, washout_start_dt, pain_diagnosis_dt, LINE_PRCDR_CD)]

# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims <- claims[, .(counseling_washout_cal = as.numeric(.N > 0)), by = "BENE_ID"]
claims <- merge(claims, cohort[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a claim
fix <- "counseling_washout_cal"
claims[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

saveRDS(claims, file.path(drv_root, "baseline_covariates/baseline_has_counseling.rds"))
