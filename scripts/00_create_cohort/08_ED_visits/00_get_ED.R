# -------------------------------------
# Script: exploring data for ED visits and pain codes
# Author:
# Purpose:
# Notes:
# -------------------------------------

library(tidyverse)
library(data.table)
library(arrow)
library(yaml)

# claims data
source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

# base cohort
cohort <- load_data("inclusion_exclusion_cohort_with_exposure_outcomes.fst", file.path(drv_root, "final"))

excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)

# Read in OTL (Other services line)
otl <- open_otl()

start_dt <- as.Date("2016-01-01")
end_dt <- as.Date("2019-12-31")

# Filter OTL to claims codes
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD", "REV_CNTR_CD")
otl <- 
  select(otl, any_of(otl_vars)) |> 
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter((grepl("^045[0-9]$|^0981$", REV_CNTR_CD) |
           grepl("^9928[1-5]|99288",LINE_PRCDR_CD)) &
           !LINE_PRCDR_CD %in% excluded_cpt_cds &
           !is.na(BENE_ID)) |>  collect() |> 
  as.data.table()

otl[, SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_E4ND_DT, LINE_SRVC_BGN_DT)]

otl <- otl[LINE_SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT)]

otl <- otl |>
  distinct(CLM_ID, .keep_all=TRUE)

write_data(otl, "ED_visits_filtered.fst", file.path(drv_root, "ED_visits"))
