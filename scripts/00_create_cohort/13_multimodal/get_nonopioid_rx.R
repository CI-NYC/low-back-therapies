# -------------------------------------
# Script: 03_mediator_nonopioid_pain_rx.R
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

rxl <- open_rxl()

# otl <- open_otl()

# Read in cohort and dates
dts_cohorts <- load_data("pain_cohort_cleaned_with_opioids.fst", file.path(drv_root, "final"))
setDT(dts_cohorts)
setkey(dts_cohorts, BENE_ID)
dts_cohorts <- dts_cohorts[, .(BENE_ID, index_dt)]

# Read in non opioid pain list
nop <- readRDS("/mnt/general-data/disability/mediation_unsafe_pain_mgmt/mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds")


# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% nop$NDC) |>
  collect() |> 
  as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, dts_cohorts, by = "BENE_ID"))
rxl <- merge(rxl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(index_dt, 
                                        index_dt + days(91)), 
                 .(BENE_ID, RX_FILL_DT, index_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_nop)]


# Non opioid type ------------------------------------------------------------------

nop <- nop |>
  mutate(nonopioid_type = case_when(grepl("^N03AE|^N05BA|^N05CD", atc, ignore.case=T) ~ "benzodiazepine",
                                    grepl("^N02B", atc, ignore.case = TRUE) & !grepl("^N02BF", atc, ignore.case = TRUE) ~ "other analgesics",
                                    grepl("^N02BF", atc, ignore.case = TRUE) ~ "gabapentin",
                                    grepl("^M02A", atc, ignore.case = TRUE) ~ "topical",
                                    grepl("^M01", atc, ignore.case = TRUE) ~ "antiinflammatory",
                                    grepl("^M03", atc, ignore.case = TRUE) ~ "muscle relaxant",
                                    grepl("^N06A", atc, ignore.case = TRUE) ~ "antidepressant",
                                    TRUE ~ NA)) |>
  select(NDC, nonopioid_type)

rxl2 <- rxl |>
  left_join(nop)

write_data(rxl2, "all_nonopioid_rx.fst", file.path(drv_root, "analysis/non-opioid"))
