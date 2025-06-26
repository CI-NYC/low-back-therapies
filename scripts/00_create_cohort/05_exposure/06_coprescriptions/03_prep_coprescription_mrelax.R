# -------------------------------------
# Script: 03_mediator_nonopioid_pain_rx.R
# Author: Nick Williams
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(data.table)
library(fst)
library(yaml)
library(foreach)

source("~/medicaid/low-back-therapies/R/helpers.R")


# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, pain_diagnosis_dt, exposure_end_dt)

# Read in non opioid pain list
mrelax <- readRDS(file.path(drv_root, "treatments/nonopioid_pain_ndc.rds")) |> 
  filter(grepl("^M03", atc, ignore.case = TRUE))


# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% mrelax$NDC) |>
  collect() |> 
  as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, cohort, by = "BENE_ID"))
rxl <- merge(rxl, mrelax[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(pain_diagnosis_dt, 
                                        exposure_end_dt), 
           .(BENE_ID, RX_FILL_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_nop)]

# Export ------------------------------------------------------------------

saveRDS(rxl, file.path(drv_root, "treatments/treatment_mrelax_rx.rds"))