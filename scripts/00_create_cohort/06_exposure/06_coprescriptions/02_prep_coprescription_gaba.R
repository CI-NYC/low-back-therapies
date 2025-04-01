# -------------------------------------
# Script: 01_mediator_gabapentinoid.R
# Author: Sarah Forrest
# Updated:
# Purpose: 
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(data.table)
library(fst)

source("~/medicaid/low-back-therapies/R/helpers.R")

rxl <- open_rxl()

otl <- open_otl()

# load cohort and opioid data
cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, pain_diagnosis_dt, exposure_end_dt)
opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatments"))

# Read in gabapentinoid list
gab <- readRDS(file.path(drv_root, "treatments/gabapentinoid_ndc.rds"))

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% gab$NDC) |>
  collect() |> 
  as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, cohort, by = "BENE_ID"))
rxl <- merge(rxl, gab[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(pain_diagnosis_dt, 
                                        exposure_end_dt), 
           .(BENE_ID, RX_FILL_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_gab)]

# Export ------------------------------------------------------------------

saveRDS(rxl, file.path(drv_root, "treatments/treatment_gaba_rx.rds"))

