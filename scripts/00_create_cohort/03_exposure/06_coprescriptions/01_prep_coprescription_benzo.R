# -------------------------------------
# Script: 01_mediator_benzo.R
# Author: Nick Williams
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

# Read in benzo list
ben <- readRDS(file.path(drv_root, "treatments/benzo_ndc.rds"))

# OTL ---------------------------------------------------------------------

# Filter OTL to benzo NDC
otl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")

otl <- select(otl, all_of(otl_vars)) |> 
  filter(NDC %in% ben$NDC) |>
  collect() |> 
  as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# Inner join with cohort 
otl <- unique(merge(otl, cohort, by = "BENE_ID"))
otl <- merge(otl, ben[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
otl <- otl[LINE_SRVC_BGN_DT %within% interval(pain_diagnosis_dt, 
                                              exposure_end_dt), 
           .(BENE_ID, LINE_SRVC_BGN_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, flag_benzo)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% ben$NDC) |>
  collect() |> 
  as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, cohort, by = "BENE_ID"))
rxl <- merge(rxl, ben[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(pain_diagnosis_dt, 
                                        exposure_end_dt), 
           .(BENE_ID, RX_FILL_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_benzo)]

# Export ------------------------------------------------------------------

# saveRDS(otl, file.path(drv_root, "mediator_otl_benzo_rx.rds"))
saveRDS(rxl, file.path(drv_root, "treatments/treatment_benzo_rx.rds"))

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
benzo <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, flag_benzo)], 
               rxl[, .(BENE_ID, NDC, NDC_QTY, flag_benzo)])

benzo <- unique(benzo)

benzo <- merge(cohort, benzo, all.x = TRUE) |> as.data.table()

benzo[, mediator_benzo_rx := as.numeric(any(!is.na(NDC))), by = BENE_ID]
benzo <- unique(benzo[, .(BENE_ID, mediator_benzo_rx)])

saveRDS(benzo, file.path(drv_root, "treatments/treatment_benzo_rx_bin.rds"))
