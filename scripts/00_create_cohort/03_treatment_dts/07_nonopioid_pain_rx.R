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

source("~/medicaid/low-back-therapies/R/helpers.R")

rxl <- open_rxl()

otl <- open_otl()

# load cohort and opioid data
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  mutate(exposure_end_dt = pain_diagnosis_dt + days(90)) |>
  select(BENE_ID, pain_diagnosis_dt, exposure_end_dt)

# Read in non opioid pain list
nop <- readRDS(file.path(drv_root, "treatments/nonopioid_pain_ndc.rds"))

# OTL ---------------------------------------------------------------------

# Filter OTL to non-opioid pain NDC
otl_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "NDC", "NDC_QTY")

otl <- select(otl, all_of(otl_vars)) |> 
  filter(NDC %in% nop$NDC) |>
  collect() |> 
  as.data.table()

otl[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                  LINE_SRVC_END_DT, 
                                  LINE_SRVC_BGN_DT)]

# Inner join with cohort 
otl <- unique(merge(otl, cohort, by = "BENE_ID"))
otl <- merge(otl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
otl <- otl[LINE_SRVC_BGN_DT %within% interval(pain_diagnosis_dt, 
                                              exposure_end_dt), 
           .(BENE_ID, LINE_SRVC_BGN_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, flag_nop)]

# RXL ---------------------------------------------------------------------

rxl_vars <- c("BENE_ID", "RX_FILL_DT", "NDC", "NDC_QTY", "DAYS_SUPPLY")

rxl <- select(rxl, all_of(rxl_vars)) |> 
  filter(NDC %in% nop$NDC) |>
  collect() |> 
  as.data.table()

# Inner join with cohort 
rxl <- unique(merge(rxl, cohort, by = "BENE_ID"))
rxl <- merge(rxl, nop[, c(1, 3)], all.x = TRUE, by = "NDC")

# Filter to claims within mediator time-frame
rxl <- rxl[RX_FILL_DT %within% interval(pain_diagnosis_dt, 
                                        exposure_end_dt), 
           .(BENE_ID, RX_FILL_DT, pain_diagnosis_dt, exposure_end_dt, NDC, NDC_QTY, DAYS_SUPPLY, flag_nop)]

# Combine dates --------------------------------------------------------------

nop_categorized <- nop |>
  mutate(treatment = case_when(
    grepl("^M01", atc, ignore.case = TRUE) ~ "nonopioid_antiinflamatory",
    grepl("^M02A", atc, ignore.case = TRUE) ~ "nonopioid_topical",
    grepl("^M03", atc, ignore.case = TRUE) ~ "nonopioid_muscle_relax",
    grepl("^N02B", atc, ignore.case = TRUE) ~ "nonopioid_other_analgesic",
    grepl("^N06A", atc, ignore.case = TRUE) ~ "nonopioid_antidepressant",
    TRUE ~ "nonopioid_uncategorized"))

nop_rx_dts <- rbind(otl |> select(BENE_ID, treatment_start_dt = LINE_SRVC_BGN_DT, treatment_end_dt = LINE_SRVC_BGN_DT, NDC),
                rxl |> 
                  mutate(treatment_end_dt = RX_FILL_DT + days(DAYS_SUPPLY - 1)) |>
                  select(BENE_ID, treatment_start_dt = RX_FILL_DT, treatment_end_dt, NDC)
                ) |>
  left_join(nop_categorized) |>
  select(BENE_ID,treatment_start_dt,treatment_end_dt,treatment)
  arrange(treatment_start_dt, desc(treatment_end_dt))

saveRDS(nop_rx_dts, file.path(drv_root, "exclusion/nop_rx_dts.rds"))

# Non-opioid NDC/ATC codes ---------------------------------------------------

M01 <- nop[grepl("^M01", atc, ignore.case = TRUE)]
M02A <- nop[grepl("^M02A", atc, ignore.case = TRUE)]
M03 <- nop[grepl("^M03", atc, ignore.case = TRUE)]
N02B <- nop[grepl("^N02B", atc, ignore.case = TRUE)]
N06A <- nop[grepl("^N06A", atc, ignore.case = TRUE)]

rxl[, `:=`(exposure_nonopioid_antiinflammatory = fifelse(NDC %in% M01$NDC, 1, 0), 
           exposure_nonopioid_topical = fifelse(NDC %in% M02A$NDC, 1, 0), 
           exposure_nonopioid_muscle_relax = fifelse(NDC %in% M03$NDC, 1, 0), 
           exposure_nonopioid_other_analgesic = fifelse(NDC %in% N02B$NDC, 1, 0), 
           exposure_nonopioid_antidepressant = fifelse(NDC %in% N06A$NDC, 1, 0))]

otl[, `:=`(exposure_nonopioid_antiinflammatory = fifelse(NDC %in% M01$NDC, 1, 0), 
           exposure_nonopioid_topical = fifelse(NDC %in% M02A$NDC, 1, 0), 
           exposure_nonopioid_muscle_relax = fifelse(NDC %in% M03$NDC, 1, 0), 
           exposure_nonopioid_other_analgesic = fifelse(NDC %in% N02B$NDC, 1, 0), 
           exposure_nonopioid_antidepressant = fifelse(NDC %in% N06A$NDC, 1, 0))]

# Make binary -------------------------------------------------------------

# Combine both datasets and keep only unique rows
nop <- rbind(otl[, .(BENE_ID, NDC, NDC_QTY, 
                     exposure_nonopioid_antiinflammatory, 
                     exposure_nonopioid_topical,
                     exposure_nonopioid_muscle_relax,
                     exposure_nonopioid_other_analgesic,
                     exposure_nonopioid_antidepressant)], 
             rxl[, .(BENE_ID, NDC, NDC_QTY, 
                     exposure_nonopioid_antiinflammatory, 
                     exposure_nonopioid_topical,
                     exposure_nonopioid_muscle_relax,
                     exposure_nonopioid_other_analgesic,
                     exposure_nonopioid_antidepressant)])

nop <- unique(nop)

nop <- group_by(nop, BENE_ID) |> 
  summarize(across(starts_with("exposure"), \(x) as.numeric(sum(x) > 0))) |> 
  as.data.table(key = "BENE_ID")

nop <- merge(cohort |> select(BENE_ID), nop, all.x = TRUE)
nop[is.na(nop)] <- 0

saveRDS(nop, file.path(drv_root, "treatments/nop_binary_refactor.rds"))
