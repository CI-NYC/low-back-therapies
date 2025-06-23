# -------------------------------------
# Script: 04_filter_opioid_ndc.R
# Author: Nick Williams
# Purpose: Filter to observations with no opioids in washout but opioids in the exposure period
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/low-back-therapies/R/helpers.R")

ndc <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(treatment_start_dt_possible_latest = pain_diagnosis_dt + days(90))

# find opioid ndcs --------------------------------------------------------

opioids <- names(codes[["Opioid pain"]]$ATC)

opioid_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(opioids, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc_opioids <- ndc[opioid_flag]

saveRDS(ndc_opioids, "~/medicaid/low-back-therapies/data/public/ndc_to_atc_opioids.rds")

# filter rxl and otl files ------------------------------------------------

ndc_opioids <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_opioids.rds")

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find beneficiaries with an opioid in the washout period in OTL
otl <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT)
  ) |> 
  filter((LINE_SRVC_BGN_DT >= pain_diagnosis_dt) & 
           (LINE_SRVC_BGN_DT <= treatment_start_dt_possible_latest), 
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  select(BENE_ID, rx_start_dt = LINE_SRVC_BGN_DT, rx_end_dt = LINE_SRVC_BGN_DT) |>
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid in the washout period in RXL
rxl <- 
  #select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, NDC) |> 
  rxl |>
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= pain_diagnosis_dt) & 
           (RX_FILL_DT <= treatment_start_dt_possible_latest), 
         NDC %in% ndc_opioids$NDC,
         !(NDC %in% c("27505005036", "27505005096"))) |> # lucemyra -- not an opioid
  distinct()

rxl <- collect(rxl) |> 
  mutate(rx_end_dt = RX_FILL_DT + days(DAYS_SUPPLY - 1)) |>
  select(BENE_ID, rx_start_dt = RX_FILL_DT, rx_end_dt) |>
  as.data.table()

# # remove observations with opioid in washout period
# remove <- rbind(otl, rxl) |> unique()
# 
# # number of patients with opioids in washout
# remove |> nrow()
# 
# cohort <- anti_join(cohort, remove)

# # Read in RXL (pharmacy line)
# rxl <- open_rxl()

# # Read in OTL (Other services line)
# otl <- open_otl()

# # Find beneficiaries with an opioid in the exposure period in OTL
# otl <-
#   select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |>
#   inner_join(cohort, by = "BENE_ID") |>
#   mutate(LINE_SRVC_BGN_DT = ifelse(
#     is.na(LINE_SRVC_BGN_DT),
#     LINE_SRVC_END_DT,
#     LINE_SRVC_BGN_DT
#   )) |>
#   filter((LINE_SRVC_BGN_DT > pain_diagnosis_dt) &
#            (LINE_SRVC_BGN_DT <= exposure_end_dt),
#          NDC %in% ndc_opioids$NDC) |>
#   select(BENE_ID) |>
#   distinct()

# otl <- collect(otl) |> as.data.table()

# # Find beneficiaries with an opioid in the exposure period in RXL
# rxl <-
#   select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, NDC) |>
#   inner_join(cohort, by = "BENE_ID") |>
#   filter((RX_FILL_DT > pain_diagnosis_dt) &
#            (RX_FILL_DT <= exposure_end_dt),
#          NDC %in% ndc_opioids$NDC) |>
#   select(BENE_ID) |>
#   distinct()

# rxl <- collect(rxl) |> as.data.table()

# # Combine and export
# keep <- unique(rbind(otl, rxl))
# # keep <- unique(rxl)
# cohort <- unique(left_join(keep, cohort))

opioid_claims <- unique(rbind(otl, rxl)) |>
  mutate(treatment_name = "opioid")


write_data(opioid_claims, "opioid_dts.fst", file.path(drv_root,"treatment"))
