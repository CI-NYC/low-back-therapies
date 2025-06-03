# -------------------------------------
# Script: 00_opioid_strength.R
# Author: Nick Williams
# Updated:
# Purpose: Add strength and dose form to NDC lookup table for opioids
# Notes:
# -------------------------------------

library(data.table)
library(rxnorm)
library(foreach)
library(doFuture)
library(furrr)
library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/low-back-therapies/R/helpers.R")

ndc <- readRDS(file.path(drv_root,"exclusion/ndc_to_atc_crosswalk_check.rds"))
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |> as.data.table()
cohort[, let(exposure_end_dt = pain_diagnosis_dt + days(90))] # because diagnosis dt is included in exposure period, total length = 91 days


# find opioid ndcs --------------------------------------------------------

opioids <- names(codes[["Opioid pain"]]$ATC)

opioid_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(opioids, \(x) str_detect(code, x)), na.rm = TRUE)
}

opioids <- ndc[opioid_flag]

saveRDS(opioids, file.path(drv_root, "exclusion/ndc_to_atc_opioids.rds"))

# -------------------------------------------------------------------------

opioids <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_opioids.rds")
local <- FALSE

plan(multisession)

strength <- foreach(code = opioids[, rxcui]) %dofuture% {
  get_rxcui_strength(code, local_host = local)
}

opioids[, strength := strength]
opioids[, dose_form := future_map_chr(opioids$rxcui, get_dose_form, local_host = local)]

plan(sequential)

saveRDS(opioids, file.path(drv_root, "exclusion/ndc_to_atc_opioids_with_strength.rds"))
