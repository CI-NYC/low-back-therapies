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

opioids <- readRDS("~/medicaid/undertreated-pain/data/public/ndc_to_atc_opioids.rds")
local <- FALSE

plan(multisession)

strength <- foreach(code = opioids[, rxcui]) %dofuture% {
  get_rxcui_strength(code, local_host = local)
}

opioids[, strength := strength]
opioids[, dose_form := future_map_chr(opioids$rxcui, get_dose_form, local_host = local)]

plan(sequential)

saveRDS(opioids, "~/medicaid/undertreated-pain/data/public/ndc_to_atc_opioids_with_strength.rds")