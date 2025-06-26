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
