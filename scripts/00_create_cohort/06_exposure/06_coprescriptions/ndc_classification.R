library(arrow)
library(tidyverse)
library(data.table)
library(fst)
library(yaml)
library(foreach)

source("~/medicaid/low-back-therapies/R/helpers.R")

ndc <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/mediator_codes.yml")


# Non-opioid pain ---------------------------------------------------------

nop_codes <- c(names(codes[["Non-opioid pain"]]$ATC))

nop_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(nop_codes, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_nop = nop_flag)][flag_nop == TRUE] |>
  saveRDS(file.path(drv_root, "treatments/nonopioid_pain_ndc.rds"))



# Benzodiazepines ---------------------------------------------------------

benzo_codes <- c(names(codes[["Benzodiazepines"]]$ATC))

benzo_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(benzo_codes, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_benzo = benzo_flag)][flag_benzo == TRUE, ] |> 
  saveRDS(file.path(drv_root, "treatments/benzo_ndc.rds"))



# Gabapentin --------------------------------------------------------------

gab_code <- c(names(codes[["Gabapentin"]]$ATC))

gab_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(gab_code, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc[, .(NDC, atc, flag_gab = gab_flag)][flag_gab == TRUE, ] |> 
  saveRDS(file.path(drv_root, "treatments/gabapentinoid_ndc.rds"))