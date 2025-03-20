library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

ndc <- readRDS("~/medicaid/undertreated-pain/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/undertreated-pain/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))
cohort <- cohort |>
  mutate(exposure_end_dt = pain_diagnosis_dt + days(91))

# find opioid ndcs --------------------------------------------------------

opioids <- names(codes[["Opioid pain"]]$ATC)

opioid_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(opioids, \(x) str_detect(code, x)), na.rm = TRUE)
}

ndc_opioids <- ndc[opioid_flag]

rxl <- open_rxl()

# # Read in OTL (Other services line) 
# otl <- open_otl()
# 
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
#   select(BENE_ID, LINE_SRVC_BGN_DT) |>
#   distinct()
# 
# otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid in the exposure period in RXL
rxl <- 
  select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, DAYS_SUPPLY, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT > pain_diagnosis_dt) & 
           (RX_FILL_DT <= exposure_end_dt), 
         NDC %in% ndc_opioids$NDC) |> 
  distinct()

rxl <- collect(rxl) |> 
  mutate(RX_END_DT = RX_FILL_DT + days(DAYS_SUPPLY)) |>
  select(BENE_ID, RX_FILL_DT, RX_END_DT) |>
  as.data.table()

# # Combine and export
# keep <- unique(rbind(otl, rxl))

write_data(distinct(rxl, BENE_ID, .keep_all = T), "all_opioids.fst", file.path(drv_root, "ED_visits"))
