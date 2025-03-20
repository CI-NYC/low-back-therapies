# -------------------------------------
# Script: 08_mediator_physical_therapy.R
# Author: Nick Williams
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for physical therapy
#   during the mediator period.
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

otl <- open_otl()

# Read in cohort and dates
cohort <- load_data("pain_cohort_cleaned_with_opioids.fst", file.path(drv_root, "final"))
setDT(cohort)
setkey(cohort, BENE_ID)
cohort <- cohort[, .(BENE_ID, index_dt)]

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("~/medicaid/undertreated-pain/data/public/mediator_codes.yml")
mediators <- c("Physical therapy",
               "Counseling",
               # "Massage therapy",
               "Chiropractic",
               "Ablative techniques",
               "Acupuncture",
               "Botulinum toxin injections",
               "Electrical nerve stimulation",
               "Intrathecal drug therapies",
               "Epidural steroids",
               "Blocks",
               "Minimally invasive spinal procedures",
               "Trigger point injection")
treatments_df <- data.frame()
for (mediator in mediators) {
  for (code in codes[[mediator]]){
    treatments_df <- rbind(treatments_df, 
          data.frame(cd = names(code), 
                     treatment = rep(mediator, length(names(code))))
    )
  }
}

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
    filter(LINE_PRCDR_CD %in% treatments_df$cd) |>
    collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(index_dt, 
                                                    index_dt + days(91)), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, index_dt, LINE_PRCDR_CD)]

# # Create indicator variable for whether or not a patient had claim in mediator period
# # Right join with cohort
# claims <- claims[, .(mediator_has_physical_therapy = as.numeric(.N > 0), 
#                      mediator_count_physical_therapy_claims = .N), by = "BENE_ID"]
# claims <- merge(claims, cohort[, .(BENE_ID)], all.y = TRUE, by = "BENE_ID")
# 
# # Convert NAs to 0 for observations in the cohort that didn't have a PT claim
# fix <- c("mediator_has_physical_therapy", "mediator_count_physical_therapy_claims")
# claims[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

claims <- claims |>
  left_join(treatments_df, by = c("LINE_PRCDR_CD" = "cd"))

write_data(claims, "all_nonopioid_claims.fst", file.path(drv_root, "analysis/non-opioid"))
