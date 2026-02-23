# -------------------------------------
# Script: nonpharma
# Author: Anton Hung
# Updated:
# Purpose: get all dates for non-pharmacologic treatments, and record the type of treatment.
#           this is done by looping through the ICD-10 codes for the following types
#           of treatments:
#           1. Physical therapy
#           2. Chiropractic
#           3. Acupuncture
#           4. Blocks
#           5. Interventions (a group combining the following less common treatments:)
#             a. "Ablative techniques"
#             b. "Botulinum toxin injections"
#             c. "Electrical nerve stimulation"
#             d. "Intrathecal drug therapies"
#             e. "Epidural steroids"
#             f. "Minimally invasive spinal procedures"
#             g. "Trigger point injection"
# Notes:
# -------------------------------------


library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/low-back-therapies/R/helpers.R")

otl <- open_otl()

# Read in cohort and dates
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(exposure_end_dt_possible_latest = diagnosis_dt + days(121))

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml(file.path(home_dir, "data/public/mediator_codes.yml"))
mediators <- c("Physical therapy",
               # "Counseling",
               "Massage therapy",
               "Chiropractic",
               # "Acupuncture",
               "Blocks",
               "Ablative techniques", ##
               "Botulinum toxin injections", ## 
               "Spinal cord stimulation",
               "Electrical nerve stimulation", ##
               "Intrathecal drug therapies", ## 
               "Epidural steroids", ##
               "Minimally invasive spinal procedures", ##
               "Trigger point injection"
               ) ##
treatments_df <- data.frame()
for (mediator in mediators) {
  for (code in codes[[mediator]]){
    treatments_df <- rbind(treatments_df, 
                           data.frame(cd = names(code), 
                                      treatment = rep(mediator, length(names(code))))
    )
  }
}
treatments_df <- treatments_df  |>
  mutate(treatment = ifelse(treatment %in% c("Physical therapy",
                                             "Massage therapy",
                                             # "Acupuncture",
                                             "Chiropractic",
                                             # "Blocks",
                                             "Spinal cord stimulation"
                                             ), treatment, "Intervention"))

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
claims <- claims[LINE_SRVC_BGN_DT %within% interval(diagnosis_dt, 
                                                    exposure_end_dt_possible_latest), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, diagnosis_dt, exposure_end_dt_possible_latest, LINE_PRCDR_CD)]

treatments_dts <- claims |>
  left_join(treatments_df, by = c("LINE_PRCDR_CD" = "cd")) |>
  select(BENE_ID, treatment_start_dt = LINE_SRVC_BGN_DT, treatment_end_dt = LINE_SRVC_BGN_DT, treatment_name = treatment) |>
  arrange(treatment_start_dt, desc(treatment_end_dt)) |>
  distinct()

# write_data(unique(treatments_dts), "nonpharma_dts.fst", file.path(drv_root, "treatment"))
write_data(treatments_dts, "nonpharma_dts.fst", file.path(drv_root, "treatment"))
