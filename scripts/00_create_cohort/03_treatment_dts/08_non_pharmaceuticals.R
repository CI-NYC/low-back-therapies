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

source("~/medicaid/low-back-therapies/R/helpers.R")

otl <- open_otl()

# Read in cohort and dates
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  mutate(exposure_end_dt = pain_diagnosis_dt + days(90)) |>
  select(BENE_ID, pain_diagnosis_dt, exposure_end_dt)

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/mediator_codes.yml")
mediators <- c("Physical therapy",
               "Counseling",
               "Massage therapy",
               "Chiropractic",
               "Ablative techniques", ##
               "Acupuncture",
               "Botulinum toxin injections", ## 
               "Electrical nerve stimulation", ##
               "Intrathecal drug therapies", ## 
               "Epidural steroids",
               "Blocks",
               "Minimally invasive spinal procedures", ##
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
treatments_df <- as.data.table(treatments_df)

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
claims <- claims[LINE_SRVC_BGN_DT %within% interval(pain_diagnosis_dt, 
                                                    exposure_end_dt), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, pain_diagnosis_dt, exposure_end_dt, LINE_PRCDR_CD)]

treatments_dts <- claims |>
  left_join(treatments_df |> distinct(cd, .keep_all=T), 
            by = c("LINE_PRCDR_CD" = "cd")) |>
  select(BENE_ID, treatment_start_dt = LINE_SRVC_BGN_DT, treatment_end_dt = LINE_SRVC_END_DT, treatment) |>
  arrange(treatment_start_dt, desc(treatment_end_dt))

saveRDS(treatments_dts, file.path(drv_root, "exclusion/treatments_dts.rds"))


counseling <- treatments_df[treatment == "Counseling"]
physical_therapy <- treatments_df[treatment == "Physical therapy"]
acupuncture <- treatments_df[treatment == "Acupuncture"]
blocks <- treatments_df[treatment == "Blocks"]
chiropractic <- treatments_df[treatment == "Chiropractic"]
epidural_steroid <- treatments_df[treatment == "Epidural steroids"]
massage_therapy <- treatments_df[treatment == "Massage therapy"]
trigger_point_injection <- treatments_df[treatment == "Trigger point injection"]
other_nonpharma <- treatments_df[treatment %in% c("Ablative techniques", 
                                                  "Botulinum toxin injections",
                                                  "Electrical nerve stimulation",
                                                  "Intrathecal drug therapies",
                                                  "Minimally invasive spinal procedures")]


claims[, `:=`(exposure_nonopioid_counseling = fifelse(LINE_PRCDR_CD %in% counseling$cd, 1, 0), 
              exposure_nonopioid_physical_therapy = fifelse(LINE_PRCDR_CD %in% physical_therapy$cd, 1, 0), 
              exposure_nonopioid_acupuncture = fifelse(LINE_PRCDR_CD %in% acupuncture$cd, 1, 0), 
              exposure_nonopioid_blocks = fifelse(LINE_PRCDR_CD %in% blocks$cd, 1, 0), 
              exposure_nonopioid_chiropractic = fifelse(LINE_PRCDR_CD %in% chiropractic$cd, 1, 0), 
              exposure_nonopioid_epidural_steroid = fifelse(LINE_PRCDR_CD %in% epidural_steroid$cd, 1, 0),
              exposure_nonopioid_massage_therapy = fifelse(LINE_PRCDR_CD %in% massage_therapy$cd, 1, 0),
              exposure_nonopioid_trigger_point_injection = fifelse(LINE_PRCDR_CD %in% trigger_point_injection$cd, 1, 0),
              exposure_nonopioid_other_nonpharma = fifelse(LINE_PRCDR_CD %in% other_nonpharma$cd, 1, 0)
              )]

claims <- unique(claims)
claims <- group_by(claims, BENE_ID) |> 
  select(-c(LINE_SRVC_BGN_DT, pain_diagnosis_dt, exposure_end_dt, LINE_PRCDR_CD)) |>
  summarize(across(starts_with("exposure"), \(x) as.numeric(sum(x) > 0))) |> 
  as.data.table(key = "BENE_ID")

claims <- merge(cohort |> select(BENE_ID), claims, all.x = TRUE)
claims[is.na(claims)] <- 0

saveRDS(claims, file.path(drv_root, "treatments/nonpharma_bin.rds"))
