library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)

source("~/medicaid/low-back-therapies/R/helpers.R")

# Read in OTH and IPH as arrow datsets ----------------------------------

oth <- open_oth()
otl <- open_otl()

# read in cohort dates file
dts_cohorts <- load_data("pain_cohort.fst", file.path(drv_root_30_day_treatment, "modified_final"))
ED_visits <- load_data("ED_visits_cleaned_with_procedures_and_inpatients_excluded.fst", file.path(drv_root, "outcome"))

# # https://resdac.org/sites/datadocumentation.resdac.org/files/2021-01/5011_Identifying_IP_Stays.pdf
# inpatient_cds <- c("001", "060", "084", "086", "090", "091", "092", "093")
pos_codes_acute <- c(13, 21, 32, 24, 55, 31, 09, 51,  # inpatient
                     23, # emergency department
                     81, # independent lab
                     10 # telehealth
)
# outpatient_TOS_CD <- c("002", "003", "028", "060", "061", "014", "049")
ed_visit_cds <- c(paste0("045", 0:9), "0981", # Emergency department
                  "0526", "0516" # Urgent care
)

tos_exclude <- c(
  "005", "006",  # Laboratory
  "033", #Prescribed drugs X
  "034", #Over-the-counter medications X
  "029","035", #Dental X
  "036", #Medical equipment/prosthetic devices X X
  "037", #Eyeglasses X
  "038", #Hearing Aids X
  "056", #Transportation services X
  # Financial / administrative payments
  "119", "120", "121", "122", "123",
  "131", "132", "133", "134", "135",
  "138", "139", "140", "141", "142", "143", "144"
)

# outpatient visits ------------------------------------------------------------
icd_codes_to_check_oth <-
  oth |>
  filter(BENE_ID %in% dts_cohorts$BENE_ID,
         !CLM_ID %in% ED_visits$ed_visit_ID,
         !POS_CD %in% pos_codes_acute) |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT) |>
  collect()

# obtain the date for all outpatient visits within washout period
all_oth_icds_in_washout_cal <-
  icd_codes_to_check_oth |>
  inner_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt))  

oth_tos_cd <- otl |>
  select(CLM_ID, TOS_CD) |>
  right_join(all_oth_icds_in_washout_cal, by="CLM_ID") |>
  filter(!TOS_CD %in% tos_exclude) |>
  collect()


# # count number of outpatient visits during washout period
# num_oth_washout_cal <-
#   all_oth_icds_in_washout_cal |>
#   distinct(BENE_ID, SRVC_BGN_DT) |>
#   group_by(BENE_ID) |>
#   summarise(num_oth_washout_cal = n(), .groups = "drop")



result <- oth_tos_cd |>
  group_by(BENE_ID, SRVC_BGN_DT, TOS_CD) |>
  distinct()

print(sort(prop.table(table(result$TOS_CD, useNA="ifany"))))
