library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)
library(fst)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  mutate(exclusion_start_dt = washout_start_dt + days(91))

# Load necessary datasets
oth <- open_oth()
iph <- open_iph()

# codes for other pain diagnoses, based on search terms
other_pain_icds <- read.csv("~/medicaid/low-back-therapies/data/public/chronic_pain_icd10_20230216.csv") |>
  filter(!grepl("low back", ICD_DESC, ignore.case = TRUE) &
           !grepl("lumb", ICD_DESC, ignore.case = TRUE) &
           !grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")


codes <- other_pain_icds$ICD9_OR_10

start_dt <- as.Date("2016-07-01")
end_dt <- as.Date("2019-10-01")

keep <- c("BENE_ID", 
          "CLM_ID", 
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_pain <- 
  select(oth, any_of(keep)) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(SRVC_BGN_DT = ifelse(
    is.na(SRVC_BGN_DT), 
    SRVC_END_DT, 
    SRVC_BGN_DT)
  ) |> 
  filter((SRVC_BGN_DT >= exclusion_start_dt) & 
           (SRVC_BGN_DT < day0_dt), 
         DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |> 
  select(BENE_ID) |> 
  distinct()

oth_pain <- collect(oth_pain) |> as.data.table()


iph_pain <-
  select(iph, any_of(keep)) |>
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(SRVC_BGN_DT = ifelse(
    is.na(SRVC_BGN_DT), 
    SRVC_END_DT, 
    SRVC_BGN_DT)
  ) |> 
  filter((SRVC_BGN_DT >= exclusion_start_dt) & 
           (SRVC_BGN_DT < day0_dt)) |>
  filter(DGNS_CD_1 %in% codes |
           DGNS_CD_2 %in% codes |
           DGNS_CD_3 %in% codes |
           DGNS_CD_4 %in% codes |
           DGNS_CD_5 %in% codes |
           DGNS_CD_6 %in% codes |
           DGNS_CD_7 %in% codes |
           DGNS_CD_8 %in% codes |
           DGNS_CD_9 %in% codes |
           DGNS_CD_10 %in% codes) |>
  select(BENE_ID) |> 
  distinct()

iph_pain <- collect(iph_pain) |> as.data.table()


remove <- rbind(oth_pain, iph_pain) |> unique()

# number of patients with opioids in washout
remove |> nrow()

cohort <- cohort |>
  mutate(exclusion_washout_pain = ifelse(BENE_ID %in% remove$BENE_ID, 1, 0)) |>
  select(BENE_ID, exclusion_washout_pain)

write_data(cohort, "pain_washout_continuous_enrollment_washout_pain.fst", file.path(drv_root, "exclusion"))
