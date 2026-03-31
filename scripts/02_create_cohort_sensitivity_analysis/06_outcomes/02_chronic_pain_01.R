################################################################################
################################################################################
###  CREATE CHRONIC PAIN VARIABLES
###  Kat Hoffman, March 2023
###  Purpose: clean TAFOTH and TAFIPH files for chronic pain ICD codes
###  Output: cleaned data file containing minimum date the beneficiary ("data/final/chronic_pain.rds")
###        has a chronic pain ICD code in the study duration
###         and indicators of whether it occurs in washout or overall study duration
###
###  Modified by Anton: Simplified from 04_define_comorbidity_vars/define_chronic_pain.R
###                     to only check over the 6-month follow-up period for chronic pain.
###                     Previously, iterated over 0:17 to calculate 6-month rolling windows
###
################################################################################
################################################################################

# Set up -----------------------------------------------------------------------

# load libraries
library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(tictoc)
library(foreach)
library(future)
library(furrr)
library(ggalluvial)
library(doParallel)
# options(cores=50)
registerDoParallel()
# plan(multicore)
getDoParWorkers()
options(future.globals.maxSize = 850 * 1024^2)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_with_exposures.fst", file.path(drv_root, "treatment"))


############################################################################
############################################################################
# Step 1: read in all pain codes created in  define_pain.R script
############################################################################
############################################################################


# Load necessary datasets
oth <- open_oth()
iph <- open_iph()

low_back_pain_icds <- read.csv("~/medicaid/low-back-therapies/data/public/chronic_pain_icd10_20230216.csv") |>
  filter(grepl("low back", ICD_DESC, ignore.case = TRUE) |
           grepl("lumb", ICD_DESC, ignore.case = TRUE) |
           grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")


codes <- low_back_pain_icds$ICD9_OR_10

start_dt <- as.Date("2016-07-01")
end_dt <- as.Date("2019-12-31")

keep <- c("BENE_ID", 
          "CLM_ID", 
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_pain <- 
  select(oth, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |>
  collect() |> 
  as.data.table()

iph_pain <-
  select(iph, any_of(keep)) |>
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
  collect() |>
  as.data.table()

oth_pain[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)]

oth_pain <- oth_pain[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                     .(BENE_ID, CLM_ID, SRVC_BGN_DT, DGNS_CD_1, DGNS_CD_2)]

iph_pain <- iph_pain[SRVC_BGN_DT %within% interval(start_dt, end_dt),
                     .SD,
                     .SDcols = c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", paste0("DGNS_CD_", 1:10))]

setkey(oth_pain, BENE_ID, CLM_ID)
oth_pain <- unique(oth_pain, by = c(1, 2))

setkey(iph_pain, BENE_ID, CLM_ID)
iph_pain <- unique(iph_pain, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_pain <- oth_pain[!is.na(BENE_ID)]
iph_pain <- iph_pain[!is.na(BENE_ID)]

# oth_pain <- unique(oth_pain[, .(BENE_ID, SRVC_BGN_DT)], by = 1)
# iph_pain <- unique(iph_pain[, .(BENE_ID, SRVC_BGN_DT)], by = 1)

pain_all <- rbindlist(list(oth_pain[, .(BENE_ID, SRVC_BGN_DT)], 
                           iph_pain[, .(BENE_ID, SRVC_BGN_DT)]))

cohort <- cohort |>
  mutate(followup_start_dt = exposure_period_end_dt + days(1)) |>
  select(BENE_ID, followup_start_dt) |>
  left_join(pain_all)


### FUNCTION TO MAP OVER PAIN CAT DFS

rolling_windows <- function(pain_cat_df, pain_cat_name, month_start){
  print(paste(month_start, pain_cat_name, Sys.time()))
  
  relevant_pain_dts <-
    pain_cat_df |>
    # keep only codes within the 6 month window of interest
    mutate(start_month = followup_start_dt + months(month_start),
           end_month = followup_start_dt + months(month_start) + months(6)) |>
    # then, filter the diagnosis codes to only contain those filled within the relevant time frame
    filter(SRVC_BGN_DT %within% interval(start_month, end_month)) |>
    group_by(BENE_ID) |>
    add_count() |> # add number of dg codes within this window per beneficiary
    filter(n > 1)  |> # only keep codes that show up more than once 
    mutate(first_pain = min(SRVC_BGN_DT),
           pain_90 = first_pain + days(90)) |>
    filter(!(SRVC_BGN_DT %within% interval(first_pain, pain_90))) # filter out first pain and everything within 90 days
  
  chronic_pain_per_month <-
    relevant_pain_dts |>
    ungroup() |>
    select(BENE_ID) |>
    distinct() |>
    mutate(month = month_start)
  
  chronic_pain_per_month[[pain_cat_name]] <- 1
  
  saveRDS(chronic_pain_per_month, paste0(drv_root, "/outcome/chronic_pain_pieces/", pain_cat_name, "_month_", month_start, ".rds"))
  
  print(paste(month_start, pain_cat_name, "COMPLETE", Sys.time()))
  
  return(chronic_pain_per_month)
}

future_map(c(0,6), ~rolling_windows(cohort, "lowback",  .x))


dir <- file.path(drv_root, "outcome/chronic_pain_pieces/")
files_all <- list.files(dir)

overall_pain_by_month <- function(month_number){
  month_files <- files_all[which(str_detect(files_all, paste0("month_", month_number, ".rds")))]
  month_df <- map(month_files, ~read_rds(paste0(dir, .x))) |>
    reduce(full_join)  |>
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
    mutate(chronic_pain_n = lowback,
           chronic_pain_any = 1) |> # only in this data set if they have chronic pain for that month
    select(BENE_ID, month, chronic_pain_n, chronic_pain_any)
  return(month_df)
}

chronic_pain_all_months <- map_dfr(c(0,6), overall_pain_by_month)
saveRDS(chronic_pain_all_months, file.path(drv_root, "outcome/chronic_pain_all_months.rds"))

chronic_pain_all_months <- read_rds(file.path(drv_root, "outcome/chronic_pain_all_months.rds"))

chronic_pain_wide <- pivot_wider(chronic_pain_all_months,
                                 id_cols = BENE_ID,
                                 names_from = month,
                                 names_prefix = "chronic_pain_any_month_",
                                 values_from = chronic_pain_any,
                                 values_fill = 0) |>
  mutate(chronic_pain_n_months = rowSums(across(where(is.numeric))))

saveRDS(chronic_pain_wide, file.path(drv_root, "outcome/chronic_pain_wide.rds"))
