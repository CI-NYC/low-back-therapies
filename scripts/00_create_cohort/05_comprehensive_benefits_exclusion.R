# -------------------------------------
# Script: comprehensive_benefits_exclusion
# Author: Anton Hung
# Updated:
# Purpose: exclusion for not having full or at least comprehensive benefits coverage
#           throughout the washout period.
# Notes: Resdac brief "Identifying Beneficiaries with Full-Scope, Comprehensive, and Limited Benefits in the TAF"
#         from June 2025 (https://www.medicaid.gov/dq-atlas/downloads/supplemental/4151-Scope-of-Benefits.pdf)
#         contains information on how to define benefits coverage.
#       
#        This definition utilizes the RSTRCTD_BNFTS_CD columns (1-12). Valid codes for 
#         full-scope OR comprehensive coverage across all our states in the years
#         2016-2019 include 1, 7, A, and D.
#        The code "5" was considered, but is known to be unreliable prior to 2020.
#        Code B is not included because it is appears to only exist prior to 2010.
#        Code 4 is not included because it is only relevant to pregnant individuals
#         - This project does not include pregnant individuals, but consider adding
#           this code if your project includes pregnant individuals.
#         
# -------------------------------------
library(collapse)
library(fst)
library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(yaml)
library(purrr)
library(stringr)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort <- load_data("pain_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# Load demographics dataset
demo <- open_demo()

demo <- right_join(demo, cohort) |> 
  collect()

benefits_codes <- 
  select(demo, BENE_ID, RFRNC_YR, starts_with("RSTRCTD_BNFTS_CD"), -RSTRCTD_BNFTS_CD_LTST) |>
  pivot(ids = c("BENE_ID", "RFRNC_YR"), 
        how = "l", 
        names = list("month", "code")) |> 
  mutate(code = replace_na(code, "0")) |>
  fmutate(month = str_extract(month, "\\d+$"), 
          year = as.numeric(RFRNC_YR),
          elig_dt = as.Date(paste0(year, "-", month, "-01"))) |> 
  join(cohort, how = "inner") |> 
  fselect(BENE_ID, washout_start_dt, washout_end_dt, code, elig_dt)

benefits_codes <- 
  fsubset(benefits_codes, elig_dt %within% interval(washout_start_dt, washout_end_dt))

exclusion_codes <- benefits_codes |>
  fgroup_by(BENE_ID) |>
  fsummarise(exclusion_comp_bnfts = as.integer(any(!code %in% c("1","7","A","D"))))

write_data(exclusion_codes, "exclusion_benefits.fst", file.path(drv_root, "exclusion"))
