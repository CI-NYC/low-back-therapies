library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)
library(tidyverse)

source("~/medicaid/undertreated-pain/R/helpers.R")

ED_visits <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned.rds")

claims <- readRDS("/mnt/general-data/disability/pain-severity/intermediate/procedures_and_IP.rds")

ED_visits_joined <- ED_visits |>
  left_join(claims, by="BENE_ID")

ED_visits_exclude <- ED_visits_joined |>
  filter(SRVC_BGN_DT %within% interval(start_dt %m-% days(7), start_dt %m+% days(6)))

# ED_visits_include <- ED_visits |>
#   filter(!(SRVC_BGN_DT >= start_dt %m-% days(7)) & !(SRVC_BGN_DT <= start_dt %m+% days(6)))

ED_visits <- ED_visits |>
  filter(!CLM_ID %in% ED_visits_exclude$CLM_ID.x)

saveRDS(ED_visits, "/mnt/general-data/disability/pain-severity/intermediate/visits_cleaned_with_procedures_and_inpatients_excluded.rds")
