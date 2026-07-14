library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)
library(tidyverse)

source("~/medicaid/low-back-therapies/R/helpers.R")

ED_visits <- load_data("ED_visits_unclean.fst", file.path(drv_root, "outcome")) |>
  rename(ED_visit_dt = LINE_SRVC_BGN_DT) |>
  select(BENE_ID, CLM_ID, ED_visit_dt)

claims <- load_data("procedures_and_IP.rds", file.path(drv_root, "outcome"))

ED_visits_joined <- ED_visits |>
  left_join(claims, by="BENE_ID")

ED_visits_exclude <- ED_visits_joined |>
  filter(SRVC_BGN_DT %within% interval(ED_visit_dt %m-% days(7), ED_visit_dt %m+% days(6)))

ED_visits <- ED_visits |>
  filter(!CLM_ID %in% ED_visits_exclude$CLM_ID.x)

ED_visits <- ED_visits |>
  select(BENE_ID, ED_visit_dt) |>
  distinct()

write_data(ED_visits, "ED_visits_cleaned_with_procedures_and_inpatients_excluded.fst", file.path(drv_root, "outcome"))

# ED_visits <- filter(!is.na(PAIN_CAT)) # NA in pain cat means that the dgns_cd is not in the original list of chronic pain codes
