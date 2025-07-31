library(dplyr)
library(data.table)
library(tidyr)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort_OUD <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  filter(subset_oud==1) |>
  select(BENE_ID, first_treatment_dt, last_treatment_dt)

opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, rx_start_dt, opioid, dose_form)

cohort_opioids <- cohort_OUD |>
  left_join(opioids) |>
  filter(rx_start_dt <= last_treatment_dt)



sort(table(cohort_opioids$opioid), decreasing=T)


# buprenorphine     oxycodone   hydrocodone      tramadol     methadone      morphine       codeine 
# 67865          9026          7915          3561          2799          1883          1109 
# hydromorphone      fentanyl   oxymorphone      lbuphine    meperidine    tapentadol   butorphanol 
# 1100           690            84            18            13            10             3 
# pentazocine    alfentanil 
# 3             1 


length(unique(cohort_opioids$BENE_ID))
# 19056 (out of 32408)


cohort_opioids_wide <- cohort_opioids %>%
  filter(opioid %in% c("buprenorphine", "oxycodone", "hydrocodone", "tramadol", "methadone")) |>
select(BENE_ID, opioid) |>
  distinct() %>%    
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = opioid,
    values_from = present,
    values_fill = list(present = 0)
  )

table(cohort_opioids_wide$buprenorphine)
table(cohort_opioids_wide$oxycodone)
table(cohort_opioids_wide$hydrocodone)
table(cohort_opioids_wide$tramadol)
table(cohort_opioids_wide$methadone)

(table(cohort_opioids_wide$buprenorphine)[2])/nrow(cohort_OUD)*100
(table(cohort_opioids_wide$oxycodone)[2])/nrow(cohort_OUD)*100
(table(cohort_opioids_wide$hydrocodone)[2])/nrow(cohort_OUD)*100
(table(cohort_opioids_wide$tramadol)[2])/nrow(cohort_OUD)*100
(table(cohort_opioids_wide$methadone)[2])/nrow(cohort_OUD)*100

cohort_opioids_no_bup_or_met <- cohort_opioids |>
  filter(!opioid %in% c("buprenorphine", "methadone"))

sort(table(cohort_opioids_no_bup_or_met$opioid), decreasing=T)

# oxycodone   hydrocodone      tramadol      morphine       codeine hydromorphone      fentanyl 
# 9026          7915          3561          1883          1109          1100           690 
# oxymorphone      lbuphine    meperidine    tapentadol   butorphanol   pentazocine    alfentanil 
# 84            18            13            10             3             3             1 

length(unique(cohort_opioids_no_bup_or_met$BENE_ID))
# 9348




## ----------------------------------------------------
cohort <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  filter(subset_oud==0) |>
  select(BENE_ID, first_treatment_dt, last_treatment_dt)

cohort_opioids <- cohort |>
  left_join(opioids) |>
  filter(rx_start_dt <= last_treatment_dt)



sort(table(cohort_opioids$opioid), decreasing=T)



## Dose form --------------------------------------------

cohort_opioids_wide <- cohort_opioids %>%
  filter(opioid == "buprenorphine") |>
  select(BENE_ID, dose_form) |>
  distinct() %>%    
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = dose_form,
    values_from = present,
    values_fill = list(present = 0)
  )

table(cohort_opioids_wide$`Sublingual Film`)
table(cohort_opioids_wide$`Sublingual Tablet`)
table(cohort_opioids_wide$`Buccal Film`)
table(cohort_opioids_wide$`Transdermal System`)
table(cohort_opioids_wide$Injection)
table(cohort_opioids_wide$`Prefilled Syringe`)

(table(cohort_opioids_wide$`Sublingual Film`)[2])/104.22
(table(cohort_opioids_wide$`Sublingual Tablet`)[2])/104.22
(table(cohort_opioids_wide$`Buccal Film`)[2])/104.22
(table(cohort_opioids_wide$`Transdermal System`)[2])/104.22
(table(cohort_opioids_wide$Injection)[2])/104.22
(table(cohort_opioids_wide$`Prefilled Syringe`)[2])/104.22
