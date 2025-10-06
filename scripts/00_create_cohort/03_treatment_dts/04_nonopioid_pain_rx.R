# -------------------------------------
# Script: 04_filter_nonopioid_ndc.R
# Author: Anton Hung
# Purpose: Filter to observations with non-opioid pharmacological treatment in the exposure period
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)
library(rxnorm)

source("~/medicaid/low-back-therapies/R/helpers.R")

ndc <- readRDS("~/medicaid/low-back-therapies/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("low_back_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(exposure_end_dt_possible_latest = pain_diagnosis_dt + days(121)) # 30 days from diagnosis + 90 day exposure period



# Notes ------------------------------------------------------------------------

# Putting together codes for non-opioid prescriptions and their corresponding drug category
# 1. benzo: names(codes$Benzodiazepines[["ATC"]])
# 2. gabapentin: names(codes$Gabapentin[["ATC"]])
# 3. muscle relaxant: "M03"
# 4. anti-inflammatories: "M01"
# 5. duloxetine: "N06AX" => tmp <- sapply(df$rxcui, get_rx) => filter(grepl("duloxetine",tmp))


nonopioid_names_df <- data.frame(code = c(names(codes$Benzodiazepines[["ATC"]]),
                                          names(codes$Gabapentin[["ATC"]]),
                                          "M03", "M01", "N06AX",
                                          names(codes$Steroids[["ATC"]]),
                                          "N02BE"),
                                 nonopioid_name = c(rep("Benzodiazepine", 3),
                                                    "Gabapentin",
                                                    "Muscle relaxant",
                                                    "Anti-inflammatory",
                                                    "Duloxetine",
                                                    "Glucocorticoid",
                                                    "Acetaminophen"))

rx_flag <- foreach(code = ndc[, atc], .combine = "c") %do% {
  any(sapply(nonopioid_names_df$code, \(x) grepl(x, code)), na.rm = TRUE)
}

ndc_rx <- ndc[rx_flag]

ndc_rx <- ndc_rx |>
  mutate(atc = sapply(atc, function(x) if (is.vector(x)) x[1] else x))

# subsetting antidepressants to just duloxetine prescriptions ------------------
ndc_duloxetine <- ndc_rx |>
  filter(atc == "N06AX") 

ndc_duloxetine$rx_name <- sapply(ndc_duloxetine$rxcui, get_rx)

ndc_duloxetine <- ndc_duloxetine |>
  filter(grepl("duloxetine",rx_name)) |>
  select(-rx_name)


# subsetting antidepressants to just duloxetine prescriptions ------------------
ndc_acetaminophen <- ndc_rx |>
  filter(atc == "N02BE") 

ndc_acetaminophen$rx_name <- sapply(ndc_acetaminophen$rxcui, get_rx)

ndc_acetaminophen <- ndc_acetaminophen |>
  filter(grepl("acetaminophen",rx_name)) |>
  select(-rx_name)


# combine duloxetine with other non-opioid pain prescriptions ------------------

ndc_rx <- rbind(ndc_rx |> filter(!atc=="N06AX", !atc=="N02BE"),
                ndc_duloxetine,
                ndc_acetaminophen) |>
  mutate(treatment_name = case_when(
    atc %in% names(codes$Benzodiazepines[["ATC"]]) ~ "Benzodiazepine",
    atc %in% names(codes$Gabapentin[["ATC"]]) ~ "Gabapentin",
    grepl("M03",atc) ~ "Muscle relaxant",
    grepl("M01",atc) ~ "Anti-inflammatory",
    atc == "N06AX" ~ "Duloxetine",
    grepl("H02AB", atc) ~ "Steroid",
    atc == "N02BE" ~ "Acetaminophen",
    TRUE ~ NA
  )) |>
  filter(!is.na(treatment_name))



# process claims  ------------------ ------------------ ------------------------

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line) 
otl <- open_otl()

# Find non-opioids in OTL following diagnosis
otl_opioids <- 
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |> 
  inner_join(cohort, by = "BENE_ID") |> 
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT), 
    LINE_SRVC_END_DT, 
    LINE_SRVC_BGN_DT
  )) |> 
  filter((LINE_SRVC_BGN_DT >= pain_diagnosis_dt) & 
           (LINE_SRVC_BGN_DT <= exposure_end_dt_possible_latest), 
         NDC %in% ndc_rx$NDC) |>
  select(BENE_ID, treatment_start_dt = LINE_SRVC_BGN_DT, treatment_end_dt = LINE_SRVC_BGN_DT, NDC) |>
  distinct()

otl_opioids <- collect(otl_opioids) |> as.data.table()

# Find non-opioids in RXL following diagnosis
rxl_opioids <- 
  rxl |>
  inner_join(cohort, by = "BENE_ID") |> 
  filter((RX_FILL_DT >= pain_diagnosis_dt) & 
           (RX_FILL_DT <= exposure_end_dt_possible_latest), 
         NDC %in% ndc_rx$NDC) |>
  distinct()

# setting treatment end dt to the last day of supply, assuming medication starts on the fill date
# replace NAs with 1 day supply
rxl_opioids <- collect(rxl_opioids) |> 
  mutate(treatment_end_dt = RX_FILL_DT + days(coalesce(DAYS_SUPPLY, 1) - 1)) |>
  select(BENE_ID, treatment_start_dt = RX_FILL_DT, treatment_end_dt, NDC) |>
  as.data.table()

# re-joining the specific name of the non-opioid pharmacalogic (benzo, gaba, anti-inflammatory, etc.)
all <- unique(rbind(otl_opioids, rxl_opioids)) |>
  left_join(ndc_rx |> select(NDC, treatment_name)) |>
  select(-NDC)


write_data(unique(all), "nonopioid_rx_dts.fst", file.path(drv_root,"treatment"))
