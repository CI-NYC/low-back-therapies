# READ ME -----------------------------------------------------------------
#
# Author: Sarah Forrest
# Created: 2023-08-01
# Last edited: 2024-06-03 (Shodai)
#
# Creates a binary indicator variable for whether or not an observation in
#   the analysis cohort had a claim for a nonopioid pain medication
#   prescription overall, and for each nonopioid pain medication category: 
#   antidepressants, muscle relaxants, anti-inflammatory and antirheumatic
#   products, topical products for joint and muscular pain, gabapentinoids,
#   benzodiazepines, other analgesics and antipyretics (excluding gabapentinoids), 
#   antispasmodics in combination with analgesics, and antispasmodics, 
#   psycholeptics and analgesics in combination.
# 
# Might need to check in with Nick if using the mediator_nonopioid_pain_rx variable -- might not be up to date in this file
# -------------------------------------------------------------------------

# Load libraries
library(arrow)
library(dplyr)
library(tidyverse)

# Define root directory for data
drv_root <- "/mnt/general-data/disability/mediation_unsafe_pain_mgmt"
low_back_dir <- "/mnt/general-data/disability/low-back-therapies"

# Read in data frames
analysis_df <- readRDS(file.path(low_back_dir, "exclusion/low_back_cohort.rds"))
nonopioid_rds <- readRDS(file.path(drv_root, "mediation_unsafe_pain_mgmt_nonopioid_pain_ndc.rds"))

# Read the mediator datasets for nonopiod pain prescriptions without an without an overlapping opioid prescription
nonopioid_pain_rx_otl <- readRDS(file.path(drv_root, "mediator_otl_nonopioid_pain_rx.rds"))
nonopioid_pain_rx_rxl <- readRDS(file.path(drv_root, "mediator_rxl_nonopioid_pain_rx.rds"))

# Combine both datasets
nonopioid_pain_rx_df <- bind_rows(nonopioid_pain_rx_otl, nonopioid_pain_rx_rxl) 

#####################

terms <- "^M03|^N06A|^M01|^M02A|^N02BF|^N02B|^N03AE|^N05BA|^N05CD"


BENE_IDs <- nonopioid_pain_rx_df |>
  left_join(nonopioid_rds |> select(NDC, atc)) |>
  filter(BENE_ID %in% analysis_df$BENE_ID) |>
  # filter(!is.na(NDC) & !is.na(NDC_QTY)) |>
  group_by(BENE_ID) |>
  summarise(contains = as.integer(any(grepl(terms, atc))),
            does_not_contain = as.integer(!any(grepl(terms, atc)))) |>
  pull(BENE_ID)

nonopioid_pain_rx_df <- nonopioid_pain_rx_df |>
  left_join(nonopioid_rds |> select(NDC, atc)) |>
  filter(BENE_ID %in% analysis_df$BENE_ID) |>
  filter(!is.na(NDC) & !is.na(NDC_QTY)) |>
  group_by(BENE_ID) |>
  summarise(contains = as.integer(any(grepl(terms, atc))),
            does_not_contain = as.integer(!any(grepl(terms, atc))))
#####################


# Create a binary variable to indicate whether a beneficiary received a prescription
nonopioid_pain_rx_df <- nonopioid_pain_rx_df |>
  group_by(BENE_ID) |>
  mutate(mediator_nonopioid_pain_rx = as.integer(any(!is.na(NDC) & !is.na(NDC_QTY)))) |>
  ungroup() |>
  select(BENE_ID, NDC, mediator_nonopioid_pain_rx)

# Create a binary variable to indicate whether a beneficiary received a prescription for an antidepressant (ATC code N06A)
# Filter and save NDC list for values starting with "N06A"
filtered_data_N06A <- nonopioid_rds  %>%
  filter(grepl("^N06A", atc, ignore.case = TRUE))

if (nrow(filtered_data_N06A) > 0) {
  N06A_codes <- filtered_data_N06A$NDC
} else {
  N06A_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for a muscle relaxant (ATC code M03)
# Filter and save NDC list for values starting with "M03"
filtered_data_M03 <- nonopioid_rds  %>%
  filter(grepl("^M03", atc, ignore.case = TRUE))

if (nrow(filtered_data_M03) > 0) {
  M03_codes <- filtered_data_M03$NDC
} else {
  M03_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for an anti-inflammatory and antirheumatic products (ATC code M01)
# Filter and save NDC list for values starting with "M01"
filtered_data_M01 <- nonopioid_rds  %>%
  filter(grepl("^M01", atc, ignore.case = TRUE))

if (nrow(filtered_data_M01) > 0) {
  M01_codes <- filtered_data_M01$NDC
} else {
  M01_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for a topical product for joint and muscular pain (ATC code M02A)
# Filter and save NDC list for values starting with "M02A"
filtered_data_M02A <- nonopioid_rds  %>%
  filter(grepl("^M02A", atc, ignore.case = TRUE))

if (nrow(filtered_data_M02A) > 0) {
  M02A_codes <- filtered_data_M02A$NDC
} else {
  M02A_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for gabapentin (ATC code N02BF)
# Filter and save NDC list for values starting with "N02BF"
filtered_data_N02BF <- nonopioid_rds  %>%
  filter(grepl("^N02BF", atc, ignore.case = TRUE))

if (nrow(filtered_data_N02BF) > 0) {
  N02BF_codes <- filtered_data_N02BF$NDC
} else {
  N02BF_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for other analgesics and antipyretics (ATC code N02B)
# Filter and save NDC list for values starting with "N02B" (excluding "N02BF")
filtered_data_N02B <- nonopioid_rds  %>%
  filter(grepl("^N02B", atc, ignore.case = TRUE) & !grepl("^N02BF", atc, ignore.case = TRUE))

if (nrow(filtered_data_N02B) > 0) {
  N02B_codes <- filtered_data_N02B$NDC
} else {
  N02B_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for benzodiazepines (ATC codes N03AE/N05BA/N05CD)
filtered_data_N03AE_N05BA_N05CD <- nonopioid_rds  %>%
  filter(grepl("^N03AE", atc, ignore.case = TRUE) |
           grepl("^N05BA", atc, ignore.case = TRUE) |
           grepl("^N05CD", atc, ignore.case = TRUE))

if (nrow(filtered_data_N03AE_N05BA_N05CD) > 0) {
  N03AE_N05BA_N05CD_codes <- filtered_data_N03AE_N05BA_N05CD$NDC
} else {
  N03AE_N05BA_N05CD_codes <- character(0)
}

# Create a binary variable to indicate whether a beneficiary received a prescription for antispasmodics in combination with analgesics (ATC code A03D)
# Filter and save NDC list for values starting with "A03D"
# filtered_data_A03D <- nonopioid_rds  %>%
# filter(grepl("^A03D", atc, ignore.case = TRUE))

# if (nrow(filtered_data_A03D) > 0) {
# A03D_codes <- filtered_data_A03D$NDC
# } else {
# A03D_codes <- character(0)
# }

# Create a binary variable to indicate whether a beneficiary received a prescription for antispasmodics, psycholeptics and analgesics in combination (ATC code A03EA)
# Filter and save NDC list for values starting with "A03EA"
# filtered_data_A03EA <- nonopioid_rds  %>%
# filter(grepl("^A03EA", atc, ignore.case = TRUE))

# if (nrow(filtered_data_A03EA) > 0) {
# A03EA_codes <- filtered_data_A03EA$NDC
# } else {
# A03EA_codes <- character(0)
# }

# Create binary indicators for each category
nonopioid_pain_rx_df <- nonopioid_pain_rx_df %>%
  group_by(BENE_ID) %>%
  mutate(
    mediator_nonopioid_gabapentin_rx = as.integer(any(NDC %in% N02BF_codes)),
    mediator_nonopioid_other_analgesic_rx = as.integer(any(NDC %in% N02B_codes)),
    mediator_nonopioid_antidepressant_rx = as.integer(any(NDC %in% N06A_codes)),
    mediator_nonopioid_muscle_relaxant_rx = as.integer(any(NDC %in% M03_codes)),
    mediator_nonopioid_antiinflammatory_rx = as.integer(any(NDC %in% M01_codes)),
    mediator_nonopioid_topical_rx = as.integer(any(NDC %in% M02A_codes)),
    mediator_nonopioid_benzodiazepine_rx = as.integer(any(NDC %in% N03AE_N05BA_N05CD_codes))
    # mediator_nonopioid_analgesic_antispasmodic_rx = as.integer(any(NDC %in% A03D_codes)), # A03D_codes contains 0 rows
    # mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx = as.integer(any(NDC %in% A03EA_codes)) # A03EA_codes contains 0 rows
  ) %>%
  distinct(BENE_ID, .keep_all = TRUE)

# Perform the left join and fill in 0s for missing mediator values
nonopioid_pain_rx_df <- left_join(analysis_df, nonopioid_pain_rx_df, by = "BENE_ID") |>
  mutate(mediator_nonopioid_pain_rx = ifelse(is.na(mediator_nonopioid_pain_rx), 0, mediator_nonopioid_pain_rx),
         mediator_nonopioid_gabapentin_rx = ifelse(is.na(mediator_nonopioid_gabapentin_rx), 0, mediator_nonopioid_gabapentin_rx),
         mediator_nonopioid_other_analgesic_rx = ifelse(is.na(mediator_nonopioid_other_analgesic_rx), 0, mediator_nonopioid_other_analgesic_rx),
         mediator_nonopioid_antidepressant_rx = ifelse(is.na(mediator_nonopioid_antidepressant_rx), 0, mediator_nonopioid_antidepressant_rx),
         mediator_nonopioid_muscle_relaxant_rx = ifelse(is.na(mediator_nonopioid_muscle_relaxant_rx), 0, mediator_nonopioid_muscle_relaxant_rx),
         mediator_nonopioid_antiinflammatory_rx = ifelse(is.na(mediator_nonopioid_antiinflammatory_rx), 0, mediator_nonopioid_antiinflammatory_rx),
         mediator_nonopioid_topical_rx = ifelse(is.na(mediator_nonopioid_topical_rx), 0, mediator_nonopioid_topical_rx),
         mediator_nonopioid_benzodiazepine_rx = ifelse(is.na(mediator_nonopioid_benzodiazepine_rx), 0, mediator_nonopioid_benzodiazepine_rx)
         # mediator_nonopioid_analgesic_antispasmodic_rx = ifelse(is.na(mediator_nonopioid_analgesic_antispasmodic_rx), 0, mediator_nonopioid_analgesic_antispasmodic_rx),
         # mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx = ifelse(is.na(mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx), 0, mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx)
  )

# Select only the needed relevant variables
nonopioid_pain_rx_df <- nonopioid_pain_rx_df |>
  select(BENE_ID, 
         mediator_nonopioid_pain_rx, 
         mediator_nonopioid_gabapentin_rx,
         mediator_nonopioid_other_analgesic_rx,
         mediator_nonopioid_antidepressant_rx,
         mediator_nonopioid_muscle_relaxant_rx,
         mediator_nonopioid_antiinflammatory_rx,
         mediator_nonopioid_topical_rx,
         mediator_nonopioid_benzodiazepine_rx
         # mediator_nonopioid_analgesic_antispasmodic_rx,
         # mediator_nonopioid_analgesic_antispasmodic_psycholeptic_rx
  )

# nonopioid_pain_rx_df <- nonopioid_pain_rx_df |>
#   mutate(mediator_nonopioid_pain_rx2 = as.numeric(mediator_nonopioid_antiinflammatory_rx | 
#                                                     mediator_nonopioid_topical_rx |
#                                                     mediator_nonopioid_muscle_relaxant_rx |
#                                                     mediator_nonopioid_other_analgesic_rx |
#                                                     mediator_nonopioid_antidepressant_rx |
#                                                     mediator_nonopioid_gabapentin_rx |
#                                                     mediator_nonopioid_benzodiazepine_rx))
# 
# # BENE_ID_pain <- analysis_df |> filter(disability_pain_cal=="chronic pain only") |> pull(BENE_ID)
# nonopioid_cp <- nonopioid_pain_rx_df |> filter(BENE_ID %in% BENE_ID_pain)
# table(nonopioid_cp$mediator_nonopioid_pain_rx2)


# Save as R dataset
saveRDS(nonopioid_pain_rx_df, file.path(low_back_dir, "treatments/mediator_nonopioid_pain_rx_bin.rds"))

# tmp <- readRDS(file.path(drv_root, "mediator_nonopioid_pain_rx_bin.rds"))
# 
# tmp_cp <- tmp |> filter(BENE_ID %in% BENE_ID_pain)
# table(tmp_cp$mediator_nonopioid_pain_rx)