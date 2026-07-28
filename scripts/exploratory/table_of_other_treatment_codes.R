library(dplyr)

# codes for other pain diagnoses, based on search terms
other_pain_icds <- read.csv("~/medicaid/low-back-therapies/data/public/chronic_pain_icd10_20230216.csv") |>
  filter(!grepl("low back", ICD_DESC, ignore.case = TRUE) &
           !grepl("lumb", ICD_DESC, ignore.case = TRUE) &
           !grepl("sciatica", ICD_DESC, ignore.case = TRUE)) |>
  filter(CRITERIA == "Inclusion")

# Replace `my_codes_vector` with your actual vector of codes
code_summary <- tibble(PAIN_CAT = other_pain_icds$PAIN_CAT,
                       code = other_pain_icds$ICD9_OR_10) |>
  mutate(
    # Clean non-alphanumeric characters if any exist
    clean_code = gsub("[^A-Za-z0-9]", "", code),
    # Extract 3-character prefix
    prefix = substr(clean_code, 1, 3)
  ) |>
  distinct(PAIN_CAT, prefix) |>
  arrange(prefix)

# View or copy the resulting prefixes
print(code_summary)
