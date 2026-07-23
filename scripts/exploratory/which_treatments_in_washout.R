library(tidyverse)
library(fst)
library(collapse)
library(data.table)
library(xtable)

source("~/medicaid/low-back-therapies/R/helpers.R")


# opioid naive exclusion
opioid_naive <- load_data("pain_washout_continuous_enrollment_opioid_naive.fst", file.path(drv_root, "exclusion"))
# base cohort
cohort <- load_data(paste0("pain_washout_continuous_enrollment_dts.fst"), file.path(drv_root_30_day_treatment, "modified_variables"))
# washout pain exclusion
washout_pain <- load_data("pain_washout_continuous_enrollment_washout_pain.fst", file.path(drv_root, "exclusion"))
# washout pain treatment exclusion
washout_tx <- load_data("previous_tx_exclusions.fst", file.path(drv_root, "exclusion"))
# debse exclusions
debse_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))
# iph exclusions
iph_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst", file.path(drv_root, "exclusion"))
# oth exclusions
oth_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
# oud exclusions
oud_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst", file.path(drv_root, "exclusion"))
# exposures
exposures <- load_data(paste0("exposures.fst"), file.path(drv_root_30_day_treatment, "modified_variables"))
# censoring
cens <- load_data("pain_washout_continuous_enrollment_censoring.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
# outcomes
oud <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_outcomes.fst", file.path(drv_root_30_day_treatment, "modified_variables"))
# chronic_pain <- load_data("outcome_chronic_pain.fst", file.path(drv_root, "outcome"))
# prolonged_opioid_use <- load_data("outcome_prolonged_opioid_use.fst", file.path(drv_root,"outcome")) |> select(BENE_ID, outcome_prolonged_opioid_use) |> distinct()
# chronic_opioid_therapy <- load_data("outcome_chronic_opioid_therapy.fst", file.path(drv_root, "outcome"))


cohort <- list(
  cohort,
  opioid_naive,
  oud_exclusions,
  washout_pain,
  debse_exclusions,
  iph_exclusions,
  oth_exclusions
) |>
  reduce(join, how = "left") |>
  mutate(across(everything(), ~ replace_na(., 0)))

cohort <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))

cohort_modified <- cohort |>
  left_join(washout_tx) |>
  filter(exclusion_previous_tx == 1) |>
  select(BENE_ID, washout_start_dt, washout_end_dt)

################################################

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt, treatment_end_dt, treatment_name) |> distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment")) |> filter(!treatment_name=="Counseling")

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  mutate(treatment_name = ifelse(treatment_name %in% c("Other analgesic", "Acupuncture"), "Other treatment", treatment_name)) |>
  as.data.table()

washout_tx <- cohort_modified |>
  left_join(treatments) |>
  filter(treatment_start_dt >= washout_start_dt,
         treatment_start_dt <= washout_end_dt)

washout_tx <- washout_tx |>
  group_by(treatment_name) |>
  summarise(number = length(unique(BENE_ID)),
            proportion = round(number/nrow(cohort_modified)*100,1),
            number_proportion = paste0(number, " (",proportion,"\\%)"))

washout_tx <- washout_tx |>
  select(treatment_name, number_proportion)

print(
  xtable(
    caption = "",
    washout_tx,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)
