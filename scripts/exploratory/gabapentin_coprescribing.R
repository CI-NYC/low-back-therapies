library(tidyverse)
library(collapse)
library(xtable)

source("~/medicaid/low-back-therapies/R/helpers.R")


dat <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  filter(subset_oud == 0)

# Determine who has one of the treatments, and whether they are overlapping ------------------------------------------------------

treatment_end_dt <- load_data("exposure_end_dt_30_days.fst", file.path(drv_root, "treatment"))
gaba <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment")) |>
  filter(BENE_ID %in% dat$BENE_ID) |>
  filter(treatment_name == "Gabapentin") |>
  left_join(treatment_end_dt, by="BENE_ID") |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  select(BENE_ID, gaba_start_dt = treatment_start_dt, gaba_end_dt = treatment_end_dt)
opioid <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment"))|>
  filter(BENE_ID %in% dat$BENE_ID) |>
  left_join(treatment_end_dt, by="BENE_ID") |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  select(BENE_ID, op_start_dt = treatment_start_dt, op_end_dt = treatment_end_dt)

check_overlap <- opioid |> 
  full_join(gaba, by="BENE_ID") |>
  fmutate(overlap = case_when(
          is.na(op_start_dt) | is.na(gaba_start_dt) ~ FALSE,
          op_start_dt <= gaba_end_dt & gaba_start_dt <= op_end_dt ~ TRUE,
          TRUE ~ FALSE
        ))

has_overlapping_treatments <- check_overlap |>
  group_by(BENE_ID) |>
  summarise(overlap = any(overlap)) |>
  ungroup()


# Count number belonging to each gaba-opioid combination (overlapping) ------------------------------------------------------

dat2 <- dat |>
  select(BENE_ID, exposure_gabapentin, "exposure_opioid_>50mme", "exposure_opioid_>7days_<=50mme", "exposure_opioid_<=7days_<=50mme") |>
  left_join(has_overlapping_treatments, by="BENE_ID") |>
  # groups:
  # 1. gabapentin w/o overlapping opioid
  # 2. gabapentin w/ overlapping opioid <=7 days, <=50 MME
  # 3. gabapentin w/ overlapping opioid >7 days, <=50 MME
  # 4. gabapentin w/ overlapping opioid >50 MME
  # 5. opioid <=7 days, <=50 MME w/o overlapping gabapentin
  # 6. opioid >7 days, <=50 MME w/o overlapping gabapentin
  # 7. opioid >50 MME w/o overlapping gabapentin
  mutate(group = case_when(
    exposure_gabapentin == 1 & overlap == FALSE ~ "gabapentin w/o overlapping opioid",
    exposure_gabapentin == 1 & overlap == TRUE & `exposure_opioid_<=7days_<=50mme` == 1 ~ "gabapentin w/ overlapping opioid $\\le7$ days, $\\le50$ MME",
    exposure_gabapentin == 1 & overlap == TRUE & `exposure_opioid_>7days_<=50mme` == 1 ~ "gabapentin w/ overlapping opioid $>7$ days, $\\le50$ MME",
    exposure_gabapentin == 1 & overlap == TRUE & `exposure_opioid_>50mme` == 1 ~ "gabapentin w/ overlapping opioid $>50$ MME",
    exposure_gabapentin == 0 & overlap == FALSE & `exposure_opioid_<=7days_<=50mme` == 1 ~ "opioid $\\le7$ days, $\\le50$ MME w/o overlapping gabapentin",
    exposure_gabapentin == 0 & overlap == FALSE & `exposure_opioid_>7days_<=50mme` == 1 ~ "opioid $>7$ days, $\\le50$ MME w/o overlapping gabapentin",
    exposure_gabapentin == 0 & overlap == FALSE & `exposure_opioid_>50mme` == 1 ~ "opioid $>50$ MME w/o overlapping gabapentin",
    TRUE ~ "neither gabapentin nor opioid"
  )) 

group <- names(table(dat2$group))
number <- table(dat2$group)
proportion <- prop.table(number)
number_proportion <- paste0(number, " (", round(proportion*100,1), "\\%)")

dat2 <- as.data.frame(cbind(group, number_proportion)) |>
  slice(-5)


#                                                  group number_proportion
# 1  gabapentin w/ overlapping opioid <=7 days, <=50 MME     3468 (0.7\\%)
# 2             gabapentin w/ overlapping opioid >50 MME     2783 (0.5\\%)
# 3   gabapentin w/ overlapping opioid >7 days, <=50 MME     5721 (1.1\\%)
# 4                    gabapentin w/o overlapping opioid    41905 (8.2\\%)
# 5 opioid <=7 days, <=50 MME w/o overlapping gabapentin   60235 (11.8\\%)
# 6            opioid >50 MME w/o overlapping gabapentin    12756 (2.5\\%)
# 7  opioid >7 days, <=50 MME w/o overlapping gabapentin    24788 (4.8\\%)

# part1[part1 == "0 (NaN\\%)"] <- ""
print(
  xtable(
    caption = "",
    dat2,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)
