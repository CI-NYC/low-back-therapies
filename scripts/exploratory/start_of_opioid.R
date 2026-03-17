library(tidyverse)
library(collapse)

source("~/medicaid/low-back-therapies/R/helpers.R")


dat <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  filter(subset_oud == 0)

opioid_data <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  right_join(dat |> 
              filter(`exposure_opioid_>50mme` | `exposure_opioid_>7days_<=50mme` | `exposure_opioid_<=7days_<=50mme`) |> 
              select(BENE_ID, day0_dt), by="BENE_ID") |>
  group_by(BENE_ID) |>
  summarise(
    op_start_dt = min(treatment_start_dt),
    days_until_opioid_start = as.numeric(as.Date(op_start_dt) - as.Date(day0_dt))
  ) |>
  distinct()

p <- ggplot(opioid_data, aes(x=days_until_opioid_start)) +
  geom_histogram(binwidth=1) +
  labs(x="Days until opioid start", y="Count") +
  labs(title = "Histogram of Days Until Opioid Start among those with Opioid Exposure (n=114,294)",
       subtitle = "75,229 individuals received opioids on day 0") +
  theme_minimal()

ggsave("/home/amh2389/medicaid/low-back-therapies/scripts/exploratory/days_until_opioid_start_histogram.png", p, width=10, height=7.5, dpi=300)
