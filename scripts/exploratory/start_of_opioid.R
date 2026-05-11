library(tidyverse)
library(collapse)
library(scales)

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

p <- ggplot(opioid_data, aes(x = days_until_opioid_start)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 7) +
  scale_y_continuous(limits = c(0,0.8),
                     labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  labs(x = "Days until opioid start", y = "Percent") +
  theme_minimal()

dim(opioid_data)
# "Histogram of Days Until Opioid Start among those with Opioid Exposure (n=119998)",
table(opioid_data$days_until_opioid_start <= 6)
# "91087 individuals received opioids in first 7 days") +

ggsave("/home/amh2389/medicaid/low-back-therapies/scripts/exploratory/days_until_opioid_start_histogram.png", p, width=8, height=6, dpi=300)
