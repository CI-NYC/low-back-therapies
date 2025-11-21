
library(tidyverse)
library(data.table)
library(glue)

source("~/medicaid/low-back-therapies/R/helpers.R")

data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |> as.data.table()
version <- "trim"

A <- (c("exposure_acetaminophen",
        # "exposure_acupuncture",
        "exposure_anti_inflammatory",
        "exposure_benzodiazepine",
        "exposure_chiropractic",
        "exposure_duloxetine",
        "exposure_gabapentin",
        "exposure_intervention",
        "exposure_muscle_relaxant",
        "exposure_massage_therapy",
        "exposure_physical_therapy",
        "exposure_steroid",
        "exposure_opioid",
        "exposure_max_daily_dose_mme",
        "exposure_days_supply"
))

plot_function <- function(name, Y, subset) {
  # read in fits and combine into a dataframe
  diffs <- do.call(rbind, lapply(A, function(treatment) {
    fit <- readRDS(file.path(drv_root, "analysis", version,
                            glue("fit_{subset}_{Y}_outcome_fix_treatment_{treatment}.rds")))
    data.frame(
      treatment = treatment,
      estimate = fit$estimate@x,
      conf.low = fit$estimate@conf_int[1],
      conf.high = fit$estimate@conf_int[2],
      stringsAsFactors = FALSE
    )
  }))

  ref <- readRDS(file.path(drv_root, "analysis", version, 
                          glue("fit_{gsub(' ', '_', subset)}_{Y}_outcome_fix_no_cens.rds")))
  ref <- c(ref$estimate@x, ref$estimate@conf_int[1], ref$estimate@conf_int[2])

  p <- ggplot(diffs, aes(x=treatment, y=estimate)) +
    geom_point() + 
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.2) +
    geom_hline(yintercept=ref[1], linetype="dashed", color = "red") +
    geom_hline(yintercept=ref[2], linetype="dotted", color = "blue") +
    geom_hline(yintercept=ref[3], linetype="dotted", color = "blue") +
    coord_flip() +
    labs(title = name,
        y = "Risk Difference (95% CI)",
        x = "Treatment") +
    theme_minimal()

  ggsave(filename = glue("/home/amh2389/medicaid/low-back-therapies/figures/exploratory/{Y}_{subset}.png"), plot = p, width = 8, height = 6)
}

plot_function(">=90 opioid days supply", "outcome_chronic_opioid_therapy", 0)
plot_function("OUD 12mos", "oud_period_4", 0)
plot_function("OUD 6mos", "oud_period_2", 0)
plot_function("OUD (ICD) 12mos", "oud_hillary_period_4", 0)
plot_function("OUD (ICD) 6mos", "oud_hillary_period_2", 0)
plot_function("Chronic pain 12mos", "outcome_chronic_pain_period_4", 0)
plot_function("Chronic pain 6mos", "outcome_chronic_pain_period_2", 0)
plot_function("At least 1 opioid monthly", "outcome_prolonged_opioid_use", 0)

plot_function(">=90 opioid days supply", "outcome_chronic_opioid_therapy", 1)
plot_function("OUD 12mos", "oud_period_4", 1)
plot_function("OUD 6mos", "oud_period_2", 1)
plot_function("OUD (ICD) 12mos", "oud_hillary_period_4", 1)
plot_function("OUD (ICD) 6mos", "oud_hillary_period_2", 1)
plot_function("Chronic pain 12mos", "outcome_chronic_pain_period_4", 1)
plot_function("Chronic pain 6mos", "outcome_chronic_pain_period_2", 1)
plot_function("At least 1 opioid monthly", "outcome_prolonged_opioid_use", 1)