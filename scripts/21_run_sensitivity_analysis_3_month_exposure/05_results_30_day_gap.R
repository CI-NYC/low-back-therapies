# -------------------------------------
# Script: 02_results.R
# Author: Nick Williams
# Purpose: Load and combine results from runs of 01_run_main.R
# Notes:
# -------------------------------------

library(lmtp)
library(glue)
library(purrr)
library(dplyr)
library(ggplot2)
library(data.table)
library(showtext)

showtext_auto()

source("~/medicaid/low-back-therapies/R/helpers.R")

data <- load_data("pain_cohort_clean_imputed.fst", file.path(drv_root, "final")) |> as.data.table()

version <- "30_day_gap" # sensitivity analysis using 30-day allowable gaps

for (Y in c("oud_period_1", "oud_period_2")){
  
A <- (c("exposure_acetaminophen",
            # "exposure_acupuncture",
            "exposure_anti_inflammatory",
            "exposure_benzodiazepine",
            "exposure_chiropractic",
            "exposure_other_treatment",
            "exposure_gabapentin",
            "exposure_intervention",
            "exposure_muscle_relaxant",
            "exposure_massage_therapy",
            "exposure_physical_therapy",
            "exposure_steroid",
            # "exposure_opioid",
            # "exposure_max_daily_dose_mme",
            # "exposure_days_supply"
            "exposure_opioid_<=7days_<=50mme",
            "exposure_opioid_>7days_<=50mme",
            "exposure_opioid_>50mme"
))

read_res <- function(Y, intervention) {
  map_dfr(A, function(treatment) {
    file.path(drv_root, "analysis", version,
              glue("fit_{intervention}_outcome_{Y}_treatment_{treatment}.rds")) |> 
      readRDS() |> 
      tidy() |> 
      mutate(treatment = treatment, .before = "estimate")
  })
}

read_diff <- function(Y, intervention1, intervention2) {
  map_dfr(A, function(treatment) {
    diff <- file.path(drv_root, "analysis", version,
                      glue("fit_{intervention1}_outcome_{Y}_treatment_{treatment}.rds")) |> 
      readRDS() |> 
      lmtp_contrast(ref = readRDS(file.path(drv_root, "analysis", version, glue("fit_{intervention2}_outcome_{Y}_treatment_{treatment}.rds"))))
    mutate(diff$estimates, treatment = treatment, .before = "shift") #|>
      # mutate(estimate = shift - ref)
  })
}

read_relr <- function(Y, intervention1, intervention2) {
  map_dfr(A, function(treatment) {
    diff <- file.path(drv_root, "analysis", version,
                      glue("fit_{intervention1}_outcome_{Y}_treatment_{treatment}.rds")) |> 
      readRDS() |> 
      lmtp_contrast(ref = readRDS(file.path(drv_root, "analysis", version, glue("fit_{intervention2}_outcome_{Y}_treatment_{treatment}.rds"))), 
                    type = "rr")
    mutate(diff$estimates, treatment = treatment, .before = "shift") |> 
      mutate(estimate = estimate - 1,
             conf.low = conf.low - 1,
             conf.high = conf.high - 1)
  })
}

# # Results for non-OUD group
# res_n_oud <- file.path(drv_root, "analysis", version, glue("fit_off_outcome_{Y}_treatment_{treatment}.rds")) |> 
#   readRDS() |> 
#   tidy() |> 
#   mutate(treatment = "No censoring", .before = "estimate") |> 
#   bind_rows(read_res(Y, subset=0))

# # Results for OUD group
# res_y_oud <- file.path(drv_root, "analysis", version, glue("fit_1_{Y}_outcome_fix_no_cens.rds")) |> 
#   readRDS() |> 
#   tidy() |> 
#   mutate(treatment = "No censoring", .before = "estimate") |> 
#   bind_rows(read_res(Y, subset=1))

# Plot results ------------------------------------------------------------

theme_set(theme_minimal(base_family = "sans", 
                        base_size = 3,
                        base_line_size = 0.2,
                        base_rect_size = 0.2))
theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(color = "black", linewidth = .15),
  axis.ticks.x = element_line(color = "black", linewidth = .15),
  axis.title.y = element_blank(),
  plot.margin = margin(10, 15, 10, 15),
  text = element_text(color = "black", 
                      size = 3)
)

label_counts <- function(data, subset, m) {
  subsetted <- data[subset_oud == subset]
  has_mediator <- subsetted[subsetted[[m]] > 0]
  vals <- table(has_mediator[[Y]])
  glue("Outcome count: {vals['1']}")
}

cl_n_oud <-setNames(map_chr(A[1:length(A)], \(m) label_counts(data, 0, m)),
                  A[1:length(A)])
# cl_y_oud <- setNames(map_chr(A[1:length(A)], \(m) label_counts(data, 1, m)), 
#                   A[1:length(A)])


relabel <- function(data) {
  mutate(data, 
         treatment = case_when(
           treatment == "exposure_acetaminophen" ~ "Acetaminophen", 
           # treatment == "exposure_acupuncture" ~ "Acupuncture",
           treatment == "exposure_anti_inflammatory" ~ "Anti-inflammatory", 
           treatment == "exposure_benzodiazepine" ~ "Benzodiazepine", 
           treatment == "exposure_chiropractic" ~ "Chiropractic", 
           treatment == "exposure_other_treatment" ~ "Other treatment", 
           treatment == "exposure_gabapentin" ~ "Gabapentin", 
           treatment == "exposure_intervention" ~ "Intervention", 
           treatment == "exposure_muscle_relaxant" ~ "Muscle relaxant", 
           treatment == "exposure_massage_therapy" ~ "Massage therapy", 
           treatment == "exposure_physical_therapy" ~ "Physical therapy",
           treatment == "exposure_steroid" ~ "Steroid", 
           treatment == "exposure_opioid_<=7days_<=50mme" ~ "Opioid, \u2264 7 days & \u2264 50 MME", 
           treatment == "exposure_opioid_>7days_<=50mme" ~ "Opioid, > 7 days & \u2264 50 MME", 
           treatment == "exposure_opioid_>50mme" ~ "Opioid, > 50 MME", 
           TRUE ~ treatment
         ))
}

plot_diff <- function(data) {
  ggplot(data, aes(estimate, treatment)) +
    geom_col(aes(fill = estimate > 0), 
             width = 0.3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                   height = 0.2, 
                   position = position_nudge(y = 0.4), 
                   size = 0.15) + 
    geom_vline(xintercept = 0, 
               linetype = "dashed", 
               size = 0.15, 
               color = "grey50") + 
    scale_y_discrete() +
    scale_x_continuous(name = "Risk difference (95% CI)") + 
    geom_text(
      aes(label = paste0("  ", sprintf("%.4f", estimate), "  "), 
          hjust = ifelse(estimate < 0, 1, 0)),
      size = 0.75, family = "sans"
    ) +
    scale_color_manual(values = c("black"), guide = "none") + 
    scale_fill_manual(values = c("#1D785A", "red3"), guide = "none") +
    theme(axis.text.y = element_text(
      hjust = 0, margin = margin(1, 0, 1, 0), 
      size = rel(1.1), 
      color = "black"
    ))
}

plot_relr <- function(data) {
  ggplot(data, aes(estimate, treatment)) +
    geom_col(aes(fill = estimate > 0),
             width = 0.3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2,
                   position = position_nudge(y = 0.4), 
                   size = 0.1) +
    geom_vline(xintercept = 0, 
               linetype = "dashed", 
               size = 0.15, 
               color = "grey50") + 
    scale_y_discrete() +
    scale_x_continuous(name = "Relative risk (95% CI)", 
                       labels = scales::label_percent()) +
    geom_text(
      aes(label = paste0("  ", sprintf("%2.1f", estimate * 100), "%  "), 
          hjust = ifelse(estimate < 0, 1, 0)),
      size = 0.75, 
      family = "sans"
    ) +
    scale_color_manual(values = c("black"), guide = "none") + 
    scale_fill_manual(values = c("#1D785A", "red3"), guide = "none") +
    theme(axis.text.y = element_text(
      hjust = 0, margin = margin(1, 0, 1, 0),
      size = rel(1.1),
      color = "black"
    ))
}

plot_res <- function(data, limits) {
  ggplot(data, aes(x = reorder(treatment, estimate), y = estimate)) + 
    geom_point() + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2) + 
    scale_y_continuous(name = "Incidence") + 
    coord_flip(ylim = limits) + 
    theme(axis.text.y = element_text(
      hjust = 0, margin = margin(1, 0, 1, 0), 
      size = rel(1.1), 
      # face = "bold", 
      color = "black"
    ))
}

extract_count <- function(x) {
  map_dbl(x, function(i) {
    if (!stringr::str_detect(i, "\\d+")) return(14)
    as.numeric(unlist(stringr::str_extract_all(i, "\\d+")))
  })
}


pdf(
  glue("~/medicaid/low-back-therapies/figures/{version}/relative_risks/{version}_onvsoff_{Y}.pdf"), 
  width = 7/2.54, height = 3.5/2.54
)

print(read_relr(Y, "on", "off") |> 
  relabel() |> 
  filter(extract_count(cl_n_oud) > 10) |> 
  mutate(treatment = forcats::fct_reorder(treatment, estimate, .desc = F)) |> 
  plot_relr())

dev.off()

}