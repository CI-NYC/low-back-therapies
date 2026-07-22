library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)
library(tidylog)

source("~/medicaid/low-back-therapies/R/helpers.R")

cohort_full <- load_data("pain_washout_continuous_enrollment_with_exposures.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, exposure_period_end_dt)

opioid_fills <- load_data("opioid_dts_12mos.fst", file.path(drv_root,"treatment"))

cohort <- cohort_full |>
  left_join(opioid_fills)

# cohort <- cohort |>
#   left_join(opioid_fills) |>
#   group_by(BENE_ID) |>
#   summarise(opioid_use_1mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(1), exposure_period_end_dt + days(30)))), # 30 days
#             opioid_use_2mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(31), exposure_period_end_dt + days(60)))), # 30 days
#             opioid_use_3mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(61), exposure_period_end_dt + days(91)))), # 31 days
#             opioid_use_4mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(92), exposure_period_end_dt + days(121)))), # 30
#             opioid_use_5mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(122), exposure_period_end_dt + days(151)))), # 30
#             opioid_use_6mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(152), exposure_period_end_dt + days(182)))), # 31
#             opioid_use_7mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(183), exposure_period_end_dt + days(212)))), # 30
#             opioid_use_8mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(213), exposure_period_end_dt + days(242)))), # 30
#             opioid_use_9mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(243), exposure_period_end_dt + days(273)))), # 31
#             opioid_use_10mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(274), exposure_period_end_dt + days(303)))), # 30
#             opioid_use_11mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(304), exposure_period_end_dt + days(333)))), # 30
#             opioid_use_12mos = as.numeric(any(rx_start_dt %within% interval(exposure_period_end_dt + days(334), exposure_period_end_dt + days(364)))) # 31
#             )

setDT(cohort)
setkey(cohort, BENE_ID, exposure_period_end_dt)

windows <- data.table(
  month = 1:12,
  start = c(  1,  31,  61,  92, 122, 152, 183, 213, 243, 274, 304, 334),
  end   = c( 30,  60,  91, 121, 151, 181, 212, 242, 273, 303, 333, 364)
)


periods <- cohort[, .(start_dt  = exposure_period_end_dt + days(windows$start),
               end_dt    = exposure_period_end_dt + days(windows$end),
               month     = windows$month),
           by = .(BENE_ID, exposure_period_end_dt)][, .(BENE_ID, month, start_dt, end_dt)]

setkey(periods,   BENE_ID, start_dt, end_dt)
setkey(cohort,    BENE_ID, rx_start_dt)

matches <- periods[cohort,
                   on = .(BENE_ID,
                          start_dt <= rx_start_dt,
                          end_dt   >= rx_start_dt),
                   nomatch = 0,
                   .(month, opioid_fill = 1L),
                   by = .EACHI
]

wide <- matches |>
  select(BENE_ID, month, opioid_fill) |>
  distinct() |>
  pivot_wider(
    id_cols = BENE_ID,
    names_from = month,
    values_from = opioid_fill,
    values_fill = 0L,
    names_prefix = "opioid_use_period_"
  )

wide$num_opioid_months <- rowSums(wide[,2:13], na.rm=T)

prolonged_opioid_use <- cohort |>
  left_join(wide) |>
  mutate(outcome_prolonged_opioid_use = replace_na(as.numeric(num_opioid_months == 12),0)) |> 
  distinct()

write_data(prolonged_opioid_use, "outcome_prolonged_opioid_use.fst", file.path(drv_root,"outcome"))