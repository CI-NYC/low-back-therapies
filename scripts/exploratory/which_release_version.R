
source("~/medicaid/low-back-therapies/R/helpers.R")

demo <- open_demo()

demo <- demo |> filter(RFRNC_YR==2018, STATE_CD=="MI") |>
  select(RACE_ETHNCTY_CD) |>
  collect()
