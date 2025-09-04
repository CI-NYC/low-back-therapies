

path <- "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables"

source(file.path(path, "00_table_one_function.R"))

overall_1 <- table_one_function(data |> filter(subset_oud %in% c(0,1)))[1:38,]
oud_no_1 <- table_one_function(data |> filter(subset_oud == 0))[1:38,2]
oud_yes_1 <- table_one_function(data |> filter(subset_oud == 1))[1:38,2]

overall_2 <- table_one_function(data |> filter(subset_oud %in% c(0,1)))[39:70,]
oud_no_2 <- table_one_function(data |> filter(subset_oud == 0))[39:70,2]
oud_yes_2 <- table_one_function(data |> filter(subset_oud == 1))[39:70,2]

overall_2_7day_gap <- table_one_function(data_7day_gap |> filter(subset_oud %in% c(0,1)))[39:70,]
oud_no_2_7day_gap <- table_one_function(data_7day_gap |> filter(subset_oud == 0))[39:70,2]
oud_yes_2_7day_gap <- table_one_function(data_7day_gap |> filter(subset_oud == 1))[39:70,2]

part1 <- cbind(overall_1, oud_no_1, oud_yes_1)
part2 <- cbind(overall_2, oud_no_2, oud_yes_2)
part2_7day_gap <- cbind(overall_2_7day_gap, oud_no_2_7day_gap, oud_yes_2_7day_gap)

write.csv(part1, file.path(path, "table_one_part1_all.csv"), row.names=F)
write.csv(part2, file.path(path, "table_one_part2_all.csv"), row.names=F)
write.csv(part2_7day_gap, file.path(path, "table_one_part2_7_day_gap.csv"), row.names=F)

dim(data |> filter(subset_oud %in% c(0,1)))
dim(data |> filter(subset_oud == 0))
dim(data |> filter(subset_oud == 1))

part1[part1 == "0 (NaN\\%)"] <- ""
print(
  xtable(
    caption = "",
    part1,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)

part2[part2 == "0 (NaN\\%)"] <- ""
print(
  xtable(
    caption = "",
    part2,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)

part2_7day_gap[part2_7day_gap == "0 (NaN\\%)"] <- ""
print(
  xtable(
    caption = "",
    part2_7day_gap,
  ),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  booktabs  = TRUE,
  caption.placement      = "top",
)
