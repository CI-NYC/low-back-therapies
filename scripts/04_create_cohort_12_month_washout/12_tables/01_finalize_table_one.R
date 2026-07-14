library(xtable)

path <- "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables/"

source(file.path(path, "00_table_one_function.R"))

# overall_1 <- table_one_function(data |> filter(subset_oud %in% c(0,1)))[1:52,]
oud_no_1 <- table_one_function(data |> filter(subset_oud == 0)) |>
  slice(1:which(Characteristic == "\\hspace{0.5cm}26+"))
# oud_yes_1 <- table_one_function(data |> filter(subset_oud == 1))[1:52,2]

# overall_2 <- table_one_function(data |> filter(subset_oud %in% c(0,1)))[60:91,]
oud_no_2 <- table_one_function(data |> filter(subset_oud == 0)) |>
  slice(which(Characteristic == "\\textbf{Treatments (month 1)}"):which(Characteristic == "Uncensored through 13 months"))
# oud_yes_2 <- table_one_function(data |> filter(subset_oud == 1))[60:91,2]


# part1 <- cbind(overall_1, oud_no_1, oud_yes_1)
# part2 <- cbind(overall_2, oud_no_2, oud_yes_2)

write.csv(oud_no_1, file.path(path, "table_one_part1_all.csv"), row.names=F)
write.csv(oud_no_2, file.path(path, "table_one_part2_all.csv"), row.names=F)

part1 <- cbind(oud_no_1)
part2 <- cbind(oud_no_2)

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

# part2_7day_gap[part2_7day_gap == "0 (NaN\\%)"] <- ""
# print(
#   xtable(
#     caption = "",
#     part2_7day_gap,
#   ),
#   include.rownames = FALSE,
#   sanitize.text.function = identity,
#   booktabs  = TRUE,
#   caption.placement      = "top",
# )
