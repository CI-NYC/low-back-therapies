

path <- "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables"

source(file.path(path, "00_table_one_function.R"))

overall_1 <- table_one_function(data)[1:38,]
oud_no_1 <- table_one_function(data |> filter(subset_oud == 0))[1:38,]
oud_yes_1 <- table_one_function(data |> filter(subset_oud == 1))[1:38,]

overall_2 <- table_one_function(data)[39:63,]
oud_no_2 <- table_one_function(data |> filter(subset_oud == 0))[39:63,]
oud_yes_2 <- table_one_function(data |> filter(subset_oud == 1))[39:63,]

part1 <- cbind(overall_1, oud_no_1, oud_yes_1)

part2 <- cbind(overall_2, oud_no_2, oud_yes_2)

write.csv(part1, file.path(path, "table_one_part1_all.csv"), row.names=F)
write.csv(part2, file.path(path, "table_one_part2_all.csv"), row.names=F)
