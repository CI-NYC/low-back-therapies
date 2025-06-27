
path <- "~/medicaid/low-back-therapies/scripts/00_create_cohort/12_tables"

overall_1 <- read.csv(file.path(path, "table_one_part1_overall.csv"))
oud_no_1 <- read.csv(file.path(path, "table_one_part1_oud_no.csv"))[,2]
oud_yes_1 <- read.csv(file.path(path, "table_one_part1_oud_yes.csv"))[,2]

overall_2 <- read.csv(file.path(path, "table_one_part2_overall.csv"))
oud_no_2 <- read.csv(file.path(path, "table_one_part2_oud_no.csv"))[,2]
oud_yes_2 <- read.csv(file.path(path, "table_one_part2_oud_yes.csv"))[,2]

part1 <- cbind(overall_1, oud_no_1, oud_yes_1)

part2 <- cbind(overall_2, oud_no_2, oud_yes_2)

write.csv(part1, file.path(path, "table_one_part1_all.csv"), row.names=F)
write.csv(part2, file.path(path, "table_one_part2_all.csv"), row.names=F)
