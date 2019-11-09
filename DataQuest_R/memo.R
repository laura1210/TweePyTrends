library(dplyr)
library(ggplot2)

original_data <- read.csv("/Users/Laura/Desktop/stat/pp312den.csv")
original_data$uhrswork[original_data$uhrswork == "N/A"] <- 0
original_data$uhrswork <- as.numeric(original_data$uhrswork)
original_data$uhrswork[is.na(original_data$uhrswork)] <- 0

race <- original_data %>%
  filter(race == "White" | race == "Black/African American/Negro")
white <- race %>%
  filter(race == "White")
black <- race %>%
  filter(race == "Black/African American/Negro")

hispan <- original_data
hispan$hispan <- as.character(hispan$hispan)
hispan$hispan[hispan$hispan == "Other"] <- "Not Hispanic"
hispan$hispan[hispan$hispan != "Not Hispanic"] <- "Hispanic"
hispanic <- hispan %>%
  filter(hispan == "Hispanic")
not_hispanic <- hispan %>%
  filter(hispan == "Not Hispanic")

ggplot(data = race) +
  aes(x = race, y = inctot) +
  geom_boxplot() +
  labs(title = "Comparison of total income between black and white people") +
  xlab("Race") +
  ylab("Total personal income") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = hispan) +
  aes(x = hispan, y = inctot) +
  geom_boxplot() +
  labs(title = "Comparison of total income between hispanic and non-hispanic people") +
  xlab("Ethnic group") +
  ylab("Total personal income") +
  theme(plot.title = element_text(hjust = 0.5))

white_inctot <- as.numeric(as.list(white[["inctot"]]))
black_inctot <- as.numeric(as.list(black[["inctot"]]))
t.test(white_inctot, black_inctot, var.equal = FALSE)
median_white_inctot <- median(as.numeric(white[["inctot"]]))
median_black_inctot <- median(as.numeric(black[["inctot"]]))

his_inctot <- as.numeric(as.list(hispanic[["inctot"]]))
not_inctot <- as.numeric(as.list(not_hispanic[["inctot"]]))
t.test(his_inctot, not_inctot, var.equal = FALSE)
median_his_inctot <- median(as.numeric(hispanic[["inctot"]]))
median_not_inctot <- median(as.numeric(not_hispanic[["inctot"]]))


white_incwage <- as.numeric(as.list(white[["incwage"]]))
black_incwage <- as.numeric(as.list(black[["incwage"]]))
t.test(white_incwage, black_incwage, var.equal = FALSE)
median_white_incwage <- median(as.numeric(white[["incwage"]]))
median_black_incwage <- median(as.numeric(black[["incwage"]]))

his_incwage <- as.numeric(as.list(hispanic[["incwage"]]))
not_incwage <- as.numeric(as.list(not_hispanic[["incwage"]]))
t.test(his_incwage, not_incwage, var.equal = FALSE)
median_his_incwage <- median(as.numeric(hispanic[["incwage"]]))
median_not_incwage <- median(as.numeric(not_hispanic[["incwage"]]))


white_uhrswork <- as.numeric(as.list(white[["uhrswork"]]))
black_uhrswork <- as.numeric(as.list(black[["uhrswork"]]))
t.test(white_uhrswork, black_uhrswork, var.equal = FALSE)
median_white_uhrswork <- median(as.numeric(white[["uhrswork"]]))
median_black_uhrswork <- median(as.numeric(black[["uhrswork"]]))

his_uhrswork <- as.numeric(as.list(hispanic[["uhrswork"]]))
not_uhrswork <- as.numeric(as.list(not_hispanic[["uhrswork"]]))
t.test(his_uhrswork, not_uhrswork, var.equal = FALSE)
median_his_uhrswork <- median(as.numeric(hispanic[["uhrswork"]]))
median_not_uhrswork <- median(as.numeric(not_hispanic[["uhrswork"]]))
