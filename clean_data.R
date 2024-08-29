library(readr)
dummy_data <- read_csv("~/Downloads/dummy_data_practical.csv")
library(dplyr)
library(lubridate)
col_na_count <- colSums(is.na(dummy_data))
print(col_na_count)
library(tidyr)
dummy_data <- drop_na(dummy_data, start_t)

library(stringr)
dummy_data$start_t <- as.numeric(as.character(
  str_replace_all(dummy_data$start_t, "/[1-9]|/1[0-2]", "")
  ))

dummy_data <- dummy_data %>%
  filter(start_t < 2024) %>%
  mutate(start_t = case_when(
    start_t < 1911 ~ start_t + 1911,
    TRUE ~ start_t
  ))

print(dummy_data)
