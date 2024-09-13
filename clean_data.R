library(readr)
dummy_data <- read_csv("~/Downloads/dummy_data_practical.csv")
ecg_output <- read_csv("~/Downloads/ecg_output.csv")
library(dplyr)
library(lubridate)
merged_data <- dummy_data %>%
  left_join(ecg_output, by = "id")
  
merged_data <- merged_data %>%
  select(-department.y) %>%
  rename(department = department.x)
print(merged_data)

col_na_count <- colSums(is.na(merged_data))
print(col_na_count)
library(tidyr)
merged_data <- drop_na(merged_data, start_t)

library(stringr)
merged_data$start_t <- as.numeric(as.character(
  str_replace_all(merged_data$start_t, "/[1-9]|/1[0-2]", "")
  ))

merged_data <- merged_data %>%
  filter(start_t < 2024) %>%
  mutate(start_t = case_when(
    start_t < 1911 ~ start_t + 1911,
    TRUE ~ start_t
  ))

merged_data$start_t <- as.character(merged_data$start_t)

print(merged_data)

replacements <- list(
  sex = "女",
  smoke = "不吸煙",
  drink = "不喝酒",
  betal_nut = "不吃檳榔",
  vr = "無明顯異常",
  vl = "無明顯異常",
  mednow = "無"
)

data <- merged_data %>%
  mutate(across(
    all_of(names(replacements)), 
    ~ ifelse(. == replacements[[cur_column()]], 0, 1)
  )) %>%
  #package lubridate
  mutate(age = as.numeric(floor(lubridate::interval(dob, Sys.Date())/ years(1))))

print(data)

