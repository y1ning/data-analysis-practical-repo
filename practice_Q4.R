#Q4
library(readr)
library(tidyr)
ecg_output <- read_csv("~/Downloads/ecg_output.csv")
col_na_count <- colSums(is.na(ecg_output))
print(col_na_count)
library(dplyr)
ecg_data <- ecg_output %>% 
  drop_na(`ECG Report`) %>%
  rename(ecg_report = `ECG Report`) %>%
  mutate(high_CVD_risk = case_when(
    ecg_report == "Normal sinus rhythm\nNormal ECG" ~ "normal",
    TRUE ~ "abnormal"
  ))
print(ecg_data)

#sol1
abnormal_count <- sum(ecg_data$high_CVD_risk == "abnormal")
total_count <- sum(ecg_data$high_CVD_risk == "abnormal" | ecg_data$high_CVD_risk == "normal")
CVD_proportion <- abnormal_count / total_count
print(paste("the proportion of people at high risk for CVD is", CVD_proportion, sep=" "))

#sol2
