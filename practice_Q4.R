#Q4
library(readr)
library(tidyr)
ecg_output <- read_csv("~/Downloads/ecg_output.csv")
col_na_count <- colSums(is.na(ecg_output))
print(col_na_count)
library(dplyr)
ecg_data <- ecg_output %>% 
  drop_na(`ECG Report`) %>%
  rename(ecg_report = `ECG Report`)
print(ecg_data)
