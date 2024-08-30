source("clean_data.R")
library(dplyr)
library(tidyr)
#Q1

#sex
sex_distribution <- data %>%
  group_by(department, sex) %>%
  summarize(count = n()) %>%
  ungroup()
print(sex_distribution)

c_sex <- sex_distribution %>%
  pivot_wider(names_from = sex, values_from = count) %>%
  mutate(across(everything(), ~ replace_na(., 0)))
print(c_sex)

#age distribution
c_age <- data %>%
  group_by(department) %>%
  summarize(
    mean_age = mean(age),
    median_age = median(age)
  )
print(c_age)

#smoke status
smoke_status <- sum(data$smoke == 1)
print(paste("number of smokers =", smoke_status, sep=" "))

#alcohol consumption
alcohol_consumption <- sum(data$drink)
print(paste("number of consuming alcohol =", alcohol_consumption, sep=" "))

#taking medications
medication <- sum(data$mednow)
print(paste("number of taking medications =", medication, sep=" "))

#Q2
data <- data %>%
  mutate(BMI = wt / ((height / 100) ^ 2),
         BMI_status = case_when(
           BMI < 18.5 ~ "underweight",
           BMI >= 18.5 & BMI <= 22.9 ~ "normal",
           TRUE ~ "overweight")
  )
print(data)

#Q3
data <- data %>%
  mutate(BP = case_when(
    bp1 >140 | bp2 >=90 ~ "at risk",
    TRUE ~ "no risk"
  ))
print(data)

n_hypertension <- sum(data$BP == "at risk")
print(paste("number of hypertension =", n_hypertension, sep=" "))

#Q4
data <- data %>%
  rename(ecg_report = `ECG Report`) %>%
  mutate(high_CVD_risk = case_when(
    ecg_report == "Normal sinus rhythm\nNormal ECG" ~ "normal",
    TRUE ~ "abnormal"
  ))
print(data)

abnormal_count <- sum(data$high_CVD_risk == "abnormal")
total_count <- sum(data$high_CVD_risk == "abnormal" | data$high_CVD_risk == "normal")
CVD_proportion <- abnormal_count / total_count
print(paste("the proportion of people at high risk for CVD is", CVD_proportion, sep=" "))

c_CVD <- data %>%
  group_by(department) %>%
  mutate(CVD_abnormal = high_CVD_risk == "abnormal") %>%
  count(CVD_abnormal) %>%
  mutate(proportion = n / sum(n)) %>%
  filter(CVD_abnormal == TRUE) %>%
  select(-CVD_abnormal)
print(c_CVD)

#Q5
data$start_t <- as.Date(data$start_t, format = "%Y")
data <- data %>%
  mutate(start_t = as.Date(paste0(format(start_t, "%Y"), "-01-01"), format = "%Y-%m-%e"))
data$consult_date <- as.Date(data$consult_date, format = "%Y-%m-%e")

data <- data %>%
  mutate(tenure_t = as.numeric(floor(lubridate::interval(start_t, consult_date)/ years(1))))
print(data)

#merge the table
merged_table <- c_sex %>%
  left_join(c_age, by = "department") %>%
  left_join(c_CVD, by = "department") %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  rename(
    female = '0',
    male = '1',
    n_CVD = n,
    CVD_proportion = proportion
  )

print(merged_table)

merged_table <- t(merged_table)
merged_table <- as.data.frame(merged_table)
print(merged_table)
