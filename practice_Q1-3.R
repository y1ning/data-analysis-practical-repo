source("clean_data.R")
#Q1

#sex
sex_distribution <- data %>%
  group_by(department, sex) %>%
  summarize(count = n()) %>%
  ungroup()
print(sex_distribution)

#age distribution
age_distribution <- data %>%
  group_by(department) %>%
  summarize(
    mean_age = mean(age),
    median_age = median(age)
  )
print(age_distribution)

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
    bp1 >140 | bp2 >=90 ~ "hypertension",
    TRUE ~ "na"
  ))
print(data)

n_hypertension <- sum(data$BP == "hypertension")
print(paste("number of hypertension =", n_hypertension, sep=" "))

#Q5
data$start_t <- as.Date(data$start_t, format = "%Y")
data <- data %>%
  mutate(start_t = as.Date(paste0(format(start_t, "%Y"), "-01-01"), format = "%Y-%m-%e"))

