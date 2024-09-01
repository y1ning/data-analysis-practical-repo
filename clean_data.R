# Common practice: put the packages at the beginning of your codes. 
# Reason: you can install them before you start coding. 
# Alternative: I usually put all packages in the other file and source it. 
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# Hit: please take notes as much as you can. Usually your comments are 
# much longer than your codes.

# Read in data
dummy_data <- read_csv("~/Downloads/dummy_data_practical.csv")
ecg_output <- read_csv("~/Downloads/ecg_output.csv")

# Merge ECG data to the main data
merged_data <- dummy_data %>%
  left_join(ecg_output, by = c("id"="id", "department" = "department"))
## if there are two identical columns, you can use them to merge. 
## this will make the linkage more accurate, and you don't need to remove another spare column.


# It is great practice to check missing data. Well done.
col_na_count <- colSums(is.na(merged_data))
print(col_na_count)

# This part is a bit dangerous. If you're going to answer questions not related to start_t, 
# you will miss the whole row, ie the person won't be included in the following analysis
# merged_data <- drop_na(merged_data, start_t)

merged_data$start_t %>% head
merged_data$start_t %>% tail

# merged_data$start_t <- as.numeric(as.character(
#   str_replace_all(merged_data$start_t, "/[1-9]|/1[0-2]", "")
#   ))
# I personally prefer using %>% , which will make your code easier to understand. 
# However, the following codes simply remove some strings, such as MK99, MK97,
# 103(2014), 97年, 102/7. Can you figure out how to cleam them?
merged_data$start_t <- str_replace_all(merged_data$start_t, "/[1-9]|/1[0-2]", "") %>% 
      as.character() %>% as.numeric
# Hint: my gut feeling is trying to use str_length. 
# You can check the textbook: https://r4ds.hadley.nz/strings 

# Again, when you use filter, you remove rows does not fulfil the criteria. 
# but we still need these people to answer other questions. 
# Here I use another mutate to keep them and recode unlikely years into NA
merged_data <- merged_data %>%
      mutate(start_t = ifelse(start_t < 2024, start_t, NA)) %>% 
      mutate(start_t = case_when(
            start_t < 1911 ~ start_t + 1911,
            TRUE ~ start_t
            ))

# merged_data$start_t <- as.character(merged_data$start_t) 
# You don't need to transform it back to strings.

# print(merged_data) Don't need to print them. You can execute the object directly. 

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
  )) %>% # this is smart. 
  #package lubridate
  mutate(age = as.numeric(floor(lubridate::interval(dob, Sys.Date())/ years(1))))
# I forgot to mention: their ages are the time they visited the health 
# examination centre. So you need to calculate the age on the consult_date


