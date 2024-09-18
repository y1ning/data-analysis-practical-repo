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

#MK=民國(?)
clean_time <- select(merged_data, id, start_t)

index <- c()
patterns <- c("[A-Za-z]", "年") ##欄位中是否包含英文和年##
for (pattern in patterns) {
  index <- c(index, grep(pattern, clean_time$start_t))
}
index <- unique(index)
print(clean_time[index, ]) ##查看不合理的值##

#修正start_t欄位裡不合理的變數
clean_time$start_t <- gsub("年", "", clean_time$start_t) ##去除中文字“年”##
clean_time$start_t <- gsub("/\\d+", "", clean_time$start_t) ##去除“/”和之後的數字##
clean_time$start_t <- gsub("\\(.*\\)", "", clean_time$start_t) ##去除“( )”和之後的數字##
clean_time$start_t <- 
  gsub("MK", "", clean_time$start_t) ##直接把MK去掉，年份會變成民國(=str_length2)##
print(clean_time)
#依據不同字串長度分組整理
clean_time <- clean_time %>%
  mutate(length_2_3 = case_when(
    str_length(start_t) == 2 ~ as.numeric(start_t) + 1911,
    str_length(start_t) == 3 ~ as.numeric(start_t) + 1911,
    TRUE ~ NA
  )) %>% ##length為2,3確定是民國年份->轉西元##
  mutate(length_4 = case_when(
    str_length(start_t) == 4 ~ start_t,
    TRUE ~ NA
  )) %>%
  mutate(length_5 = case_when(
    str_length(start_t) == 5 ~ start_t,
    TRUE ~ NA
  )) %>%
  mutate(length_other = case_when(
    str_length(start_t) == 2 ~ NA,
    str_length(start_t) == 3 ~ NA,
    str_length(start_t) == 4 ~ NA,
    str_length(start_t) == 5 ~ NA,
    TRUE ~ start_t
  ))

clean_time <- clean_time %>%
  mutate(
    length_2_3 = as.character(length_2_3), #將length_2_3改為字串形式
    year = case_when(
      str_length(length_2_3) == 4 ~ length_2_3,
      str_length(length_4) == 4 ~ length_4,
      TRUE ~ NA
    )
  )

# merged_data$start_t <- as.numeric(as.character(
#   str_replace_all(merged_data$start_t, "/[1-9]|/1[0-2]", "")
#   ))
# I personally prefer using %>% , which will make your code easier to understand. 
# However, the following codes simply remove some strings, such as MK99, MK97,
# 103(2014), 97年, 102/7. Can you figure out how to clean them?
# merged_data$start_t <- str_replace_all(merged_data$start_t, "/[1-9]|/1[0-2]", "") %>% 
# as.character() %>% as.numeric
# Hint: my gut feeling is trying to use str_length. 
# You can check the textbook: https://r4ds.hadley.nz/strings 

# Again, when you use filter, you remove rows does not fulfill the criteria. 
# but we still need these people to answer other questions. 
# Here I use another mutate to keep them and recode unlikely years into NA

# merged_data$start_t <- as.character(merged_data$start_t) 
# You don't need to transform it back to strings.

#merge data
year_data <- data.frame(id = clean_time$id, start_year = clean_time$year)
##把clean_t中的id和year儲存為新的data.frame##
merged_data <- left_join(merged_data, year_data, by = "id")
##使用left_join透過id合併兩個data##

replacements <- list(
  sex = "女",
  smoke = "不吸煙",
  drink = "不喝酒",
  betal_nut = "不吃檳榔",
  vr = "無明顯異常",
  vl = "無明顯異常",
  mednow = "無"
)
##建立replacements的list##
##使用replacements替換""為0##
data <- merged_data %>%
  mutate(across(
    all_of(names(replacements)), 
    ~ ifelse(. == replacements[[cur_column()]], 0, 1) 
  )) %>% # this is smart. 
  ##package lubridate##
  mutate(age = as.numeric(round(interval(dob, consult_date)/ years(1))))
  ##round為四捨五入(floor是無條件捨去,ceiling是無條件進位)##

# I forgot to mention: their ages are the time they visited the health 
# examination centre. So you need to calculate the age on the consult_date

