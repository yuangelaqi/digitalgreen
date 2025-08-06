library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)

# Set your directory where the CSV files are located
setwd("/Users/angelayu/Documents/RStudio TAF")

# List all CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Read all CSV files into a list
data_list <- lapply(csv_files, read.csv)

names(data_list) <- tools::file_path_sans_ext(csv_files)

View(data_list)

#####
#Checking if the column names are the same across all files

# Extract column names from each data frame
#colnames_list <- lapply(data_list, colnames)

# Compare all column names to the first one
#all_same <- all(sapply(colnames_list, function(x) identical(x, colnames_list[[1]])))

#print(all_same)  # TRUE if all have the same columns
####

#Append all datasets
combined_df <- bind_rows(data_list)%>% 
  distinct() #1283956 obs

##########################################################

#Daily counts for verification with Elia

breakdown <- combined_df %>%
  
  
  date_df <- combined_df %>%
  mutate(date = sub(" .*","",created_on)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

date_group <- date_df%>%
  group_by(date) %>%
  summarise(
    distinct_convo = n_distinct(conversation_id),
    distinct_messages = n(),
    .groups = "drop"
  )

write.csv(date_group, "daily_counts.csv", row.names = FALSE)

##########################################################

#combined_df <- combined_df %>% distinct() #

# Number of total rows
#total_rows <- nrow(combined_df)

# Number of unique rows
#unique_rows <- nrow(distinct(combined_df))

# Number of duplicates
# num_duplicates <- total_rows - unique_rows

#print(paste("Number of duplicate rows:", num_duplicates)) #duplicate rows: 476008

#Looking at distinct values of certain variables
# n_distinct(combined_df$user_id) #59436
# n_distinct(combined_df$user_country) #84
# unique(combined_df$land_holding)
# summary(dgdata$user_age)
# unique(dgdata$user_country)
# unique(dgdata$user_intent)
#View(dgdata %>% filter(user_intent == "Disappointment"))

#Number of rows (or messages) of each farmer
dgdata_group <- combined_df%>%
  group_by(user_id, sort = TRUE) %>%
  summarise(count = n(), .groups = "drop")

#Number of distinct users in this data set
View(dgdata_group) #59436 distinct users / groups
summary(dgdata_group$count) #min 1, median 6, mean 21.6, max 21402

##########################################################
#Observing messages of user with high turns
# filtered_df <- filter(dgdata_group, count==27507)
# 
# #user_id wth 27507 rows: 52a46aaf-ac27-4efb-af6d-a5e7133e7483
# filtered_user <- filter(combined_df, user_id== "52a46aaf-ac27-4efb-af6d-a5e7133e7483")
# 
# #observe number of conversations of this user
# n_distinct(filtered_user$conversation_id)  #1069
# 
# #Count of relevant messages
# filtered_user %>% count(user_intent) #7600 our of 27507 messages are farming related
# 
# #Look at farming related messages
# filtered_user %>% filter(user_intent == "Farming_related")
# 
# #Filter farming related messages and look at distinct ones
# filtered_user %>%
#   filter(user_intent == "Farming_related") %>%
#   distinct(original_message)

##########################################################

#Calculate percentage of farmers who ask 1, 2, 3, etc. questions

sorted_df <- arrange(dgdata_group, count) %>%
  rename(number_of_messages = count)

messagecount <- sorted_df%>%
  count(number_of_messages, sort = TRUE) %>%
  rename(number_of_users = n)

#Plot number of messages vs. number of farmers
ggplot(messagecount, aes(y = number_of_messages, x = number_of_users)) +
  geom_point(color = "blue", alpha = 0.6, size = 0.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Number of Messages", x = "Number of Farmers")
#We observe that number of farmers who ask more than 100 questions is extremely limited
#Many farmers ask less than 25 questions

farmer_q_proportion <- sorted_df %>%
  count(number_of_messages) %>%
  mutate(percent = n / sum(n) * 100)

write.csv(farmer_q_proportion, "farmer_q_proportion.csv", row.names = FALSE)

###############################################

#What proportion of farmers have asked a question in the last 30 days?

date_df <- combined_df %>%
  mutate(date = sub(" .*","",created_on)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Define date boundaries
today <- Sys.Date()
start_of_current_month <- floor_date(today, "month")
start_of_last_month <- start_of_current_month %m-% months(1)
end_of_last_month <- start_of_current_month - 1

# Add dummy column
date_df <- date_df %>%
  mutate(in_last_month = if_else(
    date >= start_of_last_month & date <= end_of_last_month,
    1, 0
  ))

farmer_q_last30days <- date_df %>%
  count(in_last_month) %>%
  mutate(percent = n / sum(n) * 100) #need to update this on the full data set!

###############################################

#Of those who haven’t asked a question in the last 30 days, how many questions have they ever asked?

farmer_no_q_last30days <- date_df %>%
  filter(in_last_month==0) %>%
  group_by(user_id, sort = TRUE) %>%
  summarise(count = n(), .groups = "drop")

farmer_no_q_last30days_sorted_df <- arrange(farmer_no_q_last30days, count) %>%
  rename(number_of_messages = count)

farmer_no_q_last30days_messagecount <- farmer_no_q_last30days_sorted_df%>%
  count(number_of_messages, sort = TRUE) %>%
  rename(number_of_users = n)

#Plot number of messages vs. number of farmers
ggplot(farmer_no_q_last30days_messagecount, aes(y = number_of_messages, x = number_of_users)) +
  geom_point(color = "blue", alpha = 0.6, size = 0.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(y = "Number of Messages", x = "Number of Farmers")

farmer_no_q_last30days_proportion <- farmer_no_q_last30days_messagecount %>%
  count(number_of_users) %>%
  mutate(percent = n / sum(n) * 100)

###############################################

#How many farmers have never asked a relevant farming-related question?







###############################################
dgdata_group_country <- combined_df%>%
  group_by(user_country, user_id, sort = TRUE) %>%
  summarise(count = n(), .groups = "drop")

View(dgdata_group_country)

##########################################

#Find the average number of questions asked by age group#

dgdata_agegroup_mean <- combined_df %>%
  filter(user_age!="") %>%
  group_by(user_age, user_id) %>%        # Count rows per user per age
  summarise(user_row_count = n()) %>%
  group_by(user_age) %>%                 # For each age, calculate mean rows per user
  summarise(mean_rows_per_user = mean(user_row_count))

View(dgdata_agegroup_mean)

#Plot age vs. average number of questions

ggplot(dgdata_agegroup_mean, aes(y = mean_rows_per_user, x = user_age)) +
  geom_point(color = "red", alpha = 0.6, size = 0.5) +
  scale_y_continuous(limits = c(0, 75)) +
  labs(y = "Mean Number of Messages", x = "Farmer Age")

#Find the MEDIAN number of questions asked by age group#

dgdata_agegroup_median <- combined_df %>%
  filter(user_age!="") %>%
  group_by(user_age, user_id) %>%        # Count rows per user per age
  summarise(user_row_count = n()) %>%
  group_by(user_age) %>%                 # For each age, calculate mean rows per user
  summarise(mean_rows_per_user = median(user_row_count))

View(dgdata_agegroup_median)

#Plot age vs. MEDIAN number of questions

ggplot(dgdata_agegroup_median, aes(y = mean_rows_per_user, x = user_age)) +
  geom_point(color = "blue", alpha = 0.6, size = 0.5) +
  scale_y_continuous(limits = c(0, 75)) +
  labs(y = "Median Number of Messages", x = "Farmer Age")

#Combine plots

combined_mean_median <- full_join(dgdata_agegroup_mean, dgdata_agegroup_median, by = "user_age")

long_data <- combined_mean_median %>%
  pivot_longer(cols = c(mean_rows_per_user.x, mean_rows_per_user.y), names_to = "source", values_to = "y")

ggplot(long_data, aes(x = user_age, y = y, color = source)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 75)) +
  labs(title = "Scatterplot of Mean/Median Number of Questions asked by Farmer Age",
       x = "Farmer Age",
       y = "Mean/Median Number of Questions Asked",
       color = "Mean/Median") +  # Legend title
  theme_minimal()

View(dgdata_age)
#243281 users with age data#

##########################################

#Find average number of questions by country
#59437
dgdata_country <- combined_df %>%
  filter(user_country!="") %>%
  filter(user_country %in% c("Kenya", "Ethiopia", "India", "Nigeria")) %>%
  group_by(user_country, user_id) %>%        # Count rows per user per country
  summarise(user_row_count = n()) %>%
  group_by(user_country) %>%                 # For each country, calculate mean rows per user
  summarise(mean_rows_per_user = mean(user_row_count), n_users_country = n()) %>%
  arrange(mean_rows_per_user)

View(dgdata_country)

#Nigeria and Kenya have average of 29 messages, Ethiopia 24, India only 9. 

##########################################

#Difference between "all practicing farmer" and "all extension worker"?

unique(combined_df$user_role)
n(combined_df$user_role)
View(combined_df %>% filter(user_role == "role_selection_all_extension_worker")) #222,927
View(combined_df %>% filter(user_role == "role_selection_all_practicing_farmer"))  #930,537

dgdata_role <- combined_df %>%
  filter(user_role!="") %>%
  group_by(user_role, user_id) %>%        # Count rows per user per country
  summarise(user_row_count = n()) %>%     #57439 distinct users whose role info is not missing
  group_by(user_role) %>%                 # For each country, calculate mean rows per user
  summarise(mean_rows_per_user = mean(user_row_count)) %>%
  arrange(mean_rows_per_user)

#Average for practicing is only 21, average for extension is 56. Why is extension significantly higher?

#Observe if this difference holds true across countries?

dgdata_role_country <- combined_df %>%
  filter(user_role!="" & user_country!="" ) %>%
  group_by(user_role, user_id, user_country) %>%        # Count rows per user per country
  summarise(user_row_count = n()) %>%     #57439 distinct users whose role info is not missing
  group_by(user_role, user_country) %>%                 # For each country, calculate mean rows per user
  summarise(mean_rows_per_user = mean(user_row_count)) %>%
  arrange(user_country)

#Answer: yes this generally holds true by country as well



##########################################

#Does it vary by geography, for example, in Kenya?

dgdata_kenya <- combined_df %>%
  filter(user_country=="Kenya") %>%
  group_by(user_role, user_id, user_geography_level3) %>%        # Count rows per user per country
  summarise(user_row_count = n()) %>%     #57439 distinct users whose role info is not missing
  group_by(user_role, user_geography_level3) %>%                 # For each country, calculate mean rows per user
  summarise(mean_rows_per_user = mean(user_row_count)) %>%
  arrange(user_geography_level3)

#Not sure there is observable trend as not enough data atm

##########################################

#Does land holding matter?

unique(combined_df$land_holding)
table(combined_df$land_holding)




##########################################

#Look at data of users who have more than 5 questions or responses
high_users <- dgdata %>%
  group_by(user_id) %>%            # Group by user
  filter(n() > 5) %>%              # Keep users with more than 5 rows
  arrange(user_id) %>%
  ungroup()                       # Ungroup to return regular dataframe

View(high_users)


Questions:
  - difference between all practicing farmer and all extension worker?

