library(dplyr)
#Load Data
activity <- read.csv("dailyActivity_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")

#Clean Column names
library(janitor)
activity <- clean_names(activity)
sleep <- clean_names(sleep)

#Date conversion
activity$activity_date <- as.Date(activity$activity_date, format = "%m/%d/%Y")
sleep$sleep_day <- as.POSIXct(sleep$sleep_day, format = "%m/%d/%Y %I:%M:%S %p")
sleep$sleep_day <- as.Date(sleep$sleep_day)

str(activity$activity_date)
str(sleep$sleep_day)

#Check for Duplicates
any(duplicated(activity))
any(duplicated(sleep))

sum(duplicated(activity))
sum(duplicated(sleep))

#removing duplicates
activity <- distinct(activity)
sleep <- distinct(sleep)

#Checking for NA values
sum(is.na(activity)) 
sum(is.na(sleep))

#Merging the two datasets
combined <- merge(activity, sleep,
                  by.x = c("id", "activity_date"),
                  by.y = c("id", "sleep_day"),
                  all.x = TRUE)

str(combined)              
head(combined, 3)          
nrow(combined)            

combined$date <- combined$activity_date

#Creating Engagement Metrics
#A. Creating active_day flag
combined$active_day <- ifelse(combined$total_steps > 0, 1, 0)

#B. Creating user level summary table
user_summary <- combined %>%
  group_by(id) %>%
  summarise(
    total_days_tracked = n(),
    active_days = sum(active_day, na.rm = TRUE),
    inactive_days = total_days_tracked - active_days
  ) %>%
  arrange(desc(active_days))

#Creating Drop-Off Detection Metric
#A. Sorting data by user and date
combined <- combined %>%
  arrange(id, date)

#B. Creating a running drop off day flag
# Adding a column to count day sequence per user
combined <- combined %>%
  group_by(id) %>%
  mutate(day_number = row_number())

# Creating drop-off row for first inactive day
drop_off_table <- combined %>%
  group_by(id) %>%
  filter(active_day == 0) %>%
  slice_head(n = 1) %>%
  select(id, drop_off_date = date, drop_off_day_number = day_number)

#C.Merging with user_summary
user_summary <- user_summary %>%
  left_join(drop_off_table, by = "id")

#Categorizing users by consistency
user_summary <- user_summary %>%
  mutate(
    consistency_group = case_when(
      active_days >= 25 ~ "Consistent Engager",
      active_days >= 10 ~ "Moderate User",
      TRUE ~ "Drop-Off User"
    )
  )

#Descriptive summary 
# Merge group label back to daily data
combined <- combined %>%
  left_join(user_summary %>% select(id, consistency_group), by = "id")

# Summary stats by consistency group
library(ggplot2)

group_summary <- combined %>%
  group_by(consistency_group) %>%
  summarise(
    avg_steps = mean(total_steps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE),
    avg_minutes_asleep = mean(total_minutes_asleep, na.rm = TRUE),
    avg_sedentary = mean(sedentary_minutes, na.rm = TRUE),
    user_count = n_distinct(id)
  )
print(group_summary)

combined %>%
  filter(consistency_group == "Drop-Off User") %>%
  summarise(any_sleep_data = any(!is.na(total_minutes_asleep)))

#Chart 1: Steps by Consistency Group
ggplot(group_summary, aes(x = consistency_group, y = avg_steps, fill = consistency_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Steps by Consistency Group", y = "Avg. Steps", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

#Chart 2: Sedentary Time by Group
ggplot(group_summary, aes(x = consistency_group, y = avg_sedentary, fill = consistency_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Sedentary Minutes", y = "Minutes", x = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

#Chart 3 : Sleep vs Steps
# Create sleep quartiles
sleep_steps_plot <- combined %>%
  filter(!is.na(total_minutes_asleep), !is.na(total_steps)) %>%
  mutate(sleep_group = ntile(total_minutes_asleep, 4)) %>%
  mutate(sleep_group = case_when(
    sleep_group == 1 ~ "Very Low Sleep",
    sleep_group == 2 ~ "Low Sleep",
    sleep_group == 3 ~ "Moderate Sleep",
    sleep_group == 4 ~ "High Sleep"
  ))

# Boxplot: Steps by Sleep Group
ggplot(sleep_steps_plot, aes(x = sleep_group, y = total_steps, fill = sleep_group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Steps vs. Sleep Duration Group",
       x = "Sleep Group",
       y = "Total Steps") +
  theme_minimal() +
  theme(legend.position = "none")
#Chart 4: Drop-off Pattern -"User retention over time"
#Creating daily engagement table
daily_retention <- combined %>%
  filter(!is.na(active_day)) %>%
  group_by(date) %>%
  summarise(active_users = sum(active_day))

#Plotting the retention trend
ggplot(daily_retention, aes(x = date, y = active_users)) +
  geom_line(color = "#008080", size = 1.2) +
  geom_point(color = "#008080") +
  labs(title = "Daily Active Users Over Time",
       x = "Date",
       y = "Number of Active Users") +
  theme_minimal()
