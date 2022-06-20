## import libraries

library(tidyverse)
library(lubridate)

## importing datasets
daily_activity<-read.csv("fitbase_data/dailyActivity_merged.csv")
sleep_day<-read.csv("fitbase_data/sleepday_merged.csv")
weight_info<-read.csv("fitbase_data/weightLogInfo_merged.csv")
hourly_steps<-read.csv("fitbase_data/hourlySteps_merged.csv")
hourly_calories<-read.csv("fitbase_data/hourlyCalories_merged.csv")

## having a look at datasets and column

#Activity
head(daily_activity)
colnames(daily_activity)

#sleep
head(sleep_day)
colnames(sleep_day)

#weight
head(weight_info)
colnames(weight_info)

#steps
head(hourly_steps)
colnames(hourly_steps)

#Calories
head(hourly_calories)
colnames(hourly_calories)

##finding number of distict id and rows

#distict Ids
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_info$Id)
n_distinct(hourly_steps$Id)
n_distinct(hourly_calories$Id)


#Rows
nrow(daily_activity)
nrow(sleep_day)
nrow(weight_info)
nrow(hourly_steps)
nrow(hourly_calories)

# to check if any significant weight change
weight_info%>%group_by(weight_info$Id)%>%summarise(min(WeightKg),max(WeightKg))

## Processing the data

library(janitor)

cleaned_daily_activity<- clean_names(daily_activity)%>%
rename(date=activity_date)%>%
  remove_empty(which = c('rows'))%>%
  remove_empty(which = c('cols'))
str(cleaned_daily_activity$date)

cleaned_daily_activity$date <- as.Date(cleaned_daily_activity$date,format = '%m/%d/%Y')

cleaned_sleep_day<- clean_names(sleep_day)%>%
  rename(date=sleep_day)%>%
  remove_empty(which = c('rows'))%>%
  remove_empty(which = c('cols'))
cleaned_sleep_day$date<- as.Date(cleaned_sleep_day$date,format = "%m/%d/%Y")

cleaned_hourly_steps<- clean_names(hourly_steps)%>%
  remove_empty(which = c('rows'))%>%
  remove_empty(which = c('cols'))

cleaned_hourly_calories<- clean_names(hourly_calories)%>%
  remove_empty(which = c('rows'))%>%
  remove_empty(which = c('cols'))

## now calculate summary statistics of the dataset used

# cleaned_daily_activity
cleaned_daily_activity%>%
  select(total_steps,total_distance,sedentary_minutes)%>%summary()

# The statistics tells that total steps take by a person on average is 7638 with average distance covered is 5.490 and average time spent on sedentary activities is 991.2

#cleaned_sleep_day
cleaned_sleep_day%>%
  select(total_sleep_records,total_minutes_asleep,total_time_in_bed)%>%
  summary()

# this shows that maximum time is spent sleeping when on bed

# cleaned_hourly_steps
cleaned_hourly_steps%>%
  select(step_total)%>%
  summary()

# It shows maximum steps taken are 10554

cleaned_hourly_calories%>%
  select(calories)%>%
  summary()

# maximum calories burn is 948

## ANALYZE AND VISUALIze

# relationship between steps taken and sedentary time 

library(ggplot2)

ggplot(data = cleaned_daily_activity,mapping = aes(x=sedentary_minutes,y=total_steps))+
  geom_point()+labs(title = "total sedentary time and steps count daily")

# From the above graph, it is not clear if time you are active is related with sedentary minutes because there are two clusters of points at same steps but one cluster has more sedentary time.Thus, we will consider hourly steps rather than total steps in a day.

# relationship between asleep and time in bed

ggplot(data = cleaned_sleep_day,mapping = aes(x=total_minutes_asleep,y=total_time_in_bed))+geom_point() + labs(title = 'total time asleep and total time in bed')

## From the graph ,it is clearly that total_minutes_asleep and total_time in bed are positively correlated.
## Now, we merge hourly steps and hourly calories data and calculate step count on hourly basis rather than daily.

combined_steps_calories<-merge(cleaned_hourly_steps,cleaned_hourly_calories,by=c('id','activity_hour'))
glimpse(combined_steps_calories)


combined_steps_calories%>%
  group_by(id)%>%
  summarise(count_records=n())%>%
  arrange(count_records)

## find relationship between sleep and activity by merging daily_activity and sleep_day datasets

sleep_combined_daily_activity <- merge(cleaned_daily_activity,cleaned_sleep_day,by = c('id','date'))

# adding new variable "awake time in bed"

sleep_combined_daily_activity <- mutate(sleep_combined_daily_activity,awake_time_in_bed=total_time_in_bed - total_minutes_asleep)

n_distinct(sleep_combined_daily_activity$id)

## find relationship between hourly calories and hourly step total

ggplot(data = combined_steps_calories,mapping = aes(x=calories,y=step_total))+
  geom_jitter()+labs(title = 'Hourly step count and calories burn for each user')+
  facet_wrap(~id)+geom_smooth(formula = y~x,method = 'lm')

cor(sleep_combined_daily_activity$total_minutes_asleep,sleep_combined_daily_activity$sedentary_minutes)










sleep_combined_daily_activity <- mutate(sleep_combined_daily_activity,day=wday(date,label = TRUE))
summarized_activity_sleep <-sleep_combined_daily_activity%>%
  group_by(day)%>%
  summarise(average_daily_steps=mean(total_steps),
            average_asleep_minutes=mean(total_minutes_asleep),
            average_awake_time_in_bed=mean(awake_time_in_bed),
            lightly_active_minutes=mean(lightly_active_minutes),
            fairly_active_minutes=mean(fairly_active_minutes),
            very_active_minutes=mean(very_active_minutes),
            average_calories=mean(calories))

head(summarized_activity_sleep)




ggplot(data = summarized_activity_sleep,mapping = aes(x=day,y=average_daily_steps))+
  geom_col(fill='blue')+labs(title = 'Daily step count')
