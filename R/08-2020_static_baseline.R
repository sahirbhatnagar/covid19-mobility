# This script reads the aggregated mobility summary data and returns the
# baselines
library(dplyr)
library(magrittr)
library(tidyverse)
library(stringr)
library(data.table)

# Read in the CSV file containing the 2020 data
summary_2020 <- read.csv("data/aggregated_2020-12-08_2020.csv")

# Step 1: All the (#) should be divided by the device_count so that they are %
# Extract all variables of interest 

# Number of devices that leave full-time? (#)
full_time_work_behavior_devices  <- 
  summary_2020$SD_fullTimeWorkBehaviorDevice_share

# Number of devices that leave part time? (#)
part_time_work_behavior_devices  <-
  summary_2020$SD_partTimeWorkBehaviorDevice_share

# Devices not at home (#)
# % device not at home = (device_count - completely_home_device)/device_count
not_at_home_device_count <- summary_2020$SD_device_not_home

# Lots of stops (<20min for >3 locations) in delivery behaviour? (#)
delivery_behavior_devices  <-
  summary_2020$SD_deliveryBehaviorDevice_share

# How far devices go (meters)? 
median_distance_traveled_from_home <- 
  summary_2020$SD_distTraveledFromHome_median

# How long devices are away from home (min)?
median_non_home_dwell_time  <- summary_2020$SD_dwellNotHome_median

# Extracting the fips, state and date variables 
fips <- str_pad(summary_2020$fips, width = 5, side = "left", pad = "0")
state <-summary_2020$state
date <- summary_2020$date

# Creating new columns to know which day of the week and what month a certain date is
day_of_the_week <- weekdays(as.Date(summary_2020$date))
month <- month(summary_2020$date)

# Creating a new data frame with the variables of interest, the date, FIPS and states
metrics_2020 <- data.frame(fips, state,full_time_work_behavior_devices ,
                           part_time_work_behavior_devices ,
                           not_at_home_device_count, delivery_behavior_devices ,
                           median_distance_traveled_from_home,median_non_home_dwell_time , date, 
                           day_of_the_week, month)

# 01-01-08 to 01-01-31 baselines
DT_jan_8_31 <- metrics_2020[metrics_2020$date >= "2020-01-08" 
                            & metrics_2020$date <= "2020-01-31",]

# Step 2: We need a “baseline” measure by the day of the week I.e. for a baseline 
# measure of June 2020 take the mean of each variable for Mondays, Tuesdays etc,
# From June 2020

metrics_2020_baseline <- DT_jan_8_31 %>%
  group_by(fips, month, day_of_the_week) %>%
  summarize(full_time_work_behavior_devices_baseline = 
              mean(full_time_work_behavior_devices ),
            
            part_time_work_behavior_devices_baseline =
              mean(part_time_work_behavior_devices ),
            
            not_at_home_device_count_baseline = 
              mean(not_at_home_device_count),
            
            delivery_behavior_devices_baseline = 
              mean(delivery_behavior_devices),
            
            median_distance_traveled_from_home_baseline = 
              mean(median_distance_traveled_from_home),
            
            median_non_home_dwell_time_baseline = 
              mean(median_non_home_dwell_time)) 

write.csv(metrics_2020_baseline, "metrics_2020_summary_and_static_baseline.csv")

