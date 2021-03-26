# This script reads the baseline data and returns the change
library(dplyr)
library(magrittr)
library(tidyverse)
library(stringr)
library(data.table)

# Read in the CSV file containing the 2020 summary and baseline values
summary_and_baseline_2020 <- read.csv("metrics_2020_summary_and_static_baseline.csv")

# Read in the CSV file containing the 2020 summary data 
summary_2020 <- read.csv("data/aggregated_2020-12-08_2020.csv")

# Step 1: # Extract all variables of interest from the 2020 summary ----
# All the (#) should be divided by the device_count so that they are %

# Number of devices that leave full-time? (#)
full_time_work_behavior_devices <- 
  summary_2020$SD_fullTimeWorkBehaviorDevice_share

# Number of devices that leave part time? (#)
part_time_work_behavior_devices <-
  summary_2020$SD_partTimeWorkBehaviorDevice_share

# Devices not at home (#)
# % device not at home = (device_count - completely_home_device)/device_count
not_at_home_device_count <- summary_2020$SD_device_not_home

# Lots of stops (<20min for >3 locations) in delivery behaviour? (#)
delivery_behavior_devices <-
  summary_2020$SD_deliveryBehaviorDevice_share

# How far devices go (meters)? 
median_distance_traveled_from_home <- 
  summary_2020$SD_distTraveledFromHome_median

# How long deivces are away from home (min)?
median_non_home_dwell_time <- summary_2020$SD_dwellNotHome_median

# Extracting the fips, state and date variables 
fips <- str_pad(summary_2020$fips, width = 5, side = "left", pad = "0")
state <-summary_2020$state
date <- summary_2020$date

# Creating new columns to know which day of the week and what month a certain date is
day_of_the_week <- weekdays(as.Date(summary_2020$date))
month <- month(summary_2020$date)

# Step 2: Create a new data table for 2020 ----
# Creating a new data frame with the variables of interest, the date, FIPS and states
metrics_2020 <- data.frame(fips, state, full_time_work_behavior_devices,
                           part_time_work_behavior_devices,
                           not_at_home_device_count,
                           delivery_behavior_devices,
                           median_distance_traveled_from_home,
                           median_non_home_dwell_time, date, day_of_the_week,month)

# Step 3: Extract all the variables of interest from the 2020 baseline and summary ----
fips <- str_pad(summary_and_baseline_2020$fips, width = 5, 
                side = "left", pad = "0")

full_time_work_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  full_time_work_behavior_devices_baseline

part_time_work_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  part_time_work_behavior_devices_baseline

not_at_home_device_count_2020_baseline <- summary_and_baseline_2020$
  not_at_home_device_count_baseline

delivery_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  delivery_behavior_devices_baseline

median_distance_traveled_from_home_2020_baseline <- summary_and_baseline_2020$
  median_distance_traveled_from_home_baseline

median_non_home_dwell_time_2020_baseline <- summary_and_baseline_2020$
  median_non_home_dwell_time_baseline

day_of_the_week <- summary_and_baseline_2020$day_of_the_week

month <- summary_and_baseline_2020$month


# Step 4: Make a new data frame for the 2020 baselines and days of the week/month ----

baselines_2020 <- data.frame(fips,full_time_work_behavior_devices_2020_baseline,
                             part_time_work_behavior_devices_2020_baseline,
                             not_at_home_device_count_2020_baseline,
                             delivery_behavior_devices_2020_baseline,
                             median_distance_traveled_from_home_2020_baseline,
                             median_non_home_dwell_time_2020_baseline,
                             day_of_the_week, month)


# Step 5:Join the baselines from 2020 to the metrics from 2020 ----

baselines_metrics_2020 <- left_join(metrics_2020, baselines_2020, 
                                         by = c("fips", "day_of_the_week"))

DT_change <- baselines_metrics_2020 %>%
  group_by(fips, day_of_the_week) %>%
  mutate(full_time_work_behavior_devices_change = 
           ((full_time_work_behavior_devices - 
               full_time_work_behavior_devices_2020_baseline) /
              full_time_work_behavior_devices_2020_baseline) * 100,
         
         part_time_work_behavior_devices_change =
           ((part_time_work_behavior_devices - 
               part_time_work_behavior_devices_2020_baseline) /
              part_time_work_behavior_devices_2020_baseline) * 100,
         
         not_at_home_device_count_change = 
           ((not_at_home_device_count - 
               not_at_home_device_count_2020_baseline) /
              not_at_home_device_count_2020_baseline) * 100,
         
         delivery_behavior_devices_change = 
           ((delivery_behavior_devices - 
               delivery_behavior_devices_2020_baseline) /
              delivery_behavior_devices_2020_baseline) * 100,
         
         median_distance_traveled_from_home_change = 
           ((median_distance_traveled_from_home - 
               median_distance_traveled_from_home_2020_baseline) /
              median_distance_traveled_from_home_2020_baseline) * 100,
         
         median_non_home_dwell_time_change = 
           ((median_non_home_dwell_time - 
               median_non_home_dwell_time_2020_baseline) /
              median_non_home_dwell_time_2020_baseline) * 100) 

write.csv(DT_change, "2020_change_2020_static_baselines.csv")
