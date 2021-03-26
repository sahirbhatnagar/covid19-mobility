# This script reads in the aggregated mobility summary data produced from the 
# Safegraph.R script and calculates the baseline values.
# The Safegraph data until Aug 27th 2020 is needed 
# since the metrics be pushed back by 1 and thus Aug 26th will be the 
# last day with data 
# A 7-day rolling average is calculated and used as a baseline. 
# Then the days are shifted back by 1 so that the rolling average of 
# the 7 previous days will be a baseline for the 8th. 

# Packages ----
library(dplyr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(magrittr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(tidyverse, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(stringr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(data.table, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(here, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")

# Reading in the mobility data ----
# Read in the aggregated mobility summary data produced from the 
# Safegraph.R script, which takes the daily Safegraph data and 
# aggregates it. 
# Assuming that the aggregated data file is in the same working directory
# as this script 
summary_2020 <- read.csv("data/aggregated_2020-12-08_2020.csv")

# Mobility metrics ----
# Extracting all of the mobility metrics of interest from the aggregated 
# mobility data summary 

# Number of devices that leave full-time
full_time_work_behavior_devices  <- 
  summary_2020$SD_fullTimeWorkBehaviorDevice_share

# Number of devices that leave part time
part_time_work_behavior_devices  <-
  summary_2020$SD_partTimeWorkBehaviorDevice_share

# Devices not at home 
not_at_home_device_count <- summary_2020$SD_device_not_home

# Stops (<20min for >3 locations) in delivery behavior
delivery_behavior_devices  <-
  summary_2020$SD_deliveryBehaviorDevice_share

# How far devices go (meters)
median_distance_traveled_from_home <- 
  summary_2020$SD_distTraveledFromHome_median

# How long devices are away from home (min)
median_non_home_dwell_time  <- summary_2020$SD_dwellNotHome_median

# Other variables of interest ----
# Also, extracting the FIPS, state and date variables
# and making sure the FIPS are in the proper 6 digit format 
fips <- str_pad(summary_2020$fips, width = 5, side = "left", pad = "0")
state <-summary_2020$state
date <- summary_2020$date

# Creating a new column corresponding to the day of the week know which day 
# of the week it is and what month a certain date is in 
day_of_the_week <- weekdays(as.Date(summary_2020$date))
month <- month(summary_2020$date)

# Combining everything together ----
# Creating a new data frame to combine the mobility metrics extracted, 
# the FIPS, date, State, day of the week and month  
metrics_2020 <- data.frame(fips, state, full_time_work_behavior_devices,
                           part_time_work_behavior_devices,
                           not_at_home_device_count, delivery_behavior_devices,
                           median_distance_traveled_from_home, median_non_home_dwell_time, 
                           date, day_of_the_week, month)

# Calculating the mobility metric baselines ----
# Calculate the baseline measure which is a 7 day rolling average using the
# previous 7 days of each mobility metric. 
# Also make new columns where the mobility metrics will be shifted back by 1 
# day so that the mobility metrics will align  correctly with the baseline 
# values for when the change will be calculated. Because we want the average 
# of the 7 previous days to be the baseline for the 8th. 

metrics_2020_baseline <- metrics_2020 %>%
  group_by(fips) %>%
  mutate(full_time_work_behavior_devices_baseline = 
           frollmean(full_time_work_behavior_devices, n = 7, align = "right"),
         full_time_work_behavior_devices_lead = 
           lead(full_time_work_behavior_devices,n = 1),
         
         part_time_work_behavior_devices_baseline = 
           frollmean(part_time_work_behavior_devices, n = 7, align = "right"),
         part_time_work_behavior_devices_lead = 
           lead(part_time_work_behavior_devices,n = 1),
         
         
         not_at_home_device_count_baseline = 
           frollmean(not_at_home_device_count, n = 7, align = "right"),
         not_at_home_device_count_lead = 
           lead(not_at_home_device_count,n = 1),
         
         delivery_behavior_devices_baseline = 
           frollmean(delivery_behavior_devices, n = 7, align = "right"),
         delivery_behavior_devices_lead = 
           lead(delivery_behavior_devices,n = 1),
         
         median_distance_traveled_from_home_baseline = 
           frollmean(median_distance_traveled_from_home, n = 7, align = "right"),
         median_distance_traveled_from_home_lead = 
           lead(median_distance_traveled_from_home,n = 1),
         
         median_non_home_dwell_time_baseline = 
           frollmean(median_non_home_dwell_time, n = 7, align = "right"),
         median_non_home_dwell_time_lead = 
           lead(median_non_home_dwell_time,n = 1)) 

# Writing the data frame with the baselines ----
# The resulting data frame will have the mobility metrics,
# the FIPS, date, State, day of the week and month. Along with new columns 
# for the the baselines calculated from the mobility metrics 
# (using a 7 day rolling average) and the shifted mobility metrics
data.table::fwrite(metrics_2020_baseline, file = "metrics_2020_summary_and_rolling_baseline.csv")

