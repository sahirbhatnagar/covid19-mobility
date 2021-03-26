# This script reads in the "metrics_2020_summary_and_baseline.csv" file 
# produced from the baselines_2020.R script, which calculates the baselines 
# from the aggregated mobility data summary produced by Safegraph.R
# This script then calculates the change from baseline of each mobility 
# metric and returns the aggregated social distancing data 

setwd("/scratch/bhatnagar-lab/sbhatnagar/mcgill/research/COVID19")

# Packages -----
library(dplyr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(magrittr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(tidyverse, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(stringr, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(data.table, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(here, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")

# Reading in the file with the mobility data and baselines ----
# Read in the file containing the aggregated mobility summary data along with 
# the baseline values calculated for each mobility metric. 
# This file also has the FIPS, date, State, day of the week, month and 
# shifted mobility metrics
# Assuming that the file is in the same working directory as this script 
summary_and_baseline_2020 <- read.csv("metrics_2020_summary_and_baseline.csv")

# Organizing the data 1: extracting the shifted metrics ----
# Reminder that the mobility metrics have been shifted back by 1 
# day so that they will align correctly with the baseline values for when 
# the change will be calculated. Because we want the average of the 7 previous 
# days to be the baseline for the 8th

# Shifted mobility metric: number of devices that leave full-time
full_time_work_behavior_devices <- 
  summary_and_baseline_2020$full_time_work_behavior_devices_lead

# Shifted mobility metric: number of devices that leave part time
part_time_work_behavior_devices <-
  summary_and_baseline_2020$part_time_work_behavior_devices_lead

# Shifted mobility metric: devices not at home
not_at_home_device_count <- 
  summary_and_baseline_2020$not_at_home_device_count_lead

# Shifted mobility metric: stops (<20min for >3 locations) in delivery behavior
delivery_behavior_devices <-
  summary_and_baseline_2020$delivery_behavior_devices_lead

# Shifted mobility metric: how far devices go (meters)
median_distance_traveled_from_home <- 
  summary_and_baseline_2020$median_distance_traveled_from_home_lead

# Shifted mobility metric: how long devices are away from home (min)
median_non_home_dwell_time <- 
  summary_and_baseline_2020$median_non_home_dwell_time_lead


# Organizing the data 2: extracting the baselines ----
# Reminder that the baselines were calculated using a 7 day rolling average 
# of the previous 7 days for each mobility metric 

# 7 day rolling average baseline: number of devices that leave full-time
full_time_work_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  full_time_work_behavior_devices_baseline

# 7 day rolling average baseline: number of devices that leave part time
part_time_work_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  part_time_work_behavior_devices_baseline

# 7 day rolling average baseline: devices not at home
not_at_home_device_count_2020_baseline <- summary_and_baseline_2020$
  not_at_home_device_count_baseline

# 7 day rolling average baseline: stops (<20min for >3 locations) in delivery behavior
delivery_behavior_devices_2020_baseline <- summary_and_baseline_2020$
  delivery_behavior_devices_baseline

# 7 day rolling average baseline: how far devices go (meters)
median_distance_traveled_from_home_2020_baseline <- summary_and_baseline_2020$
  median_distance_traveled_from_home_baseline

# 7 day rolling average baseline: how long devices are away from home (min)
median_non_home_dwell_time_2020_baseline <- summary_and_baseline_2020$
  median_non_home_dwell_time_baseline


# Organizing the data 3: other variables of interest ----
# Making sure the FIPS are in the proper 6 digit format
fips <- str_pad(summary_and_baseline_2020$fips, width = 5, 
                side = "left", pad = "0")

state <- summary_and_baseline_2020$state

date <- summary_and_baseline_2020$date

day_of_the_week <- summary_and_baseline_2020$day_of_the_week

month <- summary_and_baseline_2020$month

# Combine all of the data together ----
# Make a new data frame for the mobility metrics which have been shifted 
# back one day, the baselines (7 day rolling averages) and the other variables 
# of interest
shifted_metrics_baselines_2020 <- data.frame(fips, state,
                                             full_time_work_behavior_devices,
                                             part_time_work_behavior_devices,
                                             not_at_home_device_count,
                                             delivery_behavior_devices,
                                             median_distance_traveled_from_home,
                                             median_non_home_dwell_time,
                                             full_time_work_behavior_devices_2020_baseline,
                                             part_time_work_behavior_devices_2020_baseline,
                                             not_at_home_device_count_2020_baseline,
                                             delivery_behavior_devices_2020_baseline,
                                             median_distance_traveled_from_home_2020_baseline,
                                             median_non_home_dwell_time_2020_baseline,
                                             day_of_the_week, date, month)


# Calculating the change from baseline for the mobility metrics  ----
# A baseline calculation is done by 
DT_change <- shifted_metrics_baselines_2020 %>%
  group_by(fips, date) %>%
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


# Imputing ----
# Impute the NA's/NaN's/INF's to 0 
# These values arise from divisions by 0 
is.na(DT_change) <- sapply(DT_change, is.infinite)
DT_change[is.na(DT_change)] <- 0

# Writing the data frame with the change from baseline ----
# The resulting data frame will have the mobility metrics (shifted),
# the FIPS, date, State, day of the week and month and the baselines. Along 
# with a new column with the change from baseline value for each mobility metric
data.table::fwrite(DT_change, file = "2020_change_2020_rolling_baseline.csv")

