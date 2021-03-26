#########################################################################
# SafeGraph Social Distancing Data
# Created by: Sahir Bhatnagar (McGill U)
# Date: May 10, 2020
# Description: This script reads in the daily social distancing data,
# summarizes it by county, and combines it into one data.table
# Notes:
# This script assumes you have downloaded the census fips codes and
# the social distancing data from safegraph
# There is an option to run the script in parallel using the
# doParallel package
###########################################################################

#### in terminal
#### first install awscli:
# pip install awscli
#### then set access key:
# aws configure --profile safegraph


# pacman::p_load(aws.s3)
# Sys.setenv(
#   "AWS_ACCESS_KEY_ID"     = "AKIAWWZ7POZOGSC3KT64",
#   "AWS_SECRET_ACCESS_KEY" = "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",
#   "AWS_DEFAULT_REGION"    = "us-west-2")
# aws.s3::bucket_list_df()
# data <- s3read_using(read.csv,
#                      object = "s3://sg-c19-response/social-distancing/v2/main-file/2020-04-27-social-distancing.csv.gz")
#
# pacman::p_load(data.table)
# Access Key: AKIAWWZ7POZOPTKH2WSQ
# Secret Access Key: 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
# Default region name: us-west-2
# You can leave Default output format blank
# cbg <- fread(here::here("data/safegraph_open_census_data/metadata/cbg_geographic_data.csv"),
#              keepLeadingZeros = TRUE)
# cbg <- fread(here::here("data/safegraph_open_census_data/metadata/cbg_field_descriptions.csv"),
#              keepLeadingZeros = TRUE)
# head(cbg)
# dim(cbg)
# dat$origin_census_block_group[1:5]


col_names <- c(
  "origin_census_block_group",
  "date_range_start",
  "date_range_end",
  "device_count",
  "distance_traveled_from_home",
  # "bucketed_distance_traveled",
  # "median_dwell_at_bucketed_distance_traveled",
  "completely_home_device_count",
  "median_home_dwell_time",
  # "bucketed_home_dwell_time",
  # "at_home_by_each_hour",
  "part_time_work_behavior_devices",
  "full_time_work_behavior_devices",
  # "destination_cbgs",
  "delivery_behavior_devices",
  "median_non_home_dwell_time",
  "candidate_device_count",
  # "bucketed_away_from_home_time",
  # "bucketed_percentage_time_home"
  "median_percentage_time_home"
)
#
# rm_col_names <- c(
#   "bucketed_distance_traveled",
#   "median_dwell_at_bucketed_distance_traveled",
#   "bucketed_home_dwell_time",
#   "at_home_by_each_hour",
#   "destination_cbgs",
#   "bucketed_away_from_home_time",
#   "bucketed_percentage_time_home"
# )

# load packages -----------------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")
library(data.table, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(tidyverse, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(here, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
library(doParallel, lib.loc = "/data/bhatnagar-lab/Rlibs/R4.0.2/")
# lib.loc needs to be changed 
library(R.utils, lib.loc = "/home/bhatnagar-lab/pher/R/x86_64-pc-linux-gnu-library/4.0")
doParallel::registerDoParallel(cores = 40)


# file path ---------------------------------------------------------------

# adjust accordingly. here I assume the data is in a folder called data
# which is located in your R working directory
# Specifying /data in /scratch/bhatnagar-lab/sbhatnagar/mcgill/research/COVID19 
datain <- here::here("data")


# other inputs ------------------------------------------------------------

startM <- 1 # which month start
endM <- 12 # which month end
year <- 2020
m <- 1
d <- 1

# get census block groups -------------------------------------------------

# download file from https://docs.safegraph.com/docs/open-census-data
# Specifying /data in /scratch/bhatnagar-lab/sbhatnagar/mcgill/research/COVID19 
cbg <- fread(here::here("data/safegraph_open_census_data/metadata/cbg_fips_codes.csv"),
             keepLeadingZeros = TRUE)

# check that all state fips are 2 and county fips are 3 characters
nchar(cbg$state_fips) %>% table
nchar(cbg$county_fips) %>% table

# update some codes
# source: https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
cbg[state_fips == "02" & county_fips == "270", `:=`(county_fips = 158,
                                                    county = "Kusilvak Census Area")]
cbg[state_fips == "46" & county_fips == "113", `:=`(county_fips = 102,
                                                    county = "Oglala Lakota County")]

cbg[, fips := stringr::str_c(cbg$state_fips, cbg$county_fips)]

# check all fips are 5 characters long
cbg[,table(nchar(fips))]

# for faster data.table joins
setkey(cbg, "fips")



# read SD data, merge with cbg, summarise by county -----------------------

# this return a list of length equal to the number of months
result <- lapply(startM:endM, function(m){

  # get all days of the month for which theres data
  # If the month is a single digit
  if (m <= 9) {
    f <- list.files(file.path(datain,year,paste("0", m, sep = ""))) 
  # For double digit months 
  } else {
    f <- list.files(file.path(datain,year,paste(m, sep = ""))) 
  }
  
  daily_combined <- foreach(d = f) %dopar% {
    
    # daily Social distancing data
    if (m <= 9) {
      dat <- fread(file.path(datain,year, paste("0", m, "/", d, "/",year,"-0",m,"-",
                                                d,"-social-distancing.csv.gz",sep="")),
                   select = col_names,
                   keepLeadingZeros = TRUE)
    } else {
      dat <- fread(file.path(datain,year, paste(m, "/", d, "/",year,"-",m,"-",
                                                d,"-social-distancing.csv.gz",sep="")),
                   select = col_names,
                   keepLeadingZeros = TRUE)
    }

    # check that all CBG are 12 characters
    # dat[, table(nchar(origin_census_block_group))]

    dat[, fips:=stringr::str_sub(origin_census_block_group,1,5)]
    setkey(dat,"fips")

    DT <- merge(x = dat, y = cbg, by = "fips")

    # the fips which are in the SD data but not in cbg_fips_codes.csv
    # dat[which(!(dat$fips %in% cbg$fips)),c("origin_census_block_group","fips")]

    # the fips which are in the cbg_fips_codes.csv but not in SD data
    # cbg[which(!(cbg$fips %in% dat$fips)),]

    # see paper by http://www-personal.umich.edu/~yingfan/Fan_Orhun_Turjeman.pdf
    DT[, `:=`(adjusted_completely_home_device_count = completely_home_device_count + pmax(0, candidate_device_count - device_count))]
    DT[, `:=`(SD_device_not_home = (device_count - completely_home_device_count)/device_count,
              SD_deviceCount_sum = sum(device_count,na.rm=T),
              SD_candidateDeviceCount_sum = sum(candidate_device_count,na.rm=T),
              SD_distTraveledFromHome_median = median(distance_traveled_from_home,na.rm=T),
              SD_completelyHomeDevice_sum = sum(completely_home_device_count,na.rm=T),
              SD_adjustedCompletelyHomeDevice_sum = sum(adjusted_completely_home_device_count, na.rm = T),
              SD_dwellHome_median = median(median_home_dwell_time,na.rm=T),
              SD_dwellNotHome_median = median(median_non_home_dwell_time, na.rm = T),
              SD_partTimeWorkBehaviorDevice_sum = sum(part_time_work_behavior_devices,na.rm=T),
              SD_fullTimeWorkBehaviorDevice_sum = sum(full_time_work_behavior_devices,na.rm=T),
              SD_deliveryBehaviorDevice_sum = sum(delivery_behavior_devices, na.rm = T),
              SD_percentageTimeHome_median =  median(median_percentage_time_home,na.rm=T)), by = fips]

    DT[, `:=`(SD_partTimeWorkBehaviorDevice_share = SD_partTimeWorkBehaviorDevice_sum / SD_deviceCount_sum,
              SD_fullTimeWorkBehaviorDevice_share = SD_fullTimeWorkBehaviorDevice_sum / SD_deviceCount_sum,
              SD_completelyHomeDevice_share = SD_completelyHomeDevice_sum / SD_deviceCount_sum,
              SD_deliveryBehaviorDevice_share = SD_deliveryBehaviorDevice_sum / SD_deviceCount_sum)]

    # take first row only of each fips
    DTf <- DT[, .SD[1], by = fips, .SDcols = c("state",grep("SD", colnames(DT), value = T))]

    DTf[, date := lubridate::as_date(DT$date_range_start[1])]
  }

  rbindlist(daily_combined)

})


# this combines all the months together
DT_final <- rbindlist(result)
DT_final
data.table::fwrite(DT_final, file = here::here(sprintf("aggregated_%s_%s.csv",max(DT_final$date),year)))



