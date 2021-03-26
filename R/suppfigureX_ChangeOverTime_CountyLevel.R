library(locfit)
library(colorspace)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(cowplot)

# Adding county names to the data set ----
# download file from https://docs.safegraph.com/docs/open-census-data
cbg <- fread(here::here("cbg_fips_codes.csv"),
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

fips <- cbg$fips

new_cbg <- data.frame(cbg$county, fips)

DT_merged <- left_join(DT_change, new_cbg, by = "fips")

# Date 
DT_merged$date <- lubridate::ymd(DT_merged$date)

# plot ----

pdf(file = "Mo - Overlaid Mobility Metrics Median - 2020 Baseline", 
    width = 120, height = 200)

focus_cn <- "MO"

ggplot() + 
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (full_time_work_behavior_devices_change/100), 
                  color = "Change in Full Time Work Behaviour Device"), 
              method = "locfit", size = 2, se = F) +
  
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (part_time_work_behavior_devices_change/100), 
                  color = "Change in Part Time Work Behaviour Device"),
              method = "locfit", size = 2, se = F) +
  
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (not_at_home_device_count_change/100), 
                  color = "Change in Devices not at Home"), 
              method = "locfit", size = 2, se = F) +
  
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (delivery_behavior_devices_change/100), 
                  color = "Change in Delivery Behaviour"), 
              method = "locfit", size = 2, se = F) +
  
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (median_distance_traveled_from_home_change/100), 
                  color = "Change in the Median Distance Travelled from Home"), 
              method = "locfit", size = 2, se = F) +
  
  
  geom_smooth(data = filter(DT_merged, state %in% focus_cn),
              aes(x= date, y = (median_non_home_dwell_time_change/100), 
                  color = "Change in the Median Non Dwell Time at Home"), 
              method = "locfit", size = 2, se = F) +
  
  geom_hline(yintercept = 0) + 
  
  facet_wrap(~cbg.county, ncol = 5 ,scales = "free") +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  scale_y_continuous(labels = scales::percent_format()) + 
  
  theme_cowplot() +
  
  labs(x = "Date",
       y = "Mobility Metrics",
       color = "Mobility Metrics",
       title = "Overlaid Mobility Metrics - MO",
       subtitle = paste("Change from 7 Day Rolling Average as a Baseline",
                        "\nData as of", max(DT_merged$date)),
       caption = "Peter Her / Data: https://docs.safegraph.com/docs/social-distancing-metrics") +
  
  theme(
    legend.position = 'top',
    legend.title = element_text(size = 55),
    legend.text = element_text(size = 50),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 45),
    plot.caption = element_text(size = 35),
    axis.title.y = element_text(size = 65),
    axis.title.x = element_text(size = 65),
    axis.text = element_text(size = 40), 
    strip.text = element_text(size = 45),
    legend.key.size = unit(10, "cm"), 
    axis.line = element_line(colour = 'black', size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
    panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
  ) +
  
  guides(colour = guide_legend(override.aes = list(size=10))) +
  
  scale_color_discrete_qualitative(palette = "Dynamic")

dev.off()
