


figure1_change_from_baseline_fun <- function(DT_change, input.params){
  
  #DT_change should be the safegraph data in targets
  # DT_change <- read.csv(data_path)  
  
  # browser()
  
  DT_change$date <- lubridate::ymd(DT_change$date)
  
  DT_change$state %>% table
  
  input.params <- input.params %>% 
    filter(pc_metric_type == "cluster1") %>% 
    dplyr::select(state_name, state_code, close, open) 
  
  DT_change <- DT_change %>% 
    left_join(input.params, by = c("state" = "state_code")) %>% 
    dplyr::filter(!is.na(state_name)) %>% 
    dplyr::select(-state) %>% 
    dplyr::rename(state = state_name) %>% 
    dplyr::select(fips, state, everything()) %>% 
    mutate(state = factor(state, levels = input.params$state_name))
  
  
  # # Plot one names 
  # DT_change$state[(DT_change$state) == "OH"] <- "Ohio"
  # DT_change$state[(DT_change$state) == "MO"] <- "Missouri"
  # DT_change$state[(DT_change$state) == "SC"] <- "South Carolina"
  # DT_change$state[(DT_change$state) == "IN"] <- "Indiana"
  
  # Plot one order
  # DT_change$state = factor(DT_change$state, 
  #                          levels=c("Ohio","Indiana", 
  #                                   "Missouri","South Carolina"))
  
  # States of interest for plot one 
  focus_cn <- input.params$state_name
  
  #pdf(file = "Figure1.pdf", width = 6, height = 4)
  
  dt_first <- DT_change %>% 
    group_by(state) %>% 
    dplyr::slice_head(n = 1)
  
  # Plot one ----
  p1 <- ggplot(data = filter(DT_change, state %in% focus_cn)) + 
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (full_time_work_behavior_devices_change/100), 
                    color = "Full-time work behavior"), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (part_time_work_behavior_devices_change/100), 
                    color = "Part-time work behavior"),
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (not_at_home_device_count_change/100), 
                    color = "Devices leaving home"), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (delivery_behavior_devices_change/100), 
                    color = "Delivery behaviours"), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (median_distance_traveled_from_home_change/100), 
                    color = "Distance traveled from home"), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_smooth(data = filter(DT_change, state %in% focus_cn),
                aes(x= date, y = (median_non_home_dwell_time_change/100), 
                    color = "Time spent away from home"), 
                method = "locfit", size = 0.5, se = F) + 
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    geom_vline(data = dt_first, aes(xintercept = close),
               linetype = "solid", size = 0.2) +
    
    geom_vline(data = dt_first, aes(xintercept = open),
               linetype = "dashed", size = 0.2) +
    
    # geom_vline(data = subset(DT_change, state == "South Carolina"), aes(xintercept = as.Date("2020-04-07")), 
    #            linetype = "solid", size = 0.2) + 
    # geom_vline(data = subset(DT_change, state == "South Carolina"), aes(xintercept = as.Date("2020-04-20")), 
    #            linetype = "dashed", size = 0.2) + 
    # 
    # geom_vline(data = subset(DT_change, state == "Missouri"), aes(xintercept = as.Date("2020-04-06")), 
    #            linetype = "solid", size = 0.2) + 
    # geom_vline(data = subset(DT_change, state == "Missouri"), aes(xintercept = as.Date("2020-05-04")), 
    #            linetype = "dashed", size = 0.2) + 
    # 
    # geom_vline(data = subset(DT_change, state == "Ohio"), aes(xintercept = as.Date("2020-03-23")), 
    #            linetype = "solid", size = 0.2) + 
    # geom_vline(data = subset(DT_change, state == "Ohio"), aes(xintercept = as.Date("2020-05-12")), 
    #            linetype = "dashed", size = 0.2) + 
    # 
    # geom_vline(data = subset(DT_change, state == "Indiana"), aes(xintercept = as.Date("2020-03-25")), 
    #            linetype = "solid", size = 0.2) + 
    # geom_vline(data = subset(DT_change, state == "Indiana"), aes(xintercept = as.Date("2020-05-04")), 
    #            linetype = "dashed", size = 0.2) + 
    
    
    facet_wrap(~state, ncol = 2, nrow = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    scale_y_continuous(labels = scales::percent_format()) +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "% Change",
         color = "Mobility metrics") + 
    # title = "Single Mobility Metrics Change from 7 Day Rolling Average Baseline as a Function of Time",
    # subtitle = paste("7 Day Rolling Average as a Baseline",
    #                  "\nData as of", max(DT_change$date)),
    # caption = "Peter Her / Data: https://docs.safegraph.com/docs/social-distancing-metrics") +
    
    theme(
      legend.position="bottom",
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      plot.title = element_text(size = 4),
      plot.subtitle = element_text(size = 3),
      plot.caption = element_text(size = 3),
      axis.title.y = element_text(size = 6),
      axis.text = element_text(size = 6),
      axis.title.x = element_text(size = 6),
      strip.text = element_text(size = 6),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2)),
           fill = guide_legend(nrow=3, byrow=TRUE)) +
    
    scale_color_discrete_qualitative(palette = "Dynamic")
  
  cowplot::save_plot("figures/figure1_change_from_baseline.pdf", plot = p1)
  
  #dev.off()
  
  return("figures/figure1_change_from_baseline.pdf")
  
}
