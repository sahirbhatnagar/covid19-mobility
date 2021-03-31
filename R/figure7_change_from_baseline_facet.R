
# DT <- read_csv("data/aggregated_2020-12-08_2020.csv")
# 
# DT %>% 
#   filter(state %in% c("SC","MO","OH","IN"))
# 
# 
# library(targets)
# source("packages.R")
# source("R/figure1_change_from_baseline.R")
# tar_load(nyt)
# tar_load(SES)
# tar_load(pop_density)
# tar_load(cbg)
# tar_load(safegraph)
# source("R/fpca_pipeline.R")
# conflicted::conflict_prefer("filter", "dplyr")
# conflicted::conflict_prefer("lag", "dplyr")
# conflicted::conflict_prefer("Position", "ggplot2")
# conflicted::conflict_prefer("rbind", "BiocGenerics")
# conflicted::conflict_prefer("cbind", "BiocGenerics")
# doParallel::registerDoParallel(cores = 30)
# tar_load(params)
# 
# 
# figure1_change_from_baseline_fun(safegraph)



figure7_change_from_baseline_ma <- function(DT_change, pop_density, input.params, ma_days = 14, n.counties.to.plot = NULL) {
  
  # DT_change is safegraph data in targets
  # pop_density is popDensity in targets. used only if we want to plot subset of counties.
  # input.params is params in targets
  # ma_days is the rolloing average days. larger means more smoothing
  # n.counties.to.plot is how many counties to plot. if NA, then plot all
  # if a number, then plot the top n counties with the highest density
  # n.counties.to.plot <- match.arg(n.counties.to.plot)

  DT_change$date <- lubridate::ymd(DT_change$date)
  
  # browser()
  
  DT_change <- DT_change %>% 
    left_join(
      dplyr::select(
      dplyr::filter(
        input.params, pc_metric_type=="cluster1"
        ),
      state_code, state_name
      ), 
      by = c("state" = "state_code")) %>% 
    mutate(state = state_name) %>% 
    dplyr::filter(!is.na(state))
  

  # States of interest for plot one 
  focus_cn <- dplyr::filter(
    input.params, pc_metric_type=="cluster1"
  )$state_name
  
  # Plot one order
  DT_change$state = factor(DT_change$state, 
                           levels=focus_cn)
  
  
  params <- dplyr::filter(input.params, pc_metric_type == "cluster1") %>% 
    dplyr::select(state_name,close, open)
  
  DT <- DT_change %>% 
    dplyr::filter(state %in% focus_cn) %>% 
    dplyr::select(
      "state","fips","date",
      "full_time_work_behavior_devices_change",
      "part_time_work_behavior_devices_change",
      "not_at_home_device_count_change",
      "delivery_behavior_devices_change",
      "median_distance_traveled_from_home_change",
      "median_non_home_dwell_time_change"
    ) %>% 
    group_by(state, fips) %>% 
    dplyr::mutate(across(ends_with("change"),~rollmean(.x, k = ma_days, fill = NA))) %>% 
    tidyr::pivot_longer(cols = -state:-date, names_to = "metric") %>% 
    mutate(value = value / 100) %>% 
    left_join(params, by = c("state" = "state_name")) %>% 
    mutate(state = factor(state, 
                          levels=focus_cn)) %>% 
    left_join(pop_density, by = "fips")
  
  
  if (!is.null(n.counties.to.plot)) {
    
    most_dense_fips_by_state <- DT %>% 
      group_by(state, fips) %>% 
      filter(date == min(date) & metric == "full_time_work_behavior_devices_change") %>% 
      ungroup() %>% 
      group_by(state) %>% 
      dplyr::slice_max(density, n = n.counties.to.plot) %>% 
      pull(fips)
    
    DT_dense <- DT %>% 
      dplyr::filter(fips %in% most_dense_fips_by_state)
    
  } else {
    
    DT_dense <- DT
    
  }

  metric_names <- tibble(metric = c(
    "full_time_work_behavior_devices_change",
    "part_time_work_behavior_devices_change",
    "not_at_home_device_count_change",
    "delivery_behavior_devices_change",
    "median_distance_traveled_from_home_change",
    "median_non_home_dwell_time_change"
  ), 
  metric_name = c(
    "Full-time work behaviour",
    "Part-time work behaviour",
    "Devices leaving home",
    "Delivery behaviours",
    "Distance traveled from home",
    "Time spent away from home"
  ))
    

  DT_dense <- DT_dense %>% 
    left_join(metric_names, by = "metric")
  
p1 <- ggplot() + 
    geom_line(data = DT_dense, aes(x = date, y = value, color = metric_name), alpha = 0.8, size = 0.3) +
    # geom_point(data = DT_dense, aes(x = date, y = value, group = fips), alpha = 0.3) +
    # geom_smooth(data = DT_dense, aes(x = date, y = value, group = fips), method = "loess", size = 0.5, se = F) +
    # facet_grid(state ~ metric_name) + 
  facet_grid(metric_name ~ state, scales = "free") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    scale_y_continuous(labels = scales::percent_format()) +
    
    theme_cowplot(font_size = 10) +
    
    labs(x = "Date",
         y = "% Change",
         color = "Mobility metrics") + 
    # title = "Single Mobility Metrics Change from 7 Day Rolling Average Baseline as a Function of Time",
    # subtitle = paste("7 Day Rolling Average as a Baseline",
    #                  "\nData as of", max(DT_change$date)),
    # caption = "Peter Her / Data: https://docs.safegraph.com/docs/social-distancing-metrics") +
    
    theme(
      legend.position="none",
      # legend.title = element_text(size = 6),
      # legend.text = element_text(size = 6),
      # plot.title = element_text(size = 4),
      # plot.subtitle = element_text(size = 3),
      # plot.caption = element_text(size = 3),
      # axis.title.y = element_text(size = 6),
      # axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 90),
      # axis.title.x = element_text(size = 6),
      # strip.text = element_text(size = ),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2)),
           fill = guide_legend(nrow=3, byrow=TRUE)) +
    
    scale_color_discrete_qualitative(palette = "Dynamic")
  
  cowplot::save_plot("figures/figure7_change_from_baseline_ma.pdf",p1,base_width = 11, base_height = 9)
  
  return("figures/figure7_change_from_baseline_ma.pdf")
  
}