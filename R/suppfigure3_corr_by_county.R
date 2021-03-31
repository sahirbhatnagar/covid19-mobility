
suppfigure3_corr_by_county <- function(res2, index){

  # index is in 1:4 --> for each state
  # res2 is the result from fpca_pipeline function with pcplot=TRUE.
  # see targets::tar_load(pcplot_data)
  tt <- lapply(res2, function(STATE){
    
    do.call(base::rbind, lapply(STATE, function(COUNTY) {
      
      COUNTY$pc %>% 
        corrr::correlate(quiet = TRUE) %>% 
        corrr::focus(FPCA, mirror = F) %>% 
        tidyr::pivot_wider(names_from = "rowname", values_from = "FPCA") %>% 
        mutate(state = COUNTY$state, county = COUNTY$county) %>% 
        dplyr::select(state, county, everything())
      
    }))
    
  })
  
  i <- index
  # browser()
  
  tpt <- tt[[i]] %>% 
    dplyr::rename("Full-time work behaviour" = "full_time_work_behavior_devices_change",
                  "Part-time work behaviour" = "part_time_work_behavior_devices_change",
                  "Devices leaving home" = "not_at_home_device_count_change",
                  "Delivery behaviours" = "delivery_behavior_devices_change",
                  "Distance traveled from home" = "median_distance_traveled_from_home_change",
                  "Time spent away from home" = "median_non_home_dwell_time_change"
    ) %>% 
    dplyr::select(-1:-2)
  
  pdf(file = sprintf("figures/suppfigure3_fpca_correlation_by_county_%s.pdf",unique(tt[[i]]$state)), height = 11, width = 8)
  pheatmap::pheatmap(abs(tpt),
                     cluster_rows = FALSE,
                     cluster_cols = FALSE,
                     breaks = seq(0,1,by=0.1),
                     labels_row = tt[[i]][,2,drop=T],
                     color = colorspace::sequential_hcl(length(seq(0,1,by=0.1)), rev = TRUE)
                     )
  dev.off()
  
  return(sprintf("figures/suppfigure3_fpca_correlation_by_county_%s.pdf",unique(tt[[i]]$state)))
  
}