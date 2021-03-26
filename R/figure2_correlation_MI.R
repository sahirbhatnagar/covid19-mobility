
figure2_correlation_data <- function(res2){
  
  # res2 is the result from fpca_pipeline function with pcplot=TRUE.
  # see targets::tar_load(pcplot_data)
  # this gives, for each state, a tibble that has the metrics for each county for each day
  ttt <- lapply(res2, function(STATE) {
    do.call(rbind, lapply(STATE, function(COUNTY){
      COUNTY$pc %>% 
        as_tibble() %>% 
        mutate(date = COUNTY$dates, county = COUNTY$county, state = COUNTY$state) %>% 
        tidyr::pivot_longer(cols = -date:-state)
    }))
  })
  
  fpca_corr_by_state <- do.call(rbind, 
                                
                                lapply(ttt, function(STATE){
                                  
                                  pp_STATE <- STATE %>% 
                                    pivot_wider(id_cols = date:state) %>% 
                                    arrange(date, county)
                                  
                                  ppp_STATE <- split(pp_STATE, pp_STATE$date)
                                  
                                  # for each day, calculate the correlation between FPCA and single metric (the correlation is calculated across counties for a single day)
                                  STATE_corr <- do.call(rbind,
                                                        lapply(ppp_STATE, function(DATE){
                                                          
                                                          DATE %>% 
                                                            dplyr::select(-date:-state) %>% 
                                                            corrr::correlate() %>% 
                                                            corrr::focus(FPCA, mirror = F) %>% 
                                                            tidyr::pivot_wider(names_from = "rowname", values_from = "FPCA") %>% 
                                                            dplyr::mutate(state = unique(DATE$state), date = unique(DATE$date)) %>% 
                                                            dplyr::select(state, date, everything())
                                                        })
                                  )
                                  
                                  return(STATE_corr)
                                  
                                })
                                
  )
  
  return(fpca_corr_by_state)
  
}

# see figure2_code.R in figures/ folder
# readr::write_csv(fpca_corr_by_state, path = here::here("figures/fpca_corr_by_state.csv"))

figure2_correlation_plot <- function(fpca_corr_by_state){

  # pdf("figure2_correlation.pdf", width = 12, height = 9)
  # browser()
  p2 <- fpca_corr_by_state %>% 
    pivot_longer(-state:-date) %>% 
    mutate(name = factor(name), state=factor(state)) %>% 
    ggplot(aes(x = date, y = value, color = name)) + 
    geom_smooth(method = "locfit", se = F) +
    facet_wrap(~state) + 
    theme(legend.position = "bottom") + ylab("Correlation with MI") + 
    theme_cowplot() +
    labs(x = "Date",
         y = "Correlation with Mobility Index (MI)",
         color = "Mobility metrics") + 
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
  
  cowplot::save_plot("figures/figure2_correlation.pdf", plot = p2)
  
  return("figures/figure2_correlation.pdf")
}


# res <- bind_rows(fpca_data) %>% 
#   tidyr::unite("state",metric_type, state) 
# 
# ggplot(res, aes(x = date, y = SDmetricScaled, group = county)) + 
#   geom_smooth(method = "locfit", se = F) + facet_wrap(~state, ncol = 4)


  