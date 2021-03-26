
figure5_prop_var_explained <- function(pve_list){
  
  # pve_list <- fpca_data_pve
  # pve_list is fpca_data_pve in _targets.R
  # we only need the FPCA data for each STATE which is in the slots 1,3,5,7 of the pve_list 
  
  # combine all states together
  state_county_pve <- do.call(rbind, 
          
          # for each state 
          lapply(pve_list[c(1,3,5,7,9)], function(STATE) {
            
            # for each county in each STATE, extract information and create state level data.frame
            do.call(rbind, lapply(STATE, function(COUNTY){
              
              data.frame(state = COUNTY$state, county = COUNTY$county, PVE = COUNTY$pve_pc1)
              
            }))
            
          })
  )
  
  
  
  state_county_pve <- state_county_pve %>% 
    mutate(state = factor(state, levels = c("Ohio", "Indiana","Missouri","South Carolina")),
           county =   stringr::str_remove(county, " County")) %>% 
    group_by(state) %>% 
    mutate(county = factor(county))
  
  p2 <- 
    state_county_pve %>% 
    ggplot(aes(x = state, y = PVE)) + 
    # geom_point() +
    # geom_text_repel() +
    geom_violin(aes(fill=state), trim = FALSE) +
    geom_boxplot(width = 0.2)+
    # theme(legend.position = "none") + 
    # facet_wrap(~state) + 
    theme_cowplot() +
    labs(
      x = "",
      y = "Proportion of variance explained by first fPCA") + 
    theme(
      legend.position="none",
      legend.title = element_text(size = 6),
      legend.text = element_text(size = 6),
      plot.title = element_text(size = 4),
      plot.subtitle = element_text(size = 3),
      plot.caption = element_text(size = 3),
      axis.title.y = element_text(size = 6),
      axis.text = element_text(size = 6),
      # axis.text.x = element_text(angle = 45),
      axis.title.x = element_text(size = 6),
      strip.text = element_text(size = 6),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    guides(colour = guide_legend(override.aes = list(size=1.2)),
           fill = guide_legend(nrow=3, byrow=TRUE)) +
    scale_fill_discrete_qualitative()
  
  cowplot::save_plot("figures/figure5_pve.pdf", plot = p2)
  
  return("figures/figure5_pve.pdf")

}




