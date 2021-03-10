
CCVI_themes_fun <- function(res){
  
  res <- dplyr::bind_rows(res)
  res <- res %>% 
    dplyr::filter(metric_type == "FPCA")
  
  # socioeconomic status ----
  # Quintile column
  MI_only_socio <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_socio <- MI_only_socio %>% dplyr::mutate(socio = cut(theme_1_socioeconomic_status, 
                                                      breaks = quantile(MI_only_socio$theme_1_socioeconomic_status,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))

  # Socio order 
  MI_socio$state = factor(MI_socio$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_socio <- ggplot(data = MI_socio) + 
    
    geom_smooth(data = MI_socio, 
                aes(x= date, y = (SDmetricScaled), 
                    color = socio), 
                method = "locfit", size = 0.5, se = F) +
    
   
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Socioeconomic status \nquintile") + 
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  
  # household composition and disability ----
  # House column
  MI_only_house <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_house <- MI_only_house %>% dplyr::mutate(house = cut(theme_2_household_composition_disability, 
                                                      breaks = quantile(MI_only_house$theme_2_household_composition_disability,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))
  
  # Plot 3 order 
  MI_house$state = factor(MI_house$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_house <- ggplot(data = MI_house) + 
    
    geom_smooth(data = MI_house, 
                aes(x= date, y = (SDmetricScaled), 
                    color = house), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Household composition \nand disability quintile") + 
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  # housing type and transportation ----
  # transport column
  MI_only_transport <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_transport <- MI_only_transport %>% dplyr::mutate(transport = cut(theme_4_housing_type_transportation, 
                                                      breaks = quantile(MI_only_transport$theme_4_housing_type_transportation,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))
  

  # Plot 3 order 
  MI_transport$state = factor(MI_transport$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_transport <- ggplot(data = MI_transport) + 
    
    geom_smooth(data = MI_transport, 
                aes(x= date, y = (SDmetricScaled), 
                    color = transport), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Housing type and \ntransportation quintile") + 
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  # epidemiological factors ----
  # epidemiological column
  MI_only_epidemiological <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_epidemiological <- MI_only_epidemiological %>% dplyr::mutate(epidemiological = cut(theme_5_epidemiological_factors, 
                                                      breaks = quantile(MI_only_epidemiological$theme_5_epidemiological_factors,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))
  
  
  # Plot 3 order 
  MI_epidemiological$state = factor(MI_epidemiological$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_epidemiological <- ggplot(data = MI_epidemiological) + 
    
    geom_smooth(data = MI_epidemiological, 
                aes(x= date, y = (SDmetricScaled), 
                    color = epidemiological), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Epidemiological factors \nquintile") + 
  
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  # healthcare system factors ----
  # healthcare column
  MI_only_healthcare <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_healthcare <- MI_only_healthcare %>% dplyr::mutate(healthcare = cut(theme_6_healthcare_system_factors, 
                                                      breaks = quantile(MI_only_healthcare$theme_6_healthcare_system_factors,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))
  
  
  # Plot 3 order 
  MI_healthcare$state = factor(MI_healthcare$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_healthcare <- ggplot(data = MI_healthcare) + 
    
    geom_smooth(data = MI_healthcare, 
                aes(x= date, y = (SDmetricScaled), 
                    color = healthcare), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Healthcare system \nfactors quintile") + 
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  # Minority status and language quintile ----
  MI_only_minority <- res %>%
    dplyr::filter(metric_type %in% "FPCA") 
  MI_minority <- MI_only_minority %>% dplyr::mutate(minority = cut(theme_3_minority_status_language, 
                                                      breaks = quantile(MI_only_minority$theme_3_minority_status_language,
                                                                        probs = seq(0,1,by=0.20), na.rm = TRUE)))

  
  # Plot 3 order 
  MI_minority$state = factor(MI_minority$state, 
                        levels=c("Ohio","Indiana", 
                                 "Missouri", "South Carolina"))
  
  plot_minority <- ggplot(data = MI_minority) + 
    
    geom_smooth(data = MI_minority, 
                aes(x= date, y = (SDmetricScaled), 
                    color = minority), 
                method = "locfit", size = 0.5, se = F) +
    
    
    geom_hline(yintercept = 0, size = 0.1, linetype = "dashed", colour = "grey69") + 
    
    facet_wrap(~state, ncol = 2, scales = "free_x") +
    
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    
    theme_cowplot() +
    
    labs(x = "Date",
         y = "Change in Z-Score",
         color = "Minority status \nand language quintile") + 
    
    theme(
      legend.justification = "left",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 7),
      plot.subtitle = element_text(size = 5),
      plot.caption = element_text(size = 5),
      axis.title.y = element_text(size = 9),
      axis.text = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      strip.text = element_text(size = 9),
      axis.line = element_line(colour = 'black', size = 0.1),
      axis.ticks = element_line(colour = "black", size = 0.1),
      panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
      panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
    ) +
    
    guides(colour = guide_legend(override.aes = list(size=1.2))) +
    
    scale_color_discrete_diverging(palette = "Blue-Red",
                                   na.translate = FALSE,
                                   labels = c(("0-20%"), ("20-40%"), ("40-60%"),
                                              ("60-80%"), ("80-100%")))
  
  #pdf(file = "CCVI", width = 9.5, height = 26)
  
  CCVI_plot <- ggarrange(plot_socio, plot_house, plot_minority, plot_transport, plot_epidemiological, plot_healthcare,
            labels = c("A", "B", "C", "D", "E", "F"),
            ncol = 2, nrow = 3)
  
  cowplot::save_plot("CCVI_themes.pdf", plot = CCVI_plot, base_height = 12)
  
  return("CCVI_themes.pdf")
  
  #dev.off()
}
