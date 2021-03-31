# library(tidyverse)
# library(dplyr)
# library(usmap)
# library(leaflet)
# library(mapview)
# library(stringr)
# library(maps)
# library(mapdata)

figure3_CountyCloseReopen_fun <- function(res, input.params, today = "2020-12-08"){

  # browser()
  # res is the fpca_data in targets
  # input.params is the params in targets which has the reopen dates
  # Get a data table of the FIPS with the regions and subregions
  # they specify 
  fips <- maps::county.fips %>%
    as_tibble %>% 
    extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") 
  # Edit the extra words added to the subregions 
  fips$subregion <- str_remove(fips$subregion, ":main")
  fips$subregion <- str_remove(fips$subregion, ":penrose")
  fips$subregion <- str_remove(fips$subregion, ":spit")
  fips$subregion <- str_remove(fips$subregion, ":north")
  fips$subregion <- str_remove(fips$subregion, ":south")
  fips$subregion <- str_remove(fips$subregion, ":knotts")
  fips$subregion <- str_remove(fips$subregion, ":chincoteague")
  fips$subregion <- str_remove(fips$subregion, ":lopez island")
  fips$subregion <- str_remove(fips$subregion, ":orcas island")
  fips$subregion <- str_remove(fips$subregion, ":san juan island")
  
  # Get the county level shape information and merge with the FIPS 
  DT_usa <- map_data("county") %>% 
    left_join(fips)
  
  # Make the FIPS the correct number of characters and type 
  DT_usa$fips <- stringr::str_pad(DT_usa$fips, width = 5, side = "left", pad = "0")
  DT_usa$fips <- as.character(DT_usa$fips)
  
  
  # library(targets)
  # tar_load(res)
  # library(tidyverse)
  
  # browser()
  
  res <- dplyr::bind_rows(res)
  res <- res %>% 
    dplyr::filter(metric_type == "FPCA")
  
  # Edit the columns of the FPCA and single metric data table to match the columns 
  # of DT_usa 
  names(res)[names(res) == "county"] <- "subregion"
  res$subregion <- tolower(res$subregion)
  res$subregion <- str_remove(res$subregion, " county")
  
  # merge DT_usa with the FPCA and single metric DT (since we need the longitude 
  # and latitude values merged with the FPCA values)
  DT_merged <- left_join(res, DT_usa, by = "fips")
  
  # browser()
  
  
  input.params <- input.params %>% 
    filter(pc_metric_type == "cluster1") %>% 
    dplyr::select(state_name, close, open) %>% 
    dplyr::rename(state = state_name)
  
  DT_merged <- DT_merged %>% 
    left_join(input.params, by = "state")

  # DT_merged$close %>% table
  # min(DT_merged$date)
  

  # DT_close <- DT_merged %>% 
  #   group_by(state) %>% 
  #   filter(date == close) %>% 
  #   dplyr::mutate(state = paste0(state, " - Close"))
  
  # Get the state specific reopening dates
  DT_open <- DT_merged %>% 
    group_by(state) %>% 
    filter(date == open) %>% 
    dplyr::mutate(state = paste0(state, " - Reopen"))
  
  # DT_open$state %>% table
  # DT_close$state %>% table
  
  # # Rename the dates 
  # DT_open$state[(DT_open$state) == "Ohio"] <- "Ohio - Reopen"
  # DT_open$state[(DT_open$state) == "Missouri"] <- "Missouri - Reopen"
  # DT_open$state[(DT_open$state) == "South Carolina"] <- "South Carolina - Reopen"
  # DT_open$state[(DT_open$state) == "Indiana"] <- "Indiana - Reopen"
  
  # Get the latest date available by state 
  DT_present <- DT_merged %>%
    group_by(state) %>%
    dplyr::filter(date == today) %>%
    dplyr::mutate(state = paste0(state, " - ",format(date, "%B %d")))
  
  # browser()
  # # Rename the dates
  # DT_present$state[(DT_present$state) == "Ohio"] <- "Ohio - December 8"
  # DT_present$state[(DT_present$state) == "Missouri"] <- "Missouri - December 8"
  # DT_present$state[(DT_present$state) == "South Carolina"] <- "South Carolina - December 8"
  # DT_present$state[(DT_present$state) == "Indiana"] <- "Indiana - December 8"
  
  # Combine the DT with both the state specific reopening dates and the 
  # latest date 
  # DT_both <- rbind(DT_close, DT_open)
  DT_both <- rbind(DT_open, DT_present)
  
  # DT_both$state %>% factor %>% levels %>% dput
  # order the dates 
  # DT_both$state = factor(DT_both$state, 
  #                        levels=c("Illinois - Reopen", "Illinois - December 08", 
  #                                 "Ohio - Reopen", "Ohio - December 08",
  #                                 "Michigan - Reopen", "Michigan - December 08", 
  #                                 "Indiana - Reopen", "Indiana - December 08"))
  
  # DT_both$state %>% unique %>% dput
  
  DT_both$state <- factor(DT_both$state, 
                          levels = paste0(rep(input.params$state, each = 2), c(" - Reopen",paste0(" - ",format(as.Date(today), "%B %d")))))
  
  
  
  # input.params
  # map
  # library(ggplot2)
  # library(cowplot)
  # library(colorspace)
  
  
  #pdf(file = "Figure 3", width = 12 , height = 9)
  # DT_both$state %>% table
  # DT_both %>% 
  #   filter(state == "sf")
  # 
  # DT_open$state %>% table
  
  
  p3 <- 
    ggplot(data = filter(DT_both, metric_type %in% "FPCA"), 
               aes(x = long, y = lat, group = group)) + 
    
    geom_polygon(color = "black", fill = NA) +
    
    geom_polygon(data = filter(DT_both, metric_type %in% "FPCA"), 
                 aes(fill = SDmetricScaled),
                 color = "black", size = 0.1) +
    
    
    labs(fill = "MI") + 
    
    
    theme_cowplot() +
    
    theme(
      axis.text = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 12),
      legend.text=element_text(size=12),
      legend.title = element_text(size = 12),
      panel.border = element_rect(colour = "black", size= 0.1),
      legend.key.size = unit(0.6, "cm")
    ) + 
    
    guides(colour = guide_legend(title.hjust = 0.5)) + 
    
    guides(colour = guide_legend(override.aes = list(size=0.5))) +
    
    facet_wrap(~state, scales = "free", ncol=4) + 
    
    scale_size_continuous(breaks = 0.5) + 
    
    
    scale_fill_gradient2(low = "royalblue", mid = "white",
                         high = "red",
                         minor_breaks = waiver(),
                         # midpoint = -0.1,
                         midpoint = 0,
                         guide = "legend",
                         breaks = scales::breaks_pretty(10))
                         # breaks = c(-6, -3, -2, -1.5,
                         #            -1, -0.5, -0.25, -0.1,
                         #            0, 0.1, 0.25, 0.5,
                         #            1, 1.5, 3))
  
  cowplot::save_plot("figures/figure3_CloseReopen.pdf", plot = p3, base_height = 7)
  
  return("figures/figure3_CloseReopen.pdf")
  
  #dev.off()
  
}

