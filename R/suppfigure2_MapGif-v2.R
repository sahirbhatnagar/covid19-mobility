# library(tidyverse)
# library(dplyr)
# library(usmap)
# library(leaflet)
# library(mapview)
# library(stringr)
# library(maps)
# library(mapdata)

map_gif_fun <- function(res, input.params){

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
  
  res <- dplyr::bind_rows(res)
  res <- res %>% 
    dplyr::filter(metric_type == "FPCA")
  
  # Edit the columns of the FPCA and single metric data table to match the columns 
  # of DT_usa 
  names(res)[names(res) == "county"] <- "subregion"
  res$subregion <- tolower(res$subregion)
  res$subregion <- str_remove(res$subregion, " county")
  
  DT_merged <- left_join(res, DT_usa, by = "fips")
  

  DT_merged$state = factor(DT_merged$state, 
                         levels=input.params$state_name)

  # library(ggplot2)
  # library(cowplot)
  # library(colorspace)
  # library(gganimate)
  # library(gifski)
  # library(transformr)
  # library(magick, lib = "/data/bhatnagar-lab/Rlibs/R4.0.2")
  
  
  DT_merged <- dplyr::filter(DT_merged, date >= "2020-05-01")
  
  map_plot <- ggplot(data = DT_merged, 
         aes(x = long, y = lat, group = group)) + 
    
    geom_polygon(color = "black", fill = NA) +
    
    geom_polygon(data = DT_merged, 
                 aes(fill = SDmetricScaled),
                 color = "black", size = 0.1) +
    
  
    facet_wrap(~state, scales = "free", ncol=2) + 
    
    theme_cowplot() +
    
    theme(
      axis.text = element_blank(),
      legend.position = "right",
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      strip.text = element_text(size = 20),
      legend.text=element_text(size= 12),
      legend.title = element_text(size = 10),
      panel.border = element_rect(colour = "black", size= 0.1),
      legend.key.size = unit(0.5, "cm")
    ) + 
    
    guides(colour = guide_legend(title.hjust = 0.1)) + 
    
    guides(colour = guide_legend(override.aes = list(size=0.1))) +
    
    scale_size_continuous(breaks = 0.5) + 
    
    scale_fill_gradient2(low = "royalblue", mid = "white",
                         high = "red",
                         minor_breaks = waiver(),
                         midpoint = -0.1,
                         guide = "legend",
                         breaks = c(-6, -3, -2, -1.5,
                                    -1, -0.5, -0.25, -0.1,
                                    0, 0.1, 0.25, 0.5,
                                    1, 1.5, 3)) + 
    
    transition_time(date) + 
  
    labs(title = "Date: {format(frame_time, '%A %B %d, %Y')}", fill = "FPCA")
    
  mapGIF <- animate(map_plot, fps = 1.0, nframes = length(unique(DT_merged$date)),
                    renderer = magick_renderer())
  
  anim_save("figures/suppfigure2_map.gif", mapGIF)
  
  return("figures/suppfigure2_map.gif")

}
