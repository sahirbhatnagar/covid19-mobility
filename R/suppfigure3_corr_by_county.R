
suppfigure3_corr_by_county <- function(res2, index){

  # index is in 1:4 --> for each state
  # res2 is the result from fpca_pipeline function with pcplot=TRUE.
  # see targets::tar_load(pcplot_data)
  tt <- lapply(res2, function(STATE){
    
    do.call(rbind, lapply(STATE, function(COUNTY) {
      
      COUNTY$pc %>% 
        correlate() %>% 
        focus(FPCA, mirror = F) %>% 
        tidyr::pivot_wider(names_from = "rowname", values_from = "FPCA") %>% 
        mutate(state = COUNTY$state, county = COUNTY$county) %>% 
        dplyr::select(state, county, everything())
      
    }))
    
  })
  
  i <- index
  
  pdf(file = sprintf("figures/suppfigure3_fpca_correlation_by_county_%s.pdf",unique(tt[[i]]$state)), height = 11, width = 8)
  pheatmap::pheatmap(tt[[i]][,3:8],
                     cluster_rows = FALSE,
                     cluster_cols = FALSE,
                     breaks = seq(-1,1,by=0.2),
                     labels_row = tt[[i]][,2,drop=T],
                     color = colorspace::diverge_hcl(length(seq(-1,1,by=0.2)) - 1))
  dev.off()
  
  return(sprintf("figures/suppfigure3_fpca_correlation_by_county_%s.pdf",unique(tt[[i]]$state)))
  
}