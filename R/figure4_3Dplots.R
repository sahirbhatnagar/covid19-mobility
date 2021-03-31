# perc <- seq(-1.5,1.5, length.out = 30)
# lapply(res, function(i) unique(i$state))

# library(targets)
# tar_load(gam_res)
# gam_result <- gam_res
# source("packages.R")
# 
# lapply(gam_res, function(i) unique(i$DTfinal$state))
# 
# 
# 
# dev.off()

figure4_3Dplots_fun <- function(gam_result, n.points = 25){
  
  rr <- lapply(seq_along(gam_result), function(index) {
    
    gam2 <- gam_result[[index]]$GAM
    cbgam2 <- gam_result[[index]]$cbgam
    res <- gam_result[[index]]  
    
    cen <- 0
    perc_rr <- quantile(res$DTfinal$SDmetricScaled, probs = seq(0.1, 0.90, length.out = n.points))
    predsdgam2 <- crosspred(cbgam2, gam2,
                            at = perc_rr,
                            cen = cen)
    
    predsdgam2$state <- res$DTfinal$state %>% unique()
    predsdgam2$metric <- res$DTfinal$metric_type %>% unique()
    
    return(predsdgam2)
    
  })
  
  clims <- range(sapply(rr, function(i) range(i$matRRfit)))
  
  m <- matrix(c(1,1,2,2, #title
                3,4,5,6, #graphs
                7,7,8,8, #title
                9,10,11,12), #graphs
              # 13,13,13,13,# legend title
              # 14,14,14,14 #legend
              # ), 
              # 13,13,14,14, #title
              # 15,16,17,17), #graphs and legend
              nrow = 4,
              ncol = 4, 
              byrow = TRUE)
  
  pdf(file="figures/figure4_3Dplots.pdf", width = 12, height = 9)
  
  par(mar=c(1,1,1,1))
  layout(mat = m, heights = c(0.4,3,0.4,3))
  # layout.show(n=14)
  plot.new()
  text(0.5,0.5,"Illinois",cex=1.5,font=2)
  plot.new()
  text(0.5,0.5,"Ohio",cex=1.5,font=2)
  
  
  zlims_il <- range(sapply(rr[1:2], function(i) range(i$matRRfit)))
  zlims_ohio <- range(sapply(rr[3:4], function(i) range(i$matRRfit)))
  
  # my_cols <- viridis::inferno(n = 30, direction = -1)
  my_cols <- "#A6E1F4"
  
  lapply(c(1,2,3,4), function(index) {
    
    # c(bottom, left, top, right)
    gam2 <- gam_result[[index]]$GAM
    cbgam2 <- gam_result[[index]]$cbgam
    res <- gam_result[[index]]  
    
    if (index %in% c(1,2)) { 
      zlims_plot <- zlims_il 
    } else {
      zlims_plot <- zlims_ohio
    }
    
    cen <- 0
    perc <- quantile(res$DTfinal$SDmetricScaled, probs = seq(0.10, 0.90, length.out = n.points))
    predsdgam2 <- crosspred(cbgam2, gam2,
                            at = perc,
                            cen = cen)
    
    predsdgam2$state <- res$DTfinal$state %>% unique()
    predsdgam2$metric <- res$DTfinal$metric_type %>% unique()
    
    par(mar = c(1.5,2,0,1))
    
    persp3D(z = predsdgam2$matRRfit, 
            # clab = c("CI Width"),
            cex.lab = 1,
            cex.axis = 1,
            xlab = ifelse(predsdgam2$metric=="FPCA","Mobility Index (MI)","Devices leaving home"), 
            ylab = "Lag", 
            zlab = "Adjusted Incidence Rate Ratio",
            x = predsdgam2$predvar, 
            y = seq(from = predsdgam2$lag[1], to = predsdgam2$lag[2], by = predsdgam2$bylag),
            ticktype = "detailed", 
            byt = "b2",
            # col = pal(sort(predsdgam2$matRRhigh-predsdgam2$matRRlow)),
            col = my_cols,
            # colvar = predsdgam2$matse,
            # colvar = predsdgam2$matRRhigh-predsdgam2$matRRlow,
            # colvar = predsdgam2$matRRfit,
            theta = 210, 
            colkey = FALSE,
            # colkey = TRUE,
            expand = 1.1,
            # axes = FALSE,
            phi = 5,
            # colkey = list(side = 1, length = 1),
            # colkey = list(dist = -0.15, shift = 0.2,
            #               side = 3, length = 0.5, width = 0.5, line.clab = 2.5,
            #               cex.clab = 0.5, 
            #               # col.clab = "white", 
            #               # col.axis = "white", 
            #               # col.ticks = "white", 
            #               cex.axis = 0.8),
            # zlim = c(0.90, 1.30),
            zlim = zlims_plot,
            # xaxs = "i",
            clim = clims,
            # curtain = TRUE,facets = FALSE,
            ltheta = 290,
            border = "black",
            # contour = list(side = c("1")),
            shade = 0.7)
    # mtext("Ohio", side = 3, outer = F)
  })
  
  
  plot.new()
  text(0.5,0.5,"Michigan",cex=1.5,font=2)
  plot.new()
  text(0.5,0.5,"Indiana",cex=1.5,font=2)
  
  lapply(rr, function(i) i$state)
  
  zlims_mich <- range(sapply(rr[5:6], function(i) range(i$matRRfit)))
  zlims_in <- range(sapply(rr[7:8], function(i) range(i$matRRfit)))
  
  lapply(c(5,6,7,8), function(index) {
    
    # c(bottom, left, top, right)
    gam2 <- gam_result[[index]]$GAM
    cbgam2 <- gam_result[[index]]$cbgam
    res <- gam_result[[index]]  
    
    if (index %in% c(5,6)) { 
      zlims_plot <- zlims_mich
    } else {
      zlims_plot <- zlims_in
    }
    
    cen <- 0
    perc <- quantile(res$DTfinal$SDmetricScaled, probs = seq(0.1, 0.90, length.out = n.points))
    predsdgam2 <- crosspred(cbgam2, gam2,
                            at = perc,
                            cen = cen)
    
    predsdgam2$state <- res$DTfinal$state %>% unique()
    predsdgam2$metric <- res$DTfinal$metric_type %>% unique()
    
    par(mar = c(1.5,2,0,1))
    
    persp3D(z = predsdgam2$matRRfit, 
            # main = paste0(predsdgam2$state,"\n", ifelse(predsdgam2$metric=="FPCA","FPCA","Devices leaving home")), 
            clab = c("CI Width"),
            cex.lab = 1,
            cex.axis = 1,
            xlab = ifelse(predsdgam2$metric=="FPCA","Mobility Index (MI)","Devices leaving home"), 
            ylab = "Lag", 
            zlab = "Adjusted Incidence Rate Ratio",
            x = predsdgam2$predvar, 
            expand = 1.1,
            y = seq(from = predsdgam2$lag[1], to = predsdgam2$lag[2], by = predsdgam2$bylag),
            ticktype = "detailed", byt = "b2",
            # col = pal(sort(predsdgam2$matRRfit)),
            col = my_cols,
            # colvar = predsdgam2$matse,
            # colvar = predsdgam2$matRRhigh-predsdgam2$matRRlow,
            theta = 210, 
            colkey = FALSE,
            border = "black",
            # axes = FALSE,
            phi = 5,
            # colkey = list(side = 1, length = 1),
            # colkey = list(dist = -0.15, shift = 0.2,
            #               side = 3, length = 0.5, width = 0.5, line.clab = 2.5,
            #               cex.clab = 0.5, 
            #               # col.clab = "white", 
            #               # col.axis = "white", 
            #               # col.ticks = "white", 
            #               cex.axis = 0.8),
            # zlim = c(0.90, 1.30),
            zlim = zlims_plot,
            # xaxs = "i",
            clim = clims,
            # curtain = TRUE,facets = FALSE,
            ltheta = 290,
            # contour = list(side = c("1")),
            shade = 0.7)
    # mtext("Ohio", side = 3, outer = F)
  })
  
  dev.off()
  
  return("figures/figure4_3Dplots.pdf")
}


# my_3d_plot_final()
# dev.off()
