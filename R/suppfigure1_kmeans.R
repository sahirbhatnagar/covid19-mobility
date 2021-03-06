
# https://web.archive.org/web/20200715194419/https://precisionforcovid.org/ccvi

ccvi_data <- function(){
  
  df <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    state = c("Alabama","Alaska","Arizona","Arkansas",
              "California","Colorado","Connecticut","Delaware",
              "District Of Columbia","Florida","Georgia","Hawaii",
              "Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
              "Louisiana","Maine","Maryland","Massachusetts",
              "Michigan","Minnesota","Mississippi","Missouri","Montana",
              "Nebraska","Nevada","New Hampshire","New Jersey",
              "New Mexico","New York","North Carolina","North Dakota",
              "Ohio","Oklahoma","Oregon","Pennsylvania",
              "Rhode Island","South Carolina","South Dakota","Tennessee",
              "Texas","Utah","Vermont","Virginia","Washington",
              "West Virginia","Wisconsin","Wyoming"),
    `ses` = c(0.92,0.42,0.7,0.9,0.74,0.12,
              0.24,0.48,0.46,0.68,0.84,0.06,0.62,0.5,0.66,0.2,
              0.4,0.88,0.94,0.38,0.18,0.1,0.58,0.04,1,0.56,0.32,
              0.14,0.78,0,0.3,0.98,0.6,0.8,0.02,0.54,0.76,
              0.64,0.44,0.52,0.86,0.28,0.82,0.72,0.26,0.08,0.34,
              0.36,0.96,0.16,0.22),
    `household` = c(0.86,0.3,0.5,0.98,0.2,0.04,0.1,
                    0.46,0,0.42,0.56,0.18,0.78,0.26,0.76,0.6,0.7,
                    0.9,0.88,0.72,0.16,0.06,0.64,0.28,1,0.8,0.54,
                    0.48,0.4,0.14,0.02,0.92,0.22,0.66,0.12,0.74,0.94,
                    0.62,0.52,0.36,0.84,0.68,0.82,0.44,0.08,0.38,0.24,
                    0.32,0.96,0.34,0.58),
    `language` = c(0.42,0.6,0.84,0.36,1,0.62,0.7,
                   0.72,0.86,0.88,0.78,0.98,0.38,0.74,0.26,0.2,0.44,
                   0.12,0.52,0.02,0.8,0.64,0.28,0.34,0.46,0.18,
                   0.06,0.4,0.92,0.08,0.9,0.94,0.82,0.68,0.1,0.22,
                   0.58,0.56,0.32,0.54,0.5,0.14,0.3,0.96,0.48,0.04,
                   0.66,0.76,0,0.24,0.16),
    `housing` = c(0.28,0.98,0.56,0.82,0.86,0.08,
                  0.1,0.26,1,0.52,0.36,0.78,0.6,0.18,0.16,0.54,0.3,
                  0.48,0.66,0.72,0.02,0.42,0.2,0.64,0.74,0.4,
                  0.88,0.34,0.12,0.38,0.14,0.68,0.92,0.62,0.94,0.06,
                  0.58,0.96,0.24,0.32,0.5,0.9,0.22,0.46,0.04,0.76,
                  0,0.84,0.7,0.44,0.8),
    `epi` = c(0.88,0.28,0.08,0.84,0.02,0.16,
              0.26,0.82,0.24,0.46,0.44,0.66,0,0.8,0.74,0.6,
              0.58,0.9,0.98,0.42,0.7,0.72,0.64,0.06,0.92,0.78,
              0.12,0.56,1,0.36,0.5,0.1,0.62,0.48,0.3,0.86,0.76,
              0.34,0.68,0.52,0.54,0.32,0.94,0.38,0.04,0.18,
              0.22,0.4,0.96,0.2,0.14),
    `healthcare` = c(0.96,0.1,0.6,0.82,0.84,0.3,0.18,
                     0.16,0,0.9,0.92,0.42,0.32,0.94,0.58,0.76,0.44,
                     0.64,0.38,0.24,0.22,0.68,0.88,0.72,0.5,0.78,
                     0.02,0.2,0.46,0.12,0.56,0.4,0.98,1,0.26,0.62,0.34,
                     0.48,0.8,0.08,0.54,0.28,0.74,0.7,0.52,0.06,0.86,
                     0.66,0.04,0.36,0.14),
    `overall` = c(1,0.3,0.5,0.96,0.68,0.12,
                  0.14,0.36,0.34,0.9,0.86,0.62,0.2,0.82,0.56,0.44,
                  0.38,0.76,0.88,0.18,0.32,0.48,0.72,0.22,0.92,0.66,
                  0.04,0.24,0.84,0,0.42,0.7,0.94,0.98,0.08,0.52,
                  0.74,0.58,0.6,0.28,0.64,0.26,0.78,0.8,0.1,0.02,
                  0.4,0.54,0.46,0.16,0.06)
  )
 
  return(df) 
}
  


kmeans_state <- function(df){

  rownames(df) <- df$state
  df <- df[,-1]
  
  set.seed(123)
  gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  # fviz_gap_stat(gap_stat)
  
  set.seed(123)
  final <- kmeans(df, 3, nstart = 25)

  p2 <- fviz_cluster(final, data = df) + cowplot::theme_cowplot()
  
  cowplot::save_plot(filename = "figures/suppfigure1_kmeans_stats.pdf", plot = p2, base_width = 10, base_height = 9)
  
  return("figures/suppfigure1_kmeans_stats.pdf")

}