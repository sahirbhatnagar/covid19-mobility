# !diagnostics off
fpca_pipeline <- function(jhu,
                          safegraph,
                          cbg,
                          type_of_outcome,
                          # mobility_start_date,
                          state_code,
                          state_name,
                          close_date,
                          open_date,
                          # lag_period,
                          # moving_average,
                          # case_more_than,
                          roll_avg_cases = 7,
                          roll_align = "right",
                          ses_data,
                          pop_data,
                          pcplot = FALSE,
                          data_with_cases = TRUE, # if true (and pcplot=FALSE), return SD data with cases, else only return SD data
                          pve = FALSE, # return percent variance explained by 1st PC
                          pcloadings = FALSE, # return loadings
                          metric_type = c("singleMetric", "cluster1"),
                          SD_metric,
                          cluster1 = c(
                            "number_of_devices_that_leave_full_time_change",
                            "number_of_devices_that_leave_part_time_change",
                            "not_at_home_device_count_change",
                            "stops_in_delivery_behavior_change",
                            "distance_devices_go_change",
                            "times_devices_away_change"),
                          cluster1_original = c(
                            "number_of_devices_that_leave_full_time",
                            "number_of_devices_that_leave_part_time",
                            "not_at_home_device_count",
                            "stops_in_delivery_behavior",
                            "distance_devices_go",
                            "times_devices_away")) {

  metric_type <- match.arg(metric_type)

  if (metric_type == "singleMetric") {
    cluster_cols <- SD_metric
  } else
    if (metric_type == "cluster1") {
      cluster_cols <- cluster1
    }

  # Case data ---------------------------------------------------------------

  jhu_cases <- jhu %>%
    filter(state == state_name & subset == type_of_outcome) %>%
    filter(fips %in% cbg[state == state_code][["fips"]]) %>%
    filter(!is.na(county)) %>%
    dplyr::arrange(fips, date) %>%
    dplyr::group_by(fips) %>%
    filter(date >= (open_date-1)) %>% 
    dplyr::mutate(dplyr::across(count, ~.x - lag(.x, order_by = date),
                                .names = "daily_{col}")) %>%
    dplyr::mutate(daily_count = if_else(daily_count < 0, 0, daily_count)) %>%
    dplyr::mutate(rollavg = data.table::frollmean(daily_count, n = roll_avg_cases, align = roll_align)) %>% 
    dplyr::mutate(rollavg_round = round(rollavg)) %>% 
    filter(!is.na(daily_count)) %>% 
    mutate(close_date = close_date,
           open_date = open_date) %>% 
    mutate(policy = factor(ifelse(date < open_date, 0, 1), levels = 0:1, labels = c("closed","open")))

  baseline_data <- jhu_cases %>% 
    filter(row_number()==1) %>% 
    dplyr::select(fips, count) %>% 
    left_join(pop_data, by = "fips") %>% 
    mutate(baselineInc = count/population) %>% 
    dplyr::select(fips, baselineInc)
  
  jhu_cases <- jhu_cases %>% 
    left_join(baseline_data, by = "fips")
  
  fips_to_keep <- unique(jhu_cases$fips)

  # Safegraph data ----------------------------------------------------------

  # get unique dates
  unique_SD_dates <- safegraph %>%
    filter(state == state_code) %>%
    arrange(date) %>%
    filter(date >= open_date) %>% 
    pull(date) %>%
    unique()

  SDdata <- lapply(fips_to_keep, function(ii) {

    if (metric_type %in% c("cluster1")) {

      tpt <- safegraph %>%
        filter(state == state_code) %>%
        filter(fips %in% ii) %>%
        arrange(date) %>%
        filter(date >= open_date) %>% 
        dplyr::select(all_of(cluster_cols)) %>%
        as.matrix() 
      
      res2 <- prcomp(tpt, rank. = 1, center = F, scale. = F)
      # library(sparsepca)
      # library("psych")
      # tt <- principal(tpt, rotate = "simplimax")
      # plot(tt$scores[,1], res2$x[,1])
      # 
      # summary(res2)
      # tt <- spca(tpt, k=1, center = FALSE, scale = FALSE, alpha = 1e-1, beta = 1e-1)
      # summary(tt)
      # plot(tt$scores[,1],res2$x[,1])
      # abline(a=0,b=1)
      # res2$rotation
      # res2 %>% summary
      # varimax(res2$rotation)
      # res2$rotation
      # all.equal(svd(tpt)$v[,1],
      #      res2$rotation[,1])
      # hist(res2$rotation[,1])
      
      # dates_used <- safegraph %>%
      #   dplyr::filter(state == state_code) %>%
      #   dplyr::filter(fips %in% ii) %>%
      #   arrange(date) %>%
      #   dplyr::filter(date >= close_date) %>% 
      #   pull(date) 
      # 
      # sd_metrics_only <- safegraph %>%
      #   dplyr::filter(state == state_code) %>%
      #   dplyr::filter(fips %in% ii) %>%
      #   arrange(date) %>%
      #   dplyr::filter(date >= close_date) %>% 
      #   dplyr::select(all_of(cluster_cols)) %>%
      #   as.matrix()

      # SD_fdata <- fdata(sd_metrics_only,
      #                   # argvals = as.numeric(dates_used)-min(as.numeric(dates_used))+1,
      #                   # rangeval = as.numeric(dates_used)-min(as.numeric(dates_used))+1,
      #                   names = list(
      #                     main = sprintf("Devices Leaving Home by County, %s", "MO"),
      #                     xlab = "Date",
      #                     ylab = "% of Devices Leaving Home"
      #                   )
      # )

      # plot(SD_fdata)
      # res3 <- fda.usc::fdata2pc(SD_fdata)
      # plot(res3$x[,c("PC1","PC2")])
      
      # this shows that the shape is the same for PCA and FPCA
      # par(mfrow = c(2,1))
      # plot(predict(loess(res2$x[,"PC1"]~as.numeric(dates_used))))
      # plot(predict(loess(res3$x[,"PC1"]~as.numeric(dates_used))))
      # browser()
      # res2$rotation
      # promax(res2$rotation)
      # 
      
      if (!pcplot & !pve & !pcloadings) {
        # return pc
        topp <- cbind(res2$x[,"PC1"])
        colnames(topp) <- ii
        return(topp)
      } else if (!pcplot & pve & !pcloadings) {
        # browser()
        # return pve
        pve_pc1 <- (res2$sdev^2 / sum(res2$sdev^2))[1]
        return(list(pve_pc1 = pve_pc1, state = state_name, county = cbg[fips==ii]$county))
        
      } else if (pcplot & !pve & !pcloadings){
        # return pc with individual metrics
        topp <- cbind(res2$x[,"PC1"])
        colnames(topp) <- "FPCA"
        pc_with_indiv <- cbind(topp,tpt)
        dimnames(pc_with_indiv)[[1]] <- unique_SD_dates
        return(list(pc = pc_with_indiv, dates = unique_SD_dates, state = state_name, county = cbg[fips==ii]$county))
      } else if (!pcplot & !pve & pcloadings) {
        # return loadings
        # browser()
        return(t(res2$rotation[,"PC1",drop=F]) %>% 
                 as_tibble() %>% 
                 mutate(county = cbg[fips==ii]$county, state = state_name) %>% 
                 tidyr::pivot_longer(cols = -county:-state, names_to = "metric", values_to = "PC1_loading")
        )
      }
    } else if (metric_type == "singleMetric") {
        sd_metrics_only <- safegraph %>%
          filter(state == state_code) %>%
          filter(fips %in% ii) %>%
          arrange(date) %>%
          filter(date >= open_date) %>% 
          dplyr::select(all_of(cluster_cols)) %>%
          as.matrix()

        colnames(sd_metrics_only) <- ii
        return(sd_metrics_only)
      }
  })

  if (!pcplot & !pve & !pcloadings) {
    # I should be scaling it here, not after
    # this doesnt matter anymore since we're scaling from opening date
    SDdata_summarised <- do.call(cbind, SDdata) %>%
      as.data.frame() %>%
      mutate(date = unique_SD_dates) %>%
      pivot_longer(cols = -date, names_to = "fips", values_to = "SDmetric") %>%  
      group_by(fips) %>% 
      arrange(fips, date) %>% 
      mutate(SDmetricScaled = scale(SDmetric)[,1])
    # dplyr::select(date, fips, county, SDmetric)
    # SDdata_summarised$SDmetricScaled %>% quantile()
    
    # Merge Cases with Safegraph ----------------------------------------------
    # browser()
    DTfinal <- jhu_cases %>%
      dplyr::select(fips, county, date, count, daily_count, rollavg, rollavg_round, policy, baselineInc) %>%
      left_join(SDdata_summarised, by = c("fips","date")) %>%
      left_join(ses_data, by = "fips") %>%
      left_join(pop_data, by = "fips") %>%
      # dplyr::filter(date >= mobility_start_date) %>%
      dplyr::filter(!is.na(SDmetric) & !is.na(daily_count)) %>%
      mutate(time = as.numeric(date) - as.numeric(min(date)) + 1,
             county = factor(county)) %>%
      dplyr::filter(!is.na(population)) %>%
      mutate(county = droplevels(county)) %>%
      # mutate(SDmetricScaled = scale(SDmetric)) %>% 
      ungroup() %>% 
      mutate(outcome_type = type_of_outcome,
             metric_type = ifelse(metric_type=="cluster1","FPCA",cluster_cols),
             state = state_name,
             state_code = state_code)
    
    DTfinal <- DTfinal %>%
      mutate(ccvi_quintile = cut(county_ccvi_score, 
                                 breaks = quantile(county_ccvi_score,
                                                   probs = seq(0,1,by=0.20), na.rm = TRUE),
                                 labels = c("0-20","20-40","40-60","60-80","80-100")))
    
    SDdata_summarised <- SDdata_summarised %>%
      left_join(cbg, by = "fips") %>% 
      ungroup() %>% 
      mutate(metric_type = ifelse(metric_type=="cluster1","FPCA",cluster_cols),
             state = state_name,
             state_code = state_code) 


    # return(list(DT_safegraph_cases = DTfinal,
    #             DT_safegraph = SDdata))
    
    # return(SDdata_summarised)
    
    # DTfinal$SDmetric %>% quantile(probs = seq(0,1,0.2))
    # DTfinal$SDmetricScaled %>% quantile(probs = seq(0,1,0.1))
    # 
    # par(mfrow=c(1,3))
    # DTfinal$SDmetric %>% hist
    # DTfinal$SDmetric %>% scale %>% hist
    # DTfinal$SDmetric %>% scales::rescale_mid(to = c(-100, 100), mid = 0) %>% hist
    # DTfinal$SDmetric %>% scales::rescale_mid(to = c(-100, 100), mid = 0) %>% scale %>% quantile(probs = seq(0,1,0.2))
    
    if (data_with_cases) {
    return(DTfinal)
    } else {
      return(SDdata_summarised)
    }
    
  } else {
    
    return(SDdata)
  }
  
}


# this gives a fixed lag period
fpca_pipeline_lag <- function(jhu,
                              safegraph,
                              cbg,
                              type_of_outcome,
                              # mobility_start_date,
                              state_code,
                              state_name,
                              close_date,
                              open_date,
                              lag_period,
                              # moving_average,
                              # case_more_than,
                              roll_avg_cases = 7,
                              roll_align = "right",
                              ses_data,
                              pop_data,
                              pcplot = FALSE,
                              metric_type = c("singleMetric", "cluster1"),
                              SD_metric,
                              cluster1 = c(
                                "number_of_devices_that_leave_full_time_change",
                                "number_of_devices_that_leave_part_time_change",
                                "not_at_home_device_count_change",
                                "stops_in_delivery_behavior_change",
                                "distance_devices_go_change",
                                "times_devices_away_change"),
                              cluster1_original = c(
                                "number_of_devices_that_leave_full_time",
                                "number_of_devices_that_leave_part_time",
                                "not_at_home_device_count",
                                "stops_in_delivery_behavior",
                                "distance_devices_go",
                                "times_devices_away")) {
  
  metric_type <- match.arg(metric_type)
  
  if (metric_type == "singleMetric") {
    cluster_cols <- SD_metric
  } else
    if (metric_type == "cluster1") {
      cluster_cols <- cluster1
    }
  
  # Case data ---------------------------------------------------------------
  
  
  jhu_cases <- jhu %>%
    dplyr::filter(state == state_name & subset == type_of_outcome) %>%
    dplyr::filter(fips %in% cbg[state == state_code][["fips"]]) %>%
    dplyr::filter(!is.na(county)) %>%
    dplyr::arrange(fips, date) %>%
    dplyr::group_by(fips) %>%
    filter(date >= (open_date-1)) %>% 
    dplyr::mutate(dplyr::across(count, ~.x - lag(.x, order_by = date),
                                .names = "daily_{col}")) %>%
    dplyr::mutate(daily_count = if_else(daily_count < 0, 0, daily_count)) %>%
    dplyr::mutate(rollavg = data.table::frollmean(daily_count, n = roll_avg_cases, align = roll_align)) %>% 
    dplyr::mutate(rollavg_round = round(rollavg)) %>% 
    dplyr::filter(!is.na(daily_count)) %>% 
    mutate(close_date = close_date,
           open_date = open_date) %>% 
    mutate(policy = factor(ifelse(date < open_date, 0, 1), levels = 0:1, labels = c("closed","open")))
  
  baseline_data <- jhu_cases %>% 
    dplyr::filter(row_number()==1) %>% 
    dplyr::select(fips, count) %>% 
    left_join(pop_data, by = "fips") %>% 
    mutate(baselineInc = count/population) %>% 
    dplyr::select(fips, baselineInc)
  
  jhu_cases <- jhu_cases %>% 
    left_join(baseline_data, by = "fips")
  
  fips_to_keep <- unique(jhu_cases$fips)
  
  # Safegraph data ----------------------------------------------------------
  
  # get unique dates
  # here we go backwards to assess mobility 'lag_period' days ago
  # so we need to artificially shift the safegraph data by 7 days forward
  # so that when we merge the mobility from 7 days ago will be matched with cases from today
  # e.g. we want the cases from March 8, 2020 to correlate with mobility from March 1, 2020
  # so we shift March 1 --> March 8 on the safegraph data, then merge with case data
  safegraph <- safegraph %>%
    dplyr::filter(state == state_code) %>%
    arrange(date) %>%
    dplyr::filter(date >= open_date) %>%
    group_by(fips) %>%
    mutate(date = lead(date, n = lag_period)) %>%
    dplyr::select(fips, state, date, everything()) %>%
    ungroup()

  
  # get unique dates
  unique_SD_dates <- safegraph %>%
    dplyr::filter(state == state_code) %>%
    arrange(date) %>%
    dplyr::filter(date >= open_date) %>% 
    pull(date) %>%
    unique()
  
  SDdata <- lapply(fips_to_keep, function(ii) {
    
    if (metric_type %in% c("cluster1")) {

      tpt <- safegraph %>%
        dplyr::filter(state == state_code) %>%
        dplyr::filter(fips %in% ii) %>%
        arrange(date) %>%
        dplyr::filter(date >= open_date) %>% 
        dplyr::select(all_of(cluster_cols)) %>%
        as.matrix() 
      
      res2 <- prcomp(tpt, center = F, scale. = F)
      # res2 %>% summary
      
      # all.equal(svd(tpt)$v[,1],
      #      res2$rotation[,1])
      # hist(res2$rotation[,1])
      
      # dates_used <- safegraph %>%
      #   dplyr::filter(state == state_code) %>%
      #   dplyr::filter(fips %in% ii) %>%
      #   arrange(date) %>%
      #   dplyr::filter(date >= close_date) %>% 
      #   pull(date) 
      # 
      # sd_metrics_only <- safegraph %>%
      #   dplyr::filter(state == state_code) %>%
      #   dplyr::filter(fips %in% ii) %>%
      #   arrange(date) %>%
      #   dplyr::filter(date >= close_date) %>% 
      #   dplyr::select(all_of(cluster_cols)) %>%
      #   as.matrix()
      
      # SD_fdata <- fdata(sd_metrics_only,
      #                   # argvals = as.numeric(dates_used)-min(as.numeric(dates_used))+1,
      #                   # rangeval = as.numeric(dates_used)-min(as.numeric(dates_used))+1,
      #                   names = list(
      #                     main = sprintf("Devices Leaving Home by County, %s", "MO"),
      #                     xlab = "Date",
      #                     ylab = "% of Devices Leaving Home"
      #                   )
      # )
      
      # plot(SD_fdata)
      # res3 <- fda.usc::fdata2pc(SD_fdata)
      # plot(res3$x[,c("PC1","PC2")])
      
      # this shows that the shape is the same for PCA and FPCA
      # par(mfrow = c(2,1))
      # plot(predict(loess(res2$x[,"PC1"]~as.numeric(dates_used))))
      # plot(predict(loess(res3$x[,"PC1"]~as.numeric(dates_used))))
      
      if (!pcplot) {
        topp <- cbind(res2$x[,"PC1"])
        colnames(topp) <- ii
        return(topp)
      } else {
        topp <- cbind(res2$x[,"PC1"])
        colnames(topp) <- "FPCA"
        pc_with_indiv <- cbind(topp,tpt)
        dimnames(pc_with_indiv)[[1]] <- unique_SD_dates
        return(list(pc = pc_with_indiv, state = state_name, county = cbg[fips==ii]$county))
        
      }
    } else if (metric_type == "singleMetric") {
      sd_metrics_only <- safegraph %>%
        dplyr::filter(state == state_code) %>%
        dplyr::filter(fips %in% ii) %>%
        arrange(date) %>%
        dplyr::filter(date >= open_date) %>% 
        dplyr::select(all_of(cluster_cols)) %>%
        as.matrix()
      
      colnames(sd_metrics_only) <- ii
      return(sd_metrics_only)
    }
  })
  
  if (!pcplot) {
    # browser()
    
    # I should be scaling it here, not after
    # this doesnt matter anymore since we're scaling from opening date
    SDdata_summarised <- do.call(cbind, SDdata) %>%
      as.data.frame() %>%
      mutate(date = unique_SD_dates) %>%
      pivot_longer(cols = -date, names_to = "fips", values_to = "SDmetric") %>%  
      group_by(fips) %>% 
      arrange(fips, date) %>% 
      mutate(SDmetricScaled = scale(SDmetric)[,1])
    # dplyr::select(date, fips, county, SDmetric)
    # SDdata_summarised$SDmetricScaled %>% quantile()
    
    # Merge Cases with Safegraph ----------------------------------------------
    # browser()
    
    DTfinal <- jhu_cases %>%
      dplyr::select(fips, county, date, count, daily_count, rollavg, rollavg_round, policy, baselineInc) %>%
      left_join(SDdata_summarised, by = c("fips","date")) %>%
      left_join(ses_data, by = "fips") %>%
      left_join(pop_data, by = "fips") %>%
      # dplyr::filter(date >= mobility_start_date) %>%
      # dplyr::filter(is.na(SDmetric)) %>%
      dplyr::filter(!is.na(SDmetric) & !is.na(daily_count)) %>%
      mutate(time = as.numeric(date) - as.numeric(min(date)) + 1,
             county = factor(county)) %>%
      dplyr::filter(!is.na(population)) %>%
      mutate(county = droplevels(county)) %>%
      # mutate(SDmetricScaled = scale(SDmetric)) %>% 
      ungroup() %>% 
      mutate(outcome_type = type_of_outcome,
             metric_type = ifelse(metric_type=="cluster1","FPCA",cluster_cols),
             state = state_name,
             state_code = state_code)
    
    
    SDdata_summarised <- SDdata_summarised %>%
      left_join(cbg, by = "fips") %>% 
      mutate(outcome_type = type_of_outcome,
             metric_type = ifelse(metric_type=="cluster1","FPCA",cluster_cols))
    
    
    # return(list(DTfinal = DTfinal,
    #             SDdata = SDdata_summarised))
    
    # return(SDdata_summarised)
    
    # DTfinal$SDmetric %>% quantile(probs = seq(0,1,0.2))
    # DTfinal$SDmetricScaled %>% quantile(probs = seq(0,1,0.1))
    # 
    # par(mfrow=c(1,3))
    # DTfinal$SDmetric %>% hist
    # DTfinal$SDmetric %>% scale %>% hist
    # DTfinal$SDmetric %>% scales::rescale_mid(to = c(-100, 100), mid = 0) %>% hist
    # DTfinal$SDmetric %>% scales::rescale_mid(to = c(-100, 100), mid = 0) %>% scale %>% quantile(probs = seq(0,1,0.2))
    return(DTfinal)
  } else {
    
    return(SDdata)
  }
  
}

