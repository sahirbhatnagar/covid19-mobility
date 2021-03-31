library(targets)
library(tarchetypes)
# tar_glimpse()
# tar_visnetwork()
# tar_make()
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# source("packages.R")
# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/fpca_pipeline.R")
source("R/map_cbg_to_fips.R")
source("R/input_parameters.R")
source("R/sd_policy.R")
source("R/table2_pve.R")
source("R/figure1_change_from_baseline.R")
source("R/figure2_correlation_MI.R")
source("R/figure3_CountyCloseReopen.R")
source("R/figure4_3Dplots.R")
source("R/figure5_prop_var_explained.R")
source("R/figure7_change_from_baseline_facet.R")
source("https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata/raw/master/attrdl.R")
source("R/suppfigure1_kmeans.R")
source("R/suppfigure3_corr_by_county.R")
# source("R/suppfigure2_MapGif.R")
source("R/suppfigure2_MapGif-v2.R")
source("R/suppfigure4_MI_CCVIQuintile.R")

# figure3_CountyCloseReopen_fun(res = fpca_data, input.params = input_params())

# Set target-specific options such as packages.

# pacman::p_load_gh("kjhealy/covdata")
#if (!requireNamespace("BiocManager")) install.packages('BiocManager')
#BiocManager::install('seandavi/sars2pack')



tar_option_set(packages = c(
  "pacman",
  "data.table",
  "tidyverse",
  "conflicted",
  "here",
  "doParallel",
  "cowplot",
  "ggrepel",
  "lubridate",
  "colorspace",
  "paletteer",
  "fda.usc",
  "prismatic",
  "sars2pack",
  "mgcv",
  "covdata",
  "gratia",
  "dlnm",
  "splines",
  "plot3D",
  "corrr",
  "pheatmap",
  "locfit",
  "cluster",
  "factoextra",
  "janitor",
  "usmap",
  "leaflet",
  "mapview",
  "stringr",
  "maps",
  "mapdata",
  "gganimate",
  "gifski",
  "transformr",
  "magick",
  "ggpubr",
  "ggplot2",
  "zoo",
  "qwraps2"
)
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("Position", "ggplot2")
conflicted::conflict_prefer("rbind", "BiocGenerics")
conflicted::conflict_prefer("cbind", "BiocGenerics")
conflicted::conflict_prefer("month", "lubridate")
conflicted::conflict_prefer("which", "Matrix")

doParallel::registerDoParallel(cores = 20)

# tar_make(names = c("params","params2","fpca_cases_data","gam_res","report"))


# End this file with a list of target objects.
list(
  
  # Aggregated safegraph data -------------------------------------
  tar_target(
    aggregated_safegraph_path, 
    "data/aggregated_2020-12-08_2020.csv",
    format = "file"
  ),
  
  tar_target(
    aggregated_safegraph, 
    readr::read_csv(aggregated_safegraph_path)
  ),

  # cbg data --------------------------------------------------------------
  tar_target(
    cbg_path, 
    "data/cbg_fips_codes.csv",
    format = "file"
    ),
  
  tar_target(
    cbg,
    read_and_clean_cbg(cbg_path)
  ),
  

  # safegraph change from baseline data -------------------------------------
  tar_target(
    safegraph_path, 
    "data/2020_change_2020_rolling_baseline_Dec08.csv",
    format = "file"
  ),
  
  tar_target(
    safegraph, 
    readr::read_csv(safegraph_path)
  ),
  
  
  # state names -------------------------------------------------------------

  tar_target(
    state_names_sars2pack,
    sars2pack::us_state_distancing_policy()
  ),

  tar_target(
    state_names,
    state_names_sars2pack %>% 
      dplyr::select(iso2c, state) %>%
      dplyr::distinct() %>%
      dplyr::filter(state!="District of Columbia")
  ),
  
  tar_target(
    state_names_list,
    lapply(seq_along(state_names$iso2c), function(i) state_names[i,,drop=TRUE])
    ),
  

  # sd policy data ----------------------------------------------------------

  tar_target(
    sdpolicy_data,
    sars2pack::us_state_distancing_policy()
  ),
  
  # case data ---------------------------------------------------------------

  tar_target(
    nyt,
    sars2pack::nytimes_county_data()
  ),


  # SES data ----------------------------------------------------------------

  tar_target(
    ses_path,
    "data/COVID-19 Community Vulnerability Index (CCVI) - County CCVI.csv",
    format = "file"
  ),
  
  tar_target(
    ses_data,
    readr::read_csv(ses_path)
  ),
  
  tar_target(
    SES,
    janitor::clean_names(ses_data) %>% 
      dplyr::rename(fips = fips_5_digit) %>% 
      dplyr::select(fips, starts_with("theme_"), ccvi_score_higher_more_vulnerable) %>% 
      dplyr::rename(county_ccvi_score = ccvi_score_higher_more_vulnerable)
  ),
  

  # population density ------------------------------------------------------

  tar_target(
    density_path,
    "data/Average_Household_Size_and_Population_Density_-_County.csv",
    format = "file"
  ),
  
  tar_target(
    density_data,
    readr::read_csv(density_path)
  ),
  
  tar_target(
    popDensity,
    density_data %>% 
      dplyr::rename(fips = GEOID,
                    population = B01001_001E,
                    density = B01001_calc_PopDensity,
                    county = NAME,
                    state = State) %>% 
      dplyr::select(fips, population, density)
  ),
  
  
  # input parameters --------------------------------------------------------

  tar_target(
    params,
    input_params(sdpolicy_raw_data = sdpolicy_data, 
                 state_names = c("IL.Illinois",
                                 "OH.Ohio",
                                 "MI.Michigan",
                                 "IN.Indiana")
                 )
  ),
  
  tar_target(
    params2,
    params %>% 
      filter(pc_metric_type == "cluster1")
  ),
  

  # run fpca pipeline for each state ----------------------------------------

  # this returns a list with with safegraph data only from opening date (without cases): 
  tar_target(
    fpca_data,
    mapply(FUN = fpca_pipeline,
           metric_type = params$pc_metric_type,
           type_of_outcome = params$type_of_outcome,
           state_code = params$state_code,
           state_name = params$state_name,
           close_date = params$close,
           open_date = params$open,
           MoreArgs = list(jhu = nyt,
                           safegraph = safegraph,
                           cbg = cbg,
                           ses_data = SES,
                           pop_data = popDensity,
                           pcplot = FALSE, 
                           data_with_cases = FALSE,
                           SD_metric = "not_at_home_device_count_change",
                           cluster1 = c(
                             "full_time_work_behavior_devices_change",
                             "part_time_work_behavior_devices_change",
                             "not_at_home_device_count_change",
                             "delivery_behavior_devices_change",
                             "median_distance_traveled_from_home_change",
                             "median_non_home_dwell_time_change"
                           )
           ),
           SIMPLIFY = FALSE)
  ),
  
  # this return the proportion variance explained
  tar_target(
    fpca_data_pve,
    mapply(FUN = fpca_pipeline,
           metric_type = params$pc_metric_type,
           type_of_outcome = params$type_of_outcome,
           state_code = params$state_code,
           state_name = params$state_name,
           close_date = params$close,
           open_date = params$open,
           MoreArgs = list(jhu = nyt,
                           safegraph = safegraph,
                           cbg = cbg,
                           ses_data = SES,
                           pop_data = popDensity,
                           pcplot = FALSE, 
                           data_with_cases = FALSE,
                           pve = TRUE,
                           SD_metric = "not_at_home_device_count_change",
                           cluster1 = c(
                             "full_time_work_behavior_devices_change",
                             "part_time_work_behavior_devices_change",
                             "not_at_home_device_count_change",
                             "delivery_behavior_devices_change",
                             "median_distance_traveled_from_home_change",
                             "median_non_home_dwell_time_change"
                           )
           ),
           SIMPLIFY = FALSE)
  ),
  
  # this returns the loadings for PC1
  tar_target(
    fpca_data_loadings,
    mapply(FUN = fpca_pipeline,
           metric_type = params$pc_metric_type,
           type_of_outcome = params$type_of_outcome,
           state_code = params$state_code,
           state_name = params$state_name,
           close_date = params$close,
           open_date = params$open,
           MoreArgs = list(jhu = nyt,
                           safegraph = safegraph,
                           cbg = cbg,
                           ses_data = SES,
                           pop_data = popDensity,
                           pcplot = FALSE, 
                           data_with_cases = FALSE,
                           pve = FALSE,
                           pcloadings = TRUE,
                           SD_metric = "not_at_home_device_count_change",
                           cluster1 = c(
                             "full_time_work_behavior_devices_change",
                             "part_time_work_behavior_devices_change",
                             "not_at_home_device_count_change",
                             "delivery_behavior_devices_change",
                             "median_distance_traveled_from_home_change",
                             "median_non_home_dwell_time_change"
                           )
           ),
           SIMPLIFY = FALSE)
  ),
  
  
  # this has the safegraph data with case data from nyt
  # there will be differences between fpca_data and fpca_cases_data. depending on which days case data was
  # available from nyt
  tar_target(
    fpca_cases_data,
    mapply(FUN = fpca_pipeline,
           metric_type = params$pc_metric_type,
           type_of_outcome = params$type_of_outcome,
           state_code = params$state_code,
           state_name = params$state_name,
           close_date = params$close,
           open_date = params$open,
           MoreArgs = list(jhu = nyt,
                           safegraph = safegraph,
                           cbg = cbg,
                           ses_data = SES,
                           pop_data = popDensity,
                           pcplot = FALSE, 
                           data_with_cases = TRUE,
                           SD_metric = "not_at_home_device_count_change",
                           cluster1 = c(
                             "full_time_work_behavior_devices_change",
                             "part_time_work_behavior_devices_change",
                             "not_at_home_device_count_change",
                             "delivery_behavior_devices_change",
                             "median_distance_traveled_from_home_change",
                             "median_non_home_dwell_time_change"
                           )
           ),
           SIMPLIFY = FALSE)
  ),
  

  # run fpca pipeline to extract just the correlations for Figure 2 and supp figure 3 ----------

  tar_target(
    pcplot_data,
    mapply(FUN = fpca_pipeline,
           metric_type = params2$pc_metric_type,
           type_of_outcome = params2$type_of_outcome,
           state_code = params2$state_code,
           state_name = params2$state_name,
           close_date = params2$close,
           open_date = params2$open,
           MoreArgs = list(jhu = nyt,
                           pcplot = TRUE,
                           safegraph = safegraph,
                           cbg = cbg,
                           ses_data = SES,
                           pop_data = popDensity,
                           SD_metric = "not_at_home_device_count_change",
                           cluster1 = c(
                             "full_time_work_behavior_devices_change",
                             "part_time_work_behavior_devices_change",
                             "not_at_home_device_count_change",
                             "delivery_behavior_devices_change",
                             "median_distance_traveled_from_home_change",
                             "median_non_home_dwell_time_change"
                           )
           ),
           SIMPLIFY = FALSE)
  ),
  
  
  tar_target(
    correlation_data,
    figure2_correlation_data(pcplot_data)
  ),
  
  
  # GAM-model-by-state -------------------------------------------------------
  

  tar_target(
    gam_res,
    lapply(fpca_cases_data, function(DTfinal) {
      
      if (unique(DTfinal$outcome_type) == "confirmed") {
        lag_period <- 21
      } else {
        lag_period <-  30
      }
      
      
      tryCatch(
        expr = {
          
          cbgam2 <- crossbasis(x = DTfinal$SDmetricScaled,
                               lag = lag_period,
                               argvar=list(fun='cr', knots = seq(-3,3,1)),
                               arglag=list(fun='cr', knots = c(0,7,14,21), intercept = FALSE),
                               group = DTfinal$county)
          cbgam2Pen <- cbPen(cbgam2)
          
          gam2 <- gam(daily_count ~
                        cbgam2 +
                        density + 
                        ccvi_quintile +
                        s(time, bs = "tp") +
                        s(time, county, bs=c("fs"), k = 5, m = 2) +
                        offset(log(population)),
                      family = quasipoisson(),
                      paraPen=list(cbgam2=cbgam2Pen),
                      data = DTfinal, 
                      method='REML')
          
          return(list(GAM = gam2,
                      cbgam = cbgam2,
                      DTfinal = DTfinal))
          },
        error = function(e) {
          message(e)
        })
    })
  ),
  

  # Report ------------------------------------------------------------------

  tar_render(
    report,
    "report/report_4states.Rmd"
  ),
  

  # PVE table ---------------------------------------------------------------

  tar_target(
    pve_table,
    table2_pve(pve_list = fpca_data_pve, input.params = params2)
  ),
  


  # cases at open and today -------------------------------------------------

  # state populations
  tar_target(
    state_pops,
    covdata::uspop %>%
      dplyr::filter(sex_id == "totsex", hisp_id == "tothisp") %>%
      dplyr::select(state_abbr, statefips, pop, state) %>%
      dplyr::rename(name = state, 
                    state = state_abbr, fips = statefips) %>%
      dplyr::mutate(state = replace(state, fips == "11", "DC"))
  ), 
  
  tar_target(
    cases_open_today,
    covdata::nytcovstate %>%
      dplyr::filter(state %in% params2$state_name) %>%
      dplyr::mutate(state = factor(state, levels = params2$state_name)) %>% 
      dplyr::group_by(state) %>%
      dplyr::left_join(state_pops, by = c("state" = "name")) %>%
      dplyr::left_join(params2, by = c("state" = "state_name")) %>% 
      dplyr::mutate(cases.per.capita = (cases/pop)*1e5) %>% 
      dplyr::filter(date == open | date == ymd("2020-12-08"))
  ),
  
  
  # Figure 1 ----------------------------------------------------------------

  tar_target(
     figure1,
     figure1_change_from_baseline_fun(DT_change = safegraph, input.params = params),
     format = "file"
   ),
  
  
  # Figure 2 ----------------------------------------------------------------
  
  tar_target(
    figure2,
    figure2_correlation_plot(correlation_data),
    format = "file"
  ),

  
  # Figure 3 ----------------------------------------------------------------

  tar_target(
    figure3,
    figure3_CountyCloseReopen_fun(res = fpca_data, input.params = params, today = "2020-07-04"),
    format = "file"
  ),
  
  
  # Figure 4 ----------------------------------------------------------------

  tar_target(
    figure4,
    figure4_3Dplots_fun(gam_result = gam_res, n.points = 25),
    format = "file"
  ),
  
  # Figure 5 ----------------------------------------------------------------

  tar_target(
    figure5,
    figure5_prop_var_explained(pve_list = fpca_data_pve, input.params = params2),
    format = "file"
  ),
  


  # Figure 7 ----------------------------------------------------------------

  tar_target(
    figure7,
    figure7_change_from_baseline_ma(DT_change = safegraph, pop_density = popDensity, 
                                    input.params = params, ma_days = 7),
    format = "file"
  ),
  
  

  # Supplemental Figure 1 --------------------------------------------------

  tar_target(
    ccvi,
    ccvi_data()
  ),
  
  tar_target(
    suppfig1,
    kmeans_state(ccvi),
    format = "file"
  ),
  
  
  
  # Supplemental Figure 3 ---------------------------------------------------

  # Illinois
  tar_target(
    suppfig3.1,
    suppfigure3_corr_by_county(res = pcplot_data, index = 1),
    format = "file"
  ),
  
  # Ohio
  tar_target(
    suppfig3.2,
    suppfigure3_corr_by_county(res = pcplot_data, index = 2),
    format = "file"
  ),
  
  # Michigan
  tar_target(
    suppfig3.3,
    suppfigure3_corr_by_county(res = pcplot_data, index = 3),
    format = "file"
  ),
  
  # Indiana
  tar_target(
    suppfig3.4,
    suppfigure3_corr_by_county(res = pcplot_data, index = 4),
    format = "file"
  ),
  
  # # map GIF ----
  tar_target(
    map_gif,
    map_gif_fun(res = fpca_data, input.params = params2),
    format = "file"
  ),
  
  # CCVI themes ----
  tar_target(
    CCVI_themes,
    CCVI_themes_fun(res = fpca_cases_data),
    format = "file"
  )
)
  
  






