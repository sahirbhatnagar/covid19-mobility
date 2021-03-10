# download file from https://docs.safegraph.com/docs/open-census-data

map_cbg_to_fips <- function(cbg) {

  # cbg <- fread(here::here("data/safegraph_open_census_data/metadata/cbg_fips_codes.csv"),
  #              keepLeadingZeros = TRUE)
  
  # check that all state fips are 2 and county fips are 3 characters
  nchar(cbg$state_fips) %>% table
  nchar(cbg$county_fips) %>% table
  
  # update some codes
  # source: https://www.cdc.gov/nchs/nvss/bridged_race/county_geography-_changes2015.pdf
  cbg[state_fips == "02" & county_fips == "270", `:=`(county_fips = 158,
                                                      county = "Kusilvak Census Area")]
  cbg[state_fips == "46" & county_fips == "113", `:=`(county_fips = 102,
                                                      county = "Oglala Lakota County")]
  
  cbg[, fips := stringr::str_c(cbg$state_fips, cbg$county_fips)]
  
  # check all fips are 5 characters long
  cbg[,table(nchar(fips))]
  
  # see extended table 1 of nature paper
  # https://www.nature.com/articles/s41586-020-2923-3/tables/1
  # https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list2_2020.xls
  
  # see 08-inputs.R
  # msa <- readxl::read_xls(here::here("data/list2_2020.xls"), skip = 2)
  # 
  # msa <-
  #   msa %>% 
  #   filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") %>% 
  #     filter(`CBSA Code` %in% c("12060","16980","19100","26420","31080","33100","35620","37980","41860","47900")) %>% 
  #     rename(FIPS = `FIPS Place Code`)
  # 
  # 
  # table(msa$`CBSA Title`)
  # 
  # 
  # unique(msa$`CBSA Title`) %>% as.matrix
  
  return(cbg)
}

read_and_clean_cbg <- function(path) {
  cbg_data <- data.table::fread(path, keepLeadingZeros = TRUE)
  map_cbg_to_fips(cbg_data)
}



