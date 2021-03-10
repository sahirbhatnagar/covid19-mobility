# these are the parameters used  in the fpca_pipeline function

input_params <- function() {
  
  # parameters to iterate over
  params <- expand.grid(pc_metric_type = c("cluster1","singleMetric"),
                        # type_of_outcome = c("confirmed","deaths"),
                        type_of_outcome = c("confirmed"),
                        state_name = c(
                          "OH.Ohio",
                          "MO.Missouri",
                          # "OK.Oklahoma",
                          "SC.South Carolina",
                          "IN.Indiana"
                        ),
                        stringsAsFactors = FALSE) %>% 
    as_tibble() %>% 
    separate(state_name, into = c("state_code","state_name"), sep = "[.]")
  
  
  open_dates <- data.frame(state_name = c(
    "Ohio",
    "Missouri",
    # "Oklahoma",
    "South Carolina",
    "Indiana"
  ),
  close = c(
    mdy("03-23-2020"),
    mdy("04-06-2020"),
    mdy("04-06-2020"),
    # mdy("04-06-2020"),
    mdy("04-06-2020")
  ),
  open = c(
    mdy("05-12-2020"),
    mdy("05-04-2020"),
    mdy("04-24-2020"),
    # mdy("04-20-2020"),
    mdy("05-04-2020")
  )
  )
  
  params <- params %>% 
    left_join(open_dates, by = "state_name")

  params  
  
}