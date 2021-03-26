
sd_policy <- function(us_state_distancing_policy, state_abb) {
  
  # us_state_distancing_policy is the raw data from sars2pack::us_state_distancing_policy()
  # state_abb is are the state abbreviations that you want e.g. c("IL","OH")
  
  us_state_distancing_policy %>%
    dplyr::select(iso2c,statepolicy,mandate,statewide,dateenacted,dateexpiry, policysource) %>%
    dplyr::filter(statepolicy %in% c("StayAtHome") & mandate & statewide) %>%
    filter(iso2c %in% state_abb) %>% 
    group_by(iso2c) %>% 
    filter(dateenacted==min(dateenacted))

}