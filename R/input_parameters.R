# these are the parameters used  in the fpca_pipeline function

# stay at home orders taken from 
# sd_policy <- sars2pack::us_state_distancing_policy()
# https://github.com/COVID19StatePolicy/SocialDistancing/blob/master/codebooks/State%20COVID-19%20policy%20documentation.pdf

# colnames(sd_policy)

  

# Mandate: Binary variable on whether the policy is a mandate (1) or is a recommendation (0). This is coded based on
# the order's phrasing (e.g., "residents are advised to stay at home and avoid unnecessary travel" would be coded as 0
# for mandate as a "StayAtHome" policy). This variable was added on March 30, 2020.

# StateWide: Binary variable indicating whether the policy applied statewide (1), or whether the policy is only
# applicable to sub‐state geographic areas (e.g., counties) or sub‐populations within a state (e.g., individuals aged 65
# and older). If either SWGeo or SWPop is 0, StateWide is also 0.

# DateEnacted: Date of policy enactment: the date of when the policy would be enforced, per descriptions available in
# policy documents. The format is YYYYMMDD. All policies with a date of 11:59 pm YYYYMMDD are coded as
# YYYYMMDD+1 (e.g., a policy that goes into effect at 11:59 pm on April 14, 2020 will be coded as 20200415). In the
# “beta” dataset, if a policy is eased, the date of easement would be considered "DateEnacted" for that particular policy
# action and then the policy it eases would be listed under "Eases." The same applies for a policy that “Ends” –
# “DateEnacted” is when the ending of a given policy is in effect.

# DateExpiry: Date of policy expiry, if or as provided in the policy issuance or executive order. This date is meant to
# reflect when the policy or order would be in effect until or unless additional action is taken to extend, amend, or halt
# its status. The format is YYYYMMDD. All policies with a date of 11:59 pm YYYYMMDD are coded as YYYYMMDD+1
# (e.g., a policy that goes into effect at 11:59 pm on April 14, 2020 will be coded as 20200415). This variable was added
# on March 29, 2020



input_params <- function(sdpolicy_raw_data, state_names) {
  
  # sdpolicy_raw_data has all the policy data from all states
  # parameters to iterate over
  params <- expand.grid(pc_metric_type = c("cluster1","singleMetric"),
                        type_of_outcome = c("confirmed"),
                        state_name = state_names,
                        stringsAsFactors = FALSE) %>% 
    as_tibble() %>% 
    separate(state_name, into = c("state_code","state_name"), sep = "[.]")
  
  
  stay_at_home_orders <- sd_policy(us_state_distancing_policy = sdpolicy_raw_data, 
                                   state_abb = unique(params$state_code)) %>% 
    dplyr::left_join(dplyr::filter(params, pc_metric_type == "cluster1"), by = c("iso2c" = "state_code")) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(close = dateenacted, open = dateexpiry) %>% 
    dplyr::select(state_name, close, open)
  
  
  # stay_at_home_orders <- as.data.frame(rbind(
  #   c("Illinois", "2020-03-21", "2020-04-08"),
  #   c("Ohio",     "2020-03-24",  "2020-04-07"),
  #   c("Michigan", "2020-03-24", "2020-04-14"),
  #   c("Indiana", "2020-03-25", "2020-04-07")
  # ))
  # 
  # colnames(stay_at_home_orders) <- c("state_name","close","open")
  # stay_at_home_orders <- stay_at_home_orders %>% 
  #   mutate(close = ymd(close),
  #          open = ymd(open))

  # open_dates <- data.frame(state_name = c(
  #   "Ohio",
  #   "Missouri",
  #   # "Oklahoma",
  #   "South Carolina",
  #   "Indiana"
  # ),
  # close = c(
  #   mdy("03-23-2020"),
  #   mdy("04-06-2020"),
  #   mdy("04-06-2020"),
  #   # mdy("04-06-2020"),
  #   mdy("04-06-2020")
  # ),
  # open = c(
  #   mdy("05-12-2020"),
  #   mdy("05-04-2020"),
  #   mdy("04-24-2020"),
  #   # mdy("04-20-2020"),
  #   mdy("05-04-2020")
  # )
  # )
  
  params <- params %>% 
    left_join(stay_at_home_orders, by = "state_name")

  params  
  
}
