
table2_pve <- function(pve_list, input.params){
  
  # pve_list <- fpca_data_pve
  # pve_list is fpca_data_pve in _targets.R
  # we only need the FPCA data for each STATE which is in the slots 1,3,5,7 of the pve_list 
  
  # combine all states together
  state_county_pve <- do.call(base::rbind, 
                              
                              # for each state 
                              lapply(pve_list[c(1,3,5,7)], function(STATE) {
                                
                                # for each county in each STATE, extract information and create state level data.frame
                                do.call(base::rbind, lapply(STATE, function(COUNTY){
                                  
                                  data.frame(state = COUNTY$state, county = COUNTY$county, PVE = COUNTY$pve_pc1)
                                  
                                }))
                                
                              })
  )
  
  
  # browser()
  
  state_county_pve <- state_county_pve %>% 
    mutate(state = factor(state, levels = input.params$state_name),
           county =   stringr::str_remove(county, " County")) %>% 
    group_by(state) %>% 
    mutate(county = factor(county))
  
  
  our_summary1 <-
    list("Proportion of variance explained by first fPCA" =
           list("median (IQR)" = ~ qwraps2::median_iqr(PVE))
    )

  by_state <- summary_table(state_county_pve, our_summary1)
  by_state
  
}