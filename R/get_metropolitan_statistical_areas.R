pacman::p_load(yaml)

msa <- readxl::read_xls(here::here("data/list2_2020.xls"), skip = 2)

msa <-
  msa %>% 
  filter(`Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area") %>% 
  filter(`CBSA Code` %in% c("12060","16980","19100","26420","31080","33100","35620","37980","41860","47900")) %>% 
  rename(FIPS = `FIPS Place Code`)

msa

# YAML config file -------------------------------------------------------------

# named list 
config_list <- list(
  study_fips = msa$FIPS,
  name_fips = msa$`Principal City Name`,
  name_msa = unique(msa$`CBSA Title`)
)

print(config_list)

# write yaml config
write_yaml(config_list, file = here::here('bin/config.yaml'))
