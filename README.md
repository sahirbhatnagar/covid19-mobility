# COVID and Mobility with SafeGraph Data

## Manuscript with Sahar and Peter

- 00-analysis-state.R has the master file to run the analysis and it uses MO_OH_Aug_2.Rmd and fpca_single_template.Rmd to generate the results.
- five_states_start_open_4_14_lags_CCVI_NYT_predifnedKnots_ns_k5_m2_ccvi_quintile.rds has the stored model results
- five_states_ccviquintile_density_popoffset_4_14_sentSep10_FINAL.html is the final results used in the paper


## Raw Data

- The raw data from safegraph is in `data/2019` and `data/2020`. These are the Social Distancing Metrics v2.1 downloaded via the aws command line interface from the safegraph data catalog https://catalog.safegraph.io/app/browse
- Note that the data is actually stored in `/data/bhatnagar-lab/COVID19`. I've created a symbolic link to the `/scratch/bhatnagar-lab/sbhatnagar/mcgill/research/COVID19/data` folder. 
- Census data comes from the American Community Survey. Census block group shapefiles, with linked data from the 5-year 2013-2017 ACS, are available [here](https://www2.census.gov/geo/tiger/TIGER_DP/2017ACS/ACS_2017_5YR_BG.gdb.zip). (We note that, as described in the Methods, we use the 1-year 2018 estimates for the current population of each census block group.) The mapping from counties to MSAs is available [here](https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2020/delineation-files/list2.xlsx). 


## Scripts located in `bin` folder

- `05-data_SD_safegraph.R` generates the aggregated data file. The CSV file generated is `data/aggregated_2020-12-08_2020.csv`
- `06-baselines.R` that takes in the aggregated mobility summary data produced from the `05-data_SD_safegraph.R` script (`data/aggregated_2020-12-08_2020.csv`) and calculates the baseline values. A 7-day rolling average is calculated and used as a baseline. Then the days are shifted back by 1 so that the rolling average of the 7 previous days will be a baseline for the 8th. The resulting file is written as  `data/metrics_2020_summary_and_baseline.csv`
- `07-change.R` which reads in the `data/metrics_2020_summary_and_baseline.csv` file produced from the `06-baselines.R` script, and calculates the change in baseline from the aggregated mobility data summary produced by `05-data_SD_safegraph.R`. The change from baseline of each mobility metric is calculated and the data is written as `data/2020_change_2020_baselines.csv`. This is the file that'll be needed for data analysis. For the variable names: `NAME_baseline` are the baseline values, `NAME_change` are the change from baseline values, and `NAME` are the actual aggregated mobility metrics.

