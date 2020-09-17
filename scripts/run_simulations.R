

##### This script contains the basic R script that was used to run simulations
##### to estimate the incidence of diagnostic delays 
##### NOTE: This script contains a combination of actual and psuedo code. 

# install delayDX package from github repository
# devtools::install_github("aarmiller/delayDX")

library(dplyr)
library(delayDX)

### Run Simulations for any ssd visits only ###

## Load time map data generated from main_analysis_datasets.R
load('/path')

## Run prep sim function to prepare the dataset for simulations.
## Set event name to run simulations for any ssd visits.
## Set change point method to lm_cube (i.e. linear cubic).
## Specify simulations to start 3 days before index.
tmp_sim_data <- prep_sim_data(final_time_map %>% rename(ed = ED),
                              event_name = "any_ssd", 
                              cp_method = "lm_cube", 
                              start_day = 3L,
                              by_days = 1L)

## Run 25,000 Simulations
simulation_results <- run_cp_bootstrap(tmp_sim_data,
                                       sim_version = "visits",
                                       boot_trials = 500,
                                       n_sim_trials = 50,
                                       new_draw_weight = NULL)

### Run Simulations for any visits ###

## Run prep sim function to prepare the dataset for simulations.
## Set event name to run simulations for any visits.
## Set change point method to lm_cube (i.e. linear cubic).
## Specify simulations to start 3 days before index.
tmp_sim_data <- prep_sim_data(final_time_map %>% rename(ed = ED),
                              event_name = "any_visits", 
                              cp_method = "lm_cube", 
                              start_day = 3L,
                              by_days = 1L)

## Run 25,000 Simulations
simulation_results <- run_cp_bootstrap(tmp_sim_data,
                                       sim_version = "visits",
                                       boot_trials = 500,
                                       n_sim_trials = 50,
                                       new_draw_weight = NULL)



