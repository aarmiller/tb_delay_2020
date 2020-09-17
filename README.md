# tb_delay_2020

Script for the delay diagnosis Tuberculosis project (2020)

The following files contain the pseudo code and R code that can be used to duplicate the findings of our paper "Incidence, duration, and risk factors associated with delayed and missed diagnostic opportunities associated with Tuberculosis: A population-based retrospective cohort study". The Truven Marketscan Research databases can be purchased from IBM Watson Health. These databases are delivered as a series of SAS data files. At the University of Iowa these data are then converted to a relational database which is stored on a local drive accessible within our High Performance Computing environment. Because this data and computing structure is specific to the University or Iowa, we have provided the psuedo code (i.e., extraction procedures) that would need to be used to extract the same datasets from the raw Truven Marketscan files. We have noted what data the extracted files should contain.

The scripts folder contains the scripts utilized in the analysis:

make_final_tb_cohort.R - The basic R script that was used to identify and build the TB cohort utilized in the analysis. Script also identifies the date and setting of index tb visit abd shift the index to the first drug date (i.e. Isoniazid and Rifampin, Pyrazinamide, or ethambutol) if the drug date occured prior to the index tb visit date.

main_analysis_datasets.R - This script contains the basic R script that was used to generate datasets for simulations and for the logistic regression analysis. 

run_simulations.R - This script contains the basic R script that was used to run simulations to estimate the incidence of diagnostic delays for any SSD visits only and all visits.
