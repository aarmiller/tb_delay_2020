# tb_delay_2020

Script for the delay diagnosis Tuberculosis project (2020)

The following files contain the pseudo code and R code that can be used to duplicate the findings of our paper "Incidence, duration, and risk factors associated with delayed and missed diagnostic opportunities associated with Tuberculosis: A population-based retrospective cohort study". The Truven Marketscan Research databases can be purchased from IBM Watson Health. These databases are delivered as a series of SAS data files. At the University of Iowa these data are then converted to a relational database which is stored on a local drive accessible within our High Performance Computing environment. Because this data and computing structure is specific to the University or Iowa, we have provided the psuedo code (i.e., extraction procedures) that would need to be used to extract the same datasets from the raw Truven Marketscan files. We have noted what data the extracted files should contain.

The scripts folder contains the scripts utilized in the analysis:

build_matched_cohorts.R - This script contains the procedures for identifying cohorts of carriers or cases of CF along with the corresponding matched cohorts. This code specifically identifies the enrollee id's to extract. After running a this script you would then need to extract the data for these enrollees from the Truven database. Note: on our system we extract the enrollee id's then store all the extracted data in a relational (SQLite) database.

main_analysis.R - This script contains the code for running the primary analysis in the study. Note: this script uses the database of matched cohorts created from the enrollee id's identified in the build_mathced_cohorts.R script. This script also relies on the condition codesidentified in the condition_codes.R script and utilizes function from the analysis_functions.R and the small_db_functions.R scripts.

figures.R - This script contains all of the code that was used to generate the figures for the paper. This script utilizes the data containing odds ratio estimates that were generated in the main_analysis.R script. This script also utilizes functions contained in the analysis_functions.R script.

simulation.R - This script condatins the procedures and code that would need to be used to run the simulation analysis described in the appendix of the paper.
