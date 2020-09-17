
##### This script contains the basic R script that was used to identify and 
##### build the TB cohort utilized in the analysis
##### NOTE: This script contains a combination of actual and psuedo code. 

# install delayDX and icdplus package from github repository
# devtools::install_github("aarmiller/delayDX")
# devtools::install_github("a1arakkal/icdplus")

## Load Libraries
library(tidyverse)
library(delayDX)
library(icdplus)


## Specify path to the database and connect to data basse
db_path <- "/path to database/"
db_tb <- src_sqlite(db_path)

## Build a time map for a specific database of interest. Returns 
## A tibble with a time map containing visit level information from the database. 
## Includes admission date, year, setting, enrolid, standard place of care, key, 
## discharge date, and location of care (i.e. inpatient, outpatient, or ED)

tb_time_map <-  build_time_map_delay(db_con = db_tb, db_path = db_path)


## Filter to only cases of TB of interest 
## Note: the  full database contains some enrollees with other TB codes

tb_dx_codes <- list(icd9_codes = children(c("010","011","012","018")) %>% unique(),
                    icd10_codes = children(c("A15","A19")) %>% unique())

## Gather all visit keys containing the specific TB diagnosis codes
tb_keys <- build_dx_indicators_delay(dx_list = tb_dx_codes, db_con = db_tb,
                                         db_path = db_path,
                                         return_keys_only  = TRUE)

## Subset TB timemap to cases of interest
tb_time_map <- tb_keys %>% 
  inner_join(tb_time_map, by="key") %>% 
  distinct(enrolid) %>% 
  inner_join(tb_time_map, by="enrolid")

## Create a list of TB drugs of interest
tb_drugs <- c("Isoniazid","Rifampicin","Rifampin","Pyrazinamide","Ethambutol")

## Identify the ndc's associated with the drugs of interest
tb_drug_codes <- tibble(drug_name=tb_drugs) %>%
  mutate(ndc=map(drug_name,~get_ndc_from_name(.,return_all = F))) %>%
  unnest()

## Gather all drug visit for a specific database of interest
tb_drug_visits <- gather_rx_data(ndc_codes = unique(tb_drug_codes$ndcnum),
                                 db_path = db_path) %>% 
  filter(setting=="inpatient") # this step is just to remove duplicates could 
                               # also filter to just outpatient


## Create a time map of TB drug visits
tb_drug_time_map <- tb_drug_visits %>%
  unnest() %>%
  inner_join(tb_drug_codes,by = "ndcnum") %>%
  select(-ndcnum) %>%
  mutate(dx=1L) %>%
  distinct() %>%
  spread(key=drug_name,value=dx) %>%
  mutate(ccae=ifelse(source=="ccae",1L,0L)) %>%
  select(-source) %>%
  mutate(key=NA,admdate=svcdate,source="Drug_visit",stdplac=NA) %>%
  rename(disdate=svcdate) %>%
  mutate(tb_rx=1L) %>% 
  mutate(Isoniazid_Rifampin=ifelse(Isoniazid==1 & Rifampin==1,1L,0L)) %>% 
  select(-setting) %>% 
  mutate(enrolid = as.integer(enrolid))

## Filter drug time_map to patients of interest
tb_drug_time_map <- tb_time_map %>% 
  distinct(enrolid) %>% 
  inner_join(tb_drug_time_map, by = "enrolid")


## Bind dx and drug visit time maps
tb_time_map <- bind_rows(tb_time_map,tb_drug_time_map) %>%
  mutate_at(vars(Ethambutol:Isoniazid_Rifampin),funs(ifelse(is.na(.),0L,.))) %>%
  mutate(ind = 1L) %>% distinct() %>%
  spread(key = source , value = ind) %>% 
  mutate_at(vars(ED, inpatient, outpatient, Drug_visit),~ifelse(is.na(.),0L,.)) %>% 
  mutate(outpatient = ifelse(inpatient == 0 & ED == 0 & Drug_visit == 0, 1, 0)) %>% 
  arrange(enrolid,admdate,inpatient, ED, outpatient)

## Add tb indicator ("dx_visit")
time_map <- tb_time_map %>%
  left_join(tb_keys %>%
              distinct(key) %>%
              mutate(dx_visit=1L), by = "key") %>%
  mutate(dx_visit=ifelse(is.na(dx_visit),0L,dx_visit))

## Compute time to first TB diagnosis and time since first visit
time_map <- time_map %>%
  group_by(enrolid) %>%
  arrange(enrolid,admdate,inpatient, ED, outpatient) %>%
  mutate(first_dx=ifelse(cumsum(dx_visit)==1 & dx_visit==1,1L,0L)) %>%
  mutate(days_since_dx=admdate-max(admdate*first_dx))%>%
  mutate(days_since_first=as.integer(admdate-min(admdate))) %>%
  mutate(visit_no=row_number()) %>%
  ungroup()

## Find first rx visit and time since first rx
time_map <- time_map %>%
  group_by(enrolid) %>%
  arrange(enrolid,admdate,inpatient, ED, outpatient) %>%
  mutate(any_rx=any(tb_rx==1)) %>%
  mutate(first_rx=ifelse(cumsum(tb_rx)==1 & tb_rx==1,1L,0L)) %>%
  mutate(days_since_rx=admdate-max(admdate*first_rx)) %>%
  ungroup()

## Filter dataset to first TB diagnosis and create setting variable
first_dx <- time_map %>% 
  filter(first_dx==1) %>% 
  mutate(first_dx_source = ifelse(inpatient == 1, "inpatient", 
                                  ifelse(outpatient == 1, "outpatient", 
                                         ifelse(ED == 1, "ED", NA)))) %>% 
  select(enrolid,first_dx=admdate, first_dx_source, stdplac, key)

## Get continuous enrollment information for each patient
enrollment_periods <- gather_collapse_enrollment(enrolid_list = first_dx$enrolid,
                                          vars = c(),
                                          db_path = db_path) 

## Identify period of first enrollment and compute duration of prior enrollment
enroll_first <- enrollment_periods %>% 
  group_by(enrolid) %>% 
  summarise(first_enroll=min(dtstart)) %>% 
  mutate(enrolid = as.integer(enrolid))

first_dx <- first_dx %>% 
  inner_join(enroll_first) %>% 
  mutate(prior_enroll=first_dx-first_enroll)

## Identify patients with both Isoniazid and Rifampin, Pyrazinamide, or ethambutol
first_treat <- time_map %>% 
  filter(Isoniazid_Rifampin ==1 | Pyrazinamide == 1 | Ethambutol==1) %>% 
  group_by(enrolid) %>% 
  summarise(first_treat=min(admdate))

## Get time of first relavent medication
first_dx <- first_dx %>% 
  inner_join(first_treat)

# Build cohort with validation of TB meds between -365 and 365 days prior/after index
final_tb_cohort <- first_dx %>% 
  filter(prior_enroll>=365) %>% 
  mutate(time_since_dx=first_treat-first_dx) %>% 
  filter(between(time_since_dx,-365,365)) %>% 
  mutate(tb_index_date=ifelse(first_treat<first_dx,first_treat,first_dx)) %>% 
  mutate(first_dx_source = ifelse(tb_index_date != first_dx, "Drug_visit", first_dx_source)) %>% 
  rename(first_tb_dx=first_dx,time_to_treat=time_since_dx) %>% 
  select(enrolid,first_tb_dx,first_enroll,first_treat, time_to_treat, tb_index_date, first_dx_source, stdplac, key)


