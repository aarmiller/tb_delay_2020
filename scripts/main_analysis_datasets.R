
##### This script contains the basic R script that was used to generate datasets
##### for simulations and for the logistic regression analysis
##### NOTE: This script contains a combination of actual and psuedo code. 

# install delayDX and icdplus package from github repository
# devtools::install_github("aarmiller/delayDX")
# devtools::install_github("a1arakkal/icdplus")

## Load Libraries
library(dplyr)
library(icdplus)
library(RSQLite)
library(delayDX)
library(bit64)
library(lubridate)
library(stringr)
library(parallel)


## Set days before index cutoff to consider miss
days_before <- 3L

## Load the required datasets
# Database path
db_path <- "/path"
# Connect to database
db_con<- src_sqlite(db_path)
# Load all dx dataset
all_dx <- readRDS("/path")
# Load all drx dataset
all_rx <- readRDS("/path")
# Load dataset created from make_final_tb_cohort.R
load("/path")
# Load SSD list
ssds_codes <- read_rds("/path")

## Compute time before index for final_tb_cohort dataset
index_data <- final_tb_cohort %>%
  mutate(time_before_index = tb_index_date - first_enroll) %>% 
  select(enrolid, index_date = tb_index_date, enroll_first = first_enroll,
         time_before_index, first_dx_source, stdplac, key)

# Select enroilids that were enrolled >356 days prior to index
enrolled_ge_1year <- index_data %>% filter(time_before_index >= 365) %>% 
  distinct(enrolid) %>% .$enrolid
index_data <- index_data %>% filter(time_before_index >= 365)
dx_data <- all_dx %>% filter(enrolid %in% enrolled_ge_1year) %>% 
  rename(days_since_dx=days_since_index) %>% 
  filter(between(days_since_dx,-365,0))
rx_data <- all_rx %>% inner_join(index_data %>% select(enrolid, index_date)) %>% 
  mutate(days_since_rx = rx_date - index_date) %>% 
  filter(between(days_since_rx,-365,0)) %>% select(enrolid, rx, days_since_rx)


## Build a time map for a specific database of interest. Returns 
## A tibble with a time map containing visit level information from the database. 
## Includes admission date, year, setting, enrolid, standard place of care, key, 
## discharge date, and location of care (i.e. inpatient, outpatient, or ED)

tb_time_map <-  build_time_map_delay(db_con = db_tb, db_path = db_path)
gc()

## Subset TB timemap to cases of interest
tb_time_map <- tb_time_map %>% inner_join(index_data %>% 
                                            distinct(enrolid, index_date)) 
## Add first_tb_dx visit
tb_time_map <-  tb_time_map %>% left_join(index_data %>% 
                                      select(enrolid, admdate = index_date,
                                             source = first_dx_source, 
                                                      stdplac, key) %>% 
                                      mutate(first_tb_dx = 1)) %>% 
                mutate(first_tb_dx = ifelse(is.na(first_tb_dx), 0, 
                                            first_tb_dx))

## Create inpatient, ED, and outpatient indicators
tb_time_map <- tb_time_map %>% mutate(ind = 1L) %>% distinct() %>%
  spread(key = source , value = ind) %>% 
  mutate_at(vars(ED, inpatient, outpatient),~ifelse(is.na(.),0L,.)) 

## Compute time to first tb diagnosis and time since first visit
time_map <- tb_time_map %>%
  group_by(enrolid) %>%
  arrange(enrolid, admdate) %>%
  mutate(days_since_dx= admdate - index_date) %>% 
  mutate(visit_no=row_number()) %>%
  ungroup()


## Add indicator for individual ssd categories and any_ssd composite
## variable for each visit in tb time map
ssd_inds <- build_dx_indicators_delay(condition_dx_list = ssds, 
                                      db_path = db_path,
                                    db_con = db_con) %>%
    rename(any_ssd=any_ind)


## Make final timemap with condition indicators and 
## extract names of the condition indicators
ind_names <- ssd_inds %>%
  select(-key) %>%
  names()

## Add indicators to time_map
final_time_map <- time_map %>%
  left_join(ssd_inds, by="key") %>%
  mutate_at(vars(ind_names),.funs = list(~ifelse(is.na(.),0L,.)))

## Filter timemap to period of interest before diagnosis 
final_time_map <- final_time_map %>% 
  filter(between(days_since_dx,-365,0)) 

## Get distinct visit (distinct enrolid, days_since_dx, std_place). 
## If on a given "distinct" visit, if it was labeled as both  outpatient and ED, 
## it should be labeled as an ED visit and not an outpatient visit.
vars_to_summarise <- c("inpatient", "ED", "first_tb_dx", ind_names)
  
final_time_map <- final_time_map %>% 
    group_by(enrolid, days_since_dx, stdplac) %>% 
    summarise_at(vars(vars_to_summarise),.funs = list(~max(.))) %>%
    ungroup() %>% 
    mutate(outpatient = ifelse(inpatient == 0 & ED == 0 , 1, 0)) %>% 
    mutate(all_visits = 1)


### Change point identification ###
## Get counts by period
## Create time map where date is shifted to reflect start date
time_map_data <- prep_sim_data(final_time_map %>% rename(ed = ED),
                                               event_name = "any_ssd", 
                                               cp_method = "lm_cube", 
                                               start_day = 3L,
                                               by_days = 1L)

final_prior_visit_counts <- tibble(cond = "miss_ind") %>% 
  mutate(counts=map(cond,~count_prior_events_truven(time_map_data$time_map, 
                                                     event_name = .,
                                                     start_day = 0L,
                                                     by_days = 1L))) %>% 
  unnest(counts) %>% 
  filter(low<365) %>% 
  nest(period:frac_miss)


table <- final_prior_visit_counts %>% mutate(model= "lm_cube") 

## Run find change point function
results <- table %>% 
  mutate(out=map2(data, model,~find_change_point(data = .x, method=.y,
                                                 var_name = "n_miss_visits")))

results <- results %>% mutate(miss_stats=map(out,~.$miss_bins %>%
                       summarise(miss_visits_est = sum(num_miss), 
                                 miss_visits_obs = sum(Y - pred1)))) %>%
  unnest(miss_stats)

## Extract change point and number of missed visits
final_results <- results %>% 
  mutate(change_point=map(out, ~.$change_point$mid)) %>% 
  mutate(n_miss_visits=map(out, ~.$miss_stats$miss_visits_est)) %>% 
  unnest(change_point, n_miss_visits)

## Identify change point
change_point <- final_results %>% .$change_point
change_point <- change_point * -1

##  Change days before to negative number
days_before <- days_before* -1

### Add demographic information ###
## Get demographic data 
demo_data <- gather_enroll_data(enrolid_list = final_time_map %>% 
                                               distinct(enrolid) %>% .$enrolid,
                                vars = c("dobyr","sex","emprel"),
                                db_path = db_path)
## Get collapsed enrollement information
enroll_collapsed <- gather_collapse_enrollment(enrolid_list = final_time_map %>% 
                                                              distinct(enrolid) %>% 
                                                              .$enrolid,
                                        vars = c("egeoloc", "msa", 
                                                 "plantyp","indstry"),
                                        db_con = db_con)

## Merge collapsed enrollment data with index dataset
demo1 <- enroll_collapsed %>% mutate(enrolid = as.integer(enrolid)) %>% 
  inner_join(index_data %>% select(enrolid, index_date))

demo2 <- demo1%>% filter(index_date<=dtend & index_date>=dtstart) %>% 
  select(enrolid, egeoloc, msa, plantyp, indstry)

## Merge with demographic  data and create urban/rural indicator
demographics <- demo_data %>% mutate(enrolid = as.integer(enrolid)) %>% 
  left_join(demo2) %>% inner_join(index_data %>% select(enrolid, index_date)) %>%
  mutate(age=year(as_date(index_date))-dobyr) %>% 
  mutate(msa_missing=ifelse(is.na(msa), 1, 0), 
         urban= ifelse(msa==0, 0, ifelse(is.na(msa), NA,1)))

### Add indicators fluoroquinolones ###
## Create a list of fluoroquinolones drugs of interest
Fluro_drugs <- c("Levofloxacin","Moxifloxacin")

## Identify the ndc's associated with the drugs of interest
fluro_codes <- tibble(drug_name=Fluro_drugs) %>%
  mutate(ndc=map(drug_name,get_ndc_from_name)) %>%
  unnest() %>% distinct(ndcnum) %>% .$ndcnum

## Gather all drug visit for a specific database of interest.
## Identify fluoroquinolones visits that occured during delay window and
## create indicator.
fluro_visits <- rx_data %>% filter(rx %in% fluro_codes)%>% 
  filter(between(days_since_rx, change_point, days_before)) %>% 
  distinct(enrolid) %>% mutate(fluro_cp_DaysBeforeIndex = 1)

## Add fluoroquinolones during delay window indicator to demographics dataset
demographics <- demographics %>% left_join(fluro_visits) %>% 
  mutate(fluro_cp_DaysBeforeIndex = ifelse(is.na(fluro_cp_DaysBeforeIndex), 0L, 
                                           fluro_cp_DaysBeforeIndex))

### Asthma and COPD indicators ###
## Identify visit where asthma was coded, filter to visits that occured
## prior to change point and create indicators by enrolid
asthma <- dx_data %>% filter(dx %in% c(asthma$icd9_codes,asthma$icd10_codes)) %>%
  filter(days_since_dx< change_point & days_since_dx >=-365) %>% 
  distinct(enrolid) %>% mutate(asthma_365_cp = 1L) 

## Add asthma prior to change point indicator to demographics dataset
demographics <- demographics %>% left_join(.,asthma) %>% 
  mutate(asthma_365_cp = ifelse(is.na(asthma_365_cp), 0, asthma_365_cp))

## Identify visit where COPD was coded, filter to visits that occured
## prior to change point and create indicators by enrolid
copd <- dx_data %>% filter(dx %in% c(copd$icd9_codes, copd$icd10_codes)) %>% 
  filter(days_since_dx< change_point & days_since_dx >=-365) %>% 
  distinct(enrolid) %>% mutate(copd_365_cp=1L) 

## Add COPD prior to change point indicator to demographics dataset
demographics <- demographics %>% left_join(.,copd) %>% 
  mutate(copd_365_cp = ifelse(is.na(copd_365_cp), 0, copd_365_cp))

## Add indicators for chest xray and CT 

## Create a list of chest ct and xray CPT codes
chest_ct <-c("71260", "71250", "71270")
chest_xray <- c("71010", "71015", "71020", "71021", "71022", "71023",
                "71030", "71034", "71035", "71101", "71111", "71120",
                "71045", "71046", "71047", "71048")

## Gather visits with the relevant cpt codes
chest_cpt_visits <- gether_proc_data(cpt_codes = c(chest_xray, chest_ct), 
                                     db_con = db_con)

## Create time map for chest xray and CT 
chest_cpt_time_map <- chest_cpt_visits %>% 
  unnest() %>%
  mutate(cpt_type = ifelse(proc1 %in% chest_ct, "chest_ct", "chest_xray")) %>%
  select(-proc1, -setting, -source, -year) %>% 
  mutate(dx=1L) %>%
  distinct() %>%
  spread(key=cpt_type,value=dx) %>%
  mutate(admdate=ifelse(is.na(admdate), svcdate, admdate)) %>% 
  select(enrolid, admdate, chest_ct, chest_xray) %>%
  mutate_at(vars(chest_ct, chest_xray),~ifelse(is.na(.),0L,.)) %>%
  mutate(enrolid = as.integer(enrolid)) %>% 
  inner_join(index_data %>% select(enrolid, index_date)) %>% 
  mutate(days_since_dx = admdate - index_date) %>% 
  select(-index_date)

## Create indicator for chest xray -365 and change point
chest_xray_prior_cp <- chest_cpt_time_map %>% filter(chest_xray == 1) %>% 
  filter(days_since_dx < change_point & days_since_dx >=-365) %>% 
  distinct(enrolid) %>% 
  mutate(chest_xray_prior_cp = 1L)

## Add chest xray -365 and change point indicator to demographics dataset
demographics <- demographics %>% left_join(chest_xray_prior_cp) %>% 
  mutate(chest_xray_prior_cp = ifelse(is.na(chest_xray_prior_cp), 0L, 
                                      chest_xray_prior_cp))

## Create indicator for chest ct -365 and change point
chest_ct_prior_cp <- chest_cpt_time_map %>% filter(chest_ct == 1) %>% 
  filter(days_since_dx < change_point & days_since_dx >=-365) %>% 
  distinct(enrolid) %>% 
  mutate(chest_ct_prior_cp = 1L)

## Add chest ct -365 and change point indicator to demographics dataset
demographics <- demographics %>% left_join(chest_ct_prior_cp) %>% 
  mutate(chest_ct_prior_cp = ifelse(is.na(chest_ct_prior_cp), 0L, 
                                    chest_ct_prior_cp))

### Create regression dataset ###

## Collapse visits that occured in a given day
final_time_map <- final_time_map %>% select(-stdplac) %>% 
  group_by(enrolid, days_since_dx) %>% 
  summarise_all(list(max)) %>% ungroup()

## Compute age, year, month, day, and weekday variables
final_time_map_w_demo <- final_time_map %>% 
  inner_join(demographics) %>% 
  mutate(admdate =as_date(index_date + days_since_dx))%>%
  mutate(age_cat=cut(age,breaks = c(-1,17,35,45,55,65,130))) %>%  
  mutate_at(vars(sex),funs(as.factor)) %>% 
  mutate(year = year(admdate)) %>% 
  mutate(emprel=as.factor(emprel)) %>% 
  mutate(day=as.numeric(admdate),
         year.factor=as.factor(year(admdate)),
         year=year(admdate),
         month.factor=as.factor(month(admdate)),
         month=month(admdate),
         weekday= as.factor(weekdays(admdate))) 

# Create the 7 different health care setting categories
final_time_map_w_demo <- final_time_map_w_demo %>% 
  mutate(setting_cat = ifelse(inpatient == 1 & outpatient == 1 & ED == 1, "All Three",
                              ifelse(inpatient == 1 & outpatient == 0 & ED == 0, "Inpatient Only",
                                     ifelse(inpatient == 0 & outpatient == 1 & ED == 0, "Outpatient Only",
                                            ifelse(inpatient == 0 & outpatient == 0 & ED == 1, "ED Only",
                                                   ifelse(inpatient == 1 & outpatient == 1 & ED == 0, "Inpatient and Outpatient", 
                                                          ifelse(inpatient == 1 & outpatient == 0 & ED == 1, "Inpatient and ED",
                                                                 ifelse(inpatient == 0 & outpatient == 1 & ED == 1, "Outpatient and ED", NA)))))))) %>% 
  mutate(setting_cat = as.factor(setting_cat))


## Load in CDC ILI dataset
ili_data <- read_rds("/path")

## If weighted ili is = 0 then change values to min value for the given observations 
## year weighted ili excluding 0
ili_data <- ili_data %>% mutate(weighted_ili= pmap(list(year, weighted_ili),
                                                   ~ifelse(..2==0, min(ili_data %>% 
                                                                         filter(weighted_ili != 0 & year == ..1) %>% 
                                                                         .$weighted_ili), ..2))) %>% 
  unnest(weighted_ili)

## Compute weighted ili for a given day
final_time_map_w_demo <- final_time_map_w_demo %>% distinct(admdate) %>%
  mutate(mmwr=map(admdate,~MMWRweek::MMWRweek(.))) %>%
  unnest() %>%
  rename(year=MMWRyear,
         week=MMWRweek) %>% select(-MMWRday) %>%
  inner_join(., select(ili_data,year,week,weighted_ili)) %>%
  select(admdate, weighted_ili) %>%
  inner_join(.,final_time_map_w_demo)

## Get misses and non miss visits 
miss_cases <- final_time_map_w_demo %>% 
  filter(any_ssd == 1, 
         between(days_since_dx, change_point, days_before)) %>% 
  mutate(miss=1L)

non_miss_cases <- final_time_map_w_demo %>% 
  filter(first_tb_dx == 1) 

## Find enrolids that had no visit assoicated with their index date 
no_index_visits <- final_time_map_w_demo %>% 
  filter(!enrolid %in% unique(non_miss_cases$enrolid)) %>% distinct(enrolid)

## Find closest visit prior to index for enrolids that had no visit assoicated 
## with their index date and label these as the index visit
closest_visit_no_index_vis <- final_time_map_w_demo %>% 
  filter(enrolid %in% no_index_visits$enrolid) %>% 
  group_by(enrolid) %>% filter(days_since_dx == max(days_since_dx)) %>% 
  ungroup() %>% 
  distinct()

## If closest visit is a SSD visit, remove it form the miss cases and lable it 
## as a non-missed case
miss_cases1 <- suppressMessages(miss_cases %>% 
                                  anti_join(., closest_visit_no_index_vis %>% 
                                              distinct(enrolid, days_since_dx,
                                                       inpatient, ED, outpatient,
                                                       any_ssd)))

## Bing non_miss_cases and closest visit prior to index for enrolids that 
## had no visit assoicated with their index date
non_miss_cases1 <- bind_rows(non_miss_cases, closest_visit_no_index_vis) %>% 
  mutate(miss = 0L)

## Create regression data 
fin_reg_data_dichoto <- bind_rows(miss_cases1,non_miss_cases1)  

