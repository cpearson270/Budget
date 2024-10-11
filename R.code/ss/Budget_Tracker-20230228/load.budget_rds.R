#### load.budget_rds
## load most recent budget .rds file
## REQUIRES: NA
## Project: Budget Tracker 4.0 - 09/01/2022 - Moved from excel tracker to R
## Last update: 14/03/2022

### Load library and function
library(tidyverse)
source("R.code/fun_main.R")

### Load most recent budget
rt_budget <- load_recent_rds("rt_budget")

## likely required, 'load.budget' (this script) not currently used in main script
#v.budget_version <- str_sub(name_recent_rds("rt_budget"), 
#                           start = 11, end = 16)


## Create rt_budget subset excluding non-budget categories
  #added to main budget tracker script, removed from 'load_tables.R'
#df_budget <- rt_budget %>%
#  filter(category_id==budget_group_id) %>% 
#  select(-c(category_id, category))