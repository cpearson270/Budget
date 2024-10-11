#### msc.limit_update
## Fix dt_log_budget and rt_budget so that limit = NA in historic records 
    #where previously limit = 0
## REQUIRES: N/A
## Project: Budget Tracker 4.0 - 26/02/2022 - Moved from excel tracker to R
## Last update: 26/02/2022

## Load packages and dt_log_budget
source('R.code/load_tables.R')


temp.dt_log_budget <- dt_log_budget %>% 
  filter(budget_group_id %in% c(902, 903, 400)) %>% 
  mutate(limit = replace(limit, limit == 0, NA))

dt_log_budget <- rows_upsert(dt_log_budget,
                             temp.dt_log_budget,
                             by = c("date", "budget_group_id")
)

source('R.code/save_log_rds.R')

#load and fix rt_budget
source('R.code/fun_main.R')
rt_budget <- load_recent_rds('rt_budget')

temp.rt_budget <- rt_budget %>% 
  filter(category_id %in% c(902, 903, 400)) %>% 
  mutate(limit = replace(limit, limit == 0, NA))

rt_budget <- rows_upsert(rt_budget,
                             temp.rt_budget,
                             by = "category_id"
)

source('R.code/save_budget_rds.R')
