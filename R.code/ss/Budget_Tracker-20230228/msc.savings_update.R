#### msc.savings_update
## Fix dt_log_budget so that budget_category = 'Bank - Savings', calculates
  # all other columns
## REQUIRES: N/A
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 08/02/2022

## Load packages and dt_log_budget
source('R.code/load_tables.R')


dt_log_budget.temp <- dt_log_budget %>% 
  filter(budget_group_id == 400) %>% 
  rowwise() %>% 
  mutate(limit = replace_na(limit, 0),
         budget_diff = sum(limit, amount))

dt_log_budget <- rows_upsert(dt_log_budget, dt_log_budget.temp, 
                                   by = c('date', 'budget_group_id'))

source('R.code/save_log_rds.R')

rm(list = ls())

source('R.code/calc.refresh.cumulative_sums.R')