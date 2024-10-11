#### calc.refresh.cumulative_sums
## Refresh the cumulative balance and cumularive average colums in dt_log_budget
## REQUIRES: N/A
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 07/02/2022

### STARTER SCRIPT ###

### Load initial functions and packages
library(tidyverse) 
library(lubridate)

source('R.code/fun_main.R')

### Load User Values
## set transactions month to start refresh at
  v.trans_file_date<- readline(prompt = 
      "Start month, first month to be refreshed (default = '202101'):")
  
if(v.trans_file_date=="") {
  v.trans_file_date <- 202101
}
  
trans_file_date <- rollforward(ym(v.trans_file_date))

### Load budget reference table, records logs, and rt_budget subset
rt_budget <- load_recent_rds('rt_budget')
source('R.code/load.tables.R')

## Create rt_budget subset excluding non-budget categories
df_budget <- rt_budget %>%
  filter(category_id==budget_group_id) %>% 
  select(-c(category_id, category))

### Create for loop
list.date <- unique(dt_log_budget$date[dt_log_budget$date>=trans_file_date])

for (i in 1:length(list.date)) {

trans_file_date <- list.date[i]
  
### pull records to be updated from dt_log_budget
ds_trans_budget <- dt_log_budget %>% 
  filter(date == trans_file_date) %>% 
  mutate(expense_group_id = floor(budget_group_id/100)*100,
         expense_group = 
           rt_budget$expense_group[match(expense_group_id, 
                                         rt_budget$expense_group_id)]) %>% 
  select(-c(cum_balance, cum_average, write_off_count, yr_average))

### Category ids of write offs, for calc.savings
v.write_off <- ds_trans_budget$budget_group_id[
                  ds_trans_budget$diff_flag %in% c(130,230)]

## calculate cumulative columns and savings

### calculate balance and average spend for each budget category with new ####
# data included
# check through other categories

source("R.code/calc.cumulative_sums.R") #(332 -431)

### combine this month's data with all previously recorded data

dt_log_budget <- rows_upsert(dt_log_budget,
                             ds_trans_budget %>% 
                               select(-c(expense_group_id, expense_group#, 
       #long term col doesnt exist here, can it be removed earlier in the main
                    #Budget tracker script?
                 #long_term
                               )),
                             by = c("date", "budget_group_id")
)
source('R.code/calc.savings.R')

}

source('R.code/save_log_rds.R')

### END OF SCRIPT ###