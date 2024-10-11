####Budget Tracker 4.0 - 05/01/2022 - create log tables

library(tidyverse) #library(lubridate) not loaded, use "lubridate::"

#dt_sum.budget <- tibble(
#  date = date(),
#  budget_group_id = numeric(),
#  budget_group = character(),
#  sum = numeric(),
#  limit = numeric(),
#  budget_diff = numeric(),
#  diff_flag = numeric()
#)

### Run 'Budget_Tracker_4' first until error

#dt_sum.budget <- df_transactions_sum_budget %>% 
#  select(-c(expense_group_id, expense_group))

#saveRDS(dt_sum.budget,
#        paste("R.code/ref_data/tables/dt_sum.budget/dt_sum.budget-",
#                         format(Sys.time(), "%Y%m%d-%H%M%S"),
#                         ".rds",
#                         sep=""))


### create dt_log_budget from .xlsx and save as .rds

source("R.code/load_budget_xlsx.R")

#source("R.code/fun_main.R") #below function removed from fun_main.R

## selects and loads most recent excel file from the sub-folder of the same name


load_recent_excel <- function(file_name, sub_folder = "R.data") {
  
  path <-paste(sub_folder, file_name, sep = "/")
  
  file <-list.files(path) %>%
    last()
  
  read_excel(file.path(path,file))
  
}

dt_log_budget <- load_recent_excel("dt_log_budget")

dt_log_budget <- dt_log_budget %>% 
  inner_join(rt_budget %>% 
              select(category, category_id),
            by = "category") %>% 
  rename(budget_group = category,
         budget_group_id = category_id) %>% 
  relocate(budget_group_id, .after = date)

saveRDS(dt_log_budget, paste("R.code/ref_data/dt_log_budget/dt_log_budget-",
                         format(Sys.time(), "%Y%m%d-%H%M%S"),
                         ".rds",
                         sep=""))
