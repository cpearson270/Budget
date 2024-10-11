#### fun.load (formerly fun_main)
## create functions required for main .R script
## REQUIRES: NA
## Project: Budget Tracker 4.0 - 26/02/2023 - Moved from excel tracker to R
## Last update: 26/02/2023

### select and load most recent .RDS file in the 'R.code/ref_data' sub-folder 
  #with same name as file ####

load_recent_rds <- function(file_name, 
                            version = c("."), 
                            sub_folder = "R.code/ref_data") {
  
  path <-paste(sub_folder, file_name, sep = "/")
  
  file <-list.files(path) %>%
    str_subset(".rds") %>%
    str_subset(version) %>%
    last()
  
  readRDS(file.path(path,file))
  
}

### get file name of most recent .RDS file in the 'R.code/ref_data' sub-folder 
    #with same name as file ####
name_recent_rds <- function(file_name, sub_folder = "R.code/ref_data") {
  
  path <-paste(sub_folder, file_name, sep = "/")
  
  file <-list.files(path) %>%
    str_subset(".rds") %>% 
    last()
}

### get file name of most recent file including file extension in the 
  #'R.data/transactions' sub-folder
name_file <- 
  function(path = "R.data/transactions", filter, n=-1L) {
  
  file <-list.files(path) %>%
    str_subset(pattern = filter) %>% 
    nth(n=n)
}

#### load commbank .csv transactions
load_file_comm.csv <- 
  function (file_name, path = "R.data/transactions") 
df_trans_load<-read_csv(file.path(path,file_name),
                        col_names = c("date", "amount", 
                                      "description", "balance")) %>%
  select(!"balance") %>% 
  # dates stored as text in .csv
  mutate(date = lubridate::dmy(date),
         description = str_to_lower(description)) %>%
  janitor::remove_empty(which = c("rows", "cols"))

### Parent group columns calculation ####

## calculates budget group and expense group colums (names and ids) using
#category_id column


# below doesnt work if category id is a new budget group 
#(ie not already in budget)

#calculate_ParentCols <- function(df) {
#  mutate(df,
#         budget_group_id = floor(category_id),
#         expense_group_id = floor(category_id/100)*100,
#        expense_group = 
#           rt_budget$expense_group[match(expense_group_id, 
#                                         rt_budget$expense_group_id)]) %>% 
#  rowwise() %>% 
#  if (is.na(df$budget_group)) {
#    mutate(df,
#         budget_group =
#           rt_budget$budget_group[match(budget_group_id, 
#                                        rt_budget$budget_group_id)])} %>% 
#  ungroup()
#}
