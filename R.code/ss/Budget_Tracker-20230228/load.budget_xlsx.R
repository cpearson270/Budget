#### load_budget_xlsx
## Load rt_budget from budget.xlsx
## REQUIRES: NA
## Project: Budget Tracker 4.0 - 26/02/2022 - Moved from excel tracker to R
## Last update: 16/04/2022 - added budget version value and removed budget check


library(readxl)
library(janitor)

df_expense_groups <- tribble(
  ~expense_group_id, ~expense_group,
  0,                 "Income - Salary & Wages", 
  100,               "Essentials",
  200,               "Non-Essentials",
  300,               "Calculated Savings",
  400,               "Bank Savings",
  900,               "Miscellaneous")

### Function - Clean up budget imported from excel
budget_import_clean <- function(x) {
  x %>% 
  remove_empty(which = c("rows", "cols")) %>% 
    clean_names("snake") %>% 
    mutate(budget_group_id = floor(category_id),
           budget_group =
             category[match(budget_group_id, category_id)],
           expense_group_id = floor(category_id/100)*100,
           expense_group = df_expense_groups$expense_group[
             match(expense_group_id, df_expense_groups$expense_group_id)],
           limit = as.numeric(limit),
           period = as.numeric(period)) %>% 
    filter(!category_id %in% c(100,200,900))
}

repeat {
  
  path_budget<-"R.data/Budget"
  
  file_budget<-list.files(path_budget) %>%
    last()
  
  if (endsWith(file_budget,'.csv')) { 
    v.budget_version <- stringr::str_sub(file_budget, start = 8, end = -5)
    rt_budget<-read_csv(file.path(path_budget,file_budget)) %>%
              budget_import_clean()

  } else {
    v.budget_version <- stringr::str_sub(file_budget, start = 8, end = -6)
    rt_budget<- read_excel(file.path(path_budget,file_budget),
                           na = c("", "NA")) %>% 
              budget_import_clean()
  }

  check_budget<- rt_budget %>% filter(category_id < 100 & category_id != 0)
  
  if (nrow(check_budget) == 0) {
    break }
  
  else if (nrow(check_budget)>0) {
    print("ERROR: Invalid Category ID")
    print(check_budget)
    print("Correct category ID and reload budget from Excel")
    budget_reload<-readline(prompt = 
                      "Type 'Y' to attempt reload, type 'N' to cancel: ")
  }
  
  if (budget_reload == "Y"|| budget_reload == "y") {
  }
  else if (budget_reload == "N"|| budget_reload == "n") {
    break
    rm(Budget_reload)
  }
}

rm(check_budget, file_budget, path_budget, df_expense_groups)
