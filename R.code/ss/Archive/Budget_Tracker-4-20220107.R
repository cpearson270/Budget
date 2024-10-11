####Budget Tracker 4.0 - 05/01/2022 - Moved from excel tracker to R

### Load User Values

## Update budget or use previous?

budget_update<-readline(prompt = "Update budget from Excel? (Y/N):")

#add line to convert any answer to lower case and then check for acceptable 
# input; able to remove some 'if' conditions below

## load most recent transactions or specific month?

repeat {
  trans_file_date<- readline(prompt = 
                 "Transaction file for import ('recent' or date eg. '202107'):")

  if(trans_file_date=="") {
    } else { break}
    }

### Load initial functions and packages
## Load main package: tidyverse

library(tidyverse) #library(lubridate) not loaded, use "lubridate::"

source("R.code/3.main_functions.R")

###Load Budget

if (budget_update == "Y"|| budget_update == "y") {
  
  ##Reference to separate RScript (below code) if new budget
  library(readxl)
  library(janitor)
  
    repeat {
    
    path_budget<-"R.data/Budget"
    
    file_budget<-list.files(path_budget) %>%
    last()
  
    rt_budget<- read_excel(file.path(path_budget,file_budget)) %>%  
#Clean up budget imported from excel
    remove_empty(which = c("rows", "cols")) %>% 
    clean_names("snake") %>% 
    mutate(budget_group_id = floor(category_id),
           budget_group =
             category[match(budget_group_id, category_id)],
           expense_group_id = floor(category_id/100)*100,
           expense_group = 
             category[match(expense_group_id, category_id)]) %>% 
    filter(!category_id %in% c(100,200,900))
        
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
        
    rm(check_budget)
  
    saveRDS(rt_budget, paste("R.code/ref_data/budget/Budget-",
                              format(Sys.time(), "%Y%m%d-%H%M%S"),
                              ".rds",
                              sep=""))
        
    rm("file_budget","path_budget")

} else if (budget_update == "N" || budget_update == "n") {        

##Load existing budget from RDS file

  file_budget<-list.files("R.code/ref_data/budget") %>%
            last()
  rt_budget<-readRDS(paste("R.code/ref_data/budget",
                              file_budget,sep = "/"))
        
  rm("file_budget")
} else {
  stop(
  "ERROR: Unacceptable input for 'Budget Update', must be 'Y' or 'N'"
  )
}

### Load and summarise transactions according to Budget

path_transactions<-"R.data/transactions"
      
## load transactions
 
file_trans.csv<- paste("ALL TRANS_", trans_file_date,".csv", sep="")
file_trans.xlsx<- paste("ALL TRANS_", trans_file_date,".xlsx", sep="")

if (file.exists(file.path(path_transactions,file_trans.csv))) {
  dt_transactions<-read_csv(
      file.path(path_transactions,file_trans.csv)) %>%
    mutate(date = lubridate::dmy(date)) %>% #dates stored as text in .csv
    select(date, description, category, amount, notes) %>% 
    janitor::remove_empty(which = c("rows", "cols"))
  
} else {
  dt_transactions<-readxl::read_xlsx(
      file.path(path_transactions,file_trans.xlsx)) %>% 
        mutate(date = lubridate::as_date(date)) %>% 
#'as_date' because .xlsx dates were converted to number of days since 1970-01-01
    select(date, description, category, amount, notes) %>% 
    janitor::remove_empty(which = c("rows", "cols"))
}
  
rm("file_trans.csv", "file_trans.xlsx", "path_transactions")

## Summarise transactions: find total of spending in each category and assign 
#      categories with category ID from rt_budget      
    

repeat {
        
  dt_transactions_sum<- dt_transactions %>% 
    group_by(category) %>% 
    summarise(sum = sum(amount)) %>% 
    ungroup() %>% 
    left_join(rt_budget %>%    #should mean no 'NA's in 'sum' col
                select(c(category_id,category,
                         budget_group_id, budget_group,
                         expense_group_id, expense_group
                         )), by="category") %>% 
    relocate(category_id,category) %>% 
    arrange(category_id)

## view categories not currently in the budget
  
  df_transactions_sum.missing_categories<-dt_transactions_sum %>% 
          filter(is.na(category_id))
          

  
## INPUT REQUIRED IF NEW CATEGORY
  
  if(nrow(df_transactions_sum.missing_categories)>0) {

## Make avaliable decision information for assigning  missing category
    
    view(df_transactions_sum.missing_categories)
    
    view(rt_budget)
      
    dt_transactions %>% 
        filter(
          category %in% df_transactions_sum.missing_categories$category) %>% 
        view("df_transactions.missing_categories")

## assign category ID if transaction without category ID
      
#Is there a more efficient way to get multiple lines of user input?
    
    nMissing <- length(df_transactions_sum.missing_categories$category)
    
    df_new_budget_category<- tibble(
      category_id = numeric(nMissing),
      category = character(nMissing),
      limit = numeric(nMissing),
      period = numeric(nMissing),
      cycle = character(nMissing),
      long_term = character(nMissing),
      comments = character(nMissing)
    )

    for (i in 1:nMissing) {
      
      print(df_transactions_sum.missing_categories$category[i])
      
      df_new_budget_category$category_id[i] <- 
        readline(prompt = "Assign new category ID:")
      df_new_budget_category$category[i] <- 
        df_transactions_sum.missing_categories$category[i]
      df_new_budget_category$limit[i] <-
        readline(prompt = "Budget limit for new category ($):")
      df_new_budget_category$period[i] <-
        readline(prompt = "Budget period in months:")
      df_new_budget_category$cycle[i] <-
        readline(prompt = "Is this a pre- or post-cycle expese? (Pre/Post):")
      df_new_budget_category$long_term[i] <-
        readline(prompt = "Is this a long term category? (Y/NA):")
      df_new_budget_category$comments[i] <-
        paste("Added CP",
              format(Sys.Date(),"%Y%m%d"),
              readline(prompt = 'Notes (remember to add ":_"):'),
              sep = "")
      
    }
       
# could possibly replace the below with 'add_row()'        
       
    df_new_budget_category<- df_new_budget_category %>% 
        mutate(across(.cols = everything(), ~na_if(.,""))) %>% 
        mutate(across(.cols=c(category_id, limit, period),
                      ~as.double(.))) %>% 
# below doesnt work if category id is a new budget group 
  #(ie not already in budget)
        calculate_ParentCols()

##add new category to the budget

# need to add check to only add unique values
      
      rt_budget<- bind_rows(rt_budget, df_new_budget_category) %>% 
        arrange(category_id)
      
      saveRDS(rt_budget, paste("R.code/ref_data/budget/Budget-",
                            format(Sys.time(), "%Y%m%d-%H%M%S"),
                            ".rds",
                            sep=""))
      
      rm(df_new_budget_category, df_transactions_sum.missing_categories,
         nMissing, i)
      
    } else if(nrow(df_transactions_sum.missing_categories)==0) {
        break
    }
} ##repeat to line 113

### Load previous records

file_dt_sum.budget <-list.files("R.code/ref_data/tables/dt_sum.budget") %>%
  last()
dt_sum.budget<-readRDS(paste("R.code/ref_data/tables/dt_sum.budget",
                             file_dt_sum.budget,sep = "/"))

### calculate balance for each budget category with previous records
# should exclude 'calculated savings'
# check through other categories

source("R.code/2.balance_calc.R")


###run reports for transaction summaries
     
df_budget <- rt_budget %>%
   filter(category_id==budget_group_id) %>% 
   select(-c(category_id, category))


df_transactions_sum_budget<- dt_transactions_sum%>% 
  group_by(budget_group_id) %>% 
  summarise(expense_group_id = unique(expense_group_id),
            budget_group = unique(budget_group),
            expense_group = unique(expense_group),
            amount = sum(sum)) %>% 
  ungroup() %>% 
  right_join(df_budget %>% 
               select(-c(period, cycle, comments)), 
              by = c("expense_group_id", "expense_group",
                     "budget_group_id", "budget_group")) %>% 
  mutate(amount = replace_na(amount, 0)) %>% 
  rowwise() %>% 
  mutate(budget_diff = sum(limit, amount),
         diff_flag = 
           expense_group_id
          +
           (if (is.na(limit) || limit == 0) {NA
           } else if (budget_diff>0) {10
           } else if (budget_diff<0) {20
           } else {NA})
          +
           (if (is.na(limit) || limit == 0) {NA
           } else if (budget_diff == 0) {NA
           } else if (is.na(long_term)) {1
           } else if (long_term == "Y") {2 
           } else {
             stop(
            "ERROR: df_transactions_sum_budget$diff_flag could not be computed"
            ) } )
         ) %>% 
  ungroup() %>% 
  add_column(.before="budget_group_id", 
             date= lubridate::rollforward(
                 lubridate::ym(trans_file_date))) %>% 
  arrange(budget_group_id)

### Mark categories as actual over-spending or actual savings
## Show relevant tables for reference to over-spending

view(df_transactions_sum_budget)

view(dr_budget.balance)

# filter out 'bank-savings' = 400, and msc-fuel = 901

df_transactions_sum_budget %>% 
  filter(budget_diff < 0) %>% 
  View("Over Budget")

# show also current balance of categories

## record relevant categories

print("Mark group as actual over spending. Budget group ids:")
  V.act_over_spend <-scan()
  if (is_empty( V.act_over_spend)) {V.act_over_spend<- NA}

# add check that budget_diff is >/< 0 as appropriate for over spend/savings
  
## Show relevant tables for reference to savings

# remove reference to income
  
df_transactions_sum_budget %>% 
  filter(budget_diff > 0) %>% 
  View("Under Budget")
  
print("Mark group as actual saving. Budget group ids:")
  V.act_save <-scan()
  if (is_empty(V.act_save)) {V.act_save<- NA}

df_transactions_sum_budget <- df_transactions_sum_budget %>% 
  rowwise() %>% 
  mutate(diff_flag = replace(diff_flag, 
                             budget_group_id %in% V.act_save, 
                             expense_group_id + 10),
         diff_flag = replace(diff_flag,
                             budget_group_id %in% V.act_over_spend,
                             expense_group_id + 20)
         ) %>% 
  ungroup()

rm(file_dt_sum.budget, V.act_over_spend, V.act_save)


### UNDER CONSTRUCTION
### Mark write-offs and write-ups

#identify category

#calculate the balance for that category up to the trans_file_date

#add row to 'dt_sum.budget' and ? df_transactions_sum_budget ? 
  #for identified category, 
  #with date = floor(trans_file_date) +1,
  #amount = -(calculated balance)

### END OF CONSTRUCTION



### Create summary table for expense groupings

df_transactions_sum_expense <- df_transactions_sum_budget%>% 
  group_by(expense_group_id) %>% 
  summarise(expense_group = unique(expense_group),
            totals= sum(amount)) %>% 
  ungroup()

df_transactions_sum_expense <- df_transactions_sum_expense %>% 
        rbind(
          rt_budget %>% 
            filter(expense_group_id == 300) %>% # adds 'calculated savings'
            select(expense_group_id, expense_group) %>% 
            #calculates savings from income(0), expenses (100), and wants (200)
            mutate(totals = with(df_transactions_sum_expense,
                                   sum(totals[
                                  expense_group_id %in% c(0,100,200)])))) %>% 
        mutate(pct = (totals
                      /totals[expense_group_id == 0])
                      *100, 
                pct = replace(pct, expense_group_id %in% c(400, 900), NA))


### combine this month's data with all previously recorded data


dt_sum.budget <- rows_upsert(dt_sum.budget,
                           df_transactions_sum_budget %>% 
                             select(-c(expense_group_id, expense_group)),
                           by = c("date", "budget_group_id")
                           )

saveRDS(dt_sum.budget,
        paste("R.code/ref_data/tables/dt_sum.budget/dt_sum.budget-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

### calculate balance for each budget category with new data included
# should exclude 'calculated savings'
# check through other categories

source("R.code/2.balance_calc.R")

df_transactions_budget_sum <- df_transactions_sum_budget %>% 
  group_by(diff_flag) %>% 
  summarise(sum = sum(budget_diff))

###UNDER CONSTRUCTION
###TESTING

