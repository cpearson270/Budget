####Budget Tracker 4.0 - 05/01/2022 - Moved from excel tracker to R

### STARTER SCRIPT ###

### Load initial functions and packages
## Load main package: tidyverse
library(tidyverse) 
library(lubridate)

source("R.code/fun_main.R")

### Load User Values
## Update budget or use previous?
budget_update<-readline(prompt = "Update budget from Excel? (Y/N):") %>% 
  tolower()

while(!(budget_update %in% c('y','n'))) {
  budget_update<-readline(prompt = "Update budget from Excel? (Y/N):") %>% 
    tolower()
}

## load most recent transactions or specific month?
v.trans_file_date<- readline(prompt = 
           "Transaction file for import ('recent' or date eg. '202107'):")

while(v.trans_file_date=="") {
  v.trans_file_date<- readline(prompt = 
           "Transaction file for import ('recent' or date eg. '202107'):")
}
  
trans_file_date <- rollforward(ym(v.trans_file_date))  

###Load Budget
if (budget_update == "y") {
  
  source("R.code/load_budget_xlsx.R")
  
  source("R.code/save_budget_rds.R")

} else if (budget_update == "n") {        

##Load existing budget from RDS file
  rt_budget <- load_recent_rds("rt_budget")
} 

### Load and summarise transactions according to Budget
path_transactions<-"R.data/transactions"
      
## load transactions
file_trans.csv<- paste("ALL TRANS_", v.trans_file_date,".csv", sep="")
file_trans.xlsx<- paste("ALL TRANS_", v.trans_file_date,".xlsx", sep="")

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
  
rm(file_trans.csv, file_trans.xlsx, path_transactions)

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

## Make available decision information for assigning  missing category

    view(df_transactions_sum.missing_categories)
    
    view(rt_budget)
      
    dt_transactions %>% 
        filter(
          category %in% df_transactions_sum.missing_categories$category) %>% 
        view("df_transactions.missing_categories")

## assign category ID if transaction without category ID
    nMissing <- length(df_transactions_sum.missing_categories$category)
    
    df_new_budget_category<- tibble(
      category_id = numeric(nMissing),
      budget_group = character(nMissing),
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
        as.double(readline(prompt = "Assign new category ID:"))
      
      df_new_budget_category$category[i] <- 
        df_transactions_sum.missing_categories$category[i]
      
      df_new_budget_category<- df_new_budget_category %>% 
        mutate(budget_group_id = floor(category_id),
               expense_group_id = floor(category_id/100)*100,
               expense_group = 
                 rt_budget$expense_group[match(expense_group_id, 
                                               rt_budget$expense_group_id)])  
      
      if(any(rt_budget$budget_group_id==
             df_new_budget_category$budget_group_id[i])) { 
        
        df_new_budget_category$budget_group[i] <-
          rt_budget$budget_group[
            match(df_new_budget_category$budget_group_id[i], 
                  rt_budget$budget_group_id)]
        df_new_budget_category[i,] <- df_new_budget_category[i,] %>% 
        mutate(across(.cols = everything(), ~replace(.,. %in% c("",0), NA)))

      } else {
        df_new_budget_category$budget_group[i] <- 
          readline(prompt = "Assign new Budget group:")
        
        if (df_new_budget_category$budget_group[i]=="") {
          df_new_budget_category$budget_group[i] <- 
            df_new_budget_category$category[i] }
        
        df_new_budget_category$limit[i] <-
          as.double(readline(prompt = "Budget limit for new category ($):"))
        df_new_budget_category$period[i] <-
          as.double(readline(prompt = "Budget period in months:"))
        df_new_budget_category$cycle[i] <-
          readline(prompt = "Is this a pre- or post-cycle expese? (Pre/Post):")
        df_new_budget_category$long_term[i] <-
          readline(prompt = "Is this a long term category? (Y/NA):")
        
        df_new_budget_category<- df_new_budget_category %>% 
          mutate(across(.cols = everything(), ~na_if(.,"")))#Replace blank values with 'NA'
      }
      
      df_new_budget_category$comments[i] <-
        paste("[CP",
              format(Sys.Date(),"%Y%m%d"),
              "]: Added for ",
              v.trans_file_date,
              ".",
              readline(prompt = 'Notes:'),
              sep = "")

    }
    
##add new category to the budget
# need to add check to only add unique values
      
      rt_budget<- bind_rows(rt_budget, df_new_budget_category) %>% 
        arrange(category_id)
      
      source("R.code/save_budget_rds.R")
      
      rm(df_new_budget_category, df_transactions_sum.missing_categories,
         nMissing, i)
      
    } else if(nrow(df_transactions_sum.missing_categories)==0) {
        break
    }
} ##repeat to line 84

### Load records log and rt_budget subset
source("R.code/load_tables.R")

###run reports for transaction summaries
ds_trans_budget <- dt_transactions_sum%>% 
  group_by(budget_group_id) %>% 
  summarise(expense_group_id = unique(expense_group_id),
            budget_group = unique(budget_group),
            expense_group = unique(expense_group),
            amount = sum(sum)) %>% 
  ungroup() %>% 
  right_join(df_budget %>% 
               filter(expense_group_id != 300) %>% 
               select(-c(period, cycle, comments)), 
              by = c("expense_group_id", "expense_group",
                     "budget_group_id", "budget_group")) %>% 
  mutate(amount = replace_na(amount, 0)) %>% 
  rowwise() %>% 
  mutate(budget_diff = sum(limit, amount),
         diff_flag = 
           expense_group_id
          +
           (if (is.na(limit)) {NA
           } else if (budget_diff>0) {10
           } else if (budget_diff<0) {20
           } else {NA})
          +
           (if (is.na(limit)) {NA
           } else if (budget_diff == 0) {NA
           } else if (is.na(long_term)) {1
           } else if (long_term == "Y") {2 
           } else {
             stop(
            "ERROR: ds_trans_budget$diff_flag could not be computed"
            ) } )
         ) %>% 
  ungroup() %>% 
  add_column(.before="budget_group_id", 
             date= trans_file_date) %>% 
  arrange(budget_group_id)

### Mark categories as actual over-spending or actual savings
## Show relevant tables for decision making

view(ds_trans_budget)

#dt_log_budget %>% 
#  filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
#  view("Current Balance")

## Show relevant tables for reference to SAVINGS

# remove reference to income

ds_trans_budget %>% 
  filter(budget_diff > 0) %>% 
  left_join(dt_log_budget %>% 
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance),
            by = "budget_group_id") %>% 
  rename(current_balance = cum_balance) %>% 
  select(-c('expense_group_id', 'expense_group', 'diff_flag')) %>% 
  View("Under Budget")

print("Mark group(s) as actual SAVINGs.      Budget group ids:")
v.act_save <-scan()
if (is_empty(v.act_save)) {v.act_save<- NA}

print("Mark group as WRITE UP.       Budget group ids:")
  v.write_off <-scan()
  if (is_empty(v.write_off)) {v.write_off<- NA}

ds_trans_budget %>% 
  filter(budget_diff < 0) %>% 
  left_join(dt_log_budget %>% 
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance),
            by = "budget_group_id") %>% 
  rename(current_balance = cum_balance) %>%
  select(-c('expense_group_id', 'expense_group', 'diff_flag')) %>% 
  View("Over Budget")

# show also current balance of categories

## record relevant categories

print("Mark group as actual over spending. Budget group ids:")
  v.act_over_spend <-scan()
  if (is_empty( v.act_over_spend)) {v.act_over_spend<- NA}

# add check that budget_diff is >/< 0 as appropriate for over spend/savings
  
print("Mark group as WRITE OFF.       Budget group ids:")
  v.write_off <- append(v.write_off, scan())
  if (is_empty(v.write_off)) {v.write_off<- NA}

## update diff_flag for identified categories

ds_trans_budget <- ds_trans_budget %>% 
  rowwise() %>% 
  mutate(diff_flag = replace(diff_flag, 
                             budget_group_id %in% v.act_save, 
                             expense_group_id + 10),
         diff_flag = replace(diff_flag,
                             budget_group_id %in% v.act_over_spend,
                             expense_group_id + 20),
         diff_flag = replace(diff_flag,
                             budget_group_id %in% v.write_off,
                             expense_group_id + 30)
         ) %>% 
  ungroup()

rm(v.act_over_spend, v.act_save)

### Create summary table for expense groupings

ds_trans_buckets <- ds_trans_budget %>% 
  group_by(expense_group_id) %>% 
  summarise(expense_group = unique(expense_group),
            totals= sum(amount)) %>% 
  ungroup()

ds_trans_buckets <- ds_trans_buckets %>% 
        rbind(
          rt_budget %>% 
            filter(category_id == 300) %>% # adds 'calculated savings'
            select(expense_group_id, expense_group) %>% 
            #calculates savings from income(0), expenses (100), and wants (200)
            mutate(totals = with(ds_trans_buckets,
                                   sum(totals[
                                  expense_group_id %in% c(0,100,200)])))) %>% 
        mutate(pct = (totals
                      /totals[expense_group_id == 0])
                      *100, 
                pct = replace(pct, expense_group_id %in% c(400, 900), NA))

### calculate balance and average spend for each budget category with new 
  #data included
# should exclude 'calculated savings'
# check through other categories

### combine this month's data with all previously recorded data

### calculate balance and average spend for each budget category with new ####
# data included
# check through other categories

source("R.code/calc.cumulative_sums.R")

### combine this month's data with all previously recorded data

dt_log_budget <- rows_upsert(dt_log_budget,
                             ds_trans_budget %>% 
                               select(-c(expense_group_id, expense_group,
                                         long_term)),
                             by = c("date", "budget_group_id")
)

source('R.code/calc.savings.R')

source('R.code/save_log_rds.R')

View(dt_log_budget %>% 
       filter((budget_group_id %in% c(902, 903) & amount != 0) | 
                budget_group_id == 901 & date == last(date)))

### END OF SCRIPT ###
