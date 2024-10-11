####Budget Tracker 4.0 - 05/01/2022 - Moved from excel tracker to R

### Load initial functions and packages
## Load main package: tidyverse

library(tidyverse) 
library(lubridate)

source("R.code/fun_main.R")

### Load User Values

## Update budget or use previous?

budget_update<-readline(prompt = "Update budget from Excel? (Y/N):")

#add line to convert any answer to lower case and then check for acceptable 
# input; able to remove some 'if' conditions below

## load most recent transactions or specific month?

repeat {
  v.trans_file_date<- readline(prompt = 
                 "Transaction file for import ('recent' or date eg. '202107'):")

  if(v.trans_file_date=="") {
  } else { 
    trans_file_date <- rollforward(ym(v.trans_file_date))  
    break}
    }


###Load Budget

if (budget_update == "Y"|| budget_update == "y") {
  
  source("R.code/load_budget_xlsx.R")
  
  source("R.code/save_budget_rds.R")


} else if (budget_update == "N" || budget_update == "n") {        

##Load existing budget from RDS file

  rt_budget <- load_recent_rds("rt_budget")
  
} else {
  stop(
  "ERROR: Unacceptable input for 'Budget Update', must be 'Y' or 'N'"
  )
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
  
rm(file_trans.csv, file_trans.xlsx, path_transactions, v.trans_file_date)

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
      
      source("R.code/save_budget_rds.R")
      
      rm(df_new_budget_category, df_transactions_sum.missing_categories,
         nMissing, i)
      
    } else if(nrow(df_transactions_sum.missing_categories)==0) {
        break
    }
} ##repeat to line 113

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

dt_log_budget %>% 
  filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
  view("Current Balance")

## Show relevant tables for reference to SAVINGS

# remove reference to income

ds_trans_budget %>% 
  filter(budget_diff > 0) %>% 
  View("Under Budget")

print("Mark group(s) as actual SAVINGs.      Budget group ids:")
v.act_save <-scan()
if (is_empty(v.act_save)) {v.act_save<- NA}

print("Mark group as WRITE UP.       Budget group ids:")
  v.write_off <-scan()
  if (is_empty(v.write_off)) {v.write_off<- NA}

# filter out 'bank-savings' = 400, and msc-fuel = 901

ds_trans_budget %>% 
  filter(budget_diff < 0) %>% 
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
            filter(expense_group_id == 300) %>% # adds 'calculated savings'
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
dt_log_budget <- rows_upsert(dt_log_budget,
                             ds_trans_budget %>% 
                               select(-c(expense_group_id, expense_group, 
                                         long_term)),
                             by = c("date", "budget_group_id")
)

source("R.code/balance_calc.R")  #(332 -431)

### combine this month's data with all previously recorded data
dt_log_budget <- rows_upsert(dt_log_budget,
                           ds_trans_budget %>% 
                             select(-c(expense_group_id, expense_group, 
                                       long_term)),
                           by = c("date", "budget_group_id")
                           )

### create buckets summary

ds_trans_budget_diff <- ds_trans_budget %>% 
  group_by(diff_flag) %>% 
  summarise(sum = sum(budget_diff))

## Calculate amount to be written-off (previous cumulative balance - not
  #excludes budget difference for current period (captured as 130/230)

v.write_off <- if (length(v.write_off) > 0) {
  dt_log_budget %>% 
  summarise(amount = sum(cum_balance[
                     date == rollforward(trans_file_date %m-% months(1)) &
                budget_group_id %in% v.write_off])) %>% 
    pull(amount)
}

### get budgeted savings amount    
budgeted_savings <- rt_budget %>% 
  filter(category_id == 305) %>% 
  pull(limit)

### create savings report
df_savings <- ds_trans_budget_diff %>% 
  summarise(
  date = trans_file_date,
  net_trans_savings = sum(sum[diff_flag %in% c(110, 210, 120, 220, 130, 230)],
                v.write_off),
  lt_savings = sum(sum[diff_flag %in% c(112, 212, 122, 222)]),
  real_savings = sum(
                  budgeted_savings,
                  net_trans_savings,
                  sum[diff_flag == 11], na.rm = TRUE
                  ),
  send_to_savings = sum(lt_savings, real_savings)
  ) %>% 
  mutate(budget_savings_diff = sum(-budgeted_savings, real_savings)) %>% 
  pivot_longer(2:6, 
               names_to = "budget_group", 
               values_to = "amount") %>% 
  mutate(budget_group = replace(budget_group,
                                budget_group %in%
                            c('net_trans_savings',
                              'lt_savings',
                              'real_savings',
                              'send_to_savings',
                              'budget_savings_diff'),
                            c('Calculated Savings - Net (transactions)',
                              'Calculated Savings - Long Term',
                              'Calculated Savings - Real (budget, net, income)',
                              'Calculated Savings - Send to Savings',
                              'Calculated Savings - Budget Difference'))
  ) %>% 
  left_join(df_budget %>% 
               select(-c(period, cycle, comments, long_term,
                         expense_group_id, expense_group, limit)), 
             by = 'budget_group')


### add savings report to log - to allow for calculations
dt_log_budget <- rows_upsert(dt_log_budget,
                             df_savings,
                             by = c("date", "budget_group_id")
)

### balance calculations for savings report
df_savings <- df_savings %>% 
  left_join(dt_log_budget %>% 
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance),
            by = "budget_group_id") %>% 
  left_join(dt_log_budget %>% 
              group_by(budget_group_id) %>% 
              summarise(cum_average = round(mean(amount, na.rm = TRUE),2)
              ) %>% 
              ungroup(),
            by = "budget_group_id") %>% 
  group_by(budget_group_id) %>% 
  mutate(cum_balance = sum(cum_balance,
                           amount, na.rm = TRUE)) %>% 
  ungroup()

### add savings report to log
dt_log_budget <- rows_upsert(dt_log_budget,
                             df_savings,
                             by = c("date", "budget_group_id")
)


source('R.code/save_log_rds.R')

### END ###