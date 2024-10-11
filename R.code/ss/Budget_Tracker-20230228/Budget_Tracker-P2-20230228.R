#### Budget Tracker PART 2 2023 02 28 - Pocketbook goes fishing

###### PART 2 ############

## Load packages
required_libs <- c("tidyverse", "readxl", "kableExtra")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

## load custom function
source("R.code/fun.load.R")

if (!any(ls() %in% c("df_trans_load"))) {
  ## ask user: load most recent transactions or specific month?
  source("R.code/ask.trans_file_date.R")
  
  ## load categorised transactions
  path_trans_cat<-paste0("R.Output/", trans_file_date)
  
  file_trans_cat<-list.files(path_trans_cat) %>% 
    str_subset('^ds_trans_cat') %>% 
    last()
  
  df_trans_cat <- read_excel(paste(path_trans_cat,file_trans_cat, sep="/"))
      
  ### Load tables (records log and budget version log)
  source("R.code/load.tables.R")
  
  # check if this transaction period has been processed before
  if (any(v.trans_file_date %in% dt_budget_version$trans_date)) {
    
    source("R.code/ask.update_cat_words.R")
    
    if (cats_update == 'y') {
      source("R.code/update.cat_words.R")
    }
  } else {
    source("R.code/update.cat_words.R")
  }
} else {
  df_trans_cat <- df_trans_load
}

## Update budget or use previous?
budget_update<-readline(prompt = "Update budget from Excel? (Y/N):") %>% 
  tolower()

while(!(budget_update %in% c('y','n'))) {
  budget_update<-readline(prompt = "Update budget from Excel? (Y/N):") %>% 
    tolower()
}

### Load Budget
if (budget_update == "y") {
  ## Load updated budget from excel
  source("R.code/load.budget_xlsx.R")
  source("R.code/save_budget_rds.R")
  
} else if (budget_update == "n") {
  ## Load budget from RDS file
  if (any(dt_budget_version$trans_date %in% v.trans_file_date)) {
    # If trans_file_date exists in the budget_version register (because these
    #transactions have previously been loaded)
    v.budget_version <- dt_budget_version$budget_version[
      dt_budget_version$trans_date == v.trans_file_date]
    # Gets the budget version recorded in register
    rt_budget <- load_recent_rds("rt_budget", v.budget_version)
    # Loads in most recent update of previous version of budget
  } else { #should use 'load.budget_rds.R' here?
    rt_budget <- load_recent_rds("rt_budget")
    v.budget_version <- str_sub(name_recent_rds("rt_budget"), 
                                start = 11, end = 16)
  } }

## Create rt_budget subset; excludes non-budget categories
df_budget <- rt_budget %>%
  filter(category_id==budget_group_id) %>% 
  select(-c(category_id, category))

### SUMMARISE transactions: find total of spending in each category and assign 
#categories with category ID from rt_budget      
repeat {
  
  ds_trans<- df_trans_cat %>% 
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
  
  ds_trans.missing_cat<-ds_trans %>% 
    filter(is.na(category_id))
  
  ## INPUT REQUIRED IF NEW CATEGORY
  
  if(nrow(ds_trans.missing_cat)>0) {
    
    ## Make available decision information for assigning  missing category
    
    view(ds_trans.missing_cat)
    
    view(rt_budget)
    
    df_trans_cat %>% 
      filter(
        category %in% ds_trans.missing_cat$category) %>% 
      view("ds_trans.missing_cat")
    
    ## assign category ID if transaction without category ID
    nMissing <- length(ds_trans.missing_cat$category)
    
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
      
      print(ds_trans.missing_cat$category[i])
      
      df_new_budget_category$category_id[i] <- 
        as.double(readline(prompt = "Assign new category ID:"))
      
      df_new_budget_category$category[i] <- 
        ds_trans.missing_cat$category[i]
      
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
    
    rm(df_new_budget_category, ds_trans.missing_cat,
       nMissing, i)
    
    source("R.code/save_budget_rds.R")
    
  } else if(nrow(ds_trans.missing_cat)==0) {
    break
  }
} ##repeats

### run reports for transaction summaries
ds_trans_budget <- ds_trans%>% 
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

## Show relevant tables for reference to SAVINGS
ds_trans_budget %>% 
  filter(budget_diff > 0) %>% 
  left_join(dt_log_budget %>% 
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance),
            by = "budget_group_id") %>% 
  left_join(df_budget %>% 
              select(budget_group_id, cycle),
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
  left_join(df_budget %>% 
              select(budget_group_id, cycle),
            by = "budget_group_id") %>% 
  rename(current_balance = cum_balance) %>%
  select(-c('expense_group_id', 'expense_group', 'diff_flag')) %>% 
  View("Over Budget")

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

dt_budget_version <- rows_upsert(dt_budget_version,
                                 tibble('trans_date'= v.trans_file_date,
                                        'budget_version' = v.budget_version),
                                 by = "trans_date")

source('R.code/calc.savings.R')

## Save everything
source('R.code/save_log_rds.R')

saveRDS(rt_category_lookup,
        paste("R.code/ref_data/rt_category_lookup/rt_category_lookup-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

saveRDS(rt_common_words,
        paste("R.code/ref_data/rt_common_words/rt_common_words-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

saveRDS(dt_budget_version,
        paste("R.code/ref_data/dt_budget_version/dt_budget_version-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

rmarkdown::render("R.code/Budget Report-202205-20220506.Rmd",
                  output_file = paste("Budget_Report_",
                                      v.trans_file_date,
                                      "-",
                                      format(Sys.time(), "%Y%m%d-%H%M%S"),
                                      sep=""),
                  output_dir = "R.output")

### END OF SCRIPT ###