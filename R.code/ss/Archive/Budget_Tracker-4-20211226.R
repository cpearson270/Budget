####Budget Tracker 4.0 - 07/12/2021 - Moved from excel tracker to R

### Load User Values

##UNDER CONSTRUCTION - attempting to request user input from console, 
#script does not wait for input


# Update budget or use previous?

Budget_update<-readline(prompt = "Update budget from Excel? (Y/N):")

#add line to convert any answer to lower case and then check for acceptable 
# input; able to remove some 'if' conditions below

#load most recent transactions or specific month?



repeat {
trans_file_date<- readline(prompt = 
                 "Transaction file for import ('recent' or date eg. '202107'):")

if(trans_file_date=="") {
} else { break}
}

library(tidyverse) #library(lubridate) not loaded, use "lubridate::"

###Load Budget


        if (Budget_update == "Y"|| Budget_update == "y") {

        ##Reference to separate RScript (below code) if new budget
        library(readxl)
        library(janitor)
        
        repeat {
          
        path_budget<-"R.data/Budget"
        file_budget<-list.files(path_budget) %>%
          last()
  
        rt_budget<- read_excel(file.path(path_budget,file_budget)) %>% 
        #Clean up budget imported from excel
          remove_empty() %>% 
          clean_names("snake") %>% 
          mutate(budget_group_id = floor(category_id),
                 budget_group =
                   category[match(budget_group_id, category_id)],
                 expense_group_id = floor(category_id/100)*100,
                 expense_group = 
                   category[match(expense_group_id, category_id)]) %>% 
          filter(!category_id %in% c(100,200,300,900))
        
 check_budget<- rt_budget %>% filter(category_id < 100 & category_id != 0)
 
 if (nrow(check_budget) == 0) {
    break }
               
else if (nrow(check_budget)>0) {
    print("ERROR: Invalid Category ID")
    print(check_budget)
    print("Correct category ID and reload budget from Excel")
    Budget_reload<-readline(prompt = 
         "Type 'Y' to attempt reload, type 'N' to cancel: ")
}
    if (Budget_reload == "Y"|| Budget_reload == "y") {
    }
    else if (Budget_reload == "N"|| Budget_reload == "n") {
      break
      rm(Budget_reload)
    }
  }
        
  rm(check_budget)
  
        saveRDS(rt_budget, paste("R.code/ref_data/Budget-",
                              format(Sys.time(), "%Y%m%d-%H%M%S"),
                              ".rds",
                              sep=""))
        
        rm("file_budget","path_budget")

        } else if (Budget_update == "N" || Budget_update == "n") {        
        #Load existing budget
        file_budget<-list.files("R.code/ref_data") %>%
            last()
        rt_budget<-readRDS(paste("R.code/ref_data",
                              file_budget,sep = "/"))
        
        rm("file_budget")
        } else {
         stop(
           "ERROR: Unacceptable input for 'Budget Update', must be 'Y' or 'N'"
           )
          }

### Load and summarise transactions according to Budget


      path_transactions<-"R.data/transactions"
      
      #load most recent transactions
      if (trans_file_date == "recent") {
      
        file_trans<-list.files(path_transactions) %>%
          last()
        
        } else {
          file_trans<- paste("ALL TRANS_", trans_file_date,".csv", sep="")
      }
      dt_transactions<-read_csv(file.path(path_transactions,file_trans)) %>% 
        mutate(date = lubridate::dmy(date)) %>% 
        select(-c(tags, bank, accountname, accountnumber))
      
      rm("file_trans", "path_transactions")

#Summarise transactions: find total of spending in each category and assign 
#      categories with category ID from rt_budget      
    

repeat {
        
  dt_transactions_sum<- dt_transactions %>% 
    group_by(category) %>% 
    summarise(sum = sum(amount)) %>% 
    ungroup() %>% 
    full_join(rt_budget %>% 
                select(c(category_id,category,
                         budget_group_id, budget_group,
                         expense_group_id, expense_group
                         )), by="category") %>% 
    relocate(category_id,category) %>% 
    arrange(category_id)
  
  #view transaction type without a category ID
  
  view(rt_budget)
  
  df_transactions_sum.missing_categories<-dt_transactions_sum %>% 
          filter(is.na(category_id)) %>% 
          view()
  
  #INPUT REQUIRED IF NEW CATEGORY
  
      #assign category ID if transaction without category ID
     if(nrow(df_transactions_sum.missing_categories)>0) {
       
       
        #Is there a more efficient way to get multiple lines of user input?
       print(df_transactions_sum.missing_categories$category)
       V.CATEGORY_ID<-readline(prompt = "Assign new category ID:") 
       #Will this work for multiple IDs?
       V.LIMIT <-readline(prompt = "Budget limit for new category ($):")
       V.PERIOD <-readline(prompt = "Budget period in months:") 
       V.CYCLE <-readline(prompt = 
                          "Is this a pre- or post-cycle expese? (Pre/Post):")
       V.LONG_TERM <-readline(prompt = "Is this a long term category? (Y/NA):")
       V.COMMENTS <-readline(prompt = "Notes:")
       
       
       
      df_new_budget_category<- tibble(
        category_id = V.CATEGORY_ID,
        category = df_transactions_sum.missing_categories$category,
        limit = V.LIMIT,
        period = V.PERIOD,
        cycle = V.CYCLE,
        long_term = V.LONG_TERM,
        comments = paste("Added CP",
                         format(Sys.Date(),"%Y%m%d"),
                         V.COMMENTS,
                         sep = "")) %>% 
        mutate(across(.cols = everything(), ~na_if(.,""))) %>% 
        mutate(across(.cols=c(category_id, limit, period),
                      ~as.double(.))) %>% 
#This is repeated, can be a function?      
        mutate(budget_group_id = floor(category_id),
               budget_group =
                 rt_budget$budget_group[match(budget_group_id, 
                                           rt_budget$budget_group_id)],
               expense_group_id = floor(category_id/100)*100,
               expense_group = 
                 rt_budget$expense_group[match(expense_group_id, 
                                            rt_budget$expense_group_id)])
      
      
      #add new category to the budget
      
      rt_budget<- bind_rows(rt_budget, df_new_budget_category)
      
      saveRDS(rt_budget, paste("R.code/ref_data/Budget-",
                            format(Sys.time(), "%Y%m%d-%H%M%S"),
                            ".rds",
                            sep=""))
      
      rm(df_new_budget_category, df_transactions_sum.missing_categories,
         V.COMMENTS, V.CYCLE, V.LIMIT, V.LONG_TERM, V.PERIOD)
      
      } else if(nrow(df_transactions_sum.missing_categories)==0) {
        break
      }
} #repeat to line 113
     
dr_budget <- rt_budget %>%
   filter(category_id==budget_group_id) %>% 
   select(-c(category_id, category))
      
      
      
dt_transactions_sum_budget<- dt_transactions_sum%>% 
    summarise(.groups = budget_group_id,
              totals= sum(sum,na.rm = TRUE),
              budget_group = budget_group,
              expense_group_id = expense_group_id,
              expense_group = expense_group
              )

#UNDER CONSTRUCTION

#dt_transactions_sum_log<-rt_budget %>%
#  select(c(category_id,category,limit)) %>% 
#  full_join(dt_transactions_sum,
#          by="category")

#add_column(.before="category", date= rollforward(ym(trans_file_date)))