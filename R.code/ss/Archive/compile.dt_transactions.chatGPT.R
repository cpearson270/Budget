#### compile.dt_transactions.chatGPT
## pull all transactions and compile into dt_transaction
## REQUIRES: N/A
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 22/01/2023

### STARTER SCRIPT ###

### Load initial functions and packages
library(tidyverse)

### Create initial table to be added to
dt_transactions <- tibble(
  description = character(),
  category = character(),
  notes = character()
)

### Load historic transactions
path_transactions<-"R.data/transactions"

list.file.trans <- list.files(path_transactions) %>% 
  .[!. == "ss"]

## load transactions  
for (i in list.file.trans) {
  if (str_detect(i,'.csv')) {
    dt_transactions<-rows_insert(dt_transactions,
                                 read_csv(file.path(path_transactions,i)) %>%
                                   select(description, category, notes) %>% 
                                   janitor::remove_empty(which = c("rows", "cols")),
                                 by = c("description", 'category', 'notes'),
                                 conflict = 'ignore')
    
  } else {
    dt_transactions<-rows_insert(dt_transactions,
                                 readxl::read_xlsx(file.path(path_transactions,i)) %>% 
                                   select(description, category, notes) %>% 
                                   janitor::remove_empty(which = c("rows", "cols")),
                                 by = c("description", 'category', 'notes'),
                                 conflict = 'ignore')
    
  }}

rm(i, list.file.trans, path_transactions)

###Clean up data
## dt to lower case and remove duplicate rows
dt_transactions<- dt_transactions %>% 
  mutate(description = tolower(description)) %>% 
  unique()

saveRDS(dt_transactions,"R.code/ref_data/dt_transactions/dt_transactions.rds")

## ChatGPT code
