#### load.bank_trans
## load .csv files from bank (complete access and credit card) or 
## REQUIRES: ask.trans_file_date.R
## CALLED BY: Budget Tracker 4.0
## Project: Budget Tracker 4.0 - 26/02/2023 - Moved from excel tracker to R
## Last update: 11/08/2024

# BETA--added load in for ING csv. 

## Load packages
required_libs <- c("tidyverse", "lubridate")

if(any(!required_libs %in% (.packages()))) {
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

source("R.code/fun.load.R")

### Load and summarise transactions according to Budget
path_trans<-"R.data/transactions"

if (str_detect(v.trans_file_date, '^$')) {
  # if the user date sting is empty
  # loads last (most recent) available file of each type, from R.data/transactions
  # recent files assumed to be ing.csv or commbank.csv
  df_trans.mc <- name_file(filter = "^commbank-mc") %>% 
    load_file_comm.csv()
  df_trans.ca <- name_file(filter = "^commbank-ca") %>% 
    load_file_comm.csv()
  df_trans.ed <- name_file(filter = "^ing-ed") %>% 
    load_file_ing.csv()
  df_trans.jt <- name_file(filter = "^ing-jt") %>% 
    load_file_ing.csv()
  
  df_trans_load <- rows_append(df_trans.ca, df_trans.mc, 
                              df_trans.ed, df_trans.jt)
} else {
  file_trans <- name_file(filter = v.trans_file_date)
  
  if (str_detect(file_trans, "^ALL TRANS")) {
    if (str_detect(file_trans, ".csv$")) {
      # load_file_at.csv
      df_trans_load<-read_csv(
        file.path(path_trans,file_trans)) %>%
        mutate(date = lubridate::dmy(date)) %>% #dates stored as text in .csv
        select(date, description, category, amount, notes) %>% 
        janitor::remove_empty(which = c("rows", "cols"))
    } else if (str_detect(file_trans, ".xlsx$")) {
      # load.file_at.xlsx
      df_transactions<-readxl::read_xlsx(
        file.path(path_trans,file_trans)) %>% 
        mutate(date = lubridate::as_date(date)) %>% 
        #'as_date' because .xlsx dates were converted to number of days since 1970-01-01
        select(date, description, category, amount, notes) %>% 
        janitor::remove_empty(which = c("rows", "cols"))
    }
  } else if (str_detect(file_trans, "^commbank")) {
    df_trans.ca <- name_file(filter = paste0("(?=.*",
                                             v.trans_file_date,
                                             ")(?=.*ca).*\\.csv$")) %>% 
      load_file_comm.csv()
    
    df_trans.mc <- name_file(filter = paste0("(?=.*",
                                             v.trans_file_date,
                                             ")(?=.*mc).*\\.csv$")) %>% 
      load_file_comm.csv()
  
  df_trans_load <- rows_append(df_trans.ca, df_trans.mc)
  rm(df_trans.ca, df_trans.mc)
  }
}
rm(file_trans, path_trans)