#### ask.trans_file_date
## Asks user to choose which month to load transactions from
## REQUIRES: N/A
## CALLED BY: Budget_Tracker, load.bank_trans
## Project: Budget Tracker 4.0 - 26/02/2023 - Moved from excel tracker to R
## Last update: 26/02/2023

## Load packages
required_libs <- c("tidyverse", "lubridate")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

## ask user: load most recent transactions or specific month?
print('Choose transaction file for import.', quote = FALSE)
print("Press Enter for most recent or type date eg. '202107'):", quote = FALSE)
v.trans_file_date<- readline()

while(str_detect(v.trans_file_date, '\\d{6}|^$', negate = TRUE)) {
  print('Transaction file for import:', quote = FALSE)
  print("Press Enter for most recent or type date eg. '202107'):", quote = FALSE)
  v.trans_file_date<- readline()
}

trans_file_date <- rollforward(ym(v.trans_file_date))
