#### Budget Tracker PART 1 20230228 - Pocketbook goes fishing

### STARTER SCRIPT ###

### Rules
  # Only save rt_budget if changed (updated from excel or new cat. added).
  # "p." prefix for user inputted values.
  # "v." prefix for formatted user values or internally generated vales.
  # "df_" data frame - temporary or subset data, not saved.
  # "ds_" data summary - summarised data, usually saved.
  # "dt_" data table - data loaded in and can be written to.
  # "rt_" reference table - data loaded in, but NOT written to.

### Load initial functions and packages
## Load main package: tidyverse
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)

#source("R.code/fun.load.R")

### Load tables
source("R.code/load.tables.R")

### Load User Values
## ask user: load most recent transactions or specific month?
source("R.code/ask.trans_file_date.R")

### Load transactions
source('R.code/load.bank_trans.R')

### CATEGORISE transactions if from bank, then STOP, else skip to P2

if (!"category" %in% colnames(df_trans_load)) {
  source("R.code/calc.categorise.R")
} else {
  source("R.code/Budget_Tracker-P2-20230228R")
}

#### END PART 1 ####