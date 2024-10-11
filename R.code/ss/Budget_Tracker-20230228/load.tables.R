#### load.tables
## load tables of previous records
## REQUIRES: fun.load.R
## CALLED BY: Budget_Tracker-4
## Project: Budget Tracker 4.0 - 26/02/2023 - Moved from excel tracker to R
## Last update: 27/02/2023

## Load packages
required_libs <- c("tidyverse")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
}
rm(required_libs)

## load custom function
source("R.code/fun.load.R")

## load log of budget-category records
dt_log_budget <- load_recent_rds("dt_log_budget")

## Load budget version list
dt_budget_version <- load_recent_rds('dt_budget_version')

##
rt_common_words <- load_recent_rds("rt_common_words")
rt_category_lookup <- load_recent_rds("rt_category_lookup")