#### save_log_rds
## save dt_log_budget from R environment to .rds data file
## REQUIRES: dt_log_budget to exist in environment - run 'load_tables.R'
## CALLED BY: Budget_Tracker-4, calc.refresh.cumulative_sums,
## Project: Budget Tracker 4.0 - 09/01/2022 - Moved from excel tracker to R
## Last update: 31/01/2022

saveRDS(dt_log_budget,
        paste("R.code/ref_data/dt_log_budget/dt_log_budget-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))