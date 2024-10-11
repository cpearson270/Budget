#### save_budget_rds
## save rt_budget from R environment to .rds data file
## REQUIRES: rt_budget to exist in environment
## Project: Budget Tracker 4.0 - 26/02/2022 - Moved from excel tracker to R
## Last update: 16/04/2022 - added v.budget_version to name

saveRDS(rt_budget, paste("R.code/ref_data/rt_budget/rt_budget-",
                         v.budget_version,
                         "-",
                         format(Sys.time(), "%Y%m%d-%H%M%S"),
                         ".rds",
                         sep=""))
