#### save_budget_version_rds
## save dt_budget_version from R environment to .rds data file
## REQUIRES: dt_budget_version to exist in environment
## Project: Budget Tracker 4.0 - 26/02/2022 - Moved from excel tracker to R
## Last update: 16/04/2022

saveRDS(dt_budget_version,
        paste("R.code/ref_data/dt_budget_version/dt_budget_version-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))
