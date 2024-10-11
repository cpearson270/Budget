#### save_all_csv
## save all data from R environment to .csv data file
## REQUIRES: run 'Budget Tracker 4.0'
## Project: Budget Tracker 4.0 - 26/01/2022 - Moved from excel tracker to R
## Last update: 27/02/2022

v.objects <- c('rt_budget')
#               ds_trans_buckets',
#               'ds_trans_budget',
#               'ds_trans_budget_diff',
#               'dt_log_budget',
#              'dt_transactions_sum',
#               'rt_budget')

#v.objects <- ls()

#v.objects <- c('words_list')
               

objects <- mget(v.objects)

if (!(exists('trans_file_date'))) {
  if(exists('dt_log_budget')) {
  trans_file_date <- last(dt_log_budget$date)
  } else {
    trans_file_date <- format(Sys.time(), "%Y-%m") %>% 
      lubridate::ym() %>% 
      lubridate::rollforward ()
}}

dir.create(paste("R.output", trans_file_date, sep="/"))

for (i in 1:length(v.objects)) {
  
path_table <- paste("R.output", trans_file_date, v.objects[i], sep="/")

write_csv(objects[[i]],
          paste(path_table,"-",
                format(Sys.time(), "%Y%m%d-%H%M%S"),
                ".csv",
                sep=""))
}          
   
