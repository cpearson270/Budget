#### calc.cumulative_sums
## Calculates the cumulative balance and cumulative average columns
    # for each budget category using previous records from dt_log_budget
## REQUIRES: main script or calc.refresh.cumulative_sums
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 07/02/2022

# should exclude 'calculated savings'
# check through other categories

ds_trans_budget <- ds_trans_budget %>% 
  left_join(dt_log_budget %>% #pull numbers from previous month
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance, write_off_count),
            by = "budget_group_id") %>% 
  left_join(dt_log_budget %>% #calculate cum_avg from all previous months
              filter(date <= trans_file_date) %>% 
              group_by(budget_group_id) %>% 
              summarise(cum_average = round(mean(amount, na.rm = TRUE),2)
              ) %>% 
              ungroup(),
            by = "budget_group_id") %>% 
  left_join(dt_log_budget %>% #calculate 12 month cum_avg
              filter(date <= trans_file_date &
                     date > trans_file_date %m-% months(12)) %>% 
              group_by(budget_group_id) %>% 
              summarise(yr_average = round(mean(amount, na.rm = TRUE),2)
              ) %>% 
              ungroup(),
            by = "budget_group_id") %>% 
  rowwise() %>% 
  mutate(cum_balance = sum(cum_balance,
                           budget_diff[
                            which(
                            !(diff_flag %in% c(110, 120, 130, 210, 220, 230))
                           )],na.rm = TRUE),
         cum_balance = replace(cum_balance, diff_flag %in% c(130, 230),
                               0),
         write_off_count = replace(write_off_count, diff_flag %in% c(130, 230),
                                   write_off_count+1),
         write_off_count = replace_na(write_off_count, 0)
  ) %>% 
  ungroup()

#dr_budget.balance <- dt_log_budget %>% 
#  group_by(budget_group_id) %>% 
#  summarise(budget_group = unique(budget_group),
#            budget = unique(limit, fromLast = TRUE),
#            balance = sum(budget_diff[
#              which(
#                !(diff_flag %in% c(110, 120, 210, 220))
#              )], na.rm = TRUE),
#            average_spend = round(mean(amount, na.rm = TRUE),2)
#  ) %>% 
#  ungroup()