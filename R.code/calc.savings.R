#### calc.savings
## Calculate the cumulative balance and cumulative average colums
# in dt_log_budget
## REQUIRES: main script or calc.refresh.cumulative_sums
## CALLED BY: calc.refresh.cumulative_sums
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 07/02/2022


### create buckets summary

ds_trans_budget_diff <- ds_trans_budget %>% 
  group_by(diff_flag) %>% 
  summarise(sum = sum(budget_diff))

## Calculate amount to be written-off (previous cumulative balance - not
#excludes budget difference for current period (captured as 130/230)

#write_off <- if (any(ds_trans_budget$diff_flag %in% c(130,230))) {
#  dt_log_budget %>% 
#    summarise(amount = sum(cum_balance[
#      date == rollforward(trans_file_date %m-% months(1)) &
#                            diff_flag %in% c(130, 230)])
#    ) %>% 
#    pull(amount)
#}

write_off <- if (length(v.write_off) > 0) {
  dt_log_budget %>% 
    summarise(amount = sum(cum_balance[
      date == rollforward(trans_file_date %m-% months(1)) &
        budget_group_id %in% v.write_off])) %>% 
    pull(amount)
}

### get budgeted savings amount    
budgeted_savings <- rt_budget %>% 
  filter(category_id == 305) %>% 
  pull(limit)

### create savings report
df_savings <- ds_trans_budget_diff %>% 
  summarise(
    date = trans_file_date,
    net_trans_savings = sum(sum[diff_flag %in% c(110, 210, 120, 220, 130, 230)],
                            write_off),
    lt_savings = sum(sum[diff_flag %in% c(112, 212, 122, 222)]),
    real_savings = sum(
      budgeted_savings,
      net_trans_savings,
      sum[diff_flag == 11], na.rm = TRUE
    ),
    send_to_savings = sum(lt_savings, real_savings)
  ) %>% 
  mutate(budget_savings_diff = sum(-budgeted_savings, real_savings)) %>% 
  pivot_longer(2:6, 
               names_to = "budget_group", 
               values_to = "amount") %>% 
  mutate(budget_group = replace(budget_group,
                                budget_group %in%
                                  c('net_trans_savings',
                                    'lt_savings',
                                    'real_savings',
                                    'send_to_savings',
                                    'budget_savings_diff'),
                                c('Calculated Savings - Net (transactions)',
                                  'Calculated Savings - Long Term',
                                  'Calculated Savings - Real (budget, net, income)',
                                  'Calculated Savings - Send to Savings',
                                  'Calculated Savings - Budget Difference'))
  ) %>% 
  left_join(df_budget %>% 
              select(-c(period, cycle, comments, long_term,
                        expense_group_id, expense_group)), #removed limit
            by = 'budget_group')


### add savings report to log - to allow for calculations
dt_log_budget <- rows_upsert(dt_log_budget,
                             df_savings,
                             by = c("date", "budget_group_id")
)

### balance calculations for savings report
df_savings <- df_savings %>% 
  left_join(dt_log_budget %>% 
              filter(date == rollforward(trans_file_date %m-% months(1))) %>% 
              select(budget_group_id, cum_balance),
            by = "budget_group_id") %>% 
  left_join(dt_log_budget %>% 
              filter(date <= trans_file_date) %>% 
              group_by(budget_group_id) %>% 
              summarise(cum_average = round(mean(amount, na.rm = TRUE),2)
              ) %>% 
              ungroup(),
            by = "budget_group_id") %>% 
  group_by(budget_group_id) %>% 
  mutate(cum_balance = sum(cum_balance,
                           amount, na.rm = TRUE)) %>% 
  ungroup()

### add savings report to log
dt_log_budget <- rows_upsert(dt_log_budget,
                             df_savings,
                             by = c("date", "budget_group_id")
                              )
