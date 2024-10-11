#### calc.categorise.Beta
## categorise transactions
## REQUIRES: 
## CALLED BY: compile.dt_transactions.R
## Project: Budget Tracker 4.0 - 09/01/2022 - Moved from excel tracker to R
## Last update: 18/02/2022

library(tidyverse)
source("R.code/fun_main.R")

### Load data
dt_transactions <- load_recent_rds("dt_transactions")
rt_common_words <- load_recent_rds("rt_common_words")
rt_category_lookup <- load_recent_rds("rt_category_lookup")
rt_category_lookup <- load_recent_rds("rt_category_lookup")
rt_category_lookup2 <- load_recent_rds("rt_category_lookup2")

# Give each transaction a unique ID
dt_transactions <- dt_transactions %>% 
  mutate(trans_ID = row_number(description))

dt_trans <- dt_transactions

##Remove ends of transaction description
dt_trans$description<- dt_trans$description %>%
  str_split("\\s(aus\\s)?(value\\sdate|tap\\sand\\spay|card)\\b") %>% 
  sapply('[',1)

# Create a new column that splits the description into separate words
# Use unnest() to create a row for each word in the 'words' column
split_words <- function(description) {
  # Split the description into individual words
  words <- str_split(description, "\\s+")[[1]]
  # Combine adjacent pairs of words with an underscore
  pairs <- sapply(1:(length(words)-1), 
                  function(i) paste(words[i], words[i+1], sep = " "))
  # Return the pairs and single words
  c(words, pairs)
}

# Create a tibble with the split words for each description
df_trans_cat <- dt_trans %>%
  mutate(words = map(description, split_words)) %>%
  unnest(words) %>%
  select(trans_ID, words)

#df_trans_cat <- dt_transactions %>%
#  mutate(words = str_split(description, " ")) %>% 
#  unnest(words) %>% 
#  select(trans_ID, words)

# Clean up words by removing common words
df_trans_cat2<- df_trans_cat %>% 
  filter(!words %in% rt_common_words)

# Pull out transactions that have no matches (all words in description 
  #were matched in rt_common_words)
v.nrows <- c(1:nrow(dt_transactions))
check <- v.nrows[!(v.nrows %in% df_trans_cat2$trans_ID)]

not_caught <- dt_transactions %>% 
  filter(trans_ID %in% check)

# take cleaned list and match to categories
df_trans_cat3.1 <- df_trans_cat2 %>% 
  left_join(rt_category_lookup, by="words")

df_trans_cat3.2 <- df_trans_cat2 %>% 
  left_join(rt_category_lookup2, by="words")
  
# summarise
ds_trans_cat3.1 <- df_trans_cat3.1 %>% 
  mutate(n = sapply(str_split(words, " "), length)) %>% 
  group_by(trans_ID, category) %>% 
  count(category, wt=n) %>% 
  ungroup()

ds_trans_cat3.2 <- df_trans_cat3.2 %>%
  mutate(n = sapply(str_split(words, " "), length)) %>% 
  group_by(trans_ID, category) %>% 
  count(category, wt=n) %>% 
  ungroup()

# Select the highest ranked category for each trans_ID
ds_trans_cat4.1 <- ds_trans_cat3.1 %>% 
  group_by(trans_ID) %>% 
  slice_max(order_by = n) %>% 
  ungroup()

ds_trans_cat4.2 <- ds_trans_cat3.2 %>% 
  group_by(trans_ID) %>% 
  slice_max(order_by = n) %>% 
  ungroup()

# Merge category to dt_trans
dt_transactions1 <- dt_transactions %>% 
  left_join(ds_trans_cat4.1, by = 'trans_ID', suffix = c("0", "1"))
  
dt_transactions2 <- dt_transactions %>% 
  left_join(ds_trans_cat4.2, by = 'trans_ID', suffix = c("0", "1"))

#save to csv for review
v.objects <- c('dt_transactions1', 'dt_transactions2')


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

