#### update.cat_words
## Extract new look up words from loaded excel and update tables used for 
  # categorisation
## REQUIRES: Budget_Tracker - P2
## CALLED BY: Budget_Tracker - P2
## Project: Budget Tracker 4.0 - 28/02/2023
## Last update: 25/04/2023

## Load packages
required_libs <- c("tidyverse")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
}
rm(required_libs)

# extract new lookup values to be added to rt_category_lookup
df_new_cat_lookup <- df_trans_cat %>%
  separate_rows(words, sep = ", ") %>%
  select(category, words) %>% 
  mutate(wt = sapply(str_split(words, " "), length)) %>% 
  na.exclude()

if (length(df_new_cat_lookup) > 0) {
  # get words already in rt and increase weighting by 1
  df_new_cat_lookup1 <- rt_category_lookup %>% 
    inner_join(df_new_cat_lookup,
               by = c('category', 'words')) %>% 
    select(!(wt.y)) %>% 
    rename(wt = wt.x) %>% 
    mutate(wt = wt+1)
  
  # update the weighting in rt_category_lookup 
  rt_category_lookup <- rt_category_lookup %>% 
    rows_update(df_new_cat_lookup1,
                by= c('category', 'words'))
  
  # add new words to rt_category_lookup and save as rds
  rt_category_lookup <- rt_category_lookup %>% 
    rows_insert(df_new_cat_lookup,
                by= c('category', 'words'),
                conflict = 'ignore')
  
  if (any(rt_common_words %in% df_new_cat_lookup$words)) {
    # remove any words now assigned to a category, from rt_common_words
    rt_common_words <- rt_common_words[
      !(rt_common_words %in% df_new_cat_lookup$words)]
  }
}
