#### update.ref_tables
## Manually update the category lookup table and common words list

### STARTER SCRIPT ###

## REQUIRES: NA
## CALLED BY: NA
## Project: Budget Tracker 4.0 - 26/02/2023 - Moved from excel tracker to R
## Last update: 07/07/2024

### Rules
# Only save rt_budget if changed (updated from excel or new cat. added).
# "p." prefix for user inputted values.
# "v." prefix for formatted user values or internally generated vales.
# "df_" data frame - temporary or subset data, not saved.
# "ds_" data summary - summarised data, usually saved.
# "dt_" data table - data loaded in and can be written to.
# "rt_" reference table - data loaded in, but NOT written to.

### Set variables
# Packages
required_libs <- c("tidyverse")
# words to be removed from category table and added to common words list
words_common <- c("cremorne", "cremorne vi")
# update the category assigned to these words
catwords <- c("")
catwords_cat <- c("")

### Load initial functions and packages
## Load packages

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
}
rm(required_libs)

## load custom function
source("R.code/fun.load.R")

## load tables
rt_common_words <- load_recent_rds("rt_common_words")
rt_category_lookup <- load_recent_rds("rt_category_lookup")

# Filter out common words from rt_category_lookup
rt_category_lookup <- rt_category_lookup %>% 
  filter(!words %in% words_common)

# Add common words to rt_common_words
rt_common_words <- append(rt_common_words, words_common) %>% 
  sort()

# Save updated ref tables
saveRDS(rt_category_lookup,
        paste("R.code/ref_data/rt_category_lookup/rt_category_lookup-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

saveRDS(rt_common_words,
        paste("R.code/ref_data/rt_common_words/rt_common_words-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))
