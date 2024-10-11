#### create.rt_category_lookup1.1
## create list of key words for each category - adjusted to include 2 word pairs
## REQUIRES: 
## CALLED BY: compile.dt_transactions.R
## Project: Budget Tracker 4.0 - 09/01/2022 - Moved from excel tracker to R
## Last update: 31/01/2022

library(tidyverse)
source("R.code/fun_main.R")

data <- load_recent_rds("dt_transactions")

##Remove ends of transaction description
data$description<- data$description %>%
  str_split("\\s(aus\\s)?(value\\sdate|tap\\sand\\spay|card)\\b") %>% 
  sapply('[',1)

# Create a new column that splits the description into separate words
#data <- data %>%
 # mutate(words = str_split(description, " "))

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
word_pairs <- data %>%
  mutate(words = map(description, split_words)) %>%
  unnest(words) %>%
  distinct(category, words)
###

# Use unnest() to create a row for each word in the 'words' column
#data <- data %>%
 # unnest(words)

# Group the data by category and count the number of occurrences of each word
word_counts <- word_pairs %>%
  group_by(category) %>%
  count(words)

# Identify words that appear in more than 3 categories
common_words <- word_counts %>%
  group_by(words) %>%
  summarize(num_categories = n()) %>% ## a column which counts the number of rows in each group
  filter(num_categories > 3) %>%
  pull(words)

# Filter out the overlapping words from the word_counts table
word_counts_filtered <- word_counts %>%
  filter(!words %in% common_words)

# Identify words that appear in two or three categories
ambiguous_words <- word_counts_filtered %>%
  group_by(words) %>%
  summarize(num_categories = n()) %>%
  filter(num_categories > 1, num_categories <= 3) %>%
  pull(words)

# Remove ambiguous words from word_counts_filtered
word_counts_filtered2 <- word_counts_filtered %>%
  filter(!words %in% ambiguous_words)

# For ambiguous words, display the categories they appear in and prompt user to choose the correct category
for (word in ambiguous_words) {
  categories <- word_counts_filtered %>%
    filter(words == word) %>%
    pull(category)
  
  cat(sprintf("The word '%s' appears in the following categories:\n", word))
  
  df_categories <- tibble(ID = row_number(categories),
                          category = categories) %>% 
    print()
  
  print("Type the ID number for the category should this word be 
    assigned to: ")
  p.cat_ID <- scan()
  
  if(is_empty(p.cat_ID)) {
    common_words <-  append(common_words, word)
  } else {
    v.cat <- df_categories %>% 
      filter(ID == p.cat_ID) %>% 
      pull(category)
    
    word_counts_filtered2 <- rows_append(word_counts_filtered2,
                                         tibble(category = v.cat,
                                                words = word,
                                                n = 1))
  }}

rm('df_categories', 'i', 'p.cat_ID', 'v.cat', 'word')

saveRDS(word_counts_filtered,
        paste("R.code/ref_data/rt_category_lookup/rt_category_lookup-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

saveRDS(word_counts_filtered2,
        paste("R.code/ref_data/rt_category_lookup2/rt_category_lookup2-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))

saveRDS(common_words,
        paste("R.code/ref_data/rt_common_words/rt_common_words-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds",
              sep=""))
