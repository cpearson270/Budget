#### create.rt_category_lookup2.0
## create list of key words for each category -re-writen to include 
  #2 word pairs
## REQUIRES: 
## CALLED BY: compile.dt_transactions.R
## Project: Budget Tracker 4.0 - 09/01/2022 - Moved from excel tracker to R
## Last update: 18/02/2022

library(tidyverse)
source("R.code/fun_main.R")

transactions <- load_recent_rds("dt_transactions")

# Define a function to extract 2-word pairs from a string
#extract_pairs <- function(x) {
#  x <- tolower(x) # Convert to lowercase
#  x <- str_remove_all(x, "[[:punct:]]") # Remove punctuation
#  words <- str_split(x, "\\s+")[[1]] # Split into words
#  pairs <- combn(words, 2, paste, collapse = " ") # Extract 2-word pairs
#  return(pairs)
#}

extract_pairs <- function(text) {
  words <- str_split(text, "\\s+")[[1]]
  pairs <- combn(words, 2, paste, collapse = "-")
  paste(pairs, collapse = ",")
}


# Extract 2-word pairs for each category
pairs <- transactions %>%
  group_by(category) %>%
  summarize(pairs = map_chr(description, extract_pairs) %>% flatten_chr()) %>%
  ungroup()



###
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
word_pairs <- transactions %>%
  mutate(words = map(description, split_words)) %>%
  unnest(words) %>%
  distinct(category, words)


####

# Count the number of times each pair appears in each category
pair_counts <- pairs %>%
  separate_rows(pairs, sep = " ") %>%
  group_by(category, pairs) %>%
  summarize(count = n()) %>%
  ungroup()

# Find pairs that appear in more than three categories
common_pairs <- pair_counts %>%
  group_by(pairs) %>%
  summarize(num_categories = n_distinct(category)) %>%
  filter(num_categories > 3) %>%
  pull(pairs)

# Remove common pairs from the data
pairs_filtered <- pairs %>%
  mutate(pairs = map(pairs, str_remove_all, common_pairs, simplify = TRUE)) %>%
  filter(map_lgl(pairs, function(x) length(x) > 0)) %>%
  unnest(pairs)

# Find pairs that appear in two or three categories
duplicates <- pairs_filtered %>%
  group_by(pairs) %>%
  summarize(num_categories = n_distinct(category)) %>%
  filter(num_categories > 1) %>%
  ungroup()

# Prompt the user to select the correct category for each duplicate pair
for (i in seq_along(duplicates$pairs)) {
  pair <- duplicates$pairs[i]
  categories <- pairs_filtered %>%
    filter(pairs == pair) %>%
    pull(category)
  correct_category <- select.list(categories, title = sprintf("Select the correct category for '%s':", pair))
  pairs_filtered <- pairs_filtered %>%
    filter(!(pairs == pair & category != correct_category))
}

# Print the final table of pairs for each category
final_pairs <- pairs_filtered %>%
  filter(!duplicated(paste(category, pairs))) %>%
  group_by(category) %>%
  summarize(pairs = paste(pairs, collapse = ", "))
print(final_pairs)
