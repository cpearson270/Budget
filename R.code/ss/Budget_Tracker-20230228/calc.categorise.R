#### calc.categorise
## categorise transactions
## REQUIRES: trans.file.date, load.bank_trans.R, load.tables.R
## CALLED BY: compile.dt_transactions.R; Budget_Tracker-P1-20230228
## Project: Budget Tracker - Pocketbook gone fishing
## Last update: 27/02/2022

## Load packages
required_libs <- c("tidyverse", "openxlsx")

if(any(!required_libs %in% (.packages()))){
  load_libs <- dplyr::setdiff(required_libs, (.packages()))
  lapply(load_libs, library, character.only = TRUE)
  rm(load_libs)
}
rm(required_libs)

## load custom function
source("R.code/fun.load.R")

# Give each transaction a unique ID
df_trans_load <- df_trans_load %>% 
  mutate(trans_ID = row_number(description))

df_trans_split <- df_trans_load

##Remove ends of transaction description
df_trans_split$description<- df_trans_split$description %>%
  str_split("\\s(aus\\s)?(value\\sdate|tap\\sand\\spay|card)\\b") %>% 
  sapply('[',1)

#### strings in the named column are split into single words and pairs of words
## output is a vector of the words and word pairs
split_words <- function(col_name) {
  # Split the description into individual words
  words <- str_split(col_name, "\\s+")[[1]]
  # Combine adjacent pairs of words with a space
  pairs <- sapply(1:(length(words)-1), 
                  function(i) paste(words[i], words[i+1], sep = " "))
  # Return the pairs and single words
  c(words, pairs)
}

# Create a new col 'word' with individual words and word pairs then use
  # unest() fn to add each word and word pair to its own row
df_trans_split <- df_trans_split %>%
  mutate(words = map(description, split_words)) %>%
  unnest(words) %>%
  select(trans_ID, words)

# Clean up words by removing common words
df_trans_split<- df_trans_split %>% 
  filter(!words %in% rt_common_words)

# take cleaned list and match to categories
df_trans_split <- df_trans_split %>% 
  left_join(rt_category_lookup, by="words", relationship = "many-to-many")

# Summarise categories assigned to each word and word pairs
ds_trans_split <- df_trans_split %>%
  # count the weighting for each category matched to each trans_ID
  group_by(trans_ID, category) %>% 
  count(category, wt = wt) %>% 
  drop_na() %>% 
  ungroup()

# Select the highest ranked category for each trans_ID
df_trans_cat <- ds_trans_split %>% 
  group_by(trans_ID) %>% 
  slice_max(order_by = n, with_ties = FALSE) %>% 
  ungroup()

# Merge category to df_trans_cat
ds_trans_cat <- df_trans_load %>% 
  left_join(df_trans_cat, by = 'trans_ID') %>% 
  select(date, amount, description, category, trans_ID, n) %>% 
  mutate(words = character(1),
         notes = character(1)) %>% 
  arrange(date)

# Fix openxlsx stupid date format default
options(openxlsx.dateFormat = "dd/mm/yyyy")

# Create workbook
wb = createWorkbook()

# Add worksheet
addWorksheet(wb, "transactions")

# Add dataframe to the sheet
writeData(wb, sheet = "transactions", x = ds_trans_cat, startCol = 1)

rt_category = unique(rt_category_lookup$category) %>% 
  sort()

# Add worksheet "Drop-down values" to the workbook
addWorksheet(wb, "refs")

# Add drop-down values dataframe to the sheet "Drop-down values"
writeData(wb, sheet = "refs", x = rt_category, startCol = 1)

# Add drop-downs to the column Gender on the worksheet "Customers"
dataValidation(wb, "transactions", col = 4, rows = 2:nrow(df_trans_load)+1,
               type = "list",
               value = "'refs'!$A:$A")

setColWidths(wb, "transactions", cols = 1:ncol(ds_trans_cat),
             widths = c(10, "auto", "auto", "auto", "auto", "auto"))

# Save workbook
dir.create(paste("R.output", trans_file_date, sep="/"))

path_table <- paste("R.output", trans_file_date, "ds_trans_cat", sep="/")

saveWorkbook(wb,  
             paste(path_table,"-",
                        format(Sys.time(), "%Y%m%d-%H%M%S"),
                        ".xlsx",
                        sep=""),
             overwrite = TRUE)
