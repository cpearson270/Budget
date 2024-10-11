#### create.rt_category
## create a data base of descriptions for categories
## REQUIRES: Imported data from pocket book
## Project: Budget Tracker 4.0 - 07/02/2022 - Moved from excel tracker to R
## Last update: 06/08/2022

### STARTER SCRIPT ###

### SPECIFIC TO NAMED FILE ###

### Rules
# Only save rt_budget if changed (updated from excel or new cat. added).
# "p." prefix for user inputted values.
# "v." prefix for formatted user values or internally generated vales.
# "df_" data frame - temporary or subset data, not saved.
# "ds_" data summary - summarised data, usually saved.
# "dt_" data table - data loaded in and can be written to.
# "rt_" reference table - data loaded in, but NOT written to.

### Load initial functions and packages
## Load main package: tidyverse
library(tidyverse) 
library(lubridate)
library(knitr)
library(kableExtra)

source("R.code/fun_main.R")

### Load in pocketbook data
rt_category1<-read_csv("R.data/pocketbook-export-ALL TIME.csv") %>% 
  select(description, category, notes)

##clean up data
v.food<-rt_category1 %>% 
  select(category) %>% 
  filter(grepl("Food", category)) %>% 
  unique() %>% 
  pull()

v.ignore<- c('Aus Tap And Pay Value Date',
             'Ptag',
             'Aus Card Value Date',
             'Melbourne',
             'Australia',
             'sq')

rt_category2<- rt_category1 %>% 
  mutate(category = replace(category, category %in% v.food, "Food & Drinks"),
         description = str_remove_all(description, 
                                      paste(v.ignore, collapse = "|")),
         # removes spaces at start and end
         description = str_remove(description, "^ | $")) %>% 
  distinct() #could also use 'unique()'

## Pull out descriptions with multiple categories assigned
df_duplicates <- rt_category2 %>% 
  filter(description %in% description[duplicated(description)]) %>% 
  arrange(description)

### WARNING: FOLLOWING IS SPECIFIC TO NAMED FILE ###

## choose default category for above descriptions (remove other rows)
df_duplicate_remove<- df_duplicates %>% 
  slice(-c(2,3,5,8, 10, 12, 24, 26, 27, 29, 34, 36, 39, 40, 45, 47, 48, 51, 
          54, 59, 66, 72, 74, 76, 78, 80))

rt_category3<- anti_join(rt_category2,df_duplicate_remove) %>% 
  mutate(notes = replace(notes, notes == "Harness + beaner", NA))

## Example filter for non-"Coles Express" descriptions
rt_category4 <- rt_category3 %>% 
  filter(grepl("coles", description, ignore.case = TRUE))

# Two ways to create the same filter
df1<-rt_category4 %>% 
  filter(str_detect(
    description, regex("Coles.(?!express)", ignore_case = TRUE)))

df2<-rt_category4 %>% 
  filter(grepl(
    "Coles.(?!express)", description, perl = TRUE, ignore.case = TRUE))
