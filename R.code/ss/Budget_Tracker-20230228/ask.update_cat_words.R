#### ask.update_cat_words
## Asks user if words from excel should update categorisation tables
## REQUIRES: N/A
## CALLED BY: Budget_Tracker P2
## Project: Budget Tracker P2 - 28/02/2023
## Last update: 25/04/2023

cats_update<-readline(
  prompt = "Update categorisation words from Excel? (Y/N):") %>% 
  tolower()

while(!(budget_update %in% c('y','n'))) {
  cats_update<-readline(
    prompt = "Update categorisation words from Excel? (Y/N):") %>% 
    tolower()
}
