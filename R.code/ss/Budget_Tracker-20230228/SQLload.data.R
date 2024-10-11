library(DBI)
library(odbc)

con<-dbConnect(odbc::odbc(),"SQLServer_DSN")

source("R.code/load.tables.R")
df.rt_common_words<-tibble(word=rt_common_words)
dbAppendTable(con,"dt_common_words", df.rt_common_words)



dbDisconnect(con)
