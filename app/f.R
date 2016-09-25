library("RPostgreSQL")

psqlQuery <- function(statement){
    drv <- dbDriver("PostgreSQL")
    source("dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = psqldburl, port = psqldbport,
                         user = psqldbuser, password = psqldbpw)
    data <- dbGetQuery(psqlcon, statement)
    RPostgreSQL::dbDisconnect(psqlcon)
    data
}

psqlInsert <- function(df, tablename){
    drv <- dbDriver("PostgreSQL")
    source("dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = psqldburl, port = psqldbport,
                         user = psqldbuser, password = psqldbpw)
    RPostgreSQL::dbWriteTable(psqlcon, tablename, value = df, append = T, row.names = F)
    RPostgreSQL::dbDisconnect(psqlcon)
}
