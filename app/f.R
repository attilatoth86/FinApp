library("RPostgreSQL")
psqlQuery <- function(statement){
    drv <- dbDriver("PostgreSQL")
    source("dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = psqldburl, port = psqldbport,
                         user = psqldbuser, password = psqldbpw)
    data <- dbGetQuery(psqlcon, statement)
    data
}
psqlInsert <- function(df, tablename){
    drv <- dbDriver("PostgreSQL")
    source("dbcon_details.R", local = T)
    psqlcon <- dbConnect(drv, dbname = psqldbname,
                         host = psqldburl, port = psqldbport,
                         user = psqldbuser, password = psqldbpw)
    dbWriteTable(psqlcon, tablename, 
                 value = df, append = TRUE, row.names = FALSE)
}


library("RMySQL")
dbquery <- function(querytext){
            # import db connection details
            source("dbcon_details.R", local = T)
            # establish db connection
            dbcon <- dbConnect(MySQL(),
                               user=dbuser, 
                               password=dbpw,
                               dbname=dbname, 
                               host=dburl)
            # query
            rs <- dbSendQuery(dbcon, querytext)
            data <- fetch(rs, n = -1)
            huh <- dbHasCompleted(rs)
            dbClearResult(rs)
            dbDisconnect(dbcon)

            data
}

dbinsert <- function(df,ld_tablename){
    # import db connection details
    source("dbcon_details.R")
    # establish db connection
    dbcon <- dbConnect(MySQL(),
                       user=dbuser, 
                       password=dbpw,
                       dbname=dbname, 
                       host=dburl)
    # Truncate table beforehand
    truncateStatement <- sprintf("TRUNCATE TABLE finapp.%s",ld_tablename)
    dbSendQuery(dbcon, truncateStatement)
    # insert into database
    dbWriteTable(dbcon, name=ld_tablename, value=df, row.names=F, append = T)
}

dbUIinsert <- function(df,tablename){
    # import db connection details
    source("dbcon_details.R")
    # establish db connection
    dbcon <- dbConnect(MySQL(),
                       user=dbuser, 
                       password=dbpw,
                       dbname=dbname, 
                       host=dburl)
    # insert into database
    dbWriteTable(dbcon, name=tablename, value=df, row.names=F, append = T)
}
