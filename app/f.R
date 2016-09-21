library("RMySQL")
dbquery <- function(querytext){
            # import db connection details
            source("dbcon_details.R")
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
