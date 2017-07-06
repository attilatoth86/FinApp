message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(RCurl)
library(XML)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

message("Creating import control table..")
fp_import_ctrltbl <- psqlQuery("SELECT f.id fund_id, f.isin, f.fund_name, f.source_id, COALESCE(fpv.value_date,f.inception_date-1)+1 import_date_from
                               FROM app.fund f
                               LEFT OUTER JOIN app.fund_price_recent_vw fpv ON f.id=fpv.fund_id
                               WHERE f.ISIN like 'HU%'")$result

message("Content of control table:")
print(fp_import_ctrltbl)

message("Importing..")
for(i_fundid in fp_import_ctrltbl$fund_id){
    message(paste0("..processing: ",fp_import_ctrltbl[fp_import_ctrltbl$fund_id==i_fundid,"fund_name"]))
    retrieve_url <- sprintf("http://www.bamosz.hu/en/alapoldal?isin=%s",fp_import_ctrltbl[fp_import_ctrltbl$fund_id==i_fundid,"isin"])
    message(paste0("..reading in from URL: ",retrieve_url))
    web_import <- getURL(retrieve_url)
    df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[7]][-1,c(1:4)]
    
    colnames(df_import) <- c("date","price","net_asset_value","paid_dividend")
    df_import$date <- as.Date(df_import$date,"%Y.%m.%d.")
    df_import$price <- as.numeric(df_import$price)
    df_import$net_asset_value <- as.numeric(gsub(",","",df_import$net_asset_value))
    df_import$paid_dividend <- as.numeric(df_import$paid_dividend)
    df_import$fund_id <- i_fundid
    df_import$fund_name <- fp_import_ctrltbl[fp_import_ctrltbl$fund_id==i_fundid,"fund_name"]

    message("Fetched data (first 7): ")
    print(head(df_import,7))
    
    db_cont <- psqlQuery(sprintf("SELECT value_date FROM app.fund_price WHERE fund_id=%i",i_fundid))$result
    
    df_to_load <- df_import[!(df_import$date %in% db_cont$value_date),]
    df_to_load$net_asset_value <- NULL
    df_to_load$paid_dividend <- NULL
    df_to_load <- data.frame(date=df_to_load$date,
                             fund_id=df_to_load$fund_id,
                             fund_name=df_to_load$fund_name,
                             price=df_to_load$price)
    
    message("Content to load in db: ")
    print(df_to_load)
    
    if(nrow(df_to_load)>0)
    {
        truncateStatus <- psqlQuery("TRUNCATE TABLE app.ld_fund_price;")
        message(paste0("Truncate app.ld_fund_price table........",truncateStatus$errorMsg))
        insertLdStatus <- psqlInsert(df_to_load,"ld_fund_price")
        message(paste0("Insert into app.ld_fund_price table........",insertLdStatus$errorMsg))
        finalInsertStatus <- psqlQuery("INSERT INTO app.fund_price (fund_id,value_date,price)
                               SELECT
                               ldfp.fund_id::int
                               ,to_date(ldfp.date, 'yyyy-mm-dd')
                               ,ldfp.price::float
                               FROM app.ld_fund_price ldfp
                               LEFT OUTER JOIN app.fund_price fp ON ldfp.fund_id::int=fp.fund_id
                               AND to_date(ldfp.date, 'yyyy-mm-dd')=fp.value_date
                               WHERE fp.id IS NULL")
        
        message(paste("Insert into app.fund_price..",finalInsertStatus$errorMsg))
        updPortfRefDate <- psqlQuery("
                            UPDATE app.portfolio SET last_valuation_date=u.vd
                            FROM (
                                                         SELECT 
                                                         pxa.portfolio_id, 
                                                         MIN(fpr.value_date) vd
                                                         FROM 
                                                         app.portfolio_x_account_rltnp pxa,
                                                         app.account a,
                                                         app.fund_investment_transaction fit,
                                                         app.fund_price_recent_vw fpr
                                                         WHERE 
                                                         pxa.account_id=a.id
                                                         AND a.id=fit.account_id
                                                         AND fit.fund_id=fpr.fund_id
                                                         GROUP BY pxa.portfolio_id
                            ) AS u
                                                         WHERE app.portfolio.id = u.portfolio_id                             
                             ")
        message(paste("Update app.portfolio with new portfolio valuation date..",updPortfRefDate$errorMsg))
    }
    message("\n")
}

refreshMatVw1 <- psqlQuery("REFRESH MATERIALIZED VIEW app.portfolio_return_calc_mvw")
refreshMatVw2 <- psqlQuery("REFRESH MATERIALIZED VIEW app.portf_acc_return_calc_mvw")

message(paste("Refresh app.portfolio_return_calc_mvw materialized view..",refreshMatVw1$errorMsg))
message(paste("Refresh app.portf_acc_return_calc_mvw materialized view..",refreshMatVw2$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")
