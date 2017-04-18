message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(RCurl)
library(XML)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Creating import control table..")
fp_import_ctrltbl <- psqlQuery("SELECT f.id fund_id, f.fund_name, f.isin, COALESCE(fpv.value_date,f.inception_date-1)+1 import_date_from
                               FROM app.fund f
                               LEFT OUTER JOIN app.fund_price_recent_vw fpv ON f.id=fpv.fund_id
                               WHERE f.description like 'Credit Suisse%'")$result

message("Content of control table:")
print(fp_import_ctrltbl)

message("Importing..")
price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character(),stringsAsFactors=F)
for(i in 1:nrow(fp_import_ctrltbl)){
    message(paste("processing: ", fp_import_ctrltbl[i,2]))
    message(paste("url: ",sprintf("https://amfunds.credit-suisse.com/ch/en/retail/fund/history/%s?currency=CHF",fp_import_ctrltbl[i,3])))
    download.file(url=sprintf("https://amfunds.credit-suisse.com/ch/en/retail/fund/history/%s?currency=CHF",fp_import_ctrltbl[i,3]),
                  destfile = "tmp.html",
                  method = "curl")
    
    html_import <- readHTMLTable("tmp.html", trim = T, header = F, as.data.frame = T, stringsAsFactors=F)
    price_df <- html_import[[1]][!is.na(html_import[[1]][,2]),][-1,]
    colnames(price_df) <- unlist(html_import[[1]][6,])
    price_df$`NAV Date` <- as.Date(price_df$`NAV Date`,"%d.%m.%Y")
    
    price_tbl_pre <- rbind(price_tbl_pre,
                           data.frame(date=price_df$`NAV Date`,
                                      fund_id=fp_import_ctrltbl[i,"fund_id"],
                                      fund_name=fp_import_ctrltbl[i,"fund_name"],
                                      price=price_df$NAV,
                                      stringsAsFactors = F))
    
    file.remove("tmp.html")
}

message("Downloaded data to be imported:")
print(head(price_tbl_pre,10))

truncateStatus <- psqlQuery("TRUNCATE TABLE app.ld_fund_price")
message(paste("Truncate app.ld_fund_price..",truncateStatus$errorMsg))

ldInsertStatus <- psqlInsert(price_tbl_pre, "ld_fund_price")
message(paste("Insert into app.ld_fund_price..",ldInsertStatus$errorMsg))

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

refreshMatVw1 <- psqlQuery("REFRESH MATERIALIZED VIEW app.portfolio_return_calc_mvw")
refreshMatVw2 <- psqlQuery("REFRESH MATERIALIZED VIEW app.portf_acc_return_calc_mvw")

message(paste("Refresh app.portfolio_return_calc_mvw materialized view..",refreshMatVw1$errorMsg))
message(paste("Refresh app.portf_acc_return_calc_mvw materialized view..",refreshMatVw2$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")