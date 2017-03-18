message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(gdata)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

message("Creating import control table..")
fp_import_ctrltbl <- psqlQuery("SELECT f.id fund_id, f.fund_name, f.source_id, COALESCE(fpv.value_date,f.inception_date-1)+1 import_date_from
                               FROM app.fund f
                               LEFT OUTER JOIN app.fund_price_recent_vw fpv ON f.id=fpv.fund_id
                               WHERE f.description like 'AEGON%'")$result

message("Content of control table:")
print(fp_import_ctrltbl)

message("Importing..")
price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character(),stringsAsFactors=F)
for(i in 1:nrow(fp_import_ctrltbl)){
    message(paste("processing: ", fp_import_ctrltbl[i,2]))
    message(paste("downloading..",
                sprintf("https://www.aegonalapkezelo.hu/elemzes/grafikonrajzolo/?id%%5B%%5D=%s&mode=download&min_date=%s&max_date=%s",
                        fp_import_ctrltbl[i,3],
                        fp_import_ctrltbl[i,4],
                        Sys.Date())))
    download.file(url=sprintf("https://www.aegonalapkezelo.hu/elemzes/grafikonrajzolo/?id%%5B%%5D=%s&mode=download&min_date=%s&max_date=%s",
                              fp_import_ctrltbl[i,3],
                              fp_import_ctrltbl[i,4],
                              Sys.Date()),
                  destfile = "tmp.xls",
                  method = "curl")
    if(length(readLines(xls2csv("tmp.xls")))==0){
        price_tbl_pre <- rbind(price_tbl_pre,
                               data.frame(date=character(),fund_id=character(),fund_name=character(),price=character(),stringsAsFactors=F))
    }
    else {
        fileContent <- read.xls("tmp.xls",sheet = 1,stringsAsFactors=F)
        price_tbl_pre <- rbind(price_tbl_pre,
                               cbind(fileContent[-1,1],fp_import_ctrltbl[i,1],fp_import_ctrltbl[i,2],fileContent[-1,3]))
    }
    file.remove("tmp.xls")
}

message("Downloaded data to be imported:")
print(price_tbl_pre)

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

refreshMatVw <- psqlQuery("REFRESH MATERIALIZED VIEW app.portfolio_return_calc_mvw")

message(paste("Refresh app.portfolio_return_calc_mvw materialized view..",refreshMatVw$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")
