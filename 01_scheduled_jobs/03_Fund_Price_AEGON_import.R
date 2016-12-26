print(paste("Job starts:",Sys.time()))

print("Importing necessary packages..")
library(gdata)

print("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

print("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

print("Creating import control table..")
fp_import_ctrltbl <- psqlQuery("SELECT f.id fund_id, f.fund_name, f.source_id, MIN(COALESCE(fpv.value_date,fit.value_date-1))+1 import_date_from
                               FROM app.fund_investment_transaction fit
                               INNER JOIN app.fund f ON fit.fund_id=f.id
                               LEFT OUTER JOIN app.fund_price_recent_vw fpv ON fit.fund_id=fpv.fund_id
                               GROUP BY f.id, f.fund_name, f.source_id")$result

print("Content of control table:")
print(fp_import_ctrltbl)

print("Importing..")
price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character(),stringsAsFactors=F)
for(i in 1:nrow(fp_import_ctrltbl)){
    print(paste("processing: ", fp_import_ctrltbl[i,2]))
    print(paste("downloading..",
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

print("Loading imported data into app.ld_fund_price..")
print(price_tbl_pre)

truncateStatus <- psqlQuery("TRUNCATE TABLE app.ld_fund_price")
print(paste("Truncate app.ld_fund_price..",truncateStatus$errorMsg))

ldInsertStatus <- psqlInsert(price_tbl_pre, "ld_fund_price")
print(paste("Insert into app.ld_fund_price..",ldInsertStatus$errorMsg))

finalInsertStatus <- psqlQuery("INSERT INTO app.fund_price (fund_id,value_date,price)
                               SELECT
                               ldfp.fund_id::int
                               ,to_date(ldfp.date, 'yyyy-mm-dd')
                               ,ldfp.price::float
                               FROM app.ld_fund_price ldfp
                               LEFT OUTER JOIN app.fund_price fp ON ldfp.fund_id::int=fp.fund_id
                               AND to_date(ldfp.date, 'yyyy-mm-dd')=fp.value_date
                               LEFT OUTER JOIN (SELECT f.id fund_id, MIN(COALESCE(fpv.value_date,fit.value_date-1))+1 import_date_from
                               FROM app.fund_investment_transaction fit
                               INNER JOIN app.fund f ON fit.fund_id=f.id
                               LEFT OUTER JOIN app.fund_price_recent_vw fpv ON fit.fund_id=fpv.fund_id
                               GROUP BY f.id) t ON ldfp.fund_id::int=t.fund_id
                               WHERE fp.id IS NULL
                               AND to_date(ldfp.date, 'yyyy-mm-dd')>=t.import_date_from
                               ORDER BY ldfp.date DESC, ldfp.fund_id")

print(paste("Insert into app.fund_price..",finalInsertStatus$errorMsg))

print(paste("Job ends:",Sys.time()))
