message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(xlsx)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

retrieve_url <- "http://www.akk.hu/hu/statisztika/hozamok-indexek-forgalmi-adatok/referenciahozamok?download=1"
message(paste("Source URL:",retrieve_url))
message("downloading source file..")
download.file(url=retrieve_url,destfile = "tmp.xlsx", method = "curl")

df_import <- read.xlsx("tmp.xlsx", sheetIndex = 1)
file.remove("tmp.xlsx")

df_import$tenor[df_import[,3]=="M3"] <- 90
df_import$tenor[df_import[,3]=="M6"] <- 180
df_import$tenor[df_import[,3]=="M12"] <- 365
df_import$tenor[df_import[,3]=="Y3"] <- 1095
df_import$tenor[df_import[,3]=="Y5"] <- 1825
df_import$tenor[df_import[,3]=="Y10"] <- 3650
df_import$tenor[df_import[,3]=="Y15"] <- 5475

ld_tbl_dump <- cbind(
    df_import[,c(2,13,7)],
    type="HUF Benchmark Rates",
    currency_id=psqlQuery("SELECT id FROM app.currency WHERE iso_code='HUF'")$result[1,1]
)

message("Downloaded data to be imported:")
print(ld_tbl_dump)

truncateStatus <- psqlQuery("TRUNCATE TABLE app.ld_yield_curve")
message(paste("Truncate app.ld_yield_curve..",truncateStatus$errorMsg))

ldInsertStatus <- psqlInsert(ld_tbl_dump, "ld_yield_curve")
message(paste("Insert into app.ld_yield_curve..",ldInsertStatus$errorMsg))

finalInsertStatus <- psqlQuery("INSERT INTO app.yield_curve (currency_id, tenor, type, value_date, yield)
                               SELECT
                               ldyc.currency_id::int,
                               ldyc.tenor::DOUBLE PRECISION,
                               ldyc.type,
                               to_date(ldyc.date,'yyyy-mm-dd'),
                               ldyc.yield::DOUBLE PRECISION
                               FROM app.ld_yield_curve ldyc
                               LEFT OUTER JOIN app.yield_curve yc ON ldyc.currency_id::INT=yc.currency_id
                               AND ldyc.tenor::DOUBLE PRECISION=yc.tenor
                               AND ldyc.type=yc.type
                               AND to_date(ldyc.date,'yyyy-mm-dd')=yc.value_date
                               WHERE yc.id IS NULL")

message(paste("Insert into app.yield_curve..",finalInsertStatus$errorMsg))

refreshMatVw <- psqlQuery("REFRESH MATERIALIZED VIEW app.yield_curve_curr_mvw")

message(paste("Refresh app.yield_curve_curr_mvw materialized view..",refreshMatVw$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")

