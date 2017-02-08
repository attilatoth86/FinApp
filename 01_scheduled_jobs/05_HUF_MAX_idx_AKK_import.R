message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(xlsx)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Other setups..")
options(scipen = 999, digits = 4, encoding = "iso-8859-2")

retrieve_url <- "http://akk.hu/hu/statisztika/hozamok-indexek-forgalmi-adatok/max-index?download=1"
message(paste("Source URL:",retrieve_url))
message("downloading source file..")
download.file(url=retrieve_url,destfile = "tmp.xlsx", method = "curl")

df_import <- read.xlsx("tmp.xlsx", sheetIndex = 1)
file.remove("tmp.xlsx")
ld_tbl_dump <- cbind(
    df_import[,c(2,6,3)]
)

message("Downloaded data to be imported:")
print(ld_tbl_dump)

truncateStatus <- psqlQuery("TRUNCATE TABLE app.ld_rate_value")
message(paste("Truncate app.ld_rate_value..",truncateStatus$errorMsg))

ldInsertStatus <- psqlInsert(ld_tbl_dump, "ld_rate_value")
message(paste("Insert into app.ld_rate_value..",ldInsertStatus$errorMsg))

finalInsertStatus <- psqlQuery("INSERT INTO app.rate_value (rate_id,value_date,value)
                                  SELECT
                                  r.id::int,
                                  to_date(ldrv.value_date,'yyyy-mm-dd'),
                                  ldrv.value::float
                                  FROM
                                  app.ld_rate_value ldrv
                                  LEFT OUTER JOIN app.rate r ON ldrv.rate_name=r.rate_name")

message(paste("Insert into app.rate_value..",finalInsertStatus$errorMsg))

message(paste("Job ends:",Sys.time()))
message("----------------------------------------")

