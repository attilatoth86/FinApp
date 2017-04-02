message("----------------------------------------")
message(paste("Job starts:",Sys.time()))

message("Importing necessary packages..")
library(RCurl)
library(XML)

message("Importing functions..")
source("/srv/shiny-server/finapp/f.R")

message("Creating import control table..")
import_fx_df <- psqlQuery("SELECT c.iso_code, rfx.id,
                            COALESCE(MAX(rv.value_date)+1, MIN(t.date_open)) import_date,
                          MIN(t.date_open) date_open_min, MAX(rv.value_date) last_fx_rate_value
                          FROM
                          app.account t,
                          app.currency c,
                          app.rate_fx rfx,
                          app.rate_value rv
                          WHERE t.currency_id=c.id
                          AND c.iso_code!='HUF'
                          AND c.id=rfx.from_currency_id
                          AND rfx.to_currency_id=(SELECT id FROM app.currency WHERE iso_code='HUF')
                          AND rfx.id=rv.rate_id
                          GROUP BY c.iso_code, rfx.id")$result

message("Content of control table:")
print(import_fx_df)

clear_ld_tbl <- psqlQuery("TRUNCATE TABLE app.ld_rate_value")
message(paste0(message("Truncate LD table.."),clear_ld_tbl$errorMsg))

# Importing
message("Importing..")
if(length(import_fx_df$iso_code)!=0){
    for(i in 1:length(import_fx_df$iso_code)) {
        message(paste("Currency being processed: ",import_fx_df$iso_code[i]))
        retrieve_url <- sprintf("https://www.mnb.hu/en/arfolyam-tablazat?deviza=rbCurrencySelect&devizaSelected=%s&datefrom=%s&datetill=%s&order=1", 
                                import_fx_df$iso_code[i],
                                import_fx_df$import_date[i],
                                Sys.Date())
        message(paste("Retrieving URL: ",retrieve_url))
        web_import <- getURL(retrieve_url)
        df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[1]]
        if(is.null(df_import)==F){
            df_import[,1] <- as.Date(df_import[,1], format = "%d %B %Y")
            df_import[,2] <- as.numeric(df_import[,2])
            df_import[,3] <- import_fx_df$iso_code[i]
            colnames(df_import) <- c("value_date","value","rate_name")
            psqlInsert(df_import, "ld_rate_value")
        }
        message("Data to be imported:")
        print(df_import)
    }
    final_import <- psqlQuery("INSERT INTO app.rate_value (rate_id,value_date,value)
                              SELECT
                              rfx.id::int,
                              to_date(ldrv.value_date,'yyyy-mm-dd'),
                              ldrv.value::float
                              FROM
                              app.ld_rate_value ldrv
                              INNER JOIN app.currency c ON ldrv.rate_name=c.iso_code
                              INNER JOIN app.rate_fx rfx ON c.id=rfx.from_currency_id AND rfx.to_currency_id=(SELECT id FROM app.currency WHERE iso_code='HUF')
                              ")
    message("Importing into app.rate_value table..")
    message(final_import$errorMsg)
}

message(paste("Script ends: ",Sys.time()))
message("----------------------------------------")
