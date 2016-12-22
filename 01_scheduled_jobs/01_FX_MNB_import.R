library(RCurl)
library(XML)

source("/home/ati/FinApp/f.R")

import_fx_df <- psqlQuery("SELECT 
                          MIN(a.date_open) date_open,
                          c.iso_code
                          FROM 
                          app.account a, 
                          app.currency c
                          WHERE a.currency_id=c.id
                          AND c.iso_code!='HUF'
                          GROUP BY c.iso_code")$result
psqlQuery("TRUNCATE TABLE app.ld_rate_value")$result

if(length(import_fx_df$iso_code)!=0){
    for(i in 1:length(import_fx_df$iso_code)) {
        iso_code <- import_fx_df$iso_code[i]
        
        fx_firstDateToImport <- psqlQuery(sprintf("SELECT COALESCE(MAX(value_date)+1,'%s') value_date 
                                                  FROM app.rate_value_fx_vw t WHERE t.rate_name='%s'",
                                                  import_fx_df$date_open[i],
                                                  paste0("HUF",iso_code)))$result[1,1]
        
        retrieve_url <- sprintf("https://www.mnb.hu/en/arfolyam-tablazat?deviza=rbCurrencySelect&devizaSelected=%s&datefrom=%s&datetill=%s&order=1", 
                                iso_code,
                                fx_firstDateToImport,
                                Sys.Date())
        web_import <- getURL(retrieve_url)
        df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[1]]
        if(is.null(df_import)==F){
            df_import[,1] <- as.Date(df_import[,1], format = "%d %B %Y")
            df_import[,2] <- as.numeric(df_import[,2])
            df_import[,3] <- sprintf("HUF%s",import_fx_df$iso_code[i])
            colnames(df_import) <- c("value_date","value","rate_name")
            psqlInsert(df_import, "ld_rate_value")
        }
    }
    psqlQuery("INSERT INTO app.rate_value (rate_id,value_date,value)
              SELECT
              r.id::int,
              to_date(ldrv.value_date,'yyyy-mm-dd'),
              ldrv.value::float
              FROM
              app.ld_rate_value ldrv
              LEFT OUTER JOIN app.rate r ON ldrv.rate_name=r.rate_name")$result
}