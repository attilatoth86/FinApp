paste("Run start:",Sys.time())

library(RCurl)
library(XML)

source("/srv/shiny-server/finapp/f.R")

import_ir_df <- psqlQuery("SELECT 
                            r.id, 
                          r.rate_name, 
                          MIN(d.value_date) date_open 
                          FROM
                          app.deposit d, app.rate r
                          WHERE d.reference_rate_id=r.id AND r.rate_type='interest'
                          GROUP BY r.id, r.rate_name")$result
psqlQuery("TRUNCATE TABLE app.ld_rate_value")$result

if(length(import_ir_df$id)!=0){
    for(i in 1:length(import_ir_df$id)) {
        ir_firstDateToImport <- as.Date(psqlQuery(sprintf("SELECT COALESCE(MAX(value_date)+1,'%s') value_date 
                                                          FROM app.rate_value 
                                                          WHERE rate_id=%i",
                                                          import_ir_df$date_open[i],
                                                          import_ir_df$id[i]))$result[1,1],'%Y-%m-%d')
        
        retrieve_url <- sprintf("https://www.mnb.hu/en/Jegybanki_alapkamat_alakulasa?datefrom=%s&datetill=%s&order=0",
                                as.character(ir_firstDateToImport,"%d%%2F%m%%2F%Y"),
                                as.character(Sys.Date(),"%d%%2F%m%%2F%Y"))
        
        web_import <- getURL(retrieve_url)
        df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[1]]
        if(is.null(df_import)==F){
            df_import[,1] <- as.Date(df_import[,1], format = "%d %B %Y")
            df_import[,2] <- as.numeric(gsub("%","",df_import[,2]))
            df_import[,3] <- import_ir_df$rate_name[i]
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
              LEFT OUTER JOIN app.rate r ON ldrv.rate_name=r.rate_name")
}
