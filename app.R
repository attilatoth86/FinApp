

# init libraries ----------------------------------------------------------
library(plotly)

library(shiny)
library(shinydashboard)
library(shinyjs)

library(DT)

library(gdata)

library(RCurl)
library(XML)

# init source() -----------------------------------------------------------

source("f.R")

# init setup --------------------------------------------------------------

options(scipen = 999, digits = 4, encoding = "iso-8859-2")

# init ShinyServer --------------------------------------------------------

server <- function(input, output, session) {

# SERV: init dynamic selectors ------------------------------------------

q_adm_ccy_selector <- reactive({
    input$adm_ccy_add_btn
    
    adm_ccy_selector_list <- psqlQuery("SELECT id FROM app.currency")[["result"]][[1]]
    names(adm_ccy_selector_list) <- psqlQuery("SELECT iso_code FROM app.currency")[["result"]][[1]]
    adm_ccy_selector_list <- c("please select.."="",adm_ccy_selector_list)
})
output$adm_acc_add_ccy_selector <- shiny::renderUI(selectInput("adm_acc_add_ccy","Currency",
                                                               q_adm_ccy_selector()))
output$adm_fund_add_ccy_selector <- shiny::renderUI(selectInput("adm_fund_add_ccy","Currency",
                                                               q_adm_ccy_selector()))

q_adm_acc_selector <- reactive({
    input$adm_acc_add_btn
    
    adm_acc_selector_list <- psqlQuery("SELECT id FROM app.account")[["result"]][[1]]
    names(adm_acc_selector_list) <- psqlQuery("SELECT account_name FROM app.account")[["result"]][[1]]
    adm_acc_selector_list <- c("please select.."="",adm_acc_selector_list)
})
output$adm_depo_add_acc_selector <- shiny::renderUI(selectInput("adm_depo_add_acc","Account",
                                                                q_adm_acc_selector()))
output$adm_invtr_add_targ_acc_selector <- shiny::renderUI(selectInput("adm_invtr_add_targ_acc","Target Account",
                                                                q_adm_acc_selector()))

q_adm_int_rate_selector <- reactive({
    input$adm_rate_add_btn
    
    adm_int_rate_selector_list <- psqlQuery("SELECT id FROM app.rate WHERE rate_type='interest'")[["result"]][[1]]
    names(adm_int_rate_selector_list) <- psqlQuery("SELECT rate_name FROM app.rate WHERE rate_type='interest'")[["result"]][[1]]
    adm_int_rate_selector_list <- c("please select.."="",adm_int_rate_selector_list)
})
output$adm_depo_add_int_rate_selector <- shiny::renderUI(selectInput("adm_depo_add_rate","Reference Rate",
                                                                     q_adm_int_rate_selector()))

q_adm_fund_selector <- reactive({
    input$adm_fund_add_btn
    
    adm_fund_selector_list <- psqlQuery("SELECT id FROM app.fund")[["result"]][[1]]
    names(adm_fund_selector_list) <- psqlQuery("SELECT fund_name FROM app.fund")[["result"]][[1]]
    adm_fund_selector_list <- c("please select.."="",adm_fund_selector_list)
})
output$adm_invtr_add_fund_selector <- shiny::renderUI(selectInput("adm_invtr_add_fund","Purchased Fund",
                                                                  q_adm_fund_selector()))

# SERV: init update FX rates --------------------------------------------

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


# SERV: init update interest rates ----------------------------------------

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
}
psqlQuery("INSERT INTO app.rate_value (rate_id,value_date,value)
           SELECT
          r.id::int,
          to_date(ldrv.value_date,'yyyy-mm-dd'),
          ldrv.value::float
          FROM
          app.ld_rate_value ldrv
          LEFT OUTER JOIN app.rate r ON ldrv.rate_name=r.rate_name")$result

# SERV: Funds > Overview --------------------------------------------------

q_funds_ov_portf_statem_reviewtbl <- reactive({
    input$adm_fundprices_imp_add_db_btn
    input$adm_invtr_add_btn
    df_fundportf_details <- rbind(
        data.frame('Account'=character(),
                   'Fund'=character(),
                   'ValueDate'=character(),
                   'PurchasedShares'=integer(),
                   'InvestedAmount'=integer(),
                   'MarketValue'=integer(),
                   'Income/Loss'=integer(),
                   'Yield%'=double(),
                   'AnnualizedYield%'=double()),
        psqlQuery("SELECT 
                  fit.account_name \"Account\", 
                  fit.fund_name \"Fund\", 
                  fit.value_date \"ValueDate\", 
                  fit.share_amount \"PurchasedShares\", 
                  ROUND(fit.tr_value) \"InvestedAmount\",
                  ROUND(fpr.price*fit.share_amount) \"MarketValue\", 
                  ROUND(fpr.price*fit.share_amount)-ROUND(fit.tr_value) \"Income/Loss\",
                  ROUND(CAST((fpr.price*fit.share_amount-fit.tr_value)/fit.tr_value*100 AS DECIMAL),2) \"Yield%\",
                  ROUND(CAST(((fpr.price*fit.share_amount/fit.tr_value)^(365/(app.get_last_fundprice_date()::date-fit.value_date+1)::float)-1)*100 AS DECIMAL),2) \"AnnualizedYield%\"
                  FROM app.fund_investment_transaction_vw fit
                  INNER JOIN app.fund_price_recent_vw fpr ON fit.fund_id=fpr.fund_id
                  ORDER BY fit.value_date DESC, fit.account_id")$result
    )
    df_fundportf_fund_details <- rbind(
        data.frame('Fund'=character(),
                   'PurchasedShares'=integer(),
                   'InvestedAmount'=integer(),
                   'MarketValue'=integer(),
                   'Income/Loss'=integer(),
                   'AverageBalance'=integer(),
                   'AnnualizedYield%'=double()),
        psqlQuery("SELECT 
                    t1.fund_name \"Fund\",
                    t1.purchasedshares \"PurchasedShares\",
                    t1.investedamount \"InvestedAmount\",
                    t1.marketvalue \"MarketValue\",
                    t1.incomeloss \"Income/Loss\",
                    ROUND(t2.avg_bal) \"AverageBalance\",
                    ROUND(CAST((((t2.avg_bal+t1.incomeloss)/t2.avg_bal)^(365/t2.tot_period)-1)*100 AS DECIMAL),2) \"AnnualizedYield%\"
                    FROM
                    (
                    SELECT
                    fit.fund_id,
                    fit.fund_name,
                    SUM(fit.share_amount) purchasedshares,
                    SUM(ROUND(fit.tr_value)) investedamount,
                    SUM(ROUND(fpr.price*fit.share_amount)) marketvalue,
                    SUM(ROUND(fpr.price*fit.share_amount)-ROUND(fit.tr_value)) incomeloss
                    FROM 
                    app.fund_investment_transaction_vw fit
                    ,app.fund_price_recent_vw fpr
                    WHERE fit.fund_id=fpr.fund_id
                    GROUP BY
                    fit.fund_id,
                    fit.fund_name
                    ) t1,
                    (
                    SELECT 
                    v.fund_id,
                    SUM(v.valid_to-v.valid_from+1) tot_period,
                    SUM((v.valid_to-v.valid_from+1)*balance)/SUM(v.valid_to-v.valid_from+1) avg_bal
                    FROM app.fund_inv_tr_fund_bal_vw v
                    GROUP BY
                    v.fund_id
                    ) t2
                    WHERE t1.fund_id=t2.fund_id")$result
        )
    df_fundportf_acc_details <- rbind(
        data.frame('Account'=character(),
                   'InvestedAmount'=integer(),
                   'MarketValue'=integer(),
                   'Income/Loss'=integer(),
                   'Yield%'=double()),
        psqlQuery("SELECT 
                  fit.account_name \"Account\", 
                  SUM(ROUND(fit.tr_value)) \"InvestedAmount\",
                  ROUND(SUM(fpr.price*fit.share_amount)) \"MarketValue\",
                  ROUND(SUM(fpr.price*fit.share_amount)-SUM(fit.tr_value)) \"Income/Loss\",
                  ROUND(CAST((SUM(fpr.price*fit.share_amount)-SUM(fit.tr_value))/SUM(fit.tr_value)*100 AS DECIMAL),2) \"Yield%\"
                  FROM app.fund_investment_transaction_vw fit
                  INNER JOIN app.fund_price_recent_vw fpr ON fit.fund_id=fpr.fund_id
                  GROUP BY fit.account_name
                  ORDER BY fit.account_name")$result
        )
    list(out_details=df_fundportf_details,
         out_fund_details=df_fundportf_fund_details,
         out_acc_details=df_fundportf_acc_details)
})

output$funds_ov_portf_statem_reviewtbl <- DT::renderDataTable(q_funds_ov_portf_statem_reviewtbl()$out_details,
                                                              options = list(searching=F, paging=F, scrollX = T),
                                                              rownames=F)

output$funds_ov_portf_statem_fund_reviewtbl <- DT::renderDataTable(q_funds_ov_portf_statem_reviewtbl()$out_fund_details,
                                                              options = list(searching=F, paging=F, scrollX = T),
                                                              rownames=F)

output$funds_ov_portf_statem_acc_reviewtbl <- DT::renderDataTable(q_funds_ov_portf_statem_reviewtbl()$out_acc_details,
                                                                      options = list(searching=F, paging=F, scrollX = T),
                                                                      rownames=F)


# SERV: Funds > Fund Prices -----------------------------------------------

q_fundprices_df <- reactive({
    input$adm_fundprices_imp_add_db_btn
    input$adm_invtr_add_btn
    fundprice_df <- psqlQuery("SELECT 
                              t.id, 
                              t.fund_name, 
                              t.value_date, 
                              t.price, 
                              t.change_pct, 
                              CASE WHEN t.purchase IS NOT NULL THEN change_pct END purchase
                              FROM
                              (
                              SELECT f.id, f.fund_name, fp.value_date, fp.price,
                              (fp.price/first_value(price) OVER (PARTITION BY f.id ORDER BY fp.value_date)-1)*100 change_pct, fit.value_date purchase
                              FROM app.fund_price fp 
                              INNER JOIN app.fund f ON fp.fund_id=f.id
                              LEFT OUTER JOIN app.fund_investment_transaction fit ON fp.fund_id=fit.fund_id AND fp.value_date=fit.value_date
                              ) t")$result
    fundprice_df$value_date <- as.Date(fundprice_df$value_date, "%Y-%m-%d")
    fundprice_df$fund_name <- as.factor(fundprice_df$fund_name)
    list(output=fundprice_df,
         out_fundid=unique(fundprice_df$id))
})

# Create plot: relative price changes
output$plot_fp_relative <- renderPlotly(
    plot_ly() %>%
        add_trace(data=q_fundprices_df()$output, 
                  x=~value_date, 
                  y=~change_pct, 
                  color = ~fund_name, 
                  mode="lines") %>%
        add_trace(data=q_fundprices_df()$output, 
                  x=~value_date, 
                  y=~purchase, 
                  name="Investments", 
                  mode="markers") %>%
        layout(xaxis=list(title=""), yaxis=list(title="Price Change %"))
)

lapply(isolate(q_fundprices_df()$out_fundid), function(i) {
    df <- isolate(q_fundprices_df()$output)
    data <- df[df$id==i,]
    output[[paste0("plot_fp_",i)]] <- renderPlotly(
        plot_ly(data=data, x=~value_date, y=~price, mode="lines") %>%
            layout(xaxis=list(title=""), yaxis=list(title=""))
    )
    output[[paste0("dyn_plot_",i)]] <- renderUI({
        box(q_fundprices_df()$output["fund_name"][q_fundprices_df()$output["id"]==i],
            plotlyOutput(paste0("plot_fp_",i)),
            width = 6, 
            solidHeader = T)
    })        
})
output$dyn_plot_block <- renderUI({
        box(title="Individual Fund Price Charts",
            solidHeader = T,
            width=12,
            "Charts below depict price development for each
            funds since the dates of first investments.",
            br(),br(),
            lapply(isolate(q_fundprices_df()$out_fundid), function(i) {
                uiOutput(paste0('dyn_plot_', i))
            })
        )
})

# SERV: Macro factors > FX Rates -------------------------------

fxrates_df <- psqlQuery("SELECT * FROM app.rate_value_fx_vw ORDER BY rate_id, value_date")$result
if(nrow(fxrates_df)!=0){
    fxrates_df$rate_name <- as.factor(fxrates_df$rate_name)
    fxrates_df$value_date <- as.Date(fxrates_df$value_date,"%Y-%m-%d")
    
    output$plot_fxrates <- renderPlotly(
        plot_ly(data=fxrates_df, 
                x=~value_date,
                y=~value, 
                color=~rate_name,
                mode="lines") %>%
            layout(xaxis=list(title=""), yaxis=list(title="Value"))
    )
}

# SERV: Macro factors > Interest Rates ------------------------------------

intrates_df <- psqlQuery("SELECT * FROM app.rate_value_intrate_vw")$result
if(nrow(intrates_df)!=0){
    intrates_df$rate_name <- as.factor(intrates_df$rate_name)
    intrates_df$value_date <- as.Date(intrates_df$value_date,"%Y-%m-%d")
    
    output$plot_intrates <- renderPlotly(
        plot_ly(data=intrates_df, 
                x=~value_date,
                y=~value, 
                color=~rate_name,
                mode="lines") %>%
            layout(xaxis=list(title=""), yaxis=list(title="Value"))
    )
}


# SERV: Admin > Manage accounts ----------------------------------

q_adm_acc_reviewtbl <- reactive({
    input$adm_acc_add_btn
    rbind(data.frame('AccountID'=integer(),
                     "AccountName"=character(), 
                     "TaxApplicable"=character(), 
                     "TaxRatePercentage"=double(),
                     "CurrencyID"=integer(),
                     "OpeningDate"=character(),
                     "LastModification"=character(),
                     stringsAsFactors=F),
          psqlQuery("SELECT id \"AccountID\", account_name \"AccountName\",
                    tax_applicable \"TaxApplicable\", tax_rate_percent \"TaxRatePercentage\",
                    currency_id \"CurrencyID\", date_open \"OpeningDate\",
                    to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
                    FROM app.account a")$result
          )
})
output$adm_acc_reviewtbl <- DT::renderDataTable(q_adm_acc_reviewtbl(),
                                                options = list(searching=F, paging=F),
                                                rownames=F)

observeEvent(input$adm_acc_add_btn, {
    adm_acc_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.account 
                                                    (account_name,tax_applicable,tax_rate_percent,currency_id,date_open)
                                                  VALUES('%s', '%s', %f, %i, '%s');"
                                                  ,input$adm_acc_add_name
                                                  ,input$adm_acc_add_taxappl
                                                  ,as.numeric(input$adm_acc_add_taxpercent)
                                                  ,as.numeric(input$adm_acc_add_ccy)
                                                  ,input$adm_acc_add_dateopen)
                                        )
    shinyjs::reset("adm_acc_add_name")
    shinyjs::reset("adm_acc_add_taxappl")
    shinyjs::reset("adm_acc_add_taxpercent")
    shinyjs::reset("adm_acc_add_ccy")
    shinyjs::reset("adm_acc_add_dateopen")
    shinyjs::hide("adm_acc_add_confirm")
    shinyjs::hide("adm_acc_add_error")
    
    if(adm_acc_add_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_acc_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_acc_add_error_txt <- renderText(adm_acc_add_btn_queryOut$errorMsg)
        shinyjs::show("adm_acc_add_error", anim = T, animType = "fade", time = 1)
    }
    
})

# SERV: Admin > Manage deposits ----------------------------------

q_adm_depo_reviewtbl <- reactive({
    input$adm_depo_add_btn
    rbind(data.frame("DepositID"=integer(),
                     "AccountID"=integer(),
                     "ValueDate"=character(),
                     "MaturityDate"=character(),
                     "Amount"=double(),
                     "IntRate"=double(),
                     "IntRateType"=character(),
                     "RateID"=integer(),
                     "IntSpread"=double(),
                     "LastModification"=character()
                     ),
          psqlQuery("SELECT id \"DepositID\",
                            account_id \"AccountID\",
                            value_date \"ValueDate\",
                            maturity_date \"MaturityDate\",
                            amount \"Amount\",
                            interest_rate \"IntRate\",
                            interest_rate_type \"IntRateType\",
                            reference_rate_id \"RateID\",
                            interest_rate_spread as \"IntSpread\",
                            to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
                     FROM app.deposit")$result
    )
})
output$adm_depo_reviewtbl <- DT::renderDataTable(q_adm_depo_reviewtbl(),
                                                options = list(searching=F, paging=F),
                                                rownames=F)

observeEvent(input$adm_depo_add_btn, {
    adm_depo_add_btn_queryOut <- psqlQuery(
        sprintf("INSERT INTO app.deposit (account_id,
                value_date,
                amount,
                maturity_date,
                interest_rate,
                interest_rate_type,
                reference_rate_id,
                interest_rate_spread)
                VALUES(%i, '%s', %f, '%s', %f,'%s', %s, %f);"
                ,as.integer(input$adm_depo_add_acc)
                ,input$adm_depo_add_valdate
                ,as.numeric(input$adm_depo_add_amt)
                ,input$adm_depo_add_matdate
                ,ifelse(input$adm_depo_add_intrate=="",0,as.numeric(input$adm_depo_add_intrate))
                ,input$adm_depo_add_intratetyp
                ,ifelse(input$adm_depo_add_rate=="","NULL",input$adm_depo_add_rate)
                ,ifelse(input$adm_depo_add_intratespread=="",0,as.numeric(input$adm_depo_add_intratespread))
                )
    )

    shinyjs::reset("adm_depo_add_acc")
    shinyjs::reset("adm_depo_add_valdate")
    shinyjs::reset("adm_depo_add_amt")
    shinyjs::reset("adm_depo_add_matdate")
    shinyjs::reset("adm_depo_add_intrate")
    shinyjs::reset("adm_depo_add_intratetyp")
    shinyjs::reset("adm_depo_add_rate")
    shinyjs::reset("adm_depo_add_intratespread")
    shinyjs::hide("adm_depo_add_confirm")
    shinyjs::hide("adm_depo_add_error")
    
    if(adm_depo_add_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_depo_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_depo_add_error_txt <- renderText(adm_depo_add_btn_queryOut$errorMsg)
        shinyjs::show("adm_depo_add_error", anim = T, animType = "fade", time = 1)
    }
    
})

# SERV: Admin > Manage funds --------------------------

q_adm_fund_reviewtbl <- reactive({
    input$adm_fund_add_btn
    rbind(data.frame("FundID"=integer(),
                     "FundName"=character(),
                     "ISIN"=character(),
                     "CurrencyID"=integer(),
                     "SourceID"=character(),
                     "LastModification"=character(),
                     stringsAsFactors = F),
          psqlQuery("SELECT id \"FundID\", fund_name \"FundName\", isin \"ISIN\", currency_id \"CurrencyID\", source_id \"SourceID\", 
                    to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\" FROM app.fund ORDER BY id")$result
    )
})
output$adm_fund_reviewtbl <- DT::renderDataTable(q_adm_fund_reviewtbl(), options=list(searching=F, paging=F), 
                                                 rownames=F)

observeEvent(input$adm_fund_add_btn,{
    adm_fund_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.fund (fund_name, isin, currency_id, source_id) 
                                                   VALUES ('%s','%s',%i,'%s')",
                                                   input$adm_fund_add_name,
                                                   input$adm_fund_add_isin,
                                                   as.integer(input$adm_fund_add_ccy),
                                                   input$adm_fund_add_srcid)
                                        )
    shinyjs::reset("adm_fund_add_name")
    shinyjs::reset("adm_fund_add_isin")
    shinyjs::reset("adm_fund_add_ccy")
    shinyjs::reset("adm_fund_add_srcid")
    shinyjs::hide("adm_fund_add_confirm")
    shinyjs::hide("adm_fund_add_error")
    
    if(adm_fund_add_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_fund_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_fund_add_error_txt <- renderText(adm_fund_add_btn_queryOut$errorMsg)
        shinyjs::show("adm_fund_add_error", anim = T, animType = "fade", time = 1)
    }
})

# SERV: Admin > Manage fund prices --------------------

q_adm_fundprices_reviewtbl <- reactive({
    input$adm_fundprices_imp_add_db_btn
    rbind(data.frame("PriceID"=integer(),
                     "ValueDate"=character(),
                     "FundID"=integer(),
                     "FundPrice"=integer(),
                     "LastModification"=character()),
          psqlQuery("SELECT id \"PriceID\", 
                            value_date \"ValueDate\", 
                            fund_id \"FundID\",
                            price \"FundPrice\",
                            to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
                     FROM app.fund_price
                     ORDER BY value_date DESC, fund_id ASC")$result
    )
})
output$adm_fundprices_reviewtbl <- DT::renderDataTable(q_adm_fundprices_reviewtbl(),
                                                       options = list(pageLength = 10),
                                                       rownames=F)

observeEvent(input$adm_fundprices_upl_btn,{
shinyjs::hide("adm_fundprices_add_confirm")
shinyjs::hide("adm_fundprices_add_error")

fp_import_ctrltbl <- psqlQuery("SELECT f.id fund_id, f.fund_name, f.source_id, MIN(COALESCE(fpv.value_date,fit.value_date-1))+1 import_date_from
                                FROM app.fund_investment_transaction fit
                                INNER JOIN app.fund f ON fit.fund_id=f.id
                                LEFT OUTER JOIN app.fund_price_recent_vw fpv ON fit.fund_id=fpv.fund_id
                                GROUP BY f.id, f.fund_name, f.source_id")$result

price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character(),stringsAsFactors=F)
withProgress(message="Downloading fund prices, please wait..", value = 0, {
    for(i in 1:nrow(fp_import_ctrltbl)){
        incProgress(1/nrow(fp_import_ctrltbl), detail = paste("importing: ", fp_import_ctrltbl[i,2]))
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
})

psqlQuery("TRUNCATE TABLE app.ld_fund_price")$result
psqlInsert(price_tbl_pre, "ld_fund_price")
q_adm_fundprices_pre_out_df <- rbind(
    data.frame("FundID"=integer(),
               "ValueDate"=character(),
               "FundPrice"=numeric()),
    psqlQuery("SELECT
                ldfp.fund_id::int \"FundID\"
                ,ldfp.date \"ValueDate\"
                ,ldfp.price::float \"FundPrice\"
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
                ORDER BY ldfp.date DESC, ldfp.fund_id")$result
)
                               

output$adm_fundprices_pre_out_df <- DT::renderDataTable(q_adm_fundprices_pre_out_df,
                                                        options = list(pageLength = 10),
                                                        rownames=F)
shinyjs::show("adm_fundprices_preload_reviewtbl")
})

observeEvent(input$adm_fundprices_imp_add_db_btn, {
    shinyjs::hide("adm_fundprices_preload_reviewtbl")
    adm_fundprices_imp_add_db_btn_queryOut <- psqlQuery("INSERT INTO app.fund_price (fund_id,value_date,price)
                                                        SELECT
                                                        ldfp.fund_id::int \"FundID\"
                                                        ,to_date(ldfp.date, 'yyyy-mm-dd') \"ValueDate\"
                                                        ,ldfp.price::float \"FundPrice\"
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
    if(adm_fundprices_imp_add_db_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_fundprices_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_fundprices_add_error_txt <- renderText(adm_fundprices_imp_add_db_btn_queryOut$errorMsg)
        shinyjs::show("adm_fundprices_add_error", anim = T, animType = "fade", time = 1)
    }
})
observeEvent(input$adm_fundprices_imp_add_db_cancel_btn, {
    shinyjs::hide("adm_fundprices_preload_reviewtbl")
    psqlQuery("TRUNCATE TABLE app.ld_fund_price")$result
})

# SERV: Admin > Manage rates --------------------------

q_adm_rate_reviewtbl <- reactive({
    input$adm_rate_add_btn
    rbind(data.frame("RateID"=integer(),
                     "RateName"=character(),
                     "RateType"=character(),
                     "LastModification"=character()
                     ),
          psqlQuery("SELECT id \"RateID\",
                            rate_name \"Rate Name\",
                            rate_type \"Rate Type\",
                            to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
                    FROM app.rate")$result
    )
})
output$adm_rate_reviewtbl <- DT::renderDataTable(q_adm_rate_reviewtbl(),
                                                options = list(searching=F, paging=F),
                                                rownames=F)

observeEvent(input$adm_rate_add_btn,{
    adm_rate_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.rate 
                                                   (rate_name,rate_type)
                                                   VALUES ('%s','%s');",
                                                   input$adm_rate_add_ratename,
                                                   input$adm_rate_add_ratetype)
                                            )
    
    shinyjs::reset("adm_rate_add_ratename")
    shinyjs::reset("adm_rate_add_ratetype")
    shinyjs::hide("adm_rate_add_confirm")
    shinyjs::hide("adm_rate_add_error")
    
    if(adm_rate_add_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_rate_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_rate_add_error_txt <- renderText(adm_rate_add_btn_queryOut$errorMsg)
        shinyjs::show("adm_rate_add_error", anim = T, animType = "fade", time = 1)
    }
})

# SERV: Admin > Manage FX rates -----------------------

q_adm_fxrates_reviewtbl <- 
    rbind(data.frame("RateValueID"=integer(),
                     "RateID"=integer(),
                     "ValueDate"=character(),
                     "Value"=numeric(),
                     "LastModification"=character()
                    ),
    psqlQuery("SELECT id \"RateValueID\",
              rate_id \"RateID\",
              value_date \"ValueDate\",
              value \"Value\",
              to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
              FROM app.rate_value
              ORDER BY value_date DESC, rate_id")$result
    )
output$adm_fxrates_reviewtbl <- DT::renderDataTable(q_adm_fxrates_reviewtbl,rownames=F)

# SERV: Admin > Manage currencies --------------------------------

q_adm_ccy_reviewtbl <- reactive({
    input$adm_ccy_add_btn
    rbind(data.frame("CurrencyID"=integer(),
                     "ISO"=character(), 
                     "Description"=character(),
                     "LastModification"=character(),
                     stringsAsFactors=F),
          psqlQuery("SELECT id \"CurrencyID\",iso_code \"ISO\",description \"Description\",
                            to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
                    FROM app.currency
                    ORDER BY id")$result
    )
})
output$adm_ccy_reviewtbl <- DT::renderDataTable(q_adm_ccy_reviewtbl(),
                                                options = list(searching=F, paging=F),
                                                rownames=F)

observeEvent(input$adm_ccy_add_btn, {
    adm_ccy_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.currency 
                                                    (iso_code, description)
                                                    VALUES('%s', '%s');"
                                                  ,input$adm_ccy_add_iso
                                                  ,input$adm_ccy_add_desc))
    shinyjs::reset("adm_ccy_add_iso")
    shinyjs::reset("adm_ccy_add_desc")
    shinyjs::hide("adm_ccy_add_confirm")
    shinyjs::hide("adm_ccy_add_error")
    
    if(adm_ccy_add_btn_queryOut$errorMsg=="OK"){
        shinyjs::show("adm_ccy_add_confirm", anim = T, animType = "fade", time = 1)
    }
    else {
        output$adm_ccy_add_error_txt <- renderText(adm_ccy_add_btn_queryOut$errorMsg)
        shinyjs::show("adm_ccy_add_error", anim = T, animType = "fade", time = 1)
    }
})

# SERV: Admin > Manage investment transactions --------

q_adm_invtr_reviewtbl <- reactive({
    input$adm_invtr_add_btn
    rbind(data.frame("TransactionID"=integer(),
                     "ValueDate"=character(),
                     "ShareAmount"=integer(),
                     "AccountID"=integer(),
                     "FundID"=integer(),
                     "LastModification"=character(),
                     stringsAsFactors = F),
          psqlQuery("SELECT id \"TransactionID\",
                      to_char(value_date,'YYYY-MM-DD') \"ValueDate\",
                      share_amount \"ShareAmount\",
                      account_id \"AccountID\",
                      fund_id \"FundID\",
                      to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\"
               FROM app.fund_investment_transaction
               ORDER BY value_date DESC")$result   
    )
})
output$adm_invtr_reviewtbl <- DT::renderDataTable(q_adm_invtr_reviewtbl(),
                                                  options = list(searching=F, paging=F),
                                                  rownames=F)

observeEvent(input$adm_invtr_add_btn,{
    adm_invtr_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.fund_investment_transaction 
                                                      (value_date,share_amount,account_id,fund_id)
                                                      VALUES('%s', %i, %i, %i);"
                                                          ,input$adm_invtr_add_valdate
                                                          ,as.numeric(input$adm_invtr_add_noshares)
                                                          ,as.numeric(input$adm_invtr_add_targ_acc)
                                                          ,as.numeric(input$adm_invtr_add_fund)
                                                      )
                                              )
        shinyjs::reset("adm_invtr_add_valdate")
        shinyjs::reset("adm_invtr_add_noshares")
        shinyjs::reset("adm_invtr_add_targ_acc")
        shinyjs::reset("adm_invtr_add_fund")
        shinyjs::hide("adm_invtr_add_confirm")
        shinyjs::hide("adm_invtr_add_error")
        
        if(adm_invtr_add_btn_queryOut$errorMsg=="OK"){
            shinyjs::show("adm_invtr_add_confirm", anim = T, animType = "fade", time = 1)
        }
        else {
            output$adm_invtr_add_error_txt <- renderText(adm_invtr_add_btn_queryOut$errorMsg)
            shinyjs::show("adm_invtr_add_error", anim = T, animType = "fade", time = 1)
        }
    })

# end ShinyServer ---------------------------------------------------------

}

# init UI -----------------------------------------------------------------

ui <- dashboardPage(

# UI: dashboardHeader -----------------------------------------------------

dashboardHeader(title="FinApp", titleWidth = "270px"),

# UI: dashboardSidebar  ---------------------------------------------------

dashboardSidebar(width = "270px",
                shinyjs::useShinyjs(),                                 
                sidebarMenu(
                    menuItem("Home", tabName = "home", icon = icon("home")),
                    menuItem("Funds",tabName = NULL, icon = icon("line-chart"),
                        menuSubItem("Overview", tabName = "funds_ov"),
                        menuSubItem("Prices", tabName = "fundprices")
                     ),
                    menuItem("Accounts", tabName = "accounts", icon = icon("bank")),
                    menuItem("Macroeconomic Factors", tabName = NULL, icon = icon("globe"),
                              menuSubItem("FX Rates", tabName = "macro_fx"),
                              menuSubItem("Interest Rates", tabName = "macro_ir")
                     ),
                    menuItem("Administration", tabName = NULL, icon = icon("gear"),
                        menuSubItem("Manage accounts", tabName = "adm_acc"),
                        menuSubItem("Manage deposits", tabName = "adm_depo"),
                        menuSubItem("Manage rates", tabName = "adm_rate"),
                              br(),
                        menuSubItem("Manage funds", tabName = "adm_fund"),
                        menuSubItem("Manage fund prices", tabName = "adm_fundprices"),
                        menuSubItem("Manage fund investment transactions", 
                                          tabName = "adm_invtr"),
                              br(),
                        menuSubItem("Manage currencies", tabName = "adm_ccy"),
                        menuSubItem("Manage FX rates", tabName = "adm_fxrates")
                     )
                 )
                ),

# UI: init dashboardBody() ------------------------------------------------

dashboardBody(

# UI: init tabItems() -----------------------------------------------------

tabItems(

# UI: Fund > Overview -----------------------------------------------------

tabItem(tabName = "funds_ov",
        h2("Funds", tags$small("Overview")),
        fluidRow(
            box(title="Detailed Portfolio Statement",
                solidHeader = T,
                DT::dataTableOutput("funds_ov_portf_statem_acc_reviewtbl"),
                br(),br(),
                DT::dataTableOutput("funds_ov_portf_statem_fund_reviewtbl"),
                br(),br(),
                DT::dataTableOutput("funds_ov_portf_statem_reviewtbl"),
                width = 12, 
                status="primary")
        )
        ),

# UI: Fund > Fund Prices --------------------------------------------------

tabItem(tabName = "fundprices",
        h2("Funds", tags$small("Prices")),
        fluidRow(
            box(plotlyOutput("plot_fp_relative", height = "575px"),
                title="Relative Price Changes",
                width = 12,
                status = "primary")
        ),
        fluidRow(uiOutput("dyn_plot_block"))
),

# UI: Macro > FX rates ----------------------------------------------------

tabItem(tabName = "macro_fx",
        h2("Macroeconomic Factors", tags$small("FX Rates")),
        fluidRow(
            box(
                plotlyOutput("plot_fxrates"),
                width = 12,
                status = "primary"
            )
        )
),

# UI: Macro > Interest rates ----------------------------------------------

tabItem(tabName = "macro_ir",
        h2("Macroeconomic Factors", tags$small("Interest Rates")),
        fluidRow(
            box(
                plotlyOutput("plot_intrates"),
                width = 12,
                status = "primary"
            )
        )
),

# UI: Admin > Manage accounts ---------------------------------------------------

tabItem(tabName = "adm_acc",
        h2("Administration", tags$small("Manage accounts")),
        fluidRow(
                box(title = "Add an account", 
                    width=12,
                    solidHeader = T,
                    status = "danger",
                    column(width = 4,
                           textInput("adm_acc_add_name","Account Name"),
                           dateInput("adm_acc_add_dateopen",label = "Opening Date", format="yyyy-mm-dd")
                     ),
                    column(width = 4,
                           textInput("adm_acc_add_taxpercent","Tax Rate %"),
                           selectInput("adm_acc_add_taxappl","Tax Applicable",
                                       c("please select.."="","Yes"="Y","No"="N"))
                     ),
                    column(width = 4,
                           uiOutput("adm_acc_add_ccy_selector"),
                           actionButton("adm_acc_add_btn","Add",icon("plus")),
                           tags$style(type='text/css', 
                                      "#adm_acc_add_btn {margin-top: 20px;}")
                     )
                    ),
                   shinyjs::hidden(
                       div(id="adm_acc_add_confirm",
                           class="alert alert-success",
                           role="alert",
                           tags$b(icon("check"), "Account has been added successfully."))
                   ),
                   shinyjs::hidden(
                       div(id="adm_acc_add_error",
                           class="alert alert-danger",
                           role="alert",
                           h4(icon("exclamation"), "Error"),
                           p(textOutput("adm_acc_add_error_txt"))
                       )
                   )
        ),
        fluidRow(
            box(width = 12,
                DT::dataTableOutput("adm_acc_reviewtbl"))
        )
),

# UI: Admin > Manage deposits ------------------------------------

tabItem(tabName = "adm_depo",
        h2("Administration", tags$small("Manage deposits")),
        fluidRow(
            box(title = "Add deposit",
                width = 12,
                solidHeader = T,
                status = "danger",
                column(width = 4,
                       uiOutput("adm_depo_add_acc_selector"),
                       selectInput("adm_depo_add_intratetyp",
                                   "Interest Rate Type",
                                   c("please select.."="",
                                     "Fixed"="fixed",
                                     "Variable"="variable")),
                       actionButton("adm_depo_add_btn",
                                    "Add", 
                                    icon("plus"))
                ),
                column(width = 4,
                       dateInput("adm_depo_add_valdate",
                                 "Value Date", 
                                 format = "yyyy-mm-dd"),
                       dateInput("adm_depo_add_matdate",
                                 "Maturity Date", 
                                 format = "yyyy-mm-dd"),
                       uiOutput("adm_depo_add_int_rate_selector")
                ),
                column(width = 4,
                       textInput("adm_depo_add_amt",
                                 "Amount"),
                       textInput("adm_depo_add_intrate",
                                 "Interest Rate"),
                       textInput("adm_depo_add_intratespread",
                                 "Interest Rate Spread")
                )
            )
        ),
        fluidRow(
            column(width = 12,
                   shinyjs::hidden(
                       div(id="adm_depo_add_confirm",
                           class="alert alert-success",
                           role="alert",
                           tags$b(icon("check"), "Deposit has been added successfully."))
                   ),
                   shinyjs::hidden(
                       div(id="adm_depo_add_error",
                           class="alert alert-danger",
                           role="alert",
                           h4(icon("exclamation"), "Error"),
                           p(textOutput("adm_depo_add_error_txt"))
                       )
                   )  
            )
        ),
        fluidRow(box(width = 12,
                     DT::dataTableOutput("adm_depo_reviewtbl")))
        
),

# UI: Admin > Manage funds ---------------------------------------

tabItem(tabName = "adm_fund",
        h2("Administration", tags$small("Manage funds")),
        fluidRow(
            column(width = 4,
                box(title = "Add funds",
                    solidHeader = T,
                    status = "danger",
                    width = NULL,
                    textInput("adm_fund_add_name","Fund Name"),
                    textInput("adm_fund_add_isin","ISIN Code"),
                    textInput("adm_fund_add_srcid","Source ID"),
                    uiOutput("adm_fund_add_ccy_selector"),
                    actionButton("adm_fund_add_btn","Add",icon("plus"))
                ),
                shinyjs::hidden(
                    div(id="adm_fund_add_confirm",
                        class="alert alert-success",
                        role="alert",
                        tags$b(icon("check"),"Fund has been added successfully.")
                        )
                ),
                shinyjs::hidden(
                    div(id="adm_fund_add_error",
                        class="alert alert-danger",
                        role="alert",
                        h4(icon("exclamation"),"Error"),
                        p(textOutput("adm_fund_add_error_txt"))
                        )
                )
            ),
            column(width = 8,
                   box(width = NULL,
                       DT::dataTableOutput("adm_fund_reviewtbl")
                   )
            )
        )
),

# UI: Admin > Manage fund prices ---------------------------------

tabItem(tabName = "adm_fundprices",
        h2("Administration", tags$small("Manage fund prices")),
        fluidRow(box(width = 6,
                     title = "Import Aegon Fund Prices",
                     solidHeader = T, status = "danger",
                     p("This module imports prices of funds managed by ",tags$a(href="http://www.aegonalapkezelo.hu","Aegon Asset Management.")),
                     actionButton("adm_fundprices_upl_btn",
                                  "Import", 
                                  icon("database"))
                     )
                 ),
        fluidRow(shinyjs::hidden(
                        div(id="adm_fundprices_preload_reviewtbl",
                            box(width=12,
                                status="success",
                                title="Review the list of prices to be added",
                                solidHeader = T,
                                DT::dataTableOutput("adm_fundprices_pre_out_df"),
                                br(),
                                actionButton("adm_fundprices_imp_add_db_btn", 
                                             "Add to database",
                                             icon("plus")
                                             ),
                                actionButton("adm_fundprices_imp_add_db_cancel_btn", 
                                             "Cancel"
                                )
                                )
                            )
                    ),
                 column(width = 12,
                        shinyjs::hidden(
                            div(id="adm_fundprices_add_confirm",
                                class="alert alert-success",
                                role="alert",
                                tags$b(icon("check"),"Fund prices have been added successfully.")
                            )
                        ),
                        shinyjs::hidden(
                            div(id="adm_fundprices_add_error",
                                class="alert alert-danger",
                                role="alert",
                                h4(icon("exclamation"),"Error"),
                                p(textOutput("adm_fundprices_add_error_txt"))
                            )
                         )
                        )
                 ),
        fluidRow(
            box(DT::dataTableOutput("adm_fundprices_reviewtbl"),
                width = 12)
        )
),

# UI: Admin > Manage rates ---------------------------------------

tabItem(tabName = "adm_rate",
        h2("Administration", tags$small("Manage rates")),
        fluidRow(
            column(width = 3,
                   box(title = "Add rate",
                       width = NULL,
                       status = "danger",
                       solidHeader = T,
                       textInput("adm_rate_add_ratename","Rate Name"),
                       selectInput("adm_rate_add_ratetype","Rate Type",
                                   c("please select.."="",
                                     "Interest Rate"="interest",
                                     "FX Rate"="FX")),
                       actionButton("adm_rate_add_btn","Add",icon("plus"))
                   ),
                   shinyjs::hidden(
                       div(id="adm_rate_add_confirm",
                           class="alert alert-success",
                           role="alert",
                           tags$b(icon("check"), "Rate has been added successfully."))
                   ),
                   shinyjs::hidden(
                       div(id="adm_rate_add_error",
                           class="alert alert-danger",
                           role="alert",
                           h4(icon("exclamation"), "Error"),
                           p(textOutput("adm_rate_add_error_txt"))
                       )
                   )
            ),
            column(width = 9,
                   box(width=NULL,
                       DT::dataTableOutput("adm_rate_reviewtbl")))
        )
),

# UI: Admin > Manage FX rates ------------------------------------

tabItem(tabName = "adm_fxrates",
        h2("Administration", tags$small("Manage FX rates")),
        fluidRow(
            box(width = 12,
                p("This module shows imported historical FX rates of HUF and 
                  various currency pairs from the official",
                  tags$a(href="http://mnb.hu/en/arfolyam-lekerdezes", "website"),
                  "of Hungarian National Bank (MNB). MNB currently quotes official central bank rates 
                  for the following currencies only: AUD, BGN, BRL, CAD, CHF, CNY, 
                  CZK, DKK, EUR, GBP, HKD, HRK, IDR, ILS, INR, ISK, JPY, KRW, MXN, MYR, NOK, NZD, PHP, PLN, RON, RSD, RUB, SEK,
                  SGD, THB, TRY, UAH, USD, ZAR."),
                p("Note: Rates are being imported automatically upon loading the application.")
                )
         ),
        fluidRow(box(DT::dataTableOutput("adm_fxrates_reviewtbl"),width = 12))
),

# UI: Admin > Manage currencies -------------------------------------------------

tabItem(tabName = "adm_ccy",
        h2("Administration", tags$small("Manage currencies")),
        fluidRow(
            column(width = 4,
                   box(title = "Add currency", 
                       solidHeader = T,
                       status = "danger",
                       width = NULL,
                       textInput("adm_ccy_add_iso","ISO Code"),
                       textInput("adm_ccy_add_desc","Description"),
                       actionButton("adm_ccy_add_btn",
                                    "Add", 
                                    icon("plus"))
                   ),
                   shinyjs::hidden(
                       div(id="adm_ccy_add_confirm",
                           class="alert alert-success",
                           role="alert",
                           tags$b(icon("check"), "Currency has been added successfully."))
                   ),
                   shinyjs::hidden(
                       div(id="adm_ccy_add_error",
                           class="alert alert-danger",
                           role="alert",
                           h4(icon("exclamation"), "Error"),
                           p(textOutput("adm_ccy_add_error_txt"))
                       )
                   )
                   ),
            column(width = 8,
                   box(DT::dataTableOutput("adm_ccy_reviewtbl"),
                       width = NULL))
        )
),

# UI: Admin > Manage investment transactions ---------------------

tabItem(tabName = "adm_invtr",
        h2("Administration", tags$small("Manage fund investment transactions")),
        fluidRow(
            box(title="Add fund investment transaction",
                solidHeader = T,
                status = "danger",
                width = 12,
                fluidRow(
                    column(width=5,uiOutput("adm_invtr_add_targ_acc_selector")),
                    column(width=5,uiOutput("adm_invtr_add_fund_selector"))
                    # ,column(width=2,uiOutput("adm_invtr_add_ccy_selector"))
                ),
                fluidRow(
                    column(width=4,
                           dateInput("adm_invtr_add_valdate",
                                     "Value Date", 
                                     format = "yyyy-mm-dd")),
                    column(width=4,
                           textInput("adm_invtr_add_noshares",
                                     "No of Purchased Share")),
                    column(width=4,
                           actionButton("adm_invtr_add_btn",
                                        "Enter transaction", 
                                        icon("plus"))
                    )
                ),
                tags$style(type='text/css', 
                           "#adm_invtr_add_btn {width:100%; 
                                   margin-top: 25px;}")
            )
        ),
        fluidRow(
            column(width=12,
                   shinyjs::hidden(
                       div(id="adm_invtr_add_confirm",
                           class="alert alert-success",
                           role="alert",
                           tags$b(icon("check"), "Transaction has been added successfully."))
                   ),
                   shinyjs::hidden(
                       div(id="adm_invtr_add_error",
                           class="alert alert-danger",
                           role="alert",
                           h4(icon("exclamation"), "Error"),
                           p(textOutput("adm_invtr_add_error_txt"))
                       )
                   )
            )
        ),
        fluidRow(
            box(width = 12,
                DT::dataTableOutput("adm_invtr_reviewtbl"))
        )
)

# UI: end tabItems() ------------------------------------------------------
    
)

# UI: end dashboardBody() -------------------------------------------------

)

# end of dashboardPage ----------------------------------------------------

)

# call shinyApp() ---------------------------------------------------------

shinyApp(ui = ui, server = server)


