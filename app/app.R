
# init library() ----------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyjs)

library(DT)

library(plotly)
library(ggplot2)

library(RCurl)
library(XML)

# init source() ---------------------------------

source("f.R")

# init variables -----------------------------------------------

fund_inv_ov_perioddays <- psqlQuery("SELECT
                                    get_last_fundprice_date()-
                                    (SELECT MIN(value_date) FROM fund_investment_transaction) AS days")$result
fund_inv_ov_periodstart <- psqlQuery("SELECT TO_CHAR(MIN(value_date),'DD Mon YY') 
                                     FROM fund_investment_transaction")$result
fund_inv_ov_periodend <- psqlQuery("SELECT MAX(value_date) FROM fund_price")$result
fund_ids <- psqlQuery("SELECT DISTINCT fund_id 
                      FROM fund_investment_transaction 
                      ORDER BY fund_id ASC")$result

# init variables for dropdown lists ------------------------------------------

acc_list <- psqlQuery("SELECT id FROM account")[["result"]][[1]]
names(acc_list) <- psqlQuery("SELECT account_name FROM account")[["result"]][[1]]
acc_list <- c("please select.."="",acc_list)

fund_list <- psqlQuery("SELECT id FROM fund")[["result"]][[1]]
names(fund_list) <- psqlQuery("SELECT fund_name FROM fund")[["result"]][[1]]
fund_list <- c("please select.."="",fund_list)

ccy_list <- psqlQuery("SELECT id FROM currency")[["result"]][[1]]
names(ccy_list) <- psqlQuery("SELECT iso_code FROM currency")[["result"]][[1]]
ccy_list <- c("please select.."="",ccy_list)

ccy_list_nonid <- psqlQuery("SELECT iso_code FROM currency WHERE iso_code!='HUF'")[["result"]][[1]]
names(ccy_list_nonid) <- psqlQuery("SELECT iso_code FROM currency WHERE iso_code!='HUF'")[["result"]][[1]]
ccy_list_nonid <- c("please select.."="",ccy_list_nonid)

# init setup ----------------------------------------------

options(scipen = 999)

# init ShinyServer --------------------------------------------------------

server <- function(input, output, session) {

# SERVERCOMPONENT: Funds > Overview --------------------------------------------------
    
    q_funds_ov_tot_fund_inv <-#reactive({
        #input$ # placeholder for reactive trigger
        psqlQuery("SELECT ROUND(SUM(tr_value)) FROM fund_investment_transaction_vw")$result
    #})
    output$funds_ov_tot_fund_inv <- shiny::renderText(format(q_funds_ov_tot_fund_inv[1,1], big.mark=" "))
    
    q_funds_ov_avg_fund_inv <-#reactive({
        #input$ # placeholder for reactive trigger
        psqlQuery("SELECT ROUND(SUM(tt.w*tt.balance)/SUM(tt.w)) avg_bal
                  FROM
                  (
                  SELECT 
                  COALESCE(LEAD(t.value_date) OVER (ORDER BY t.value_date),
                  get_last_fundprice_date())-t.value_date w,
                  SUM(t.inv_amt) OVER (ORDER BY t.value_date) balance
                  FROM
                  (
                  SELECT 
                  it.value_date,
                  SUM(it.tr_value) inv_amt
                  FROM
                  fund_investment_transaction_vw it
                  GROUP BY 
                  it.value_date
                  ORDER BY
                  it.value_date
                  ) t
                  ) tt")$result
    #})
    output$funds_ov_avg_fund_inv <- shiny::renderText(format(q_funds_ov_avg_fund_inv[1,1], big.mark=" "))
    
    q_fund_ov_tot_fund_net_yield <-#reactive({
        #input$ # placeholder for reactive trigger
        psqlQuery("SELECT 
                  ROUND(SUM((it.share_amount*fp.price-it.tr_value)*(1-it.tax_rate_percent/100)))
                  FROM 
                  fund_investment_transaction_vw it,
                  fund_price_recent_vw fp
                  WHERE 1=1
                  AND it.fund_id=fp.fund_id")$result
    #})
    output$fund_ov_tot_fund_net_yield <- shiny::renderText(format(q_fund_ov_tot_fund_net_yield[1,1], big.mark=" "))
    
    q_fund_ov_tot_fund_gross_yield <-#reactive({
        #input$ # placeholder for reactive trigger
        psqlQuery("SELECT 
                  ROUND(SUM(it.share_amount*fp.price-it.tr_value))
                  FROM 
                  fund_investment_transaction_vw it,
                  fund_price_recent_vw fp
                  WHERE 1=1
                  AND it.fund_id=fp.fund_id")$result
    #})
    output$fund_ov_tot_fund_gross_yield <- shiny::renderText(format(q_fund_ov_tot_fund_gross_yield[1,1], big.mark=" "))
    
    output$fund_ov_eff_taxpct <- shiny::renderText((q_fund_ov_tot_fund_gross_yield[1,1]
                                                    -q_fund_ov_tot_fund_net_yield[1,1])/
                                                       q_fund_ov_tot_fund_gross_yield[1,1]*100
    )
    
    output$fund_ov_tot_fund_net_yield_pct <- shiny::renderText(q_fund_ov_tot_fund_net_yield[1,1]/
                                                                   q_funds_ov_avg_fund_inv[1,1]/
                                                                   fund_inv_ov_perioddays[1,1]*365*100)
    output$fund_ov_tot_fund_gross_yield_pct <- shiny::renderText(q_fund_ov_tot_fund_gross_yield[1,1]/
                                                                     q_funds_ov_avg_fund_inv[1,1]/
                                                                     fund_inv_ov_perioddays[1,1]*365*100)
    
    # List of funds for Fund Investment Overview
    fund_df <- psqlQuery("SELECT 
                         it.fund_id AS \"Fund ID\",
                         it.fund_name AS \"Fund Name\", 
                         MIN(it.value_date) AS \"Investments Since\", 
                         SUM(ROUND(CAST(it.tr_value AS NUMERIC),2)) AS \"Investment Amount\", 
                         it.currency AS \"Currency\", 
                         SUM(it.share_amount) AS \"Number of Shares Purchased\",
                         SUM(ROUND(CAST(it.share_amount*fp.price AS NUMERIC),2)) AS \"Current Value of Investment\",
                         ROUND(CAST(SUM(it.share_amount*fp.price)-SUM(it.tr_value) AS NUMERIC),2) AS \"Gain/Loss Amount\",
                         ROUND(CAST((SUM(it.share_amount*fp.price)-SUM(it.tr_value))/SUM(it.tr_value)*100 AS NUMERIC),2) AS \"Gain/Loss %\"
                         FROM
                         fund_investment_transaction_vw it, 
                         fund_price_recent_vw fp
                         WHERE 1=1 
                         AND it.fund_id=fp.fund_id
                         GROUP BY
                         it.fund_id,
                         it.fund_name, 
                         it.isin, 
                         it.currency
                         ORDER BY
                         \"Gain/Loss %\" DESC")$result
    fund_df$`Investment Amount` <- as.integer(fund_df$`Investment Amount`)
    fund_df$`Number of Shares Purchased` <- as.integer(fund_df$`Number of Shares Purchased`)
    output$fundtable <- DT::renderDataTable(fund_df[,-1],
                                            options = list(searching=F, paging=F),
                                            rownames=F)

# SERVERCOMPONENT: Funds > Fund Prices -----------------------------------------------
    
    # Retrieve fund prices from DB // reactive to the event of fund price upload
    q_adm_fundprices_df <- reactive({
        input$adm_fundprices_upl_btn
        fundprice_df <- psqlQuery("SELECT f.id, f.fund_name, fp.value_date, fp.price,
                                  (fp.price/first_value(price) OVER (PARTITION BY f.id ORDER BY fp.value_date)-1)*100 change_pct
                                  FROM fund_price fp, 
                                  fund f, 
                                  (SELECT fund_id, MIN(value_date) min_valdat FROM fund_investment_transaction GROUP BY fund_id) fit
                                  WHERE fp.fund_id=f.id
                                  AND fp.fund_id=fit.fund_id
                                  AND fp.value_date>=fit.min_valdat")$result
        fundprice_df$value_date <- as.Date(fundprice_df$value_date, "%Y-%m-%d")
        fundprice_df$fund_name <- as.factor(fundprice_df$fund_name)
        list(output=fundprice_df)
    })
    
    # Create plot: relative price changes
    output$plot_fp_relative <- renderPlotly(
        plot_ly(data=q_adm_fundprices_df()$output, 
                x=~value_date, 
                y=~change_pct, 
                color = ~fund_name, 
                mode="lines") %>%
            layout(xaxis=list(title=""), yaxis=list(title="Price Change %"))
            )
    
    # Create dynamic plot rendering
    lapply(fund_df$`Fund ID`, function(i) {
        df <- isolate(q_adm_fundprices_df()$output)
        data <- df[df$id==i,]
        output[[paste0("plot_fp_",i)]] <- renderPlotly(
            plot_ly(data=data, x=~value_date, y=~price, mode="lines") %>%
                layout(xaxis=list(title=""), yaxis=list(title=""))
            )
        output[[paste0("dyn_plot_",i)]] <- renderUI({
            box(fund_df$`Fund Name`[fund_df$`Fund ID`==i],
                plotlyOutput(paste0("plot_fp_",i)),
                width = 6, 
                solidHeader = T)
        })        
    })

# SERVERCOMPONENT: Macro > FX Rates --------------------------------------------------
    
    ### placeholder

# SERVERCOMPONENT: Administration > Manage accounts ----------------------------------
    
    # Retrieve existing accounts in a data table
    q_adm_acc_reviewtbl <- reactive({
        input$adm_acc_add_btn
        psqlQuery("SELECT id AS \"Account ID\", 
                  account_name AS \"Account Name\",
                  tax_applicable AS \"Tax Applicable\",
                  tax_rate_percent AS \"Tax Rate %\"
                  FROM account a")$result
    })
    output$adm_acc_reviewtbl <- DT::renderDataTable(
        q_adm_acc_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)
    
    # Insert new account
    observeEvent(input$adm_acc_add_btn, {
        adm_acc_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO account (account_name, 
                                                      tax_applicable, 
                                                      tax_rate_percent)
                                                      VALUES('%s', '%s', %f);",input$adm_acc_add_name
                                                      ,input$adm_acc_add_taxappl
                                                      ,as.numeric(input$adm_acc_add_taxpercent)
                                                      )
                                              )
        shinyjs::reset("adm_acc_add_name")
        shinyjs::reset("adm_acc_add_taxappl")
        shinyjs::reset("adm_acc_add_taxpercent")
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

# SERVERCOMPONENT: Administration > Manage funds -------------------------------------
    
    # Retrieve existing funds in a data table
    q_adm_fund_reviewtbl <- reactive({
        input$adm_fund_add_btn
        psqlQuery("SELECT id AS \"Fund ID\",
                  fund_name AS \"Fund Name\",
                  isin AS \"ISIN\"
                  FROM fund
                  ORDER BY id")$result
    })
    output$adm_fund_reviewtbl <- DT::renderDataTable(
        q_adm_fund_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)
    
    # Insert new fund
    observeEvent(input$adm_fund_add_btn, {
        psqlQuery(sprintf("INSERT INTO fund (fund_name, 
                          isin)
                          VALUES('%s', '%s');"
                          ,input$adm_fund_add_name
                          ,input$adm_fund_add_isin))$result
        shinyjs::reset("adm_fund_add_name")
        shinyjs::reset("adm_fund_add_isin")
        shinyjs::show("adm_fund_add_confirm")
        
    })

# SERVERCOMPONENT: Administration > Manage currencies --------------------------------
    
    # Retrieve existing currencies in a data table
    q_adm_ccy_reviewtbl <- reactive({
        input$adm_ccy_add_btn
        psqlQuery("SELECT id AS \"Currency ID\",
                  iso_code AS \"ISO Code\",
                  description AS \"Description\"
                  FROM currency
                  ORDER BY id")$result
    })
    output$adm_ccy_reviewtbl <- DT::renderDataTable(
        q_adm_ccy_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)
    
    # Insert new currency
    observeEvent(input$adm_ccy_add_btn, {
        psqlQuery(sprintf("INSERT INTO currency (iso_code, 
                          description)
                          VALUES('%s', '%s');"
                          ,input$adm_ccy_add_iso
                          ,input$adm_ccy_add_desc))$result
        shinyjs::reset("adm_ccy_add_iso")
        shinyjs::reset("adm_ccy_add_desc")
        shinyjs::show("adm_ccy_add_confirm")
    })

# SERVERCOMPONENT: Administration > Manage deposits ----------------------------------
    
    # Retrieve existing deposits in a data table
    q_adm_depo_reviewtbl <- reactive({
        input$adm_depo_add_btn
        psqlQuery("SELECT d.id AS \"Deposit ID\",
                  a.account_name AS \"Account Name\",
                  d.value_date AS \"Value Date\",
                  d.maturity_date AS \"Maturity Date\",
                  d.amount AS \"Amount\",
                  c.iso_code AS \"Currency\",
                  d.interest_rate AS \"Interest Rate\",
                  d.interest_rate_type AS \"Interest Rate Type\",
                  d.interest_rate_spread as \"Interest Rate Spread\"
                  FROM deposit d,
                  account a,
                  currency c
                  WHERE 1=1
                  AND d.account_id=a.id
                  AND d.currency_id=c.id
                  ORDER BY d.value_date DESC, d.id DESC")$result
    })
    output$adm_depo_reviewtbl <- DT::renderDataTable(
        q_adm_depo_reviewtbl(),
        options = list(searching=F, paging=T),
        rownames=F)
    
    # Insert new deposit
    observeEvent(input$adm_depo_add_btn, {
        adm_depo_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO deposit (account_id,
                                                        value_date,
                                                        amount,
                                                        maturity_date,
                                                        currency_id,
                                                        interest_rate,
                                                        interest_rate_type,
                                                        interest_rate_spread)
                                                        VALUES('%i', '%s', '%f', '%s', '%i', '%f','%s','%f');"
                                                        ,as.numeric(input$adm_depo_add_targ_acc)
                                                        ,input$adm_depo_add_valdate
                                                        ,as.numeric(input$adm_depo_add_amt)
                                                        ,input$adm_depo_add_matdate
                                                        ,as.numeric(input$adm_depo_add_ccy)
                                                        ,as.numeric(input$adm_depo_add_intrate)
                                                        ,input$adm_depo_add_intratetyp
                                                        ,as.numeric(input$adm_depo_add_intratespread)
                                                            )
                                                       )
        shinyjs::reset("adm_depo_add_targ_acc")
        shinyjs::reset("adm_depo_add_valdate")
        shinyjs::reset("adm_depo_add_amt")
        shinyjs::reset("adm_depo_add_matdate")
        shinyjs::reset("adm_depo_add_ccy")
        shinyjs::reset("adm_depo_add_intrate")
        shinyjs::reset("adm_depo_add_intratetyp")
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

# SERVERCOMPONENT: Administration > Manage rates -------------------------------------
    
    # Retrieve existing deposits in a data table
    q_adm_rate_reviewtbl <- reactive({
        input$adm_rate_add_btn
        psqlQuery("SELECT 
                  r.id AS \"Rate ID\",
                  r.rate_name AS \"Rate Name\",
                  r.rate_type AS \"Rate Type\"
                  FROM rate r")$result
    })
    output$adm_rate_reviewtbl <- DT::renderDataTable(
        q_adm_rate_reviewtbl(),
        options = list(searching=F, paging=T),
        rownames=F)
    
    observeEvent(input$adm_rate_add_btn,{
        adm_rate_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO rate 
                                                   (rate_name, 
                                                   rate_type)
                                                   VALUES ('%s','%s');",
                                                   input$adm_rate_add_ratename,
                                                   input$adm_rate_add_ratetype
                                                   )
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

# SERVERCOMPONENT: Administration > Manage investment transactions -------------------
    
    # Display existing transactions in a data table
    q_adm_invtr_reviewtbl <- reactive({
        input$adm_invtr_add_btn
        input$adm_fundprices_upl_btn
        psqlQuery("SELECT 
                  it.value_date AS \"Value Date\",
                  it.currency AS \"Currency\",
                  it.share_amount AS \"No of Purchased Shares\",
                  ROUND(it.tr_value) AS \"Transaction Value\",
                  it.account_name AS \"Target Account\",
                  it.fund_name AS \"Puchased Fund\"
                  FROM 
                  fund_investment_transaction_vw it
                  ORDER BY
                  it.value_date DESC")$result
    })
    output$adm_invtr_reviewtbl <- DT::renderDataTable(
        q_adm_invtr_reviewtbl(),
        options = list(pageLength = 15),
        rownames=F)
    # Insert new transaction
    observeEvent(input$adm_invtr_add_btn,{
        psqlQuery(sprintf("INSERT INTO fund_investment_transaction 
                          (value_date, 
                          currency_id,
                          share_amount,
                          account_id,
                          fund_id)
                          VALUES('%s', '%i', %i, %i, %i);"
                          ,input$adm_invtr_add_valdate
                          ,as.numeric(input$adm_invtr_add_ccy)
                          ,as.numeric(input$adm_invtr_add_noshares)
                          ,as.numeric(input$adm_invtr_add_targ_acc)
                          ,as.numeric(input$adm_invtr_add_fund)
        )
    )$result
        shinyjs::reset("adm_invtr_add_valdate")
        shinyjs::reset("adm_invtr_add_ccy")
        shinyjs::reset("adm_invtr_add_tramt")
        shinyjs::reset("adm_invtr_add_noshares")
        shinyjs::reset("adm_invtr_add_targ_acc")
        shinyjs::reset("adm_invtr_add_fund")
        shinyjs::show("adm_invtr_add_confirm")
    })

# SERVERCOMPONENT: Administration > Upload fund prices -------------------------------
    
    # Display existing prices in a data table
    options(digits = 4)
    q_adm_fundprices_reviewtbl <- reactive({
        input$adm_fundprices_upl_btn
        psqlQuery("SELECT fp.id AS \"Price ID\", 
                  f.fund_name AS \"Fund Name\", 
                  fp.value_date AS \"Value Date\", 
                  fp.price AS \"Price\"
                  FROM fund_price fp, fund f
                  WHERE fp.fund_id=f.id
                  ORDER BY fp.value_date DESC, f.id ASC")$result
    })
    output$adm_fundprices_reviewtbl <- DT::renderDataTable(q_adm_fundprices_reviewtbl(),
                                                           options = list(pageLength = 15),
                                                           rownames=F)
    # Load prices file // .xls or .xlsx
    output$adm_fundprices_upl_file_desc <- renderTable(input$adm_fundprices_upl_file_inp[,-4])
    
    observeEvent(input$adm_fundprices_upl_btn, {
        library(gdata)
        options(encoding = "iso-8859-2")
        adm_fundprices_upl_file_inp_df = read.xls(input$adm_fundprices_upl_file_inp[1,4], 
                                                  sheet = 1, 
                                                  stringsAsFactors=F)
        
        df_length <- dim(adm_fundprices_upl_file_inp_df)[1]
        df_width <- dim(adm_fundprices_upl_file_inp_df)[2]
        
        n_funds <- dim(adm_fundprices_upl_file_inp_df)[2]/3
        
        price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())
        
        date <- adm_fundprices_upl_file_inp_df[2:df_length,1]
        
        for (i in 1:n_funds){
            fund_name <- gsub("\\."," ",names(adm_fundprices_upl_file_inp_df)[seq(from = 1, to = df_width, by = 3)][i])
            fund_id <- psqlQuery(sprintf("SELECT id as fund_id FROM fund WHERE fund_name='%s'", fund_name))$result
            if (nrow(fund_id)==0){
                price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())
                stop(sprintf("Process has been terminated. Unknown fund: %s. Please add first to the fund repository.", fund_name))
            }
            price <- adm_fundprices_upl_file_inp_df[2:df_length,i*3]
            price_tbl_pre <- rbind(price_tbl_pre,
                                   cbind(date,fund_id,fund_name,price)
            )
        }
        
        psqlQuery("TRUNCATE TABLE ld_fund_price")$result
        psqlInsert(price_tbl_pre, "ld_fund_price")
        
        psqlQuery("INSERT INTO fund_price (fund_id,value_date,price)
                  SELECT 
                  fund_id::int
                  ,to_date(date, 'yyyy-mm-dd')
                  ,price::float
                  FROM ld_fund_price")$result
    })

# SERVERCOMPONENT: Administration > Manage FX rates ----------------------------------
    
    #Display existing fx rates in a data table
    q_adm_fxrates_reviewtbl <- reactive({
        input$adm_fxrates_imp_add_db_btn
        psqlQuery("SELECT 
                  rate_id AS \"Rate ID\", 
                  rate_name AS \"Rate Name\", 
                  value_date AS \"Value Date\", 
                  value AS \"Rate Value\" 
                  FROM rate_value_fx_vw
                  ORDER BY value_date DESC, rate_id DESC")$result
    })
    output$adm_fxrates_reviewtbl <- DT::renderDataTable(q_adm_fxrates_reviewtbl(),
                                                        options = list(pageLength = 15),
                                                        rownames=F)
    
    observeEvent(input$adm_fxrates_imp_btn,{
        adm_fxrates_inpccy <- input$adm_fxrates_imp_ccy
        adm_fxrates_inpdatefrom <- as.character(input$adm_fxrates_imp_datefrom, format="%d%%2F%m%%2F%Y")
        adm_fxrates_inpdateto <- as.character(input$adm_fxrates_imp_dateto, format="%d%%2F%m%%2F%Y")
        retrieve_url <- sprintf("https://www.mnb.hu/en/arfolyam-tablazat?deviza=rbCurrencySelect&devizaSelected=%s&datefrom=%s&datetill=%s&order=1", 
                                adm_fxrates_inpccy,
                                adm_fxrates_inpdatefrom,
                                adm_fxrates_inpdateto)
        web_import <- getURL(retrieve_url)
        df_import <- readHTMLTable(web_import, trim = T, header = F, as.data.frame = T, stringsAsFactors=F)[[1]]
        
        df_import[,1] <- as.Date(df_import[,1], format = "%d %B %Y")
        df_import[,2] <- as.numeric(df_import[,2])
        df_import[,3] <- sprintf("HUF%s",adm_fxrates_inpccy)
        colnames(df_import) <- c("value_date","value","rate_name")
        
        shinyjs::show("adm_fxrates_imp_review")
        output$adm_fxrates_out_df <- DT::renderDataTable(df_import, 
                                                         options = list(pageLength = 5, 
                                                                        searching = F),
                                                         rownames=F)
        psqlQuery("TRUNCATE TABLE ld_rate_value")$result
        psqlInsert(df_import, "ld_rate_value")
        
    })
    observeEvent(input$adm_fxrates_imp_add_db_btn, {
        psqlQuery("INSERT INTO rate_value (rate_id,value_date,value)
                  SELECT
                  r.id::int,
                  to_date(ldrv.value_date,'yyyy-mm-dd'),
                  ldrv.value::float
                  FROM
                  ld_rate_value ldrv
                  LEFT OUTER JOIN rate r ON ldrv.rate_name=r.rate_name")$result
        shinyjs::hide("adm_fxrates_imp_review")
    })

}

# init UI -----------------------------------------------------------------

ui <- dashboardPage(

# UI: dashboardHeader() -------------------------------------------------------
    
    dashboardHeader(title="FinApp", titleWidth = "270px"),

# UI: dashboardSidebar() ------------------------------------------------------
    
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
                                  menuSubItem("Upload fund prices", tabName = "adm_fundprices"),
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

# UI: Home --------------------------------------------------------------
            
            tabItem(tabName = "home",
                    h2("Home tab content")
            ),

# UI: Funds > Overview ------------------------------------------
            
            tabItem(tabName = "funds_ov",
                    h2("Funds", tags$small("Overview")),
                    fluidRow(
                        column(width=4,
                               valueBox(value = textOutput("funds_ov_tot_fund_inv"), 
                                        "Total Fund Investment (HUF)", 
                                        icon = icon("money"),
                                        width = NULL,
                                        color = "light-blue"),
                               infoBox(value = textOutput("funds_ov_avg_fund_inv"), 
                                       title = sprintf("Average Balance (HUF) since %s",
                                                       fund_inv_ov_periodstart[1,1]), 
                                       icon = icon("money"),
                                       width = NULL,
                                       color = "light-blue")
                        ),
                        column(width=4,
                               valueBox(value = textOutput("fund_ov_tot_fund_net_yield"),
                                        "Total Net Yield (HUF)",
                                        icon = icon("bar-chart"),
                                        width = NULL,
                                        color="green"),
                               infoBox(value = textOutput("fund_ov_tot_fund_gross_yield"), 
                                       title = "Total Gross Yield (HUF)",
                                       icon = icon("bar-chart"),
                                       width = NULL,
                                       color = "green"),
                               infoBox(value = textOutput("fund_ov_eff_taxpct"), 
                                       title = "Effective Tax Rate (%)",
                                       icon = icon("percent"),
                                       width = NULL,
                                       color = "green")
                        ),
                        column(width=4,
                               valueBox(value = textOutput("fund_ov_tot_fund_net_yield_pct"),
                                        "Annualized Net Yield (%)",
                                        icon = icon("percent"),
                                        width = NULL,
                                        color="yellow"),
                               infoBox(value = textOutput("fund_ov_tot_fund_gross_yield_pct"),
                                       "Annualized Gross Yield (%)",
                                       icon = icon("percent"),
                                       width = NULL,
                                       color="yellow")
                        )
                    ),
                    fluidRow(
                        box(DT::dataTableOutput("fundtable"),
                            width = 12, 
                            status="primary")
                    )
            ),

# UI: Funds > Fund Prices -------------------------------------------------------
            
            tabItem(tabName = "fundprices",
                    h2("Funds", tags$small("Prices")),
                    fluidRow(
                        box(plotlyOutput("plot_fp_relative"),
                            title="Relative Price Changes",
                            width = 12,
                            status = "primary")
                    ),
                    fluidRow(
                        box(title="Individual Fund Price Charts",
                            solidHeader = T,
                            width=12,
                            "Charts below depict price development for each
                                funds since the dates of first investments.",
                            br(),br(),
                            lapply(fund_ids[,1], function(i) {
                                uiOutput(paste0('dyn_plot_', i))
                            })
                            )
                    )
            ),

# UI: Accounts ----------------------------------------------------------
            
            tabItem(tabName = "accounts",
                    h2("Accounts tab content")
            ),

# UI: Macro > FX Rates --------------------------------------------------
            
            tabItem(tabName = "macro_fx",
                    h2("Macroeconomic Factors", tags$small("FX Rates"))
            ),

# UI: Macro > Interest Rates --------------------------------------------
            
            tabItem(tabName = "macro_ir",
                    h2("Macroeconomic Factors", tags$small("Interest Rates"))
            ),

# UI: Administration > Manage Accounts ---------------------------------------------------
            
            tabItem(tabName = "adm_acc",
                    h2("Administration", tags$small("Manage accounts")),
                    fluidRow(
                        column(width = 3,
                               box(title = "Add an account", 
                                   width=NULL,
                                   solidHeader = T,
                                   status = "danger",
                                   textInput("adm_acc_add_name","Account Name"),
                                   selectInput("adm_acc_add_taxappl","Tax Applicable",
                                               c("please select.."="","Yes"="Y","No"="N")),
                                   textInput("adm_acc_add_taxpercent","Tax Rate %"),
                                   actionButton("adm_acc_add_btn",
                                                "Add", 
                                                icon("plus"))
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
                        column(width=9,
                               box(DT::dataTableOutput("adm_acc_reviewtbl"),
                                   width=NULL)
                               )
                    )
            ),

# UI: Administration > Manage Funds ------------------------------------------------------
            
            tabItem(tabName = "adm_fund",
                    h2("Administration", tags$small("Manage funds")),
                    fluidRow(
                        box(title = "Add a fund", 
                            solidHeader = T,
                            status = "danger",
                            width = 4,
                            textInput("adm_fund_add_name","Fund Name"),
                            textInput("adm_fund_add_isin","ISIN"),
                            actionButton("adm_fund_add_btn",
                                         "Add", 
                                         icon("plus")),
                            shinyjs::hidden(
                                div(id = "adm_fund_add_confirm",
                                    p("Fund has been added successfully.")
                                )
                            )
                        ),
                        box(DT::dataTableOutput("adm_fund_reviewtbl"),
                            width = 8)
                    )
            ),

# UI: Administration > Manage Currencies -------------------------------------------------
            
            tabItem(tabName = "adm_ccy",
                    h2("Administration", tags$small("Manage currencies")),
                    fluidRow(
                        box(title = "Add currency", 
                            solidHeader = T,
                            status = "danger",
                            width = 4,
                            textInput("adm_ccy_add_iso","ISO Code"),
                            textInput("adm_ccy_add_desc","Description"),
                            actionButton("adm_ccy_add_btn",
                                         "Add", 
                                         icon("plus")),
                            shinyjs::hidden(
                                div(id = "adm_ccy_add_confirm",
                                    p("Currency has been added successfully.")
                                )
                            )
                        ),
                        box(DT::dataTableOutput("adm_ccy_reviewtbl"),
                            width = 8)
                    )
            ),

# UI: Administration > Manage Deposits ---------------------------------------------------
            
            tabItem(tabName = "adm_depo",
                    h2("Administration", tags$small("Manage deposits")),
                    fluidRow(
                        box(title = "Add deposit",
                            width = 12,
                            solidHeader = T,
                            status = "danger",
                            column(width = 4,
                                   selectInput("adm_depo_add_targ_acc",
                                               "Target Account",
                                               acc_list),
                                   selectInput("adm_depo_add_ccy","Currency",
                                               ccy_list),
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
                                             format = "yyyy-mm-dd")
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
                    fluidRow(box(title="Review deposits",
                                 solidHeader = T,
                                 status = "primary",
                                 width = 12,
                                 DT::dataTableOutput("adm_depo_reviewtbl")))
                    
            ),

# UI: Administration > Manage Rates ------------------------------------------------------
            
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
                               box(title="Review rates",
                                   solidHeader = T,
                                   status = "primary",
                                   width=12,
                                   DT::dataTableOutput("adm_rate_reviewtbl")))
                        )
            ),

# UI: Administration > Manage Investment Transactions ------------------------------------
            
            tabItem(tabName = "adm_invtr",
                    h2("Administration", tags$small("Manage fund investment transactions")),
                    fluidRow(
                        box(title="Add fund investment transaction",
                            solidHeader = T,
                            status = "danger",
                            width = 12,
                            fluidRow(
                                column(width=5,
                                       selectInput("adm_invtr_add_targ_acc",
                                                   "Target Account",
                                                   acc_list)),
                                column(width=5,
                                       selectInput("adm_invtr_add_fund",
                                                   "Purchased Fund",
                                                   fund_list)),
                                column(width=2,
                                       selectInput("adm_invtr_add_ccy",
                                                   "Currency",
                                                   ccy_list))   
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
                            fluidRow(
                                column(width=12,
                                       shinyjs::hidden(
                                           div(id = "adm_invtr_add_confirm",
                                               p("Transaction has been added successfully.")
                                           )
                                       )   
                                )
                            ),
                            tags$style(type='text/css', 
                                       "#adm_invtr_add_btn {width:100%; 
                                       margin-top: 25px;}")
                            )
                        ),
                    fluidRow(
                        box(title="Review investment transactions",
                            solidHeader = T,
                            status = "primary",
                            width = 12,
                            DT::dataTableOutput("adm_invtr_reviewtbl"))
                    )
                    ),

# UI: Administration > Upload Fund Prices ------------------------------------------------
            
            tabItem(tabName = "adm_fundprices",
                    h2("Administration", tags$small("Upload fund prices")),
                    fluidRow(
                        box(title="Fund Price Bulk Upload", 
                            solidHeader = T,
                            status="danger",
                            fileInput("adm_fundprices_upl_file_inp", 
                                      "Choose File",
                                      accept = c(".xls",".xlsx")
                            ),
                            tableOutput("adm_fundprices_upl_file_desc"),
                            actionButton("adm_fundprices_upl_btn",
                                         "Import", 
                                         icon("database")),
                            textOutput("fp_upl_error"),
                            width = 8
                        )
                    ),
                    fluidRow(
                        box(DT::dataTableOutput("adm_fundprices_reviewtbl"),
                            width = 12)
                    )
            ),

# UI: Administration > Manage FX rates ---------------------------------------------------
            
            tabItem(tabName = "adm_fxrates",
                    h2("Administration", tags$small("Manage FX rates")),
                    fluidRow(
                        box(title="Import FX rates", width = 12,
                            solidHeader = T,
                            status = "danger",
                            p("This module imports historical FX rates of HUF and 
                              various currency pairs from the official",
                              tags$a(href="http://mnb.hu/en/arfolyam-lekerdezes", "website"),
                              "of Hungarian National Bank. (MNB)"),
                            p("MNB currently quotes official central bank rates 
                              for the following currencies only:",br(),"AUD, BGN, BRL, CAD, CHF, CNY, 
CZK, DKK, EUR, GBP, HKD, HRK, IDR, ILS, INR, ISK, JPY, KRW, MXN, MYR, NOK, NZD, PHP, PLN, RON, RSD, RUB, SEK,
SGD, THB, TRY, UAH, USD, ZAR."),
                            column(width=3,
                                   dateInput("adm_fxrates_imp_datefrom",
                                             "Date From", 
                                             format = "yyyy-mm-dd")
                            ),
                            column(width=3,
                                   dateInput("adm_fxrates_imp_dateto",
                                             "Date To", 
                                             format = "yyyy-mm-dd")
                            ),
                            column(width=3,
                                   selectInput("adm_fxrates_imp_ccy", 
                                               "Currency", 
                                               ccy_list_nonid)
                            ),
                            column(width=3,
                                   actionButton("adm_fxrates_imp_btn",
                                                "Download",
                                                icon("cloud-download")
                                   ),
                                   tags$style(type='text/css', 
                                              "#adm_fxrates_imp_btn {margin-top: 25px;}")
                            )
                        )
                    )
                    ,shinyjs::hidden(
                        div(id="adm_fxrates_imp_review",
                            h4("Review the list of rates to be added"),
                            box(status="danger",
                                solidHeader = T, 
                                width=NULL, 
                                DT::dataTableOutput("adm_fxrates_out_df"),
                                br(),
                                actionButton("adm_fxrates_imp_add_db_btn", 
                                             "Add to database",
                                             icon("plus")))
                        )
                    ),
                    h4("Stored FX Rates"),
                    fluidRow(box(DT::dataTableOutput("adm_fxrates_reviewtbl"),
                                 width = 12))
            )

# UI: end tabitems() -------------------------------------------------------
            
    )

# UI: end dashboardBody() --------------------------------------------------

)

# UI: end dashboardPage() --------------------------------------------------

)

# call shinyApp() ---------------------------------------------------------

shinyApp(ui = ui, server = server)


