
# Initialization: libraries ----------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Initialization: sourcing external code ---------------------------------

source("f.R")

# Initialization: variables -----------------------------------------------

fund_inv_ov_perioddays <- psqlQuery("SELECT
                                    get_last_fundprice_date()-
                                    (SELECT MIN(value_date) FROM fund_investment_transaction) AS days")

# Initialization: call shinyServer() ---------------------------------------

shinyServer(
    function(input, output, session) {

# Page: Funds > Overview --------------------------------------------------

    q_funds_ov_tot_fund_inv <-#reactive({
        #input$ # placeholder for reactive trigger
        psqlQuery("SELECT ROUND(SUM(tr_value)) FROM fund_investment_transaction_vw")
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
                      ) tt")
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
                    AND it.fund_id=fp.fund_id")
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
                  AND it.fund_id=fp.fund_id")
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
                            \"Gain/Loss %\" DESC")
    fund_df$`Investment Amount` <- as.integer(fund_df$`Investment Amount`)
    fund_df$`Number of Shares Purchased` <- as.integer(fund_df$`Number of Shares Purchased`)
    output$fundtable <- DT::renderDataTable(fund_df[,-1],
                                            options = list(searching=F, paging=F),
                                            rownames=F)

# Page: Funds > Fund Prices -----------------------------------------------

    # Query DB fund prices for graphs
    fundprice_df <- psqlQuery("SELECT f.id, f.fund_name, fp.value_date, fp.price,
	                                  (fp.price/first_value(price) OVER (PARTITION BY f.id ORDER BY fp.value_date)-1)*100 change_pct
                              FROM fund_price fp, 
                                   fund f, 
                                   (SELECT fund_id, MIN(value_date) min_valdat FROM fund_investment_transaction GROUP BY fund_id) fit
                              WHERE fp.fund_id=f.id
                                    AND fp.fund_id=fit.fund_id
                                    AND fp.value_date>=fit.min_valdat")
    fundprice_df$value_date <- as.Date(fundprice_df$value_date, "%Y-%m-%d")
    fundprice_df$fund_name <- as.factor(fundprice_df$fund_name)

    # Create overall price changes plot
    output$plot_fp_relative <- renderPlot(
        ggplot(fundprice_df, 
               aes(x=value_date,y=change_pct,colour=fund_name)) + 
               geom_line() + 
               labs(x="",y="Change %") +
               theme(legend.title=element_blank())
        )

    # Create dynamic plot rendering
    lapply(fund_df$`Fund ID`, function(i) {
        data <- fundprice_df[fundprice_df$id==i,]
        output[[paste0("plot_fp_",i)]] <- renderPlot(height = 250,
            ggplot(data, aes(x=value_date,y=price)) + geom_line() + labs(x="",y="")
        )        
        output[[paste0("dyn_plot_",i)]] <- renderUI({
            box(title=fund_df$`Fund Name`[fund_df$`Fund ID`==i],
                plotOutput(paste0("plot_fp_",i)),
                width = 6, 
                height = 315,
                status = "warning",
                solidHeader = T)
        })        
    })

# Page: Administration > Manage accounts ----------------------------------

    # Retrieve existing accounts in a data table
    q_adm_acc_reviewtbl <- reactive({
                    input$adm_acc_add_btn
                    psqlQuery("SELECT id AS \"Account ID\", 
                               account_name AS \"Account Name\",
                               tax_applicable AS \"Tax Applicable\",
                               tax_rate_percent AS \"Tax Rate %\"
                               FROM account a")
    })
    output$adm_acc_reviewtbl <- DT::renderDataTable(
        q_adm_acc_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)

    # Insert new account
    observeEvent(input$adm_acc_add_btn, {
            psqlQuery(sprintf("INSERT INTO account (account_name, 
                                                    tax_applicable, 
                                                    tax_rate_percent)
                                            VALUES('%s', '%s', %f);",input$adm_acc_add_name
                                                                    ,input$adm_acc_add_taxappl
                                                                    ,as.numeric(input$adm_acc_add_taxpercent)))
            shinyjs::reset("adm_acc_add_name")
            shinyjs::reset("adm_acc_add_taxappl")
            shinyjs::reset("adm_acc_add_taxpercent")
            shinyjs::show("adm_acc_add_confirm")

    })

# Page: Administration > Manage funds -------------------------------------

    # Retrieve existing funds in a data table
    q_adm_fund_reviewtbl <- reactive({
        input$adm_fund_add_btn
        psqlQuery("SELECT id AS \"Fund ID\",
                          fund_name AS \"Fund Name\",
                          isin AS \"ISIN\"
                  FROM fund
                  ORDER BY id")
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
                          ,input$adm_fund_add_isin))
        shinyjs::reset("adm_fund_add_name")
        shinyjs::reset("adm_fund_add_isin")
        shinyjs::show("adm_fund_add_confirm")
        
    })

# Page: Administration > Manage currencies --------------------------------

    # Retrieve existing currencies in a data table
    q_adm_ccy_reviewtbl <- reactive({
        input$adm_ccy_add_btn
        psqlQuery("SELECT id AS \"Currency ID\",
                          iso_code AS \"ISO Code\",
                          description AS \"Description\"
                   FROM currency
                   ORDER BY id")
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
                          ,input$adm_ccy_add_desc))
        shinyjs::reset("adm_ccy_add_iso")
        shinyjs::reset("adm_ccy_add_desc")
        shinyjs::show("adm_ccy_add_confirm")
    })

# Page: Administration > Manage deposits ----------------------------------

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
                   ORDER BY d.value_date DESC, d.id DESC")
    })
    output$adm_depo_reviewtbl <- DT::renderDataTable(
        q_adm_depo_reviewtbl(),
        options = list(searching=F, paging=T),
        rownames=F)

    # Insert new deposit
    observeEvent(input$adm_depo_add_btn, {
        psqlQuery(sprintf("INSERT INTO deposit (account_id,
                                                value_date,
                                                amount,
                                                maturity_date,
                                                currency_id,
                                                interest_rate,
                                                interest_rate_type,
                                                interest_rate_spread
                                                )
                          VALUES('%i', '%s', '%f', '%s', '%i', '%f','%s','%f');"
                          ,as.numeric(input$adm_depo_add_targ_acc)
                          ,input$adm_depo_add_valdate
                          ,as.numeric(input$adm_depo_add_amt)
                          ,input$adm_depo_add_matdate
                          ,as.numeric(input$adm_depo_add_ccy)
                          ,as.numeric(input$adm_depo_add_intrate)
                          ,input$adm_depo_add_intratetyp
                          ,as.numeric(input$adm_depo_add_intratespread)))
        shinyjs::reset("adm_depo_add_targ_acc")
        shinyjs::reset("adm_depo_add_valdate")
        shinyjs::reset("adm_depo_add_amt")
        shinyjs::reset("adm_depo_add_matdate")
        shinyjs::reset("adm_depo_add_ccy")
        shinyjs::reset("adm_depo_add_intrate")
        shinyjs::reset("adm_depo_add_intratetyp")
        shinyjs::reset("adm_depo_add_intratespread")
        shinyjs::show("adm_depo_add_confirm")
    })
    
    
# Page: Administration > Manage investment transactions -------------------

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
                    it.value_date DESC")
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
                  )
        shinyjs::reset("adm_invtr_add_valdate")
        shinyjs::reset("adm_invtr_add_ccy")
        shinyjs::reset("adm_invtr_add_tramt")
        shinyjs::reset("adm_invtr_add_noshares")
        shinyjs::reset("adm_invtr_add_targ_acc")
        shinyjs::reset("adm_invtr_add_fund")
        shinyjs::show("adm_invtr_add_confirm")
    })

# Page: Administration > Upload fund prices -------------------------------

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
                 ORDER BY fp.value_date DESC, f.id ASC")
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
            fund_id <- psqlQuery(sprintf("SELECT id as fund_id FROM fund WHERE fund_name='%s'", fund_name))
            if (nrow(fund_id)==0){
                price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())
                stop(sprintf("Process has been terminated. Unknown fund: %s. Please add first to the fund repository.", fund_name))
            }
            price <- adm_fundprices_upl_file_inp_df[2:df_length,i*3]
            price_tbl_pre <- rbind(price_tbl_pre,
                                   cbind(date,fund_id,fund_name,price)
            )
        }
        
        psqlQuery("TRUNCATE TABLE ld_fund_price")
        psqlInsert(price_tbl_pre, "ld_fund_price")
        
        psqlQuery("INSERT INTO fund_price (fund_id,value_date,price)
                    SELECT 
                        fund_id::int
                        ,to_date(date, 'yyyy-mm-dd')
                        ,price::float
                    FROM ld_fund_price")
        })

    
    
    

# end shinyServer() -------------------------------------------------------

    })

# others ------------------------------------------------------------------



