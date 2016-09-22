
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

source("f.R")

shinyServer(
    function(input, output, session) {

###############################################################################
### Page: Funds > Overview
    q_funds_ov_tot_fund_inv <-#reactive({
        #input$ # placeholder for reactive trigger
        dbquery("SELECT ROUND(SUM(amount)) FROM investment_transaction")
    #})
    output$funds_ov_tot_fund_inv <- shiny::renderText(format(q_funds_ov_tot_fund_inv[1,1], big.mark=" "))
    
    q_fund_ov_tot_fund_yield <-#reactive({
        #input$ # placeholder for reactive trigger
        dbquery("SELECT 
                    ROUND(SUM((it.shares_purchased*fp.price-it.amount)*(1-a.tax_rate_percent/100)))
                 FROM 
                    investment_transaction it,
                    account a,
                    (SELECT * FROM 
                     fund_prices 
                     WHERE value_date=(SELECT MAX(value_date) FROM fund_prices)) fp
                 WHERE 1=1
                 AND it.fund_id=fp.fund_id
                 AND it.investment_account_id=a.id")
    #})
    output$fund_ov_tot_fund_yield <- shiny::renderText(format(q_fund_ov_tot_fund_yield[1,1], big.mark=" "))

    q_fund_ov_tot_fund_yield_pct <-#reactive({
        #input$ # placeholder for reactive trigger
        dbquery("SELECT 
                    ROUND(AVG(((it.shares_purchased*fp.price-it.amount)*(1-a.tax_rate_percent/100)/DATEDIFF((SELECT MAX(value_date) FROM fund_prices),it.value_date))/it.amount*100*365),2)
                 FROM 
                    investment_transaction it,
                    account a,
                    (SELECT * FROM fund_prices WHERE value_date=(SELECT MAX(value_date) FROM fund_prices)) fp
                WHERE 1=1
                AND it.fund_id=fp.fund_id
                AND it.investment_account_id=a.id")
    #})
    output$fund_ov_tot_fund_yield_pct <- shiny::renderText(q_fund_ov_tot_fund_yield_pct[1,1])
    
    # List of funds for Fund Investment Overview
    fund_df <- dbquery("SELECT 
                            f.id AS 'Fund ID',
                            f.fund_name AS 'Fund Name', 
                            MIN(it.value_date) AS 'Investments Since', 
                            SUM(ROUND(it.amount,2)) AS 'Investment Amount', 
                            c.iso_code AS 'Currency', 
                            SUM(it.shares_purchased) AS 'Number of Shares Purchased',
                            SUM(ROUND(it.shares_purchased*fp.price,2)) AS 'Current Value of Investment',
                            ROUND(SUM(it.shares_purchased*fp.price)-SUM(it.amount),2) AS 'Gain/Loss Amount',
                            ROUND((SUM(it.shares_purchased*fp.price)-SUM(it.amount))/SUM(it.amount)*100,2) AS 'Gain/Loss %'
                        FROM fund f, 
                             investment_transaction it, 
                             currency c,
                             (SELECT * FROM fund_prices WHERE value_date=(SELECT MAX(value_date) FROM fund_prices)) fp
                        WHERE 1=1 
                              AND f.id=it.fund_id
                              AND it.currency_id=c.id
                              AND f.id=fp.fund_id
                        GROUP BY
                              f.fund_name, 
                              f.isin, 
                              c.iso_code")
    fund_df$`Investment Amount` <- as.integer(fund_df$`Investment Amount`)
    fund_df$`Number of Shares Purchased` <- as.integer(fund_df$`Number of Shares Purchased`)
    output$fundtable <- DT::renderDataTable(fund_df[,-1],
                                            options = list(searching=F, paging=F),
                                            rownames=F)


###############################################################################    
### Page: Funds > Fund Prices
    # Query DB fund prices for graphs
    fundprice_df <- dbquery("SELECT f.id, f.fund_name, fp.value_date, fp.price
                             FROM fund_prices fp, fund f
                             WHERE fp.fund_id=f.id")
    fundprice_df$value_date <- as.Date(fundprice_df$value_date, "%Y-%m-%d")
    fundprice_df$fund_name <- as.factor(fundprice_df$fund_name)

    # Create dynamic plot rendering
    lapply(fund_df$`Fund ID`, function(i) {
        data <- fundprice_df[fundprice_df$id==i&
                                 fundprice_df$value_date>=as.Date(fund_df[fund_df$`Fund ID`==i,]$`Investments Since`, 
                                                                  "%Y-%m-%d")
                             ,]
        output[[paste0("plot_fp_",i)]] <- renderPlot(height = 250,
            ggplot(data, #fundprice_df[fundprice_df$id==i,], 
                   aes(x=value_date,y=price)) + geom_line() + labs(x="",y="")
        )        
    })
    
    # Create dynamic UI content
    lapply(fund_df$`Fund ID`, function(i) {
    output[[paste0("dyn_plot_",i)]] <- renderUI({
        # column(4,plotOutput(paste0("plot_fp_",i)))
        box(title=fund_df$`Fund Name`[fund_df$`Fund ID`==i],
            plotOutput(paste0("plot_fp_",i)),
            width = 6, 
            height = 315,
            status = "warning",
            solidHeader = T)
        })
    })
    
###############################################################################
### Page: Administration > Manage accounts
    
    # Retrieve existing accounts in a data table
    q_adm_acc_reviewtbl <- reactive({
                    input$adm_acc_add_btn
                    dbquery("SELECT id AS 'Account ID', 
                             account_name AS 'Account Name',
                             tax_applicable AS 'Tax Applicable',
                             tax_rate_percent AS 'Tax Rate %'
                             FROM account a")
    })
    output$adm_acc_reviewtbl <- DT::renderDataTable(
        q_adm_acc_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)

    # Insert new account
    observeEvent(input$adm_acc_add_btn, {
            dbquery(sprintf("INSERT INTO account (account_name) VALUES ('%s')",input$adm_acc_add_name))
            shinyjs::reset("adm_acc_add_name")
            shinyjs::show("adm_acc_add_confirm")

    })
###############################################################################
### Page: Administration > Manage funds
    
    # Retrieve existing funds in a data table
    q_adm_fund_reviewtbl <- reactive({
        input$adm_fund_add_btn
        dbquery("SELECT 
                id AS 'Fund ID',
                fund_name AS 'Fund Name',
                isin AS 'ISIN'
                FROM 
                fund f")
    })
    output$adm_fund_reviewtbl <- DT::renderDataTable(
        q_adm_fund_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)
    
    # Insert new fund
    observeEvent(input$adm_fund_add_btn, {
        adm_fund_add_df <-  data.frame(fund_name=input$adm_fund_add_name,
                                          isin=input$adm_fund_add_isin)
        dbUIinsert(adm_fund_add_df,"fund")
        shinyjs::reset("adm_fund_add_name")
        shinyjs::reset("adm_fund_add_isin")
        shinyjs::show("adm_fund_add_confirm")
        
    })

###############################################################################
### Page: Administration > Manage currencies
    
    # Retrieve existing funds in a data table
    q_adm_ccy_reviewtbl <- reactive({
        input$adm_ccy_add_btn
        dbquery("SELECT 
                id AS 'Currency ID',
                iso_code AS 'ISO Code',
                description AS 'Description'
                FROM 
                currency c")
    })
    output$adm_ccy_reviewtbl <- DT::renderDataTable(
        q_adm_ccy_reviewtbl(),
        options = list(searching=F, paging=F),
        rownames=F)
    
    # Insert new currency
    observeEvent(input$adm_ccy_add_btn, {
        adm_ccy_add_df <- data.frame(
            iso_code=input$adm_ccy_add_iso,
            description=input$adm_ccy_add_desc
        )
        dbUIinsert(adm_ccy_add_df, "currency")
        shinyjs::reset("adm_ccy_add_iso")
        shinyjs::reset("adm_ccy_add_desc")
        shinyjs::show("adm_ccy_add_confirm")
    })
    
###############################################################################
### Page: Administration > Manage investment transactions
    
    # Display existing transactions in a data table
    q_adm_invtr_reviewtbl <- reactive({
        input$adm_invtr_add_btn
        dbquery("SELECT 
                it.value_date AS 'Value Date',
                it.amount AS 'Transaction Amount',
                c.iso_code AS 'Currency',
                it.shares_purchased AS 'No of Purchased Shares',
                a.account_name AS 'Target Account',
                f.fund_name AS 'Puchased Fund'
                FROM 
                investment_transaction it,
                currency c,
                account a,
                fund f
                WHERE 1=1
                AND it.currency_id=c.id
                AND it.investment_account_id=a.id
                AND it.fund_id=f.id
                ORDER BY
                it.value_date DESC")
    })
    output$adm_invtr_reviewtbl <- DT::renderDataTable(
                                              q_adm_invtr_reviewtbl(),
                                              options = list(pageLength = 15),
                                              rownames=F)
    # Insert new transaction
    observeEvent(input$adm_invtr_add_btn,{
        adm_invtr_add_df <- data.frame(
            value_date=input$adm_invtr_add_valdate,
            currency_id=input$adm_invtr_add_ccy,
            amount=input$adm_invtr_add_tramt,
            shares_purchased=input$adm_invtr_add_noshares,
            investment_account_id=input$adm_invtr_add_targ_acc,
            fund_id=input$adm_invtr_add_fund
        )
        dbUIinsert(adm_invtr_add_df, "investment_transaction")
        shinyjs::reset("adm_invtr_add_valdate")
        shinyjs::reset("adm_invtr_add_ccy")
        shinyjs::reset("adm_invtr_add_tramt")
        shinyjs::reset("adm_invtr_add_noshares")
        shinyjs::reset("adm_invtr_add_targ_acc")
        shinyjs::reset("adm_invtr_add_fund")
        shinyjs::show("adm_invtr_add_confirm")
    })

###############################################################################
### Page: Administration > Upload fund prices
    
    # Display existing prices in a data table
    options(digits = 4)
    q_adm_fundprices_reviewtbl <- reactive({
        input$adm_fundprices_upl_btn
        dbquery("SELECT fp.id AS 'Price ID', 
                        f.fund_name AS 'Fund Name', 
                        fp.value_date AS 'Value Date', 
                        fp.price AS 'Price'
                 FROM fund_prices fp, fund f
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
            fund_id <- dbquery(sprintf("SELECT id as fund_id FROM finapp.fund WHERE fund_name='%s'", fund_name))
            if (nrow(fund_id)==0){
                price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())
                stop(sprintf("Process has been terminated. Unknown fund: %s. Please add first to the fund repository.", fund_name))
            }
            price <- adm_fundprices_upl_file_inp_df[2:df_length,i*3]
            price_tbl_pre <- rbind(price_tbl_pre,
                                   cbind(date,fund_id,fund_name,price)
            )
        }
        
        dbinsert(price_tbl_pre, "ld_fund_price")
        
        dbquery("INSERT INTO fund_prices (fund_id,value_date,price)
                    SELECT
                       CAST(ld.fund_id AS UNSIGNED)
                       ,CAST(ld.date AS DATE)
                       ,CAST(ld.price AS DECIMAL(10,6))
                    FROM ld_fund_price ld LEFT OUTER JOIN fund_prices t 
                            ON ld.date = t.value_date AND ld.fund_id = t.fund_id
                    WHERE t.id IS NULL")
    })

    
    
    
        
    }
    
)