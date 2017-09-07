

# init libraries ----------------------------------------------------------
library(plotly)

library(shiny)
library(shinydashboard)
library(shinyjs)

library(DT)

library(PerformanceAnalytics)

library(plyr)
library(dplyr)

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

# SERV: Funds > Overview --------------------------------------------------

q_funds_ov_portf_statem_reviewtbl <- reactive({
    input$adm_invtr_add_btn
    
    # Retrieve data for Cummulative Yield time series
    df_portfolio_performance_timeseries <-psqlQuery("SELECT 
                                                        p.id portfolio_id,
                                                        p.name portfolio,
                                                        fp.value_date date,
                                                        fit.currency,
                                                        SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fp.price*fit.share_amount)-ROUND(fit.tr_value) 
                                                            ELSE ROUND(CAST(fp.price*fit.share_amount AS DECIMAL),2)-ROUND(CAST(fit.tr_value AS DECIMAL),2) END) accumulated_yield,
                                                        SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fit.tr_value) 
                                                            ELSE ROUND(CAST(fit.tr_value AS DECIMAL),2) END) cumulative_balance
                                                    FROM
                                                        app.fund_investment_transaction_vw fit,
                                                        app.fund_price fp,
                                                        app.portfolio_x_account_rltnp pxa,
                                                        app.portfolio p
                                                    WHERE 1=1
                                                        AND fit.account_id=pxa.account_id
                                                        AND pxa.portfolio_id=p.id
                                                        AND fit.fund_id=fp.fund_id
                                                        AND fit.value_date<=fp.value_date
                                                        AND p.last_valuation_date>=fp.value_date
                                                    GROUP BY p.id, p.name, fp.value_date, fit.currency
                                                    ORDER BY fit.currency DESC, p.name, fp.value_date
                                                    ")$result

    # Creating Annualized Portfolio Returns
    df_tmp_portf_ann_return <- psqlQuery("SELECT portfolio, date, return FROM app.portfolio_return_calc_mvw WHERE return IS NOT NULL ORDER BY portfolio, date")$result
    df_tmp_portf_ann_return$portfolio <- as.factor(df_tmp_portf_ann_return$portfolio)
    pf_idx <- unique(df_tmp_portf_ann_return$portfolio)
    
    pf_tmp_res <- data.frame(portfolio=character(),date=as.Date(character()),ann_yield_pct=numeric())
    for(i in 1:length(pf_idx)){
        idx_inner <- unique(df_tmp_portf_ann_return[df_tmp_portf_ann_return$portfolio==pf_idx[i],"date"])
        if(length(idx_inner)>=252){
            for(j in 252:length(idx_inner)){
                pf_tmp_res <- rbind(pf_tmp_res,
                                    data.frame(
                                        portfolio=pf_idx[i],
                                        date=idx_inner[j],
                                        ann_yield_pct=Return.annualized(df_tmp_portf_ann_return[df_tmp_portf_ann_return$portfolio==pf_idx[i],][1:j,"return"],
                                                                        scale = 252, geometric = T)*100
                                    ),stringsAsFactors = F)
            }
        }
    }
    dt_recent_pf_performance <- pf_tmp_res %>% inner_join(group_by(pf_tmp_res, portfolio) %>% 
                                                           summarise(recent_dt=max(date)),
                                                                by = c("portfolio" = "portfolio", "date" = "recent_dt"))
    

    # Retrieve performance details on every single investment transaction
    df_fundportf_details <-psqlQuery("SELECT 
                                      fit.account_name \"Account\", 
                                      fit.fund_name \"Fund\", 
                                      fit.currency \"Currency\",
                                      fit.value_date \"Value Date\", 
                                      fit.share_amount || ' pc' \"Purchased Shares\", 
                                      CASE WHEN fit.currency='HUF' THEN ROUND(fit.tr_value) 
                                        ELSE ROUND(CAST(fit.tr_value AS DECIMAL),2) END ||' '|| fit.currency  \"Invested Amount\",
                                      CASE WHEN fit.currency='HUF' THEN ROUND(fpr.price*fit.share_amount)-ROUND(fit.tr_value) 
                                        ELSE ROUND(CAST(fpr.price*fit.share_amount AS DECIMAL),2)-ROUND(CAST(fit.tr_value AS DECIMAL),2) END ||' '|| fit.currency \"Profit / Loss\",
                                      (fpr.price*fit.share_amount-fit.tr_value)/fit.tr_value \"Yield\",
                                      CASE WHEN (fpr.value_date-fit.value_date+1)<365.0 THEN NULL
                                        ELSE ((ROUND(fpr.price*fit.share_amount)/ROUND(fit.tr_value))^(365.0/(fpr.value_date-fit.value_date+1)::float)-1)
                                      END \"Annualized Yield\"
                                      FROM app.fund_investment_transaction_vw fit
                                      LEFT OUTER JOIN app.fund_price_recent_vw fpr ON fit.fund_id=fpr.fund_id
                                      ORDER BY fit.value_date DESC, fit.portfolio_id, fit.account_id")$result

    # Retrieve performance details aggregated to account level
    df_fundportf_acc_details <- psqlQuery("
                                          SELECT
                                              fit.account_name \"Account\",
                                              fit.currency \"Currency\",
                                              SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fit.tr_value) 
                                                    ELSE ROUND(CAST(fit.tr_value AS DECIMAL),2) END) ||' '|| fit.currency  \"Invested Amount\",
                                              SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fpr.price*fit.share_amount)
                                                    ELSE ROUND(CAST(fpr.price*fit.share_amount AS DECIMAL),2) END) ||' '|| fit.currency \"Market Value\",
                                              SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fpr.price*fit.share_amount)-ROUND(fit.tr_value) 
                                                    ELSE ROUND(CAST(fpr.price*fit.share_amount AS DECIMAL),2)-ROUND(CAST(fit.tr_value AS DECIMAL),2) END) ||' '|| fit.currency \"Profit / Loss\"
                                          FROM 
                                            app.fund_investment_transaction_vw fit
                                            LEFT OUTER JOIN app.fund_price_recent_vw fpr ON fit.fund_id=fpr.fund_id
                                          GROUP BY
                                              fit.account_id,
                                              fit.account_name,
                                              fit.currency
                                          ORDER BY
                                              fit.account_name
                                         ")$result

    # Retrieve performance details aggregated to portfolio level
    df_fundportf_pf_details <- psqlQuery("
                                        SELECT
                                        	p.name \"Portfolio Name\",
                                        	fit.currency \"Currency\",
                                        	SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fit.tr_value) 
                                        		ELSE ROUND(CAST(fit.tr_value AS DECIMAL),2) END) ||' '|| fit.currency \"Invested Amount\",
                                        	SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fpr.price*fit.share_amount)
                                        		ELSE ROUND(CAST(fpr.price*fit.share_amount AS DECIMAL),2) END) ||' '|| fit.currency \"Market Value\",
                                        	SUM(CASE WHEN fit.currency='HUF' THEN ROUND(fpr.price*fit.share_amount)-ROUND(fit.tr_value) 
                                        		ELSE ROUND(CAST(fpr.price*fit.share_amount AS DECIMAL),2)-ROUND(CAST(fit.tr_value AS DECIMAL),2) END) ||' '|| fit.currency \"Profit / Loss\"
                                        FROM 
                                        	app.fund_investment_transaction_vw fit
                                        	INNER JOIN app.portfolio_x_account_rltnp pxa ON fit.account_id=pxa.account_id
                                        	INNER JOIN app.portfolio p ON pxa.portfolio_id=p.id
                                        	LEFT OUTER JOIN app.fund_price_recent_vw fpr ON fit.fund_id=fpr.fund_id
                                        GROUP BY
                                        	p.id,
                                        	p.name,
                                        	fit.currency
                                        ORDER BY p.id;
                                         ")$result
    df_fundportf_pf_details <- df_fundportf_pf_details %>% 
                                left_join(dt_recent_pf_performance, by=c("Portfolio Name"="portfolio")) %>%
                                mutate("Annualized Yield"=ann_yield_pct/100) %>%
                                select(-c(date,ann_yield_pct))

    # Retrieve data for portfolio pie charts
    df_piechart_portfolio <- psqlQuery("SELECT id AS portfolio_id, name AS portfolio_name, fund_name, val
                                        FROM
                                        (
                                        SELECT p.id, p.name, fit.fund_name,
                                            COUNT(p.id) OVER (PARTITION BY p.id) cnt,
                                            SUM(fit.share_amount*fp.price) val
                                        FROM 
                                           app.fund_investment_transaction_vw fit,
                                           app.fund_price_recent_vw fp,
                                           app.portfolio_x_account_rltnp pxa,
                                           app.portfolio p
                                        WHERE 1=1
                                           AND fit.fund_id=fp.fund_id
                                           AND fit.account_id=pxa.account_id
                                           AND pxa.portfolio_id=p.id
                                        GROUP BY p.id, p.name, fit.fund_name
                                        ) t
                                       WHERE cnt>1
                                       ORDER BY t.id")$result

    # Retrieve key performance figures
    val_key_figures <- psqlQuery("
                                SELECT
									SUM(ROUND(CASE WHEN t1.currency='HUF' THEN ROUND(t1.tr_value) ELSE ROUND((t1.tr_value)::NUMERIC,2) END*COALESCE(rv.value,1))) total_init_investment,
                                	SUM(ROUND(CASE WHEN t1.currency='HUF' THEN ROUND(t1.share_amount*t1.price) ELSE ROUND((t1.share_amount*t1.price)::NUMERIC,2) END*COALESCE(rv.value,1))) total_market_value,
                                	SUM(ROUND(CASE WHEN t1.currency='HUF' THEN ROUND(t1.share_amount*t1.price) ELSE ROUND((t1.share_amount*t1.price)::NUMERIC,2) END*COALESCE(rv.value,1))-
									ROUND(CASE WHEN t1.currency='HUF' THEN ROUND(t1.tr_value) ELSE ROUND((t1.tr_value)::NUMERIC,2) END*COALESCE(rv.value,1))) total_yield
                                FROM
                                	(
                                	SELECT 
                                		fit.currency, fit.tr_value, fit.share_amount, fp.price, fp.value_date last_fund_price_date, rfx.id fxrate_id
                                	FROM 
                                		app.fund_investment_transaction_vw fit
                                		LEFT OUTER JOIN app.fund_price_recent_vw fp ON fit.fund_id=fp.fund_id
                                		LEFT OUTER JOIN app.rate_fx rfx ON fit.currency_id=rfx.from_currency_id AND rfx.to_currency_id = (SELECT id FROM app.currency WHERE iso_code='HUF')
                                	) t1 LEFT OUTER JOIN app.rate_value rv ON t1.fxrate_id=rv.rate_id AND t1.last_fund_price_date=rv.value_date;
                                 ")$result

    val_total_fund_inv <- val_key_figures[1,"total_init_investment"]
    val_total_mark_val <- val_key_figures[1,"total_market_value"]
    val_total_gross_yield <- val_key_figures[1,"total_yield"]


    
    df_portf_desc <- psqlQuery("SELECT 
                               name \"Fund Portfolio\", 
                               description \"Portfolio Description\", 
                               last_valuation_date \"Valuation Date\" 
                               FROM app.portfolio
                               ORDER BY name")$result
    df_portf_desc$`Valuation Date` <- format(df_portf_desc$`Valuation Date`,'%Y-%m-%d')
    
    list(out_details=df_fundportf_details,
         out_acc_details=df_fundportf_acc_details,
         out_pf_details=df_fundportf_pf_details,
         
         out_piechart_portf=df_piechart_portfolio,
         
         out_tot_fund_inv=val_total_fund_inv,
         out_tot_mark_val=val_total_mark_val,
         out_tot_gross_yield=val_total_gross_yield,
         
         out_portf_perf_ts=df_portfolio_performance_timeseries,
         out_portf_perf_annreturn_ts=pf_tmp_res,
         out_portf_desc=df_portf_desc)
})

### Infoboxes
output$funds_ov_tot_fund_inv <- renderText(format(q_funds_ov_portf_statem_reviewtbl()$out_tot_fund_inv, big.mark=" "))
output$funds_ov_tot_gross_yield <- renderText(format(q_funds_ov_portf_statem_reviewtbl()$out_tot_gross_yield, big.mark=" "))

### Piecharts
lapply(isolate(unique(q_funds_ov_portf_statem_reviewtbl()$out_piechart_portf[,1])), function(i) {
    df <- isolate(q_funds_ov_portf_statem_reviewtbl()$out_piechart_portf)
    data <- df[df$portfolio_id==i,]
    output[[paste0("dyn_plot_portfolio_piechart_",i)]] <- renderPlotly(
        plot_ly() %>% 
            add_pie(data=data, labels = ~fund_name, values = ~val, showlegend = F,
                    textposition = 'inside',
                    textinfo = 'percent',
                    insidetextfont = list(color = '#FFFFFF'),
                    marker = list(line = list(color = '#FFFFFF', width = 1))
                    ) %>% 
            layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                   yaxis = list(showgrid = F, zeroline = F, showticklabels = F)) %>%
            layout(title = unique(data$portfolio_name),
                   font=list(size = 9))
    )
    output[[paste0("dyn_ui_plot_portfolio_piechart_",i)]] <- renderUI({
        column(3,
               plotlyOutput(paste0("dyn_plot_portfolio_piechart_",i)))
    })
})
output$dyn_plot_ui_block_portfolio_piecharts <- renderUI({
    box(title = "Portfolio Distribution",
        solidHeader = T,
        width = 12, 
        status="primary",
        lapply(isolate(unique(q_funds_ov_portf_statem_reviewtbl()$out_piechart_portf[,1])), function(i) {
            uiOutput(paste0('dyn_ui_plot_portfolio_piechart_', i))
        })
    )
})

### Tables
output$funds_ov_portf_statem_pf_reviewtbl <- DT::renderDataTable(DT::datatable(q_funds_ov_portf_statem_reviewtbl()$out_pf_details, 
                                                                                options = list(searching=F, paging=F, scrollX = T),
                                                                                rownames=F) %>% formatPercentage(c("Annualized Yield"),2)
                                                                 )
output$funds_ov_portf_statem_acc_reviewtbl <- DT::renderDataTable(q_funds_ov_portf_statem_reviewtbl()$out_acc_details,
                                                                  options = list(dom = 't', ordering=F, searching=F, paging=F, scrollX = T),
                                                                  rownames=F)
output$funds_ov_portf_statem_reviewtbl <- DT::renderDataTable(DT::datatable(q_funds_ov_portf_statem_reviewtbl()$out_details, 
                                                                            options = list(searching=T, paging=T, scrollX = T, columnDefs = list(list(width = '50px', targets = c(3,6)))),
                                                                            rownames=F) %>% formatPercentage(c("Yield","Annualized Yield"),2)
                                                              )

lapply(isolate(unique(q_funds_ov_portf_statem_reviewtbl()$out_portf_perf_ts[,4])), function(i) {
    df <- isolate(q_funds_ov_portf_statem_reviewtbl()$out_portf_perf_ts)
    data <- df[df$currency==i,]
    output[[paste0("dyn_plot_portfolio_cumyield_ts_",i)]] <- renderPlotly(
        plot_ly() %>% 
            add_trace(data=data, x=~date, y=~accumulated_yield, type='scatter', mode='lines', color=~portfolio, showlegend=F) %>%
            layout(title = paste0("Cumulative Yield on ",unique(data$currency)," denominated portfolios"),
                   xaxis = list(showgrid=F, title=""),
                   yaxis = list(showgrid=F, title=unique(data$currency))
            )
    )
    output[[paste0("dyn_ui_plot_portfolio_cumyield_ts_",i)]] <- renderUI({
               plotlyOutput(paste0("dyn_plot_portfolio_cumyield_ts_",i))
    })
})
output$dyn_plot_ui_block_portfolio_accum_yield_ts <- renderUI({
        lapply(isolate(unique(q_funds_ov_portf_statem_reviewtbl()$out_portf_perf_ts[,4])), function(i) {
            column(width=6,
                   br(),
                   uiOutput(paste0('dyn_ui_plot_portfolio_cumyield_ts_', i))
                   )
        })
})

output$funds_ov_portf_perf_ts_yieldpct <- renderPlotly(plot_ly() %>% 
                                                           add_trace(data=q_funds_ov_portf_statem_reviewtbl()$out_portf_perf_annreturn_ts, x=~date, y=~ann_yield_pct, type='scatter', mode='lines', color=~portfolio, showlegend = T) %>%
                                                           layout(title = "Change of Annualized Rate of Return (%)",
                                                                  xaxis = list(showgrid=F, title=""),
                                                                  yaxis = list(showgrid=T, title="%")
                                                           )
                                                       )

output$portf_desc <- DT::renderDataTable(q_funds_ov_portf_statem_reviewtbl()$out_portf_desc,
                                         options = list(dom = 't', ordering=F, columnDefs = list(list(width = '95px', targets = 2))),
                                         rownames=F)


# SERV: Funds > Fund Prices -----------------------------------------------

q_fundprices_df <- reactive({
    input$adm_invtr_add_btn
    fundprice_df <- psqlQuery("SELECT 
                              t.id, 
                              t.fund_name, 
                              t.value_date, 
                              t.price, 
                              t.change_pct, 
                              CASE WHEN t.purchase IS NOT NULL THEN change_pct END purchase,
                              CASE WHEN t.purchase IS NOT NULL THEN price END purchase_price
                              FROM
                              (
                              SELECT f.id, f.fund_name, fp.value_date, fp.price,
                              (fp.price/first_value(price) OVER (PARTITION BY f.id ORDER BY fp.value_date)-1)*100 change_pct, fit.value_date purchase
                              FROM app.fund_price fp 
                              INNER JOIN app.fund f ON fp.fund_id=f.id
                              INNER JOIN (SELECT fund_id, MIN(value_date) value_date FROM app.fund_investment_transaction GROUP BY fund_id) tmp ON fp.fund_id=tmp.fund_id AND fp.value_date>=tmp.value_date
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
        plot_ly() %>% add_trace(data=data, x=~value_date, y=~price, mode="lines") %>%
            add_trace(data=data, x=~value_date, y=~purchase_price, name="Investments", mode="markers") %>%
            layout(showlegend = F, xaxis=list(title=""), yaxis=list(title=""))
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
        plot_ly() %>%
            add_trace(data=fxrates_df, 
                      x=~value_date,
                      y=~value, 
                      color=~rate_name,
                      mode="lines",
                      name="CHF/HUF") %>%
            layout(title = "Historical Exchange Rates For CHF to HUF", xaxis=list(title=""), yaxis=list(title="HUF"))
    )
}

# SERV: Accounts ----------------------------------

q_acc_bal_dev <-    psqlQuery("
                            SELECT
                            	rep.reporting_period,
                            	t.account_id,
                            	t.account_name,
                            	t.currency,
                            	SUM(t.amount) balance
                            FROM
                            	(
                            	SELECT 
                            		(date_trunc('month',eom) + INTERVAL '1 MONTH - 1 day')::date reporting_period
                            	FROM generate_series ((SELECT MIN(value_date) FROM app.transaction), date_trunc('month',now())::date-1, '1 month'::interval) eom
                            	) rep
                            	LEFT OUTER JOIN 
                            	(
                            	SELECT
                            		t.account_id,
                            		a.account_name,
                            		c.iso_code currency,
                            		t.amount,
                            		t.value_date
                            	FROM
                            		app.transaction t,
                            		app.account a,
                            		app.currency c
                            	WHERE
                            		t.account_id=a.id
                            		AND a.currency_id=c.id
                            	) t ON rep.reporting_period>=t.value_date
                            GROUP BY
                            	rep.reporting_period,
                            	t.account_id,
                            	t.account_name,
                            	t.currency
                            ORDER BY 
                            	rep.reporting_period,
                            	t.account_id
                              ")$result

# SERV: Accounts > Analytics ----------------------------------

output$plot_brchrt_bal_development <- renderPlotly(
        plot_ly() %>%
        add_trace(data=q_acc_bal_dev, 
                  x=~reporting_period,
                  y=~balance, 
                  color=~account_name,
                  type = 'bar'
                  ) %>%
        layout(title = "Balance History of Swiss Accounts", 
               barmode = 'stack',
               xaxis=list(title=""), yaxis=list(title=unique(q_acc_bal_dev$currency)))
)

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
                                                options = list(searching=F, paging=F, scrollX = T),
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
                                                options = list(searching=F, paging=F, scrollX = T),
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
                     "InceptionDate"=character(),
                     "Description"=character(),
                     stringsAsFactors = F),
          psqlQuery("SELECT id \"FundID\", fund_name \"FundName\", isin \"ISIN\", currency_id \"CurrencyID\", source_id \"SourceID\", 
                    to_char(last_modification,'YYYY-MM-DD HH24:MI:SS') \"LastModification\", to_char(inception_date,'YYYY-MM-DD') \"InceptionDate\", 
                    description \"Description\" FROM app.fund ORDER BY id")$result
    )
})
output$adm_fund_reviewtbl <- DT::renderDataTable(q_adm_fund_reviewtbl(), options=list(searching=F, paging=F, scrollX = T), 
                                                 rownames=F)

observeEvent(input$adm_fund_add_btn,{
    adm_fund_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.fund (fund_name, isin, currency_id, source_id, inception_date, description) 
                                                   VALUES ('%s','%s',%i,'%s','%s','%s')",
                                                   input$adm_fund_add_name,
                                                   input$adm_fund_add_isin,
                                                   as.integer(input$adm_fund_add_ccy),
                                                   input$adm_fund_add_srcid,
                                                   input$adm_fund_add_inceptiondate,
                                                   input$adm_fund_add_desc)
                                        )
    shinyjs::reset("adm_fund_add_name")
    shinyjs::reset("adm_fund_add_isin")
    shinyjs::reset("adm_fund_add_ccy")
    shinyjs::reset("adm_fund_add_srcid")
    shinyjs::reset("adm_fund_add_inceptiondate")
    shinyjs::reset("adm_fund_add_desc")
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
                     ORDER BY last_modification DESC, value_date DESC, fund_id ASC")$result
    )
})
output$adm_fundprices_reviewtbl <- DT::renderDataTable(q_adm_fundprices_reviewtbl(),
                                                       options = list(pageLength = 10, scrollX = T),
                                                       rownames=F)

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
                                                options = list(searching=F, paging=F, scrollX = T),
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
output$adm_fxrates_reviewtbl <- DT::renderDataTable(q_adm_fxrates_reviewtbl,options=list(scrollX = T), rownames=F)

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
                                                options = list(searching=F, paging=F, scrollX = T),
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
                     "ShareAmount"=double(),
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
                                                  options = list(searching=F, paging=F, scrollX = T),
                                                  rownames=F)

observeEvent(input$adm_invtr_add_btn,{
    adm_invtr_add_btn_queryOut <- psqlQuery(sprintf("INSERT INTO app.fund_investment_transaction 
                                                      (value_date,share_amount,account_id,fund_id)
                                                      VALUES('%s', %f, %i, %i);"
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
                    menuItem("Accounts", tabName = "accounts", icon = icon("bank"),
                        menuSubItem("Analytics", tabName = "account_analytics")
                     ),
                    menuItem("Macroeconomic Factors", tabName = NULL, icon = icon("globe"),
                              menuSubItem("FX Rates", tabName = "macro_fx")
                              # menuSubItem("Interest Rates", tabName = "macro_ir")
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
        h2("Funds", tags$small("Investment Overview")),
        fluidRow(
            column(width = 4,
                   valueBox(value = textOutput("funds_ov_tot_fund_inv"), 
                            "Total Fund Investment To Date (HUF)", 
                            icon = icon("money"),
                            width = NULL,
                            color = "light-blue")
                   ),
            column(width = 4,
                   valueBox(value = textOutput("funds_ov_tot_gross_yield"),
                            "Total Gross Yield (HUF)",
                            icon = icon("bar-chart"),
                            width = NULL,
                            color = "green")
                
            )
        ),
        fluidRow(
            column(width=12,
                   box(width=NULL,
                       status="primary",
                       dataTableOutput("portf_desc")
                       )
                   )
                 ),
        fluidRow(uiOutput("dyn_plot_ui_block_portfolio_piecharts")),
        fluidRow(
            box(title="Portfolio Performance History",
                solidHeader=T,
                width=12,
                status="primary",
                uiOutput("dyn_plot_ui_block_portfolio_accum_yield_ts"),
                column(width=12,
                         br(),br(),
                         plotlyOutput("funds_ov_portf_perf_ts_yieldpct")
                         )
                  )
                 ),
        fluidRow(
            box(title="Detailed Portfolio Statements",
                solidHeader = T,
                DT::dataTableOutput("funds_ov_portf_statem_pf_reviewtbl"),
                br(),br(),
                DT::dataTableOutput("funds_ov_portf_statem_acc_reviewtbl"),
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

# tabItem(tabName = "macro_ir",
#         h2("Macroeconomic Factors", tags$small("Interest Rates")),
#         fluidRow(
#             box(title = "Recent Yield Curves",
#                 solidHeader = T,
#                 plotlyOutput("plot_yieldcurve"),
#                 width = 12,
#                 status = "primary"
#             )
#         ),
#         fluidRow(
#             box(title = "Market Interest Rates",
#                 solidHeader = T,
#                 plotlyOutput("plot_intrates"),
#                 width = 12,
#                 status = "primary"
#             )
#         )
# ),

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
                    )
        ),
        fluidRow(
            column(width = 12,
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
                    dateInput("adm_fund_add_inceptiondate","Inception Date",format="yyyy-mm-dd"),
                    textInput("adm_fund_add_desc","Description"),
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
                p("Note: Rates are being imported automatically on a daily basis.")
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


