
# Initialization: libraries -----------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyjs)

# Initialization: sourcing external code ---------------------------------------

source("f.R")

# Initialization: misc setup ----------------------------------------------

options(scipen = 999)

# Initialization: variables -----------------------------------------------

fund_inv_ov_periodstart <- psqlQuery("SELECT TO_CHAR(MIN(value_date),'DD Mon YY') 
                                      FROM fund_investment_transaction")$result
fund_inv_ov_periodend <- psqlQuery("SELECT MAX(value_date) FROM fund_price")$result
fund_ids <- psqlQuery("SELECT DISTINCT fund_id 
                       FROM fund_investment_transaction 
                       ORDER BY fund_id ASC")$result

# Initialization: dropdown lists ------------------------------------------

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

# Start of shinyUI() ------------------------------------------------------

shinyUI(

# Start of dashboardPage() ------------------------------------------------

    dashboardPage(

# dashboardHeader() -------------------------------------------------------

        dashboardHeader(title="FinApp", titleWidth = "270px"),

# dashboardSidebar() ------------------------------------------------------

        dashboardSidebar(width = "270px",
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Funds",tabName = NULL, icon = icon("line-chart"),
                             menuSubItem("Overview", tabName = "funds_ov"),
                             menuSubItem("Fund Prices", tabName = "fundprices")
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

# start of dashboardBody() ------------------------------------------------

        dashboardBody(

# start of tabItems() -----------------------------------------------------

            tabItems(

# Page: Home --------------------------------------------------------------

                tabItem(tabName = "home",
                        h2("Home tab content")
                ),

# Page: Fund Investment Overview ------------------------------------------

                tabItem(tabName = "funds_ov",
                        h2("Fund Investment Overview"),
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

# Page: Fund Prices -------------------------------------------------------

                tabItem(tabName = "fundprices",
                        h2("Fund Prices"),
                        fluidRow(
                            box(plotOutput("plot_fp_relative"),
                                title="Relative Price Changes",
                                width = 12, 
                                status = "primary",
                                solidHeader = T)
                                 ),
                        fluidRow(
                            lapply(fund_ids[,1], function(i) {
                                uiOutput(paste0('dyn_plot_', i))
                                })
                        )
                ),

# Page: Accounts ----------------------------------------------------------

                tabItem(tabName = "accounts",
                        h2("Accounts tab content")
                ),

# Page: Macro > FX Rates --------------------------------------------------

                tabItem(tabName = "macro_fx",
                        h2("FX Rate tab content")
                ),

# Page: Macro > Interest Rates --------------------------------------------

                tabItem(tabName = "macro_ir",
                        h2("Interest Rates tab content")
                ),

# Page: Manage Accounts ---------------------------------------------------

                tabItem(tabName = "adm_acc",
                        shinyjs::useShinyjs(),
                        fluidRow(
                            box(title = "Add an account", 
                                solidHeader = T,
                                status = "danger",
                                width = 4,
                                textInput("adm_acc_add_name","Account Name"),
                                selectInput("adm_acc_add_taxappl","Tax Applicable",
                                            c("please select.."="","Yes"="Y","No"="N")),
                                textInput("adm_acc_add_taxpercent","Tax Rate %"),
                                actionButton("adm_acc_add_btn",
                                             "Add", 
                                             icon("plus")),
                                shinyjs::hidden(
                                    div(id = "adm_acc_add_confirm",
                                        p("Account has been added successfully.")
                                      )
                                    )
                                ),
                            box(DT::dataTableOutput("adm_acc_reviewtbl"),width = 8)
                        )
                ),

# Page: Manage Funds ------------------------------------------------------

                tabItem(tabName = "adm_fund",
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

# Page: Manage Currencies -------------------------------------------------

                tabItem(tabName = "adm_ccy",
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

# Page: Manage Deposits ---------------------------------------------------

                tabItem(tabName = "adm_depo",
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
                                                    icon("plus")),
                                       shinyjs::hidden(
                                           div(id = "adm_depo_add_confirm",
                                               p("Deposit has been added successfully.")
                                           )
                                       )
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
                        fluidRow(box(title="Review deposits",
                                     solidHeader = T,
                                     status = "primary",
                                     width = 12,
                                     DT::dataTableOutput("adm_depo_reviewtbl")))
                    
                ),

# Page: Manage Rates ------------------------------------------------------

                tabItem(tabName = "adm_rate",
                    h2("Administration Rate"),
                    fluidRow(box(title = "Add rate",
                                 width = 12,
                                 status = "danger",
                                 solidHeader = T,
                                 column(width=4,
                                        textInput("adm_rate_add_ratename",
                                                  "Rate Name")),
                                 column(width=4,
                                        selectInput("adm_rate_add_ratetype",
                                                    "Rate Type",
                                                    c("please select.."="",
                                                      "Interest Rate"="interest",
                                                      "FX Rate"="FX"))),
                                 column(width=4,
                                        actionButton("adm_rate_add_btn",
                                                     "Add rate",
                                                     icon("plus")),
                                        tags$style(type='text/css', 
                                                   "#adm_rate_add_btn {margin-top: 25px;}"))
                                ),
                             shinyjs::hidden(
                                 div(id="adm_rate_confirm",
                                     box(status = "danger",
                                         width = 12,
                                         solidHeader = T,
                                         textOutput("adm_rate_add_confirm")
                                     )    
                                 )
                                 
                             )
                    ),
                    fluidRow(box(title="Review rates",
                                 solidHeader = T,
                                 status = "primary",
                                 width=12,
                                 DT::dataTableOutput("adm_rate_reviewtbl")))
                ),

# Page: Manage Investment Transactions ------------------------------------

                tabItem(tabName = "adm_invtr",
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

# Page: Upload Fund Prices ------------------------------------------------

                tabItem(tabName = "adm_fundprices",
                        h3("Administration - Fund Prices"),
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

# Page: Manage FX rates ---------------------------------------------------

                tabItem(tabName = "adm_fxrates",
                        h3("Administration - FX Rates"),
                        fluidRow(
                            box(title="Import FX rates", width = 12,
                                solidHeader = T,
                                status = "danger",
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
                                                    "Import",
                                                    icon("database")
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

# end of tabitems() -------------------------------------------------------

            )

# end of dashboardBody() --------------------------------------------------

        )

# end of dashboardPage() --------------------------------------------------

)

# end of shinyUI() --------------------------------------------------------

)

# Others ------------------------------------------------------------------


