
library(shiny)
library(shinydashboard)
library(shinyjs)

source("f.R")

options(scipen = 999)

### Preliminary lists
acc_list <- dbquery("SELECT id FROM account")$id
names(acc_list) <- dbquery("SELECT account_name FROM account")$account_name

fund_list <- dbquery("SELECT id FROM fund")$id
names(fund_list) <- dbquery("SELECT fund_name FROM fund")$fund_name

ccy_list <- dbquery("SELECT id FROM currency")$id
names(ccy_list) <- dbquery("SELECT iso_code FROM currency")$iso_code

### Shiny UI
shinyUI(
    dashboardPage(
        dashboardHeader(title="FinApp", titleWidth = "270px"),
        dashboardSidebar(width = "270px",
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Funds",tabName = NULL, icon = icon("line-chart"),
                             menuSubItem("Overview", tabName = "funds_ov"),
                             menuSubItem("Fund Prices", tabName = "fundprices")
                             ),
                menuItem("Accounts", tabName = "accounts", icon = icon("bank")),
                menuItem("Administration", tabName = NULL, icon = icon("gear"),
                            menuSubItem("Manage accounts", tabName = "adm_acc"),
                            menuSubItem("Manage funds", tabName = "adm_fund"),
                            menuSubItem("Manage currencies", tabName = "adm_ccy"),
                            menuSubItem("Manage fund investment transactions", tabName = "adm_invtr"),
                            menuSubItem("Upload fund prices", tabName = "adm_fundprices")
                         )
            )
        ),
        dashboardBody(
            tabItems(
### Page: Home
                tabItem(tabName = "home",
                        h2("Home tab content")
                ),
################################################################################

### Page: Fund Investment Overview
                tabItem(tabName = "funds_ov",
                        h2("Fund Investment Overview"),
                        fluidRow(
                            valueBox(value = textOutput("funds_ov_tot_fund_inv"), 
                                     "Total Fund Investment (HUF)", 
                                     icon = icon("money"),
                                     color = "light-blue"),
                            valueBox(value = textOutput("fund_ov_tot_fund_yield"),
                                     "Total Net Yield (HUF)",
                                     icon = icon("bar-chart"),
                                     color="green"),
                            valueBox(value = textOutput("fund_ov_tot_fund_yield_pct"),
                                     "Annualized Net Yield (%)",
                                     icon = icon("percent"),
                                     color="yellow")
                        ),
                        fluidRow(
                            box(DT::dataTableOutput("fundtable"),width = 12, status="primary")
                            )
                ),
################################################################################

### Page: Fund Prices
                tabItem(tabName = "fundprices",
                        h2("Fund Prices"),
                        fluidRow(
                            lapply(1:3, function(i) {uiOutput(paste0('dyn_plot_', i))})
                        )
                ),
################################################################################

### Page: Accounts
                tabItem(tabName = "accounts",
                        h2("Accounts tab content")
                ),

################################################################################

### Page: Manage Accounts
                tabItem(tabName = "adm_acc",
                        shinyjs::useShinyjs(),
                        fluidRow(
                            box(title = "Add an account", 
                                solidHeader = T,
                                status = "danger",
                                width = 4,
                                textInput("adm_acc_add_name","Account Name"),
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

################################################################################

### Page: Manage Funds
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
                            box(DT::dataTableOutput("adm_fund_reviewtbl"),width = 8)
                        )
                ),

################################################################################

### Page: Manage Currencies
tabItem(tabName = "adm_ccy",
        fluidRow(
            box(title = "Add a currency", 
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
            box(DT::dataTableOutput("adm_ccy_reviewtbl"),width = 8)
        )
),

################################################################################

### Page: Manage Investment Transactions
                tabItem(tabName = "adm_invtr",
                        fluidRow(
                                box(title="Add fund investment transaction",
                                    solidHeader = T,
                                    status = "danger",
                                    width = 12,
                                    fluidRow(
                                        column(width=5,
                                               selectInput("adm_invtr_add_targ_acc","Target Account",
                                                           acc_list)),
                                        column(width=5,
                                               selectInput("adm_invtr_add_fund","Purchased Fund",
                                                           fund_list)),
                                        column(width=2,
                                               selectInput("adm_invtr_add_ccy","Currency",
                                                           ccy_list))   
                                    ),
                                    fluidRow(
                                        column(width=3,
                                               dateInput("adm_invtr_add_valdate","Value Date", format = "yyyy-mm-dd")),
                                        column(width=3,
                                               textInput("adm_invtr_add_tramt","Transaction Amount")),
                                        column(width=3,
                                               textInput("adm_invtr_add_noshares","No of Purchased Share")),
                                        column(width=3,
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
                                    tags$style(type='text/css', "#adm_invtr_add_btn { width:100%; 
                                                                                margin-top: 25px;")
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
################################################################################

### Page: Upload Fund Prices
                tabItem(tabName = "adm_fundprices",
                        h3("Administration - Fund Prices"),
                        fluidRow(
                            box(title="Fund Price Bulk Upload", 
                                solidHeader = T,
                                status="danger",
                                fileInput("adm_fundprices_upl_file_inp", "Choose File",
                                          accept = c(".xls",".xlsx")
                                ),
                                tableOutput("adm_fundprices_upl_file_desc"),
                                actionButton("adm_fundprices_upl_btn","Import", icon("database")),
                                textOutput("fp_upl_error"),
                                width = 8
                               )
                        ),
                        fluidRow(
                            box(DT::dataTableOutput("adm_fundprices_reviewtbl"),width = 12)
                        )
                )
################################################################################

            )
        )
    )
    
)

