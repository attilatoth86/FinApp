## Load functions
source("f.R")

## Load gdata package for Excel data acquisition
library(gdata)

# Load in Excel file
options(encoding = "iso-8859-2")
df = read.xls("/Users/ati/Downloads/alapok_export_20160918100953.xls", 
              sheet = 1, 
              stringsAsFactors=F)

# Define dimensions of loaded data frame
df_length <- dim(df)[1]
df_width <- dim(df)[2]

# Define number of funds: each fund occupies 3 columns
n_funds <- dim(df)[2]/3

# Create an empty data form for collecting price information
price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())

# Obtain dates from the data frame
date <- df[2:df_length,1]

# Fetch price information from data frame
for (i in 1:n_funds){
    # Obtain fund name // remove punctuation in the string
    fund_name <- gsub("\\."," ",names(df)[seq(from = 1, to = df_width, by = 3)][i])
    # Get corresponding fund id from database
    fund_id <- dbquery(sprintf("SELECT id as fund_id FROM finapp.fund WHERE fund_name='%s'", fund_name))
    # Exception handling: query for fund id must return value
    if (nrow(fund_id)==0){
        # Data frame clearance
        price_tbl_pre <- data.frame(date=character(),fund_id=character(),fund_name=character(),price=character())
        # Throw error
        stop(sprintf("Process has been terminated. Unknown fund: %s. Please add first to the fund repository.", fund_name))
    }
    # Obtain fund prices
    price <- df[2:df_length,i*3]
    # Load data into data frame
    price_tbl_pre <- rbind(price_tbl_pre,
                           cbind(date,fund_id,fund_name,price)
                           )
}

# Loading the content of data frame to database
dbinsert(price_tbl_pre, "ld_fund_price")

# Move from loading area
dbquery("INSERT INTO fund_prices (fund_id,value_date,price)
         SELECT
            CAST(ld.fund_id AS UNSIGNED)
            ,CAST(ld.date AS DATE)
            ,CAST(ld.price AS DECIMAL(10,6))
         FROM ld_fund_price ld LEFT OUTER JOIN fund_prices t 
         ON ld.date = t.value_date AND
		    ld.fund_id = t.fund_id
         WHERE t.id IS NULL")
