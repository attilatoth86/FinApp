df <- dbquery("SELECT
rp.reporting_period,
ROUND(SUM(it.shares_purchased*fp.price)) market_value,
ROUND(SUM(it.shares_purchased*fp.price-it.amount)) gain_loss,
ROUND(SUM(it.amount)) invested_amt
FROM
investment_transaction it,
monthly_rep_per_work rp,
fund_prices fp
WHERE 1=1
	AND it.value_date<=rp.date AND rp.date<=sysdate()
    AND rp.date=fp.value_date AND it.fund_id=fp.fund_id
GROUP BY
rp.reporting_period
ORDER BY rp.reporting_period")

ggplot(df, aes(x=reporting_period, y=market_value)) + geom_bar(stat="identity") + theme_bw()

dat <- t(as.matrix(df[,c(4,3)]))
colnames(dat) <- df$reporting_period

library(reshape2)

dat2 <- melt(dat, id.vars = rownames)

library(ggplot2)

ggplot(dat2, aes(x=value, y=value, fill=Var1)) + 
    geom_bar(stat="identity") +
    xlab("\nType") +
    ylab("Time\n") +
    guides(fill=FALSE) +
    theme_bw()

ggplot(df, aes(x = factor(reporting_period), y = market_value)) + geom_bar(stat = "identity")
