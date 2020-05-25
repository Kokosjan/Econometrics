library("lubridate") 
library("sandwich") 
library("lmtest") 
library("car") 
library("zoo")
library("xts")
library("dplyr") 
library("broom") 
library("ggplot2") 

library("quantmod") 
library("Quandl")

# load data from finance.google.com
Sys.setlocale("LC_TIME","C") 
# Apple stock quotes
getSymbols(Symbols = "AAPL",from="2010-01-01",
           to="2015-02-03",src="google")
head(AAPL)
tail(AAPL)
plot(AAPL)
autoplot(AAPL[,1:4])
autoplot(AAPL[,1:4],facets = NULL)
chartSeries(AAPL)



## autocorrelation
data("Investment") 
help("Investment")
start(Investment) 
end(Investment) 
time(Investment)
coredata(Investment)
d <- as.zoo(Investment)
autoplot(d[,1:2],facets = NULL)

# regression
model <- lm(data=d, RealInv~RealInt+RealGNP)
summary(model) 
vcovHAC(model) # robust variance with autocorrelation
coeftest(model,vcov. = vcovHAC(model))

# robust confidence intervals
conftable <- coeftest(model,vcov. = vcovHAC(model))
ci <- data.frame(estimate=conftable[,1],
                 se_ac=conftable[,2])
ci <- mutate(ci,left_95=estimate-1.96*se_ac,
             right_95=estimate+1.96*se_ac)
ci

# Durbin-Watson test
dwt(model)
res <- dwt(model)
res

# Breusch-Godfrey test
bgtest(model,order = 2)
res <- bgtest(model,order = 2)
res$statistic 
res$p.value 
