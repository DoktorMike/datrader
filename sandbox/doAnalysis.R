library(tidyverse)
library(datrader)
library(datools)
library(quantmod)
library(ggplot2)
library(xts)
library(TTR)

source('sandbox/functions.R')

mypath <- '/home/michael/Dropbox/Development/trading'
mypath <- "~/Dropbox/Datasets/matrix/finance/data/sp500/csv"
mylist <- loadExistingInstruments(mypath)
mylist <- filterUnwantedInstruments(mylist, lastDate = as.Date("2018-09-01"))
mylist <- imputeInstruments(mylist)


chartSeries(mylist$MSFT, TA="addMomentum(n=90);addVolatility(n=90)")
chartSeries(mylist$MSFT, TA="addMomentum(n=90);addVolatility(n=90);addVo()")


# Random 10 stock full period
eval30allyearsdf <- evaluateStrategy(sample(mylist, 30),
                                     strategy = function(x) momentumStrategy(x, 90, 0.05),
                                     cost = 1)

# Random 10 stocks last year
tmplist <- lapply(mylist, function(x) tail(x, 365))
eval10lastyeardf <- evaluateStrategy(sample(tmplist, 10), strategy = momentumStrategy90d, cost = 1)

# Random 10 stocks last 2 years
tmplist <- lapply(mylist, function(x) tail(x, 2*365))
eval10last2yeardf <- evaluateStrategy(sample(tmplist, 10), strategy = momentumStrategy90d, cost = 1)

tmplist <- lapply(mylist, function(x) tail(x, 10*365))
eval10last10yeardf <- evaluateStrategy(sample(tmplist, 10), strategy = momentumStrategy180d, cost = 1)

evalallyearsdf <- evaluateStrategy(mylist,
                                   strategy = function(x) momentumStrategy(x, 30, 0.0),
                                   cost = 1)


lapply(mylist[c('MSFT', 'NFLX')], function(x) tail(x, 365)) %>%
  evaluateStrategy(strategy = function(x) momentumStrategy(x, 90, 0.05), cost = 10)

tmpinst <- tail(mylist$MSFT, 365); plotHistoricalPositions(tmpinst, generateHistoricalPositions(tmpinst, function(x) momentumStrategy(x, 90, 0.05)))

createDebugDf <- function(x, tstrat, signal){
  tibble(Date=zoo::index(x), Price=data.frame(quantmod::Cl(x))[,1],
         Position=datrader::generateHistoricalPositions(x, tstrat),
         Signal=signal(x))
}

createDebugDf(tail(mylist$NFLX, 365),
              function(x) momentumStrategy(x, 90, 0.05),
              function(x) data.frame(ROC(Cl(x), n = 90)/volatility(x, n = 90))[,1])

# Error checking:
a <- tail(mylist$MSFT, 5)
b <- tibble(Date=index(a), Price=as.vector(Cl(a)), Return=as.vector(dailyReturn(Cl(a), leading = F)), MultRet=Return+1)
