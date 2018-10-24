library(tidyverse)
library(datrader)
library(datools)
library(quantmod)
library(ggplot2)
library(xts)
library(TTR)

mypath <- '/home/michael/Dropbox/Development/trading'
mypath <- "~/Dropbox/Datasets/matrix/finance/data/sp500/csv"
mylist <- loadExistingInstruments(mypath)
mylist <- filterUnwantedInstruments(mylist, lastDate = as.Date("2018-10-17"))
mylist <- imputeInstruments(mylist)

# Define a function for ranking an instrument and selecting it for investment
rankInstrument <- function(x) tail(ROC(Cl(x), n=min(90, nrow(x))), 1)[[1]]
myvol <- function(x) tail(volatility(x, n = min(90, nrow(x)), calc = 'garman.klass'), 1)[[1]]
selectInstrument <- function(x) rankInstrument(x)/myvol(x) > 0.05

# Create a share of investment based on our functions above
mystrat <- function(x) createPortfolio(x, selectInstrument, rankInstrument, 50)

# Evaluate this strategy
mydates <- index(tail(mylist[[1]], 365*2))
res <- evaluateStrategy(mylist, mydates, mystrat, 30, 10000)


# Run a few strategies ----------------------------------------------------

resultdf <- expand.grid(Horizon=c(10, 30, 60, 90),
                        TopN=c(3, 10, 30, 50),
                        TradeFreq=c(10, 30, 60, 90),
                        Cutoff=c(0.01, 0.05, 0.1, 0.2)) %>%
  as.tibble() %>% mutate(Investment=10000, Value=0, Cash=0, Totalworth=0) %>%
  mutate(FromDate=as.Date("1992-09-09"), ToDate=as.Date("2018-10-17"))
  # mutate(FromDate=as.Date("2015-11-24"), ToDate=as.Date("2018-10-17"))
for(i in 1:nrow(resultdf)){
  print(paste0("Simulation ", i, " of ", nrow(resultdf)))
  rankInstrument <- function(x) tail(ROC(Cl(x), n=min(resultdf$Horizon[i], nrow(x))), 1)[[1]]
  myvol <- function(x) tail(volatility(x, n = min(resultdf$Horizon[i], nrow(x)), calc = 'garman.klass'), 1)[[1]]
  selectInstrument <- function(x) rankInstrument(x)/myvol(x) > resultdf$Cutoff[i]
  mystrat <- function(x) createPortfolio(x, selectInstrument, rankInstrument, resultdf$TopN[i])
  mydates <- seq(resultdf$FromDate[i], resultdf$ToDate[i], by="1 day")
  # browser()
  res <- evaluateStrategy(mylist, mydates, mystrat, resultdf$TradeFreq[i], resultdf$Investment[i])
  resultdf$Value[i]<-res$Value
  resultdf$Cash[i]<-res$Cash
  resultdf$Totalworth[i]<-res$Value+res$Cash
}
