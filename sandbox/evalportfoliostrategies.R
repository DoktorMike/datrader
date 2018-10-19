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
