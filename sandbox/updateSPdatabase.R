library(tidyverse)
library(datrader)
library(datools)
library(quantmod)
library(ggplot2)
library(xts)
library(TTR)

source('sandbox/functions.R')

mypath <- "~/Dropbox/Datasets/matrix/finance/data/sp500/csv"
updateInstruments(mypath, startDate = findLastDateInInstruments(mypath))
mylist <- loadExistingInstruments(mypath)
mylist <- filterUnwantedStocks(mylist)
mylist <- imputeInstruments(mylist)
naInstruments(mylist)
