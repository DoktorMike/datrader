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
mylist <- filterUnwantedInstruments(mylist, cutoff = 0.05, lastDate = Sys.Date()-10)
mylist <- imputeInstruments(mylist)
naInstruments(mylist)

priceListToDf <- function(pricelist, f=quantmod::Cl) {
  extractSingle <- function(x, i, f) {
    f(x[[i]]) %>%
      as.data.frame() %>%
      tibble::rownames_to_column('date') %>%
      tibble::as_data_frame() %>%
      dplyr::mutate(date=as.Date(date)) %>%
      purrr::set_names(c('date', names(x)[i]))
  }
  retdf <- extractSingle(pricelist, 1, f)
  for(i in 2:length(pricelist))
    retdf <- dplyr::full_join(retdf, extractSingle(pricelist, i, f))
  retdf %>% arrange(date)
}

cldf <- priceListToDf(mylist, quantmod::Cl)
normalize <- function(x) (x-base::mean(x, na.rm=TRUE))/stats::sd(x, na.rm = TRUE)
clnormdf <- data.frame(cldf[,1], sapply(cldf[,-1], normalize)) %>% as_tibble()
cormat <- cor(cldf[,-1], use = 'comp')
tail(cldf[,3:120], 200) %>% prcomp(center=T, scale. = T) %>% summary

cldf2 <- priceListToDf(mylist, quantmod::Vo)
normalize <- function(x) (x-base::mean(x, na.rm=TRUE))/stats::sd(x, na.rm = TRUE)
clnormdf <- data.frame(cldf[,1], sapply(cldf[,-1], normalize)) %>% as_tibble()
cormat <- cor(cldf[,-1], use = 'comp')
tail(cldf[,3:120], 200) %>% prcomp(center=T, scale. = T) %>% summary
