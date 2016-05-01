library(datrader)
library(quantmod)
library(ggplot2)

mypath<-'/home/michael/Dropbox/Development/trading'

mylist<- loadExistingInstruments(mypath)

lapply(names(mylist), function(x) {a<-readline(paste0("Press enter to see: ", x, " and q to stop: ")); if(a=="q") stop("Bye") else chartSeries(mylist[[x]])})

a<-getSymbols("VOW.DE", auto.assign = F)
a<-mylist[[2]]
b<-adjustOHLC(a)
