library(datrader)

mypath<-'/home/michael/Dropbox/Development/trading'

mylist<- loadExistingInstruments(mypath)

lapply(names(mylist), function(x) {readline(paste0("Press enter to see: ", x)); chartSeries(mylist[[x]])})
