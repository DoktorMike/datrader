library(datrader)

mypath<-'/home/michael/Dropbox/Development/trading'

omxs30Symbols<-c('ABB.ST', 'ALFA.ST', 'ASSA-B.ST', 'AZN.ST', 'ATCO-A.ST',
                 'ATCO-B.ST', 'BOL.ST', 'ELUX-B.ST', 'ERIC-B.ST', 'GETI-B.ST', 'HM-B.ST',
                 'INVE-B.ST', 'LUPE.ST', 'MTG-B.ST', 'NDA-SEK.ST', 'SAND.ST',
                 'SCA-B.ST', 'SEB-A.ST', 'SECU-B.ST', 'SKA-B.ST', 'SKF-B.ST',
                 'SSAB-A.ST', 'SHB-A.ST', 'SWED-A.ST', 'SWMA.ST', 'TEL2-B.ST', 'TELIA.ST', 'VOLV-B.ST')
nasdaqSymbols<-c('MSFT', 'IBM', 'FB', 'GOOG', 'AMZN', 'NFLX',
                 'NVDA', 'AMD', 'INTC',
                 'TSLA', 'VLKAY', 'F')

# Should only run once!
downloadInstruments(omxs30Symbols, mypath, '2000-01-01')
downloadInstruments(nasdaqSymbols, mypath, '2000-01-01')
# updateInstruments(mypath)
# updateInstruments(mypath, startDate = Sys.Date() - 90)

