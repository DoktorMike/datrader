#' Type for Investment strategies
#'
#' @param invest a function producing a vector of buying decisions.
#' @param horizon the horizon to use
#' See details for more.
#'
#' @return a Strategy instantiated with investment stratey invest.
#' @export
#'
#' @examples
#' a<-1
Strategy <- function(invest, horizon=30){
  ret <- list(Invest=invest)
  class(ret) <- c("Strategy", "list")
  ret
}

#' A buy and hold decision function
#'
#' @param stocks the list of stocks to decide upon.
#'
#' @return a vector of investment decisions
#' @export
#'
#' @examples
#' a<-1
buyandholdSignal<-function(stocks) rep(1, length(stocks))


#' Create a portfolio from instrument picking and ranking
#'
#' Basically this operates on a list of instruments with any history length
#' and processes each instrument with the selectInstrument function which returns
#' TRUE if the instrument should be invested in right now or FALSE if it should
#' not. The second function rankInstrument should return a scalar value indicating
#' the magnitude of how good the instrument is thought to be. Of course the metric
#' in both of these functions could be the same but they could also differ.
#'
#' Both the selectInstrument and rankInstrument must operate on an xts class
#' and take that as it's first and only mandatory argument.
#'
#' @param instruments the named list of instruments to consider
#' @param topN the top N instruments to keep in a portfolio
#' @param selectInstrument function that selects whether to invest in an
#' instrument or not
#' @param rankInstrument function that returns a scalar rank for an instrument
#' higher means better
#'
#' @importFrom datools standardize
#'
#' @return the created portfolio
#' @export
#'
#' @examples
#' library(datrader)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' rankInstrument <- function(x) tail(momentum(Cl(x), n=90), 1)
#' selectInstrument <- function(x) rankInstrument(x) > 5
#' createPortfolio(mylist, selectInstrument, rankInstrument, topN=3)
createPortfolio <- function(instruments, selectInstrument, rankInstrument, topN=50) {

  # 1. Select the stocks that we wish to invest in
  myinstr <- Filter(selectInstrument, instruments)

  # 2. Rank the stocks that we wish to invest in decreasing order
  scores <- sapply(myinstr, rankInstrument)
  scoreorder <- order(scores, decreasing = TRUE)
  myinstr <- myinstr[scoreorder]
  scores <- scores[scoreorder]

  # 3. Select the topN of remaining stocks to invest in
  if(topN < length(myinstr)){
    myinstr <- myinstr[1:topN]
    scores <- scores[1:topN]
  }

  # 4. Take positions in stocks and distribute X Money
  sharesize <- datools::standardize(scores, 1, 10)
  sharesize <- sharesize/sum(sharesize)
  sharesize
}


#' Evaluate a strategy on a timeframe and market
#'
#' @param instruments the named list of instruments to consider
#' @param dates the timeframe to operate on given as a vector of Date
#' @param strategy the strategy to evaluate
#' @param investFrequency how often to reconsider the portfolio in number of days
#' @param cash total amount of money to invest
#'
#' @return the performance
#' @export
#'
#' @examples
#' library(datrader)
#' library(quantmod)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' rankInstrument <- function(x) tail(momentum(Cl(x), n=90), 1)
#' selectInstrument <- function(x) rankInstrument(x) > 5
#' mydates <- index(tail(mylist[[1]], 100))
#' mystrat <- function(x) createPortfolio(x, selectInstrument, rankInstrument, 10)
#' evaluateStrategy(mylist, mydates, mystrat, 30)
evaluateStrategy <- function(instruments,
                             dates,
                             strategy,
                             investFrequency=30,
                             cash=10000) {
  lastDate <- max(dates)
  date <- min(dates)
  cash <- cash
  holding <- NULL
  p <- progress_estimated(trunc(as.integer(lastDate-date)/investFrequency)+1)
  while (date <= lastDate) {
    p$tick()$print()
    # browser()
    # Get all available instruments in the market at date
    instravail <- getAvailableInstruments(instruments, date)
    mylist <- instruments[instravail]
    # if(length(mylist) < 1) {
      # date <- date + investFrequency
      # next
    # }
    mylist <- lapply(mylist, function(x) x[zoo::index(x)<=as.Date(date),])

    # Select top 50 instuments, rank them and convert to a suggested holding
    # shares <- createPortfolio(mylist, selectInstrument, rankInstrument, topN = 50)
    shares <- strategy(mylist)
    prices <- getLastKnownQuantity(mylist, quantmod::Cl)
    fees <- getFees(instruments)
    hnames <- names(holding)
    snames <- names(shares)
    moneyavail <- cash+sum(holding*prices[hnames])-sum(fees[base::union(snames, hnames)])
    sugholding <- sharesToHolding(shares, prices[snames], moneyavail) # Make sure we can pay the fees too!

    # Update our existing holding
    snames <- names(sugholding) # Redefine snames to reflect holding which may not have been realized
    tosell <- base::setdiff(hnames, snames)
    tokeep <- base::intersect(hnames, snames)
    tobuy <- base::setdiff(snames, hnames)

    # Sell stuff
    if(length(tosell)>0){
      cash <- cash + sum(prices[tosell]*holding[tosell]-fees[tosell])
      holding <- holding[tokeep]
    }

    # Keep stuff
    if(length(tokeep)>0){
      holddiff <- holding[tokeep]-sugholding[tokeep]
      tmpnames <- names(which(holddiff>0))
      if(length(tmpnames)>0) # Reduce holding
        cash <- cash + sum(holddiff[tmpnames]*prices[tmpnames]-fees[tmpnames])
      tmpnames <- names(which(holddiff<0))
      if(length(tmpnames)>0) # Increase holding
        cash <- cash + sum(holddiff[tmpnames]*prices[tmpnames]-fees[tmpnames])
      holding[tokeep] <- holding[tokeep]-holddiff[tokeep]
    }

    # Buy stuff
    if(length(tobuy)>0){
      holding[tobuy]=sugholding[tobuy]
      cash <- cash - sum(sugholding[tobuy]*prices[tobuy]-fees[tobuy])
    }

    # totfees <- sum(getFees(instruments[base::union(holding, sugholding)]))

    # Filter away 0 holdings
    holding <- holding[holding>0]

    # Move forward in time
    date <- date + investFrequency
  }
  tmpnames <- names(holding)
  list(Value=sum(holding[tmpnames]*prices[tmpnames]), Cash=cash)
}

#' Find out which instruments are active at date
#'
#' Looks at all instruments and writes TRUE if date is newer than the earliest
#' date for each instrument. This allows us to know if a given instrument
#' existed at the time of "date".
#'
#' @param instruments the named list of instruments to consider
#' @param date the date to use for the query
#' @return a vector of instrument names featuring TRUE or FALSE which indicates
#' whether or not they existed at the time of date.
#' @export
#' @examples
#' library(datrader)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' getAvailableInstruments(mylist, Sys.Date())
getAvailableInstruments <- function(instruments, date){
    sapply(instruments, function(x) as.Date(date) > min(zoo::index(x)))
}

#' Convert share of investment to actual holding
#'
#' @param shares the shares given as percentage for each instrument
#' @param prices the price given as a real number for each instrument
#' @param net the total amount of money that can be invested
#'
#' @return the holdings as a integer quantity for each instrument
#' @export
#'
#' @examples
#' library(datrader)
#' library(quantmod)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' rankInstrument <- function(x) tail(momentum(Cl(x), n=90), 1)
#' selectInstrument <- function(x) rankInstrument(x) > 5
#' shares <- createPortfolio(mylist, selectInstrument, rankInstrument, topN=3)
#' prices <- getLastKnownQuantity(mylist, quantmod::Cl)[names(shares)]
#' sharesToHolding(shares, prices, 10000)
sharesToHolding <- function(shares, prices, net) {
  n <- sapply(1:length(shares), function(x) trunc(net*shares[x]/prices[x]))
  n
}
