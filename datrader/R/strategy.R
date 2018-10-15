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


evaluateStrategy <- function(instruments) {
    # sapply(instruments,
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

