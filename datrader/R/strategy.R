#' Type for Investment strategies
#'
#' @param invest a function producing a vector of buying decisions.
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


evaluate <- function(strategy, market){
  h <- horizon(strategy)
  h
}

#' Create a portfolio from a strategy
#'
#' @param strategy the investment strategy
#' @param instruments the list of instruments to consider
#' @param investment the total amount of money to distribute
#' @param topN the top N instruments to keep in a portfolio
#'
#' @return the created portfolio
#' @export
#'
#' @examples
#' a<-1
createPortfolio <- function(strategy, instruments, investment=10000, topN=50) {

  # 1. Select the stocks that we wish to invest in
  myinstr <- Filter(selectInstrument, instruments)

  # 2. Rank the stocks that we wish to invest in decreasing order
  myinstr <- myinstr[order(sapply(myinstr, rankInstrument), decreasing = TRUE)]

  # 3. Select to N remaining stocks to invest in
  if(topN < length(myinstr)) myinstr <- myinstr[1:topN]

  # 4. Take positions in stocks and distribute X Money

}


