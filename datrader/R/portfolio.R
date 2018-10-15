
#' Create a portfolio
#'
#' Creates a portfolio by adding the investment, the holdings,
#' the buy prices as well as the buy dates.
#'
#' @param holdings the holdings as a named vector of positive integers
#' @param buydates a named vector of dates for the transactions
#' @param buyprices a named vector of prices for the instruments
#' @param fees a named vector of transaction fees for the instruments
#'
#' @return the newly created portfolio
#' @export
#'
#' @examples
#' a <- 1
Portfolio <- function(holdings, buydates, buyprices, fees) {
  ret <- list(Holdings = holdings,
              Buydates = buydates,
              Buyprices = buyprices,
              Fees = fees)
  class(ret) <- c("Portfolio", "list")
  ret
}


#' Check if x is a Portfolio
#'
#' @param x the object to check
#'
#' @return TRUE if x is a Portfolio FALSE otherwise
#' @export
is.Portfolio <- function(x) {
  inherits(x, "Portfolio")
}

#' Generic to estimate value of an object x
#'
#' Gives and indication of the value of the object given
#' a set of arguments.
#'
#' @param x the object to operate on
#' @param ... additional arguments to pass to specific method
#'
#' @return the estimated value as a scalar
#' @export
#'
#' @examples
#' library(datrader)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#'
#' p <- Portfolio(
#'   holdings=structure(rep(1,length(mylist)), .Names=names(mylist)),
#'   buydates=sapply(mylist, function(x) tail(index(x), 10)[1]),
#'   buyprices=sapply(mylist, function(x) tail(Cl(x), 10)[1]),
#'   fees=structure(rep(1,length(mylist)), .Names=names(mylist)) )
#'
#' value(p, sapply(mylist, function(x) tail(Cl(x), 1)))
value <- function (x, ...) UseMethod("value")

#' @export
value.default <- function(x, ...) warning("Not implemented")

#' @describeIn value calculates the value of a Portfolio
#' @param currprices the prices that represents the market
#' @export
value.Portfolio <- function(x, currprices, ...) {
  pnames <- names(x$Holdings)
  if(!all(pnames %in% names(currprices))){
    msg <- paste0("You need to provide prices for all instruments in Portfolio")
    msg <- paste0(msg, "\nPrices missing: ")
    msg <- paste0(msg, base::setdiff(pnames, currprices))
    stop(msg)
  }
  sum(x$Holdings*(currprices[pnames]))
}

#' Generic to estimate profit of an object x
#'
#' Gives and indication of the profit of the object given
#' a set of arguments.
#'
#' @param x the object to operate on
#' @param ... additional arguments to pass to specific method
#'
#' @return the estimated profit as a scalar
#' @export
#'
#' @examples
#' library(datrader)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#'
#' p <- Portfolio(
#'   holdings=structure(rep(1,length(mylist)), .Names=names(mylist)),
#'   buydates=sapply(mylist, function(x) tail(index(x), 10)[1]),
#'   buyprices=sapply(mylist, function(x) tail(Cl(x), 10)[1]),
#'   fees=structure(rep(1,length(mylist)), .Names=names(mylist)) )
#'
#' profit(p, sapply(mylist, function(x) tail(Cl(x), 1)))
profit <- function (x, ...) UseMethod("profit")

#' @export
profit.default <- function(x, ...) warning("Not implemented")

#' @describeIn profit calculates the profit of a Portfolio
#' @param currprices the prices that represents the market
#' @export
profit.Portfolio <- function(x, currprices, ...) {
  pnames <- names(x$Holdings)
  if(!all(pnames %in% names(currprices))){
    msg <- paste0("You need to provide prices for all instruments in Portfolio")
    msg <- paste0(msg, "\nPrices missing: ")
    msg <- paste0(msg, base::setdiff(pnames, currprices))
    stop(msg)
  }
  sum(x$Holdings*(currprices[pnames]-x$Buyprices))
}
