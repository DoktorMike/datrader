#' Type for Investment strategies
#'
#' @param invest a function producing a vector of buying decisions.
#' See details for more.
#'
#' @return a Strategy instantiated with investment stratey invest.
#' @export
#'
#' @examples
Strategy <- function(invest,){
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
buyandholdSignal<-function(stocks) rep(1, length(stocks))
