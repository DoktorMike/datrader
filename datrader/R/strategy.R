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


}

create_portfolio <- function(strategy, market) {

}

