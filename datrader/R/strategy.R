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
Strategy <- function(invest){
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


# Normal
a<-data.frame(A=c(10,10000, 1000000, 100000000000000), B=c(0.1,0.2,0.3,0.4))
write.csv(a, file = "crap.csv")
b<-read.csv("crap.csv")


# Scipen
a<-data.frame(A=c(10,10000, 1000000, 100000000000000), B=c(0.1,0.2,0.3,0.4))
options(scipen = 999)
write.csv(a, file = "crap.csv")
options(scipen = 0)
b<-read.csv("crap.csv")

