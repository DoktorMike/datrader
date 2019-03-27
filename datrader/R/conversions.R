
#' Reduce a list of instruments to a single tibble
#'
#' Basically this function reduces a list of instruments
#' to a single long format tibble in order to create a more
#' easy to work with format and fascilitate saving as csv
#' etc.
#'
#' @param x the list of instruments, which currently all
#' must be of class xts, to reduce
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr mutate full_join %>%
#'
#' @return a long format tibble
#' @export
#'
#' @examples
#' library(datrader)
#' library(quantmod)
#' library(zoo)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' lapply(mylist, head)
instrumentListToDataFrame<-function(x){
  # Must be a list of stock where each stock is an xts
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, function(y) class(y)[1]) == 'xts'))

  xtsToLong <- function(instName) {
    as.data.frame(x[[instName]]) %>% tibble::rownames_to_column("date") %>%
      tibble::as_tibble() %>% dplyr::mutate(ticker=instName) %>%
      tidyr::gather("measure", "value", -c("date", "ticker"))
  }
  Reduce(function(a, b) dplyr::full_join(a, b), lapply(names(x), xtsToLong))
}

#' Reduce a list of instruments to a single tidyquant tibble
#'
#' Basically this function reduces a list of instruments
#' to a single long format tibble in order to create a more
#' easy to work with format and fascilitate saving as csv
#' etc.
#'
#' @param x the list of instruments, which currently all
#' must be of class xts, to reduce
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr mutate full_join %>%
#' @importFrom tidyquant theme_tq geom_candlestick
#'
#' @return a long format tidyquant tibble
#' @export
#'
#' @examples
#' library(datrader)
#' library(quantmod)
#' library(zoo)
#' library(tidyquant)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' tqdf <- instrumentListToTidyquant(mylist)
#' mutate(tqdf, date=as.Date(date)) %>% filter(date >= "2018-01-01") %>%
#' ggplot(aes(x = date, y = close, group = symbol)) +
#' geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
#' theme_tq() + facet_grid(symbol~., scale="free_y")
instrumentListToTidyquant<-function(x){
  # Must be a list of stock where each stock is an xts
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, function(y) class(y)[1]) == 'xts'))

  xtsToLong <- function(instName) {
    as.data.frame(x[[instName]]) %>% tibble::rownames_to_column("date") %>%
      tibble::as_tibble() %>% dplyr::mutate(symbol=instName)
  }
  Reduce(function(a, b) dplyr::full_join(a, b), lapply(names(x), xtsToLong)) %>%
    dplyr::select("symbol", "date", "open", "high", "low", "close", "volume",
                    "adjusted")
}

#' Reduce a list of instruments to a single tibble of f
#'
#' Basically this function reduces a list of instruments
#' to a single wide format tibble in order to create a more
#' easy to work with format and fascilitate saving as csv
#' etc. It applies function f to each instrument and use that as a column
#' in the final tibble. The default function applied is the close price
#' extraction.
#'
#' @param x the list of instruments, which currently all
#' must be of class xts, to reduce
#' @param f the function to be applied to each instrument which must create one
#' single value for each observation in the instrument
#'
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom dplyr mutate full_join %>% arrange
#' @importFrom quantmod Cl
#' @importFrom purrr set_names
#'
#' @return a long format tibble
#' @export
#'
#' @examples
#' a<-1
instrumentListTransToDataFrame <- function(x, f=quantmod::Cl) {
  # Must be a list of stock where each stock is an xts
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, function(y) class(y)[1]) == 'xts'))

  extractSingle <- function(x, i, f) {
    f(x[[i]]) %>%
      as.data.frame() %>%
      tibble::rownames_to_column('date') %>%
      tibble::as_data_frame() %>%
      dplyr::mutate(date=as.Date(date)) %>%
      purrr::set_names(c('date', names(x)[i]))
  }
  retdf <- extractSingle(x, 1, f)
  for(i in 2:length(x))
    retdf <- dplyr::full_join(retdf, extractSingle(x, i, f))
  retdf %>% dplyr::arrange(date)
}

#' Filter instruments from the list which are unwanted
#'
#' Unwanted instruments are judged by the ratio of NA's compared to total amount
#' of observations and having data later than the given lastDate.
#'
#' @param stocks the list of instruments to filter
#' @param cutoff the cutoff value of how many NA's are acceptable which is by
#' default set to 5 per cent
#' @param lastDate the date where an instrument have to have data until at least
#'
#' @return the filtered list of instruments
#' @export
#'
#' @examples
#' a<-1
filterUnwantedInstruments <- function(stocks, cutoff=0.05, lastDate=Sys.Date()-1){
  nafilter <- function(stock) max(sapply(stock, function(x) sum(is.na(x))/length(x))) < cutoff
  datefilter <- function(stock) tail(index(stock), 1) >= lastDate
  myfilter <- function(stock) all(nafilter(stock), datefilter(stock))
  Filter(myfilter, stocks)
}

#' Impute the list of instruments
#'
#' This is a convenience function mapping na.approx to each instrument.
#'
#' @param stocks the list of instruments to impute NA's in
#'
#' @return a list of the given instruments with NA's imputed
#' @export
#' @importFrom zoo na.approx
#'
#' @examples
#' a<-1
imputeInstruments <- function(stocks) Map(zoo::na.approx, stocks)


#' Get a vector of all instruments containing NA's
#'
#' Utility function to quickly detect instruments in need of imputing. This
#' covers all columns of each instrument and reacts to an NA anywhere.
#'
#' @param instruments the list of instruments to detect NA's in
#'
#' @return a named vector of instruments containing NA's somewhere
#' @export
#'
#' @examples
#' a<-1
naInstruments <- function(instruments) which(sapply(instruments, function(y) any(sapply(y, function(x) any(is.na(x))))))

#' Generate historical positions based on a trading strategy
#'
#' The trading strategy is based on a single instrument and is allowed to take
#' multiple extra arguments.
#'
#' @param x the instrument to generate historical positions in
#' @param tstrat the trading strategy to utilize which is based on a single
#' instrument approach
#'
#' @return a position vector consisting of 0's and 1's for indicating in and out
#' of position
#' @export
#'
#' @examples
#' a<-1
generateHistoricalPositions <- function(x, tstrat) {
  # for(i in (h+1):nrow(x)) mypos[i]<-ifelse(tstrat(x[(i-h):(i-1),])$Invest==TRUE, 1, 0)
  # burnin <- 10
  if(nrow(x)<2) stop("Cannot generate historical positions for timeframe shorter than 2 days.")
  c(rep(0, 1), sapply(2:nrow(x), function(i) tstrat(x[1:(i-1),])$Invest))
}

#' Plot the historical positions
#'
#' This function illustrates the positions generated over time based on a
#' trading strategy along with the price over time.
#'
#' @param x the instrument given as an xts object
#' @param pos the position vector given by a trading strategy
#'
#' @return a ggplot object representing the plot
#' @importFrom tibble tibble
#' @importFrom quantmod Cl
#' @importFrom ggplot2 ggplot geom_line facet_grid aes
#' @importFrom tidyr gather
#' @importFrom utils tail
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' library(datrader)
#' library(readr)
#' library(quantmod)
#' f <- system.file("extdata", "FB.csv", package = "datrader", mustWork = TRUE)
#' mylist <- loadExistingInstruments(gsub("/FB.csv", "", f))
#' plotHistoricalPositions(tail(mylist$MSFT, 100), rbinom(100, 1, 0.3))
plotHistoricalPositions<-function(x, pos) {
  tibble::tibble(Date=as.Date(index(x)), Price=as.vector(quantmod::Cl(x)), Invest=pos) %>%
    tidyr::gather("Key", "Value", -"Date") %>%
    ggplot2::ggplot(ggplot2::aes_string(y="Value", x="Date", color="Key", group="Key")) +
    ggplot2::geom_line() + ggplot2::facet_grid(stats::as.formula("Key~."), scales = "free")
}

#' Calculate number of trades in this position vector
#'
#' Takes a vector of 0 and 1 and calculates how many trades are made. A
#' transition from 0 to 1 or 1 to 0 is calculated as a trade.
#'
#' @param p the position vector indicating whether you are in position or out
#'
#' @return the number of trades made
#' @export
#'
#' @examples
#' library(datrader)
#' numTrades(c(0,0,0,0,1,1,1,0,0))
numTrades<-function(p) length(which(base::abs(base::diff(p))>0))

#' Get the last known quantity from a list of instruments
#'
#' Takes a list of instruments and applies the function f to the last known
#' observation for each instrument.
#'
#' @param instruments the list of instruments to operate on
#' @param f the function that extracts the quantity we would like which defaults
#' to the closing price.
#' @importFrom quantmod Cl
#'
#' @return a named vector of the last observed quantity for each instrument
#' @export
#'
#' @examples
#' library(datrader)
#' library(quantmod)
#' library(zoo)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' getLastKnownQuantity(mylist, quantmod::Cl)
#' #' getLastKnownQuantity(mylist, zoo::index)
getLastKnownQuantity <- function(instruments, f = quantmod::Cl) {
  sapply(instruments, function(x) f(tail(x,1)))
}

#' Get the fees for each instrument
#'
#' @param instruments a list of instruments to get the transaction fees for
#'
#' @return a named vector of fees for the instruments
#' @export
#'
#' @examples
#' a <- 1
getFees <- function(instruments){
  a<-rep(1, length(instruments))
  names(a)<-names(instruments)
  a
}
