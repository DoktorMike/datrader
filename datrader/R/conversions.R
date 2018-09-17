
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
#' @importFrom dplyr mutate full_join
#'
#' @return a long format tibble
#' @export
#'
#' @examples
#' a <- 1
instrumentListToDataFrame<-function(x){
  # Must be a list of stock where each stock is an xts
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, function(y) class(y)[1]) == 'xts'))

  xtsToLong <- function(instName) {
    as.data.frame(x[[instName]]) %>% tibble::rownames_to_column("date") %>%
      tibble::as_tibble() %>% dplyr::mutate(ticker=instName) %>%
      tidyr::gather(measure, value, -c(date, ticker))
  }
  Reduce(function(a, b) dplyr::full_join(a, b), lapply(names(x), xtsToLong))
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
#'
#' @examples
#' a<-1
imputeInstruments <- function(stocks) Map(na.approx, stocks)


#' Get a vector of all instruments containing NA's
#'
#' Utility function to quickly detect instruments in need of imputing. This
#' covers all columns of each instrument and reacts to an NA anywhere.
#'
#' @param stocks the list of instruments to detect NA's in
#'
#' @return a named vector of instruments containing NA's somewhere
#' @export
#'
#' @examples
#' a<-1
naInstruments <- function(stocks) which(sapply(mylist, function(y) any(sapply(y, function(x) any(is.na(x))))))

