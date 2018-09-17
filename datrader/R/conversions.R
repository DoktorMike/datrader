
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
