# Clustering

pcastock <- function(stock){
  if(!inherits(stock, "xts")) stop("This function requires a xts object.")
  tmp <- stock %>% as.data.frame() %>% dplyr::as_tibble() %>%
    stats::prcomp(center = TRUE, scale. = TRUE)
  tmp$x[,"PC1"]
}

#' Cluster a list of stocks into groups
#'
#' The function clusters stocks into k groups based on a horizon. It
#' uses k-means clustering on the first PCA component of each stock where
#' Open, High, Low, Close, Adjusted and Volume goes into the PCA. As such
#' this clustering should take all sorts of variation into account.
#'
#' @param stocks the list of stocks to operate on typically list of xts objects
#' @param k the number of groups to use which defaults to 10
#' @param horizon the number of trading days to use for the clustering
#'
#' @return a named class vector
#' @export
#'
#' @examples
#' library(datrader)
#' mypath <- system.file('extdata', package = 'datrader')
#' mylist <- loadExistingInstruments(mypath)
#' getAvailableInstruments(mylist, Sys.Date())
#' clusterStocks(mylist, k=2, horizon=10)
clusterStocks<-function(stocks, k=10, horizon=90){
  if(!inherits(stocks, "list"))
    stop("This function requires a list of xts objects.")
  if(!all(sapply(stocks, function(x) inherits(x, "xts"))))
    stop("This function requires a list of xts objects.")
  # tmpstocks <- lapply(stocks, function(x) tail(x, horizon))
  pca1 <- stocks %>% sapply(function(x) pcastock(tail(x, horizon)))
  cl <- stats::kmeans(t(pca1), k)
  cl$cluster
}
