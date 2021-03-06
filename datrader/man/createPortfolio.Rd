% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strategy.R
\name{createPortfolio}
\alias{createPortfolio}
\title{Create a portfolio from instrument picking and ranking}
\usage{
createPortfolio(instruments, selectInstrument, rankInstrument, topN = 50)
}
\arguments{
\item{instruments}{the named list of instruments to consider}

\item{selectInstrument}{function that selects whether to invest in an
instrument or not}

\item{rankInstrument}{function that returns a scalar rank for an instrument
higher means better}

\item{topN}{the top N instruments to keep in a portfolio}
}
\value{
the created portfolio
}
\description{
Basically this operates on a list of instruments with any history length
and processes each instrument with the selectInstrument function which returns
TRUE if the instrument should be invested in right now or FALSE if it should
not. The second function rankInstrument should return a scalar value indicating
the magnitude of how good the instrument is thought to be. Of course the metric
in both of these functions could be the same but they could also differ.
}
\details{
Both the selectInstrument and rankInstrument must operate on an xts class
and take that as it's first and only mandatory argument.
}
\examples{
library(datrader)
mypath <- system.file('extdata', package = 'datrader')
mylist <- loadExistingInstruments(mypath)
rankInstrument <- function(x) tail(momentum(Cl(x), n=90), 1)
selectInstrument <- function(x) rankInstrument(x) > 5
createPortfolio(mylist, selectInstrument, rankInstrument, topN=3)
}
