% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{findLastDateInInstruments}
\alias{findLastDateInInstruments}
\title{Extract the earliest last observed date in all stored instruments}
\usage{
findLastDateInInstruments(path)
}
\arguments{
\item{path}{the path where your data is stored as csv's}
}
\value{
the earliest observed last date
}
\description{
Each instrument has it's own date range and this function runs through all
of them identifying the earliest last observed date. It is intended to help
you identify from which date you need to update your database.
}
\examples{
\dontrun{updateInstruments("/somewhere/trading")}
}
