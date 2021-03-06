% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{exchange}
\alias{exchange}
\title{Exchange rates from the European Central Bank's Statistical Data Warehouse (SDW)}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 36847 rows and 5 columns.
}
\source{
\url{https://sdw.ecb.europa.eu/}
}
\usage{
exchange
}
\description{
A dataset containing several exchange rates as denoted by the European Central Bank's Statistical Data Warehouse (SDW).
The data includes:
\itemize{
 \item USD: US-Dollar
 \item GBP: British Pound
 \item JPY: Japanese Yen
 \item CAD: Canadian Dollars
 \item CHF: Suisse Franks
 \item AUD: Australian Dollars
 \item CNY: Chinese Yuan Renminbi
 }
}
\examples{
#Subset by
exchange[exchange$to=="USD",]
}
\keyword{datasets}
