% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadSECQtr.R
\name{downloadSECQtr}
\alias{downloadSECQtr}
\title{Download an index for a year/quarter.}
\usage{
downloadSECQtr(year, qtr)
}
\arguments{
\item{year}{the year to download.}

\item{qtr}{the quarter to download.}
}
\value{
returns a data.table.
}
\description{
\code{downloadSECQtr} Download  an index for a year/quarter.
}
\details{
Downloads an SEC index file for a quarter. This function uses the
downloadSECIndex function and thus needs the `USERAGENT` environment
variable set.
}
\examples{
\dontrun{

link <- "https://www.sec.gov/Archives/edgar/daily-index/2024/QTR4/master.20241004.idx"
output <- downloadSECQtr(2020, 3)
output
}
}
