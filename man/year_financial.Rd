% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/week_monday.R
\name{year_financial}
\alias{year_financial}
\title{Function to transform date into financial year.}
\usage{
year_financial(date, start = "july")
}
\arguments{
\item{date}{Vector of parsed dates}

\item{start}{The starting month of the financial year. Currently, the only
option is \code{"july"}.}
}
\value{
Integer vector.
}
\description{
Function to transform date into financial year.
}
\examples{
\dontrun{

year_financial(data_hours$date)

}

}
\seealso{
\code{\link[lubridate]{year}}, \code{\link{week_financial}}
}
\author{
Stuart K. Grange
}
