% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_period.R
\name{to_period}
\alias{to_period}
\title{Function to calculate a \strong{lubridate} \code{period} date class based on
two dates.}
\usage{
to_period(date_one, date_two, round = FALSE, as.character = FALSE)
}
\arguments{
\item{date_one}{First date.}

\item{date_two}{Second date.}

\item{round}{Number of decimal points to round to. Default is no rounding.}

\item{as.character}{Should the period be formatted and returned as a 
character?}
}
\value{
\code{Period} or character vector.
}
\description{
Function to calculate a \strong{lubridate} \code{period} date class based on
two dates.
}
\seealso{
\code{\link[lubridate]{as.period}}
}
\author{
Stuart K. Grange
}
