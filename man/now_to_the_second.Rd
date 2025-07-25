% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lubridate_supplements.R
\name{now_to_the_second}
\alias{now_to_the_second}
\title{Function to get current system time at second accuracy.}
\usage{
now_to_the_second(tz = Sys.timezone(), as_numeric = FALSE)
}
\arguments{
\item{tz}{Time zone to represent dates in. The default is the system's time 
zone.}

\item{as_numeric}{Should the time be returned as the numeric Unix time?}
}
\value{
\code{POSIXct} or numeric vector with a length of 1.
}
\description{
\code{now_to_the_second} floor rounds the current time.
}
\seealso{
\code{\link[lubridate]{now}}, \code{\link{Sys.time}}, 
\code{\link[lubridate]{floor_date}}
}
\author{
Stuart K. Grange
}
