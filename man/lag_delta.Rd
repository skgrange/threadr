% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lag_delta.R
\name{lag_delta}
\alias{lag_delta}
\alias{lead_delta}
\title{Functions to calculate the delta between a value and the lagged or leading 
value.}
\usage{
lag_delta(x, na_as_zero = FALSE)

lead_delta(x, na_as_zero = FALSE)
}
\arguments{
\item{x}{Numeric vector.}

\item{na_as_zero}{Should missing elements be set to 0?}
}
\value{
Numeric vector.
}
\description{
Functions to calculate the delta between a value and the lagged or leading 
value.
}
\examples{

lag_delta(sample(seq(0:10)))

}
\author{
Stuart K. Grange.
}
