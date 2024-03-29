% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/any_interval_overlap.R
\name{any_interval_overlap}
\alias{any_interval_overlap}
\title{Function to test if any intervals overlap with one another.}
\usage{
any_interval_overlap(df)
}
\arguments{
\item{df}{Tibble containing at least two variables, \code{"rowid"} and
\code{"interval"}. \code{"interval"} must be of data type \code{interval}.}
}
\value{
Logical vector, length of 1.
}
\description{
Function to test if any intervals overlap with one another.
}
\author{
Stuart K. Grange
}
