% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_locf.R
\name{na_interpolate}
\alias{na_interpolate}
\title{Function for interpolating \code{NA}s.}
\usage{
na_interpolate(x, na.rm = FALSE, spline = FALSE, extrapolate = FALSE)
}
\arguments{
\item{x}{Input vector.}

\item{na.rm}{Should leading NAs be removed?}

\item{spline}{Should \code{NA}s be replaced using cubic splines rather than 
linear interpolation.}

\item{extrapolate}{Should leading and preceding \code{NA}s be extrapolated 
too? Only used for linear interpolation.}
}
\value{
Vector length of \code{x}.
}
\description{
\code{na_interpolate} wraps functions from the \strong{zoo} package.
}
\seealso{
\code{\link[zoo]{na.approx}}
}
\author{
Stuart K. Grange
}
