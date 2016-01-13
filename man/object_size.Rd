% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object_size.R
\name{object_size}
\alias{object_size}
\title{Convenience function to find size of R object.}
\usage{
object_size(object, unit = "Mb")
}
\arguments{
\item{object}{A R object.}

\item{unit}{Units of size. See \link{object.size} for options. Defaults to 
\code{"Mb"} for megabytes.}
}
\description{
\code{object_size} is a simple wrapper for \code{object.size} and 
\code{format} which allows for quick return of an object's size in memory.
}
\author{
Stuart K. Grange
}
