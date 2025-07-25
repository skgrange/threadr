% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_locf.R
\name{na_locf}
\alias{na_locf}
\title{Function for carrying last observation forwards.}
\usage{
na_locf(x, na.rm = FALSE)
}
\arguments{
\item{x}{Input vector.}

\item{na.rm}{Should leading NAs be removed? Default is \code{FALSE}.}
}
\value{
Vector of same data type and length of \code{x}.
}
\description{
Used to replace each \code{NA} with the most recent non-\code{NA} prior to 
it. \code{locf} is a simple wrapper for \code{\link[zoo]{na.approx}}.
}
\examples{
\dontrun{

# Push observations forwards, in a data frame
data_income$income_chunk <- na_locf(data_income$income_chunk)

}

}
\author{
Stuart K. Grange
}
