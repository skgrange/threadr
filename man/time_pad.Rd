% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_pad.R
\name{time_pad}
\alias{time_pad}
\title{Function to pad time series.}
\usage{
time_pad(
  df,
  interval = "hour",
  by = NA,
  round = NA,
  full = FALSE,
  uniform_padding = FALSE,
  warn = TRUE
)
}
\arguments{
\item{df}{A tibble/data frame including parsed dates. The date 
variable/column must be named \code{date}.}

\item{interval}{Interval of returned time series. Some examples could be: 
\code{"min"} \code{"hour"}, \code{"day"}, \code{"month"}, \code{"year"} but 
multiples such as \code{"5 min"} work too. \code{interval} can also be a 
numeric value such as \code{0.5} which is useful for sub-second padding.}

\item{by}{Should \code{time_pad} apply the padding function to groups within
\code{df}? This is helpful when there are many sites/other identifiers within
\code{df} which need to be padded individually.}

\item{round}{What date-unit should the first and last observations be rounded
to? This allows the padded time-series to begin and end at a "nice place". 
Examples are \code{"hour"}, \code{"day"}, \code{"month"}, and \code{"year"}.}

\item{full}{Should the date joining use the \code{full_join} function? If 
\code{TRUE}, no input dates will be lost but the default is \code{FALSE}.}

\item{uniform_padding}{If a \code{by} vector is supplied, should all groups
be padded with the same start and end dates? The minimum and maximum dates
contained in \code{df} will be used for this uniform padding process.}

\item{warn}{Should the function give a warning when dates are duplicated? 
Default is \code{TRUE}.}
}
\value{
Tibble.
}
\description{
\code{time_pad} use is similar to \strong{openair}'s \code{timeAverage}, but
the aggregation of values does not occur. \code{time_pad} does not drop 
non-numerical variables, can include identifiers post-pad, and can start and
end a padded time-series at a "nice place", for example, at the beginning of
an hour or day.
}
\details{
\code{time_pad} pads a time series by calculating the maximum and minimum 
dates within a time series and then generating a uniform date sequence 
between the maximum and minimum dates. This date sequence is then joined to 
the input data frame and the missing values are represented as \code{NA}.
}
\examples{

\dontrun{

# Pad time series so every minute is present
data_nelson_pad <- time_pad(data_nelson, interval = "min", round = "day")

# Keep identifying variables "site" and "sensor"
data_ozone_sensor_pad <- time_pad(
  data_ozone_sensor, 
  interval = "hour", 
  by = c("site", "sensor")
)

}

}
\seealso{
\code{\link{round_date_interval}}, \code{\link[openair]{timeAverage}}, 
\code{\link[lubridate]{round_date}}, \code{\link[dplyr:mutate-joins]{left_join}}, 
\code{\link{aggregate_by_date}}
}
\author{
Stuart K. Grange
}
