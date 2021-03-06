% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_html_tables.R
\name{read_html_tables}
\alias{read_html_tables}
\title{Function to read HTML tables from an URL.}
\usage{
read_html_tables(url, col_names = TRUE)
}
\arguments{
\item{url}{A URL which contains HTML tables.}

\item{col_names}{Do the HTML tables have column names/headers?}
}
\value{
Named list containing tibbles.
}
\description{
Function to read HTML tables from an URL.
}
\examples{

# A url
url <- "https://en.wikipedia.org/wiki/List_of_London_Underground_stations"

# A list
list_tables <- read_html_tables(url)
length(list_tables)

# Get a single data frame
data_stations <- list_tables[[1]]

}
\author{
Stuart K. Grange
}
