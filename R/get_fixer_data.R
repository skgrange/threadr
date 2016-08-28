#' Function to get exchange rates from \href{http://fixer.io/}{Fixer.io}.
#' 
#' @author Stuart K. Grange
#' 
#' @param from Currency code used as base. 
#' @param to Currency code as a function of \code{from}. 
#' @param start Start date. 
#' @param end End date. 
#' @param lower Should the currency codes be forced to be lower-case? 
#' @param progress Type of progress bar to display. 
#'
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_fixer_data <- function(from, to, start = NA, end = NA, lower = TRUE, 
                           progress = "none") {
  
  if (length(from) != 1) stop("'from' must have a length of 1.", call. = FALSE)
  
  # Use inputs to build urls for the api
  urls <- build_fixer_urls(start = start, end = end, from = from, to = to)
  
  # Get JSON requests
  list_json <- plyr::llply(urls, fixer_rates_worker, .progress = progress)
  
  # To data frame
  list_json <- lapply(list_json, data.frame)
  
  # Bind
  suppressWarnings(
    df <- bind_rows(list_json)
  )
  
  # Clean names
  names(df) <- stringr::str_replace_all(names(df), "rates.", "")
  
  # Make tidy data
  df <- tidyr::gather(df, to, value, -date, -base)
  
  # Transform, order, and remove nas occurs when from = to
  df <- df %>% 
    mutate(date = lubridate::ymd(date, tz = "UTC")) %>% 
    select(date, 
           from = base, 
           to, 
           value) %>% 
    filter(!is.na(value))
  
  if (lower) {
    
    df <- df %>% 
      mutate(from = stringr::str_to_lower(from),
             to = stringr::str_to_lower(to))
    
  }
  
  # Return
  df
  
}


build_fixer_urls <- function(start, end, from, to) {
  
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Create date sequence
  date_sequence <- seq(start, end, "day")
  
  # 
  from <- stringr::str_to_upper(from)
  to <- stringr::str_to_upper(to)
  to <- stringr::str_c(to, collapse = ",")
  
  # Build query strings
  urls <- stringr::str_c("http://api.fixer.io/", date_sequence, "?base=", from,
                         "&symbols=", to)
  
  # Return
  urls
  
}


fixer_rates_worker <- function(url) {
  
  # Use URL date rather than return
  date_url <- stringr::str_split_fixed(url, pattern = "/|\\?", n = 5)[, 4]
  
  # Get response
  response <- readLines(url, warn = FALSE)
  
  # Parse
  json <- fromJSON(response, flatten = TRUE)
  
  # Overwrite
  json$date <- date_url
  
  # Catch empty rates when to = from
  # if (length(json$rates) == 0) json$rates <- NA
  
  # Return
  json
  
}



#' Function to get lastest exchange rate data from 
#' \href{http://fixer.io/}{Fixer.io}.
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_fixer_latest <- function(from = "usd", lower = TRUE) {
  
  from <- stringr::str_to_upper(from)
  url <- stringr::str_c("http://api.fixer.io/latest?base=", from)
  # Get response
  response <- readLines(url, warn = FALSE)
  
  # Parse
  json <- jsonlite::fromJSON(response, flatten = TRUE)
  
  # To table
  df <- data.frame(json)
  
  # Clean names
  names(df) <- stringr::str_replace_all(names(df), "rates.", "")
  
  # Make tidy data
  df <- tidyr::gather(df, to, value, -date, -base)
  
  # Transform and order
  df <- df %>% 
    mutate(date = lubridate::ymd(date, tz = "UTC")) %>% 
    select(date, 
           from = base, 
           to, 
           value)
  
  if (lower) {
    
    df <- df %>% 
      mutate(from = stringr::str_to_lower(from),
             to = stringr::str_to_lower(to))
    
  }
  
  # Return
  df
  
}


#' Function to get Fixer.io currency codes. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_fixer_codes <- function(lower = TRUE) {
  
  df <- get_fixer_latest(lower = lower)
  
  # As vector
  x <- df$to
  
  # Add usd too
  if (lower) extra <- "usd" else extra <- "USD"
  
  # Add 
  x <- c(extra, x)
  
  # Return
  x
  
}
