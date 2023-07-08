#' Function to get package versions and return details in a tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param package Vector of package names to get details for. \code{package} can
#' also be \code{"R"}. 
#' 
#' @param as_vector Should the package and version be formatted into a string? 
#' 
#' @return Tibble or character vector. 
#' 
#' @examples
#' 
#' # Get package details
#' get_package_version(c("stats", "utils"))
#' 
#' # Get package details along with R's version
#' get_package_version(c("R", "stats", "utils"))
#' 
#' # Get details and return a vector
#' get_package_version(c("R", "stats", "utils"), as_vector = TRUE)
#' 
#' @export
get_package_version <- function(package, as_vector = FALSE) {
  
  # Get package information and return a tibble
  df <- package %>% 
    purrr::map(get_package_version_worker) %>% 
    purrr::list_rbind()
  
  # Create and return a vector, not a good object name here...
  if (as_vector) {
    df <- df %>% 
      mutate(vector = stringr::str_c(package, ":", version)) %>% 
      pull(vector)
  }
  
  return(df)
  
}


get_package_version_worker <- function(package) {
  
  if (stringr::str_to_upper(package) == "R") {
    df <- get_r_version()
  } else if (package %in% rownames(installed.packages())) {
    df <- tibble::tibble(
      date_system = lubridate::now(),
      package = package,
      version = as.character(packageVersion(package)),
      date_package = lubridate::ymd(packageDate(package), tz = "UTC")
    )
  } else {
    df <- tibble::tibble()
  }
  
  return(df)
  
}


get_r_version <- function() {
  
  # Get version information
  list_version <- R.version
  
  # Create a tibble
  tibble::tibble(
    date_system = lubridate::now(),
    package = list_version$language,
    version = stringr::str_c(list_version$major, ".", list_version$minor),
    date_package = stringr::str_c(
      list_version$year, list_version$month, list_version$day
    )
  ) %>% 
    mutate(date_package = lubridate::ymd(date_package, tz = "UTC"))
  
}
