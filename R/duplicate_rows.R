#' Function to return duplicated rows in a data frame. 
#' 
#' \code{duplicate_rows} can be though as the inverse of 
#' \code{\link{dplyr::distinct}}. 
#' 
#' @param df Data frame. 
#' @param variable A vector of variables to test for uniqueness.
#' 
#' @author Stuart K. Grange
#' 
#' @export
duplicate_rows <- function(df, variable) df[duplicated(df[, variable]), ]
