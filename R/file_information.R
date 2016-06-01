#' Function to get file information. 
#' 
#' \code{file_infomation} uses \code{file.info} but cleans up the returned data
#' frame a little. 
#' 
#' @author Stuart K. Grange
#' 
#' @param files Vector of file names.
#' @param drop Should some usually unneeded variables be dropped? Default is 
#' \code{FALSE}. 
#' 
#' @return Tidy data frame
#' 
#' @export
file_information <- function(files, drop = FALSE) {
  
  # Get information
  df <- file.info(files)
  
  # Tidy a little
  df <- add_rownames(df, "file") %>% 
    mutate(file_basename = basename(file)) %>% 
    arrange_left(c("file", "file_basename"))
  
  # Drop variables
  if (drop) df$file <- NULL
  
  # Return
  df
  
}
