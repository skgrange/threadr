#' Function to select priority variables from the return of 
#' \code{fs::file_info}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame returned by \code{fs::file_info}. 
#' 
#' @return \code{df} with selected variables. 
#' 
#' @export
select_fs_file_info <- function(df) {
  
  # Check if the table has been returned by fs::file_info
  stopifnot(c("path", "type", "size", "modification_time") %in% names(df))
  
  # Add file and select some variables
  df %>% 
    mutate(file = basename(path)) %>% 
    select(path,
           file,
           type,
           size,
           modification_time)
  
}
