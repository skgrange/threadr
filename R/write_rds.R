#' Function to read and write R's RDS files with some helpful messaging. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x An R object to write to disc. 
#' 
#' @param file File name of RDS file. 
#' 
#' @param compress Should the RDS file be written with compression? 
#' 
#' @param file_only_print If \code{verbose} is \code{TRUE}, should only the file
#' name be printed without its path? 
#' 
#' @param verbose Should the function give messages? Note that the default is 
#' \code{TRUE}. 
#' 
#' @return Invisible \code{x}.  
#' 
#' @export
write_rds <- function(x, file, compress = TRUE, file_only_print = FALSE, 
                      verbose = TRUE) {
  
  # Check file
  if (!fs::path_ext(file) %in% c("rds", "RDS")) {
    cli::cli_abort("`file` must have the `.rds` or `.RDS` extension.")
  }
  
  # Export the object
  if (verbose) cli::cli_alert_info("{cli_date()} Exporting `.rds` object...")
  saveRDS(x, file = file, ascii = FALSE, compress = compress)
  
  # Message after the file have been written
  if (verbose) {
    
    # What to print? 
    if (file_only_print) {
      file_print <- basename(file)
    } else {
      file_print <- file
    }
    
    cli::cli_alert_success("{cli_date()} Exported `{file_print}` ({fs::file_size(file)}).")
    
  }
  
  return(invisible(x))
  
}


#' @rdname write_rds
#' @export
read_rds <- function(file, file_only_print = FALSE, verbose = TRUE) {
  
  # Check file
  if (!fs::file_exists(file)) {
    cli::cli_abort("`file` does not exist.")
  }
  
  if (!fs::path_ext(file) %in% c("rds", "RDS")) {
    cli::cli_abort("`file` must have the `.rds` or `.RDS` extension.")
  }
  
  # Message the file name and size to user
  if (verbose) {
    
    # What to print? 
    if (file_only_print) {
      file_print <- basename(file)
    } else {
      file_print <- file
    }
    
    cli::cli_alert_info("{cli_date()} Reading `{file_print}` ({fs::file_size(file)})...")
    
  }
  
  # Read the file
  x <- readRDS(file)
  
  return(x)

}
