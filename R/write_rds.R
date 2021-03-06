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
#' @param verbose Should the function give messages? 
#' 
#' @return Invisible x.  
#' 
#' @export
write_rds <- function(x, file, compress = TRUE, verbose = FALSE) {
  
  # Check file
  if (!fs::path_ext(file) %in% c("rds", "RDS")) {
    stop("`file` must have the `.rds` or `.RDS` extension.", call. = FALSE)
  }
  
  # Export the object
  if (verbose) message(date_message(), "Exporting `.rds` object...")
  saveRDS(x, file = file, ascii = FALSE, compress = compress)
  
  # Message after the file have been written
  if (verbose) {
    message(date_message(), "Exported `", file, "` (`", fs::file_size(file), "`).")
  }
  
  return(invisible(x))
  
}


#' @rdname write_rds
#' @export
read_rds <- function(file, verbose = FALSE) {
  
  # Check file
  if (!fs::path_ext(file) %in% c("rds", "RDS")) {
    stop("`file` must have the `.rds` or `.RDS` extension.", call. = FALSE)
  }
  
  # Message the file name and size to user
  if (verbose) {
    message(date_message(), "Reading `", file, "` (`", fs::file_size(file), "`)...")
  }
  
  # Read the file
  x <- readRDS(file)
  
  return(x)

}
