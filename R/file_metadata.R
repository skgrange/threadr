#' Function to extract file metadata with \code{exiftool}.
#' 
#' \code{file_metadata} uses Phil Harvey's Perl \code{exiftool} to extract a 
#' file's metadata and then formats the output into a table which can be 
#' conveniently used within R. 
#' 
#' \code{file_metadata} can be useful for media files and the exploration of 
#' metadata. 
#' 
#' All variables which are supported and returned by \code{exiftool} will be 
#' present. Watch out for very large numbers of variables when imputing large
#' XML or JSON documents with a large numbers of nested elements.
#' 
#' @param file A vector of files to extract metadata from. 
#' 
#' @param progress Should a progress bar be displayed? 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \href{http://www.sno.phy.queensu.ca/~phil/exiftool/}{exiftool}
#'
#' @examples
#' 
#' \dontrun{
#' # Get metadata file a single file
#' data_metadata <- file_metadata("music.mp3")
#' }
#'
#' @export
file_metadata <- function(file, progress = FALSE) {
  
  # Check for programme
  detect_exiftool()
  
  # Ensure path is expanded, sometimes this is necessary, then get metadata
  file %>% 
    fs::path_expand() %>% 
    purrr::map(
      purrr::in_parallel(
        function(x) 
        file_metadata_worker(x),
        file_metadata_worker = file_metadata_worker,
        str_to_underscore = str_to_underscore
      ),
      .progress = progress
    ) %>% 
    purrr::list_rbind() %>% 
    as_tibble() %>% 
    mutate(across(everything(), ~type.convert(., as.is = TRUE))) %>% 
    mutate(
      across(
        dplyr::matches("date"), 
        ~lubridate::ymd_hms(., tz = "UTC", truncated = 3, quiet = TRUE)
      )
    )
  
}


file_metadata_worker <- function(file) {
  
  # For parallel processing
  requireNamespace("dplyr")
  
  # Call system programme and do some minimal formatting
  processx::run("exiftool", args = c("-json", file)) %>% 
    .[["stdout"]] %>% 
    jsonlite::fromJSON() %>% 
    mutate(across(everything(), as.character)) %>% 
    dplyr::rename_with(str_to_underscore) %>% 
    dplyr::rename_with(~make.names(., unique = TRUE))
  
}


detect_exiftool <- function() {
  
  # Test
  text <- suppressWarnings(
    system("which exiftool", intern = TRUE, ignore.stderr = TRUE)
  )
  
  # Raise error if not installed
  if (length(text) == 0 || !grepl("exiftool", text)) {
    cli::cli_abort("'exiftool' system programme not detected.")
  }
  
  # No return
  
}
