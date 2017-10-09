#' Function to download files from a remote system with \code{scp} (secure copy). 
#' 
#' \code{download_with_scp} offers an alternative to the \code{curl} based 
#' functions which can be troublesome to use with the \code{sftp} protocol. 
#' 
#' @param host Host name of remote system. 
#' 
#' @param file_remote A vector of remote files names to download. 
#' 
#' @param file_local A vector of file names which are the destination files for
#' \code{file_remote}. 
#' 
#' @param user User name for \code{scp}. 
#' 
#' @param password Password for the user for \code{scp}. 
#' 
#' @param Type of progress bar to display. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get a file
#' download_with_scp(
#'   host = "192.1.1.1",
#'   file_remote = "/network_storage/r_files/sock_data.rds", 
#'   file_local = "~/Desktop/sock_data_copied.rds",
#'   user = "username",
#'   password = "password_for_username"
#' )
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible.
#' 
#' @seealso \code{\link{list_files_scp}}
#' 
#' @export
download_with_scp <- function(host, file_remote, file_local, user, password,
                              compression = FALSE, progress = "none") {
  
  # Check
  stopifnot(length(file_remote) == length(file_local))
  
  # Add host to file remote
  file_remote <- stringr::str_c(host, ":", file_remote)
  
  # Build mapping data frame
  df <- data.frame(
    file_remote,
    file_local, 
    stringsAsFactors = FALSE
  )
  
  # Do 
  plyr::a_ply(
    df, 
    .margins = 1,
    function(x) download_with_scp_worker(
      x,
      user = user,
      password = password,
      compression = compression
    ),
    .progress = progress
  )
  
  # No return
  
}


download_with_scp_worker <- function(df, user, password, compression) {
  
  # Build system command
  command_prefix <- stringr::str_c("sshpass -p '", password, "' scp ", user, "@")
  
  # Add compression argument
  if (compression) 
    command_prefix <- stringr::str_replace(command_prefix, "\\bscp\\b", "scp -C")
  
  # And file
  command_files <- stringr::str_c(df$file_remote, df$file_local, sep = " ")
  
  # Combine commands
  command <- stringr::str_c(command_prefix, command_files)
  
  # Do
  system(command)
  
}


# # Build system call command
# # if (cypher == "arcfour") {
#   
#   # # Fastest cypher in most cases
#   # command_prefix <- stringr::str_c(
#   #   "sshpass -p '", 
#   #   password, 
#   #   "' scp ", "Cipher=arcfour ", 
#   #   user, 
#   #   "@"
#   # )
#   
#   
# } else {
# 
#   # Default
#   
#   
# }


#' Function to list files and directories on a remote system with \code{scp} 
#' (secure copy). 
#' 
#' \code{list_files_scp} offers an alternative to the \code{curl} based 
#' functions which can be troublesome to use with the \code{sftp} protocol. 
#' 
#' @param host Host name of remote system. 
#' 
#' @param directory_remote A remote directory to list files from. 
#' 
#' @param user User name for \code{scp}. 
#' 
#' @param password Password for the user for \code{scp}. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # List contents of a directory
#' list_files_scp(
#'   host = "192.1.1.1", 
#'   directory_remote = "/network_storage/r_files/",
#'   user = "username",
#'   password = "password_for_username"
#' )
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector.
#' 
#' @seealso \code{\link{download_with_scp}}
#' 
#' @export
list_files_scp <- function(host, directory_remote, user, password) {
  
  # Ensure remote has a slash and a wild card
  directory_remote <- stringr::str_c(directory_remote, "/*")
  
  # Build system call command, ssh, not scp
  command <- stringr::str_c(
    "sshpass -p '", 
    password, 
    "' ssh ", 
    user, 
    "@", 
    host, 
    " ls -d -1 ", 
    directory_remote
  )
  
  # Do
  file_list <- system(command, intern = TRUE)
  
  return(file_list)
  
}
