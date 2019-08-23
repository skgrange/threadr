#' Function to download files from a remote system with \code{scp} (secure copy). 
#' 
#' \code{download_with_scp} offers an alternative to the \code{curl} based 
#' functions which can be troublesome to use with the \code{sftp} protocol on 
#' Ubuntu systems. \code{download_with_scp} needs the \code{sshpass} system 
#' programme to be installed. 
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
#' @param compression Should the files be copied with compression on the fly? 
#' This can speed up copying time on slower networks but not always. 
#' 
#' @param verbose Should the file to be downloaded be printed to the console as 
#' a message? 
#' 
#' @param basename If \code{verbose}, should only the basename of file being 
#' handled be printed? 
#' 
#' @param quiet_streams Should the system streams be suppressed? 
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
#' @seealso \code{\link{list_files_scp}}, \href{https://gist.github.com/arunoda/7790979}{sshpass},
#' \code{\link{upload_with_scp}}
#' 
#' @export
download_with_scp <- function(host, file_remote, file_local, user, password,
                              compression = FALSE, verbose = FALSE, 
                              basename = FALSE, quiet_streams = FALSE) {
  
  # If nothing is passed, just skip
  if (!length(file_remote) == 0) {
    
    # Checks
    stopifnot(length(file_remote) == length(file_local))
    
    if (!sshpass_install_check()) {
      stop("`sshpass` system programme is not installed...", call. = FALSE)
    }
    
    # Add host to file remote
    file_remote <- stringr::str_c(host, ":", file_remote)
    
    # Do
    purrr::walk2(
      .x = file_local,
      .y = file_remote,
      .f = ~ download_with_scp_worker(
        file_local = .x,
        file_remote = .y,
        user = user,
        password = password,
        compression = compression,
        verbose = verbose,
        basename = basename,
        quiet_streams = quiet_streams
      )
    )
    
  }
  
  # message("`file_remote` has length of 0, nothing has been downloaded...")
  
  # No return
  
}


download_with_scp_worker <- function(file_local, file_remote, user, password, 
                                     compression, verbose, basename, 
                                     quiet_streams) {
  
  # Build system command
  command_prefix <- stringr::str_c(
    "sshpass -p '", password, "' scp -p ", user, "@"
  )
  
  # Add compression argument
  if (compression) {
    command_prefix <- stringr::str_replace(command_prefix, "scp -p", "scp -pC")
  }
  
  # And file
  command_files <- stringr::str_c(file_remote, file_local, sep = " ")
  
  # Combine commands
  command <- stringr::str_c(command_prefix, command_files)
  
  # A message to the user
  if (verbose) {
    message(download_with_scp_message(file_remote, basename = basename))
  }
  
  if (quiet_streams) {
    ignore.stdout <- TRUE
    ignore.stderr <- TRUE
  } else {
    ignore.stdout <- FALSE
    ignore.stderr <- FALSE
  }
  
  # Do
  system(command, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  
}


#' Function to list files and directories on a remote system with \code{scp} 
#' (secure copy). 
#' 
#' \code{list_files_scp} offers an alternative to the \code{curl} based 
#' functions which can be troublesome to use with the \code{sftp} protocol on 
#' Ubuntu systems. \code{download_with_scp} needs the \code{sshpass} system 
#' programme to be installed.  
#' 
#' @param host Host name of remote system. 
#' 
#' @param directory_remote A remote directory to list files from. 
#' 
#' @param user User name for \code{scp}. 
#' 
#' @param password Password for the user for \code{scp}. 
#' 
#' @param method Either \code{"scp"} or \code{"rsync"}. Using \code{"rsync"} can
#' be useful when the remote server does not allow scp commands. 
#' 
#' @param quiet_streams Should the system streams be suppressed? 
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
#' @seealso \code{\link{download_with_scp}}, \href{https://gist.github.com/arunoda/7790979}{sshpass},
#' \code{\link{upload_with_scp}}
#' 
#' @export
list_files_scp <- function(host, directory_remote, user, password, 
                           method = "scp", quiet_streams = FALSE) {
  
  if (!sshpass_install_check()) {
    stop("`sshpass` system programme is not installed...", call. = FALSE)
  }
  
  # Used in logic
  method <- stringr::str_to_lower(method)
  
  # For system call
  ignore.stderr <- ifelse(quiet_streams, TRUE, FALSE)
  
  if (method == "scp") {
    
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
    file_list <- system(command, intern = TRUE, ignore.stderr = ignore.stderr)
    
  } else if (method == "rsync") {
    
    # Ensure remote has a slash
    directory_remote <- stringr::str_c(directory_remote, "/")
    
    command <- stringr::str_c(
      "sshpass -p '", 
      password, 
      "' rsync --list-only ", 
      user, 
      "@", 
      host, 
      ":", 
      directory_remote
    )
    
    # Do
    file_list <- system(command, intern = TRUE, ignore.stderr = ignore.stderr)
    
    # Use the collon in the date as an anchor, certainly not perfect
    file_list <- stringr::str_split_fixed(file_list, ":", 3)[, 3]
    file_list <- stringr::str_split_fixed(file_list, " ", 2)[, 2]
    
    # Drop working directory
    file_list <- file_list[file_list != "."]
    
    # Add path
    file_list <- stringr::str_c(directory_remote, file_list)
    
  } else {
    warning("`method` not recognised...", call. = FALSE)
    file_list <- character()
  }
  
  return(file_list)
  
}


# Test if sshpass is installed
sshpass_install_check <- function() {
  
  # System call
  suppressWarnings(
    x <- system("which sshpass", intern = TRUE)
  )
  
  # Test
  x <- if (length(x) == 0) {
    x <- FALSE
  } else if (grepl("sshpass", x, ignore.case = TRUE)) {
    x <- TRUE
  } else {
    x <- FALSE
  }

  return(x)
  
}


#' Function to upload files locally to a remote system with \code{scp} (secure 
#' copy). 
#' 
#' \code{upload_with_scp} offers an alternative to the \code{curl} based 
#' functions which can be troublesome to use with the \code{sftp} protocol on 
#' Ubuntu systems. \code{upload_with_scp} needs the \code{sshpass} system 
#' programme to be installed. 
#' 
#' @param host Host name of remote system. 
#' 
#' @param file_local A vector of file names which are the destination files for
#' \code{file_remote}. 
#' 
#' @param file_remote A vector of remote files names to download. 
#' 
#' @param user User name for \code{scp}. 
#' 
#' @param password Password for the user for \code{scp}. 
#' 
#' @param compression Should the files be copied with compression on the fly? 
#' This can speed up copying time on slower networks but not always. 
#' 
#' @param verbose Should the file to be uploaded be printed to the console as a
#' message? 
#' 
#' @param basename If \code{verbose}, should only the basename of file being 
#' handled be printed? 
#' 
#' @param quiet_streams Should the system streams be suppressed? 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get a file
#' upload_with_scp(
#'   host = "192.1.1.1",
#'   file_local = "~/Desktop/sock_data_copied.rds",
#'   file_remote = "/network_storage/r_files/sock_data.rds", 
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
#' @seealso \code{\link{list_files_scp}}, \href{https://gist.github.com/arunoda/7790979}{sshpass},
#' \code{\link{download_with_scp}}
#' 
#' @export
upload_with_scp <- function(host, file_local, file_remote, user, password,
                            compression = FALSE, verbose = FALSE, 
                            basename = FALSE, quiet_streams = FALSE) {
  
  # If nothing is passed, just skip
  if (!length(file_local) == 0) {
    
    # Checks
    stopifnot(length(file_remote) == length(file_local))
    
    if (!sshpass_install_check()) {
      stop("`sshpass` system programme is not installed...", call. = FALSE)
    }
    
    # Add user and host to file remote
    file_remote <- stringr::str_c(user, "@", host, ":", file_remote)
    
    # Do
    purrr::walk2(
      .x = file_remote,
      .y = file_local,
      .f = ~ upload_with_scp_worker(
        file_remote = .x,
        file_local = .y,
        user = user,
        password = password,
        compression = compression,
        verbose = verbose,
        basename = basename,
        quiet_streams = quiet_streams
      )
    )
    
  }
  
  # message("`file_local` has length of 0, nothing has been uploaded...")
  
  # No return
  
}


upload_with_scp_worker <- function(file_remote, file_local, user, password, 
                                   compression, verbose, basename, 
                                   quiet_streams) {
  
  # Build system command
  command_prefix <- stringr::str_c("sshpass -p '", password, "' scp ")
  
  # Add compression argument
  if (compression) {
    command_prefix <- stringr::str_replace(command_prefix, "\\bscp\\b", "scp -C")
  }
  
  # Add files, the local one
  command_files <- stringr::str_c(file_local, file_remote, sep = " ")
  
  # Combine commands
  command <- stringr::str_c(command_prefix, command_files)
  
  # A message to the user
  if (verbose) {
    message(download_with_scp_message(file_remote, basename = basename))
  }
  
  if (quiet_streams) {
    ignore.stdout <- TRUE
    ignore.stderr <- TRUE
  } else {
    ignore.stdout <- FALSE
    ignore.stderr <- FALSE
  }
  
  # Do
  system(command, ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  
}


download_with_scp_message <- function(file, basename) {
  
  # Basename only?
  file <- if_else(basename, basename(file), file)
  
  # Build string
  message_string <- stringr::str_c(str_date_formatted(), ": `", file, "`...")
  
  return(message_string)
  
}
