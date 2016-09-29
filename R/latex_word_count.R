#' Function to count words and characters in a \code{LaTeX} (\code{.tex}) 
#' document. 
#' 
#' @param file File name of \code{.tex} file(s). 
#' @param json Should the return be pretty printed JSON? 
#' 
#' @return Data frame or pretty printed JSON.
#' 
#' @author Stuart K. Grange
#' 
#' @export
latex_word_count <- function(file, json = FALSE) {
  
  # Do
  df <- plyr::ldply(file, latex_word_count_worker)
  
  df <- type_convert(df)
  
  # To json
  if (json) df <- to_json(df)
  
  # Return
  df
  
}


latex_word_count_worker <- function(file) {
  
  # Load file
  text <- readLines(file)
  
  # Get basename
  file_base <- basename(file)
  
  # Do some counting
  text_info <- stringi::stri_stats_latex(text)
  
  # Add file name
  text_info <- c(file_base, text_info)
  
  # Clean names
  names <- names(text_info)
  names <- str_to_underscore(names)
  
  # Switches
  names <- ifelse(names == "", "file", names)
  names <- ifelse(names == "chars_word", "characters_words", names)
  names <- ifelse(names == "chars_cmd_envir", "characters_words_and_commands", names)
  names <- ifelse(names == "chars_white", "latex_white_spaces", names)
  names <- ifelse(names == "words", "word_count", names)
  names <- ifelse(names == "cmds", "number_of_commands", names)
  names <- ifelse(names == "envirs", "number_of_environments", names)
  
  # Give vector new name
  names(text_info) <- names
  
  # To data frame
  df <- data.frame(t(text_info))
  
  # Return
  df
  
}
