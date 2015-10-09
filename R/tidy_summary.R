#' Function to create tidy data descriptives from a data frame. 
#' 
#' @param df Data frame for descriptives to be calculated from. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' data_summary <- tidy_summary(data_air)
#'
#' }
#' 
#' @export
#' 
tidy_summary <- function (df) {
  
  # Select only numeric variables
  df <- df[ , sapply(df, is.numeric)]
  
  # Summarise
  df_summary <- data.frame(summary(df))
  
  # Also sd
  sd <- lapply(df, sd, na.rm = TRUE)
  df_sd <- data.frame(data_variable = names(sd), 
                      summary_variable = "sd",
                      value = unlist(unname(lapply(sd, '[[', 1))))
  
  # Variance
  variance <- lapply(df, var, na.rm = TRUE)
  df_variance <- data.frame(data_variable = names(variance), 
                            summary_variable = "variance",
                            value = unlist(unname(lapply(variance, '[[', 1))))
  
  # Drop
  df_summary[, 1] <- NULL
  
  # Useful names
  names(df_summary) <- c("data_variable", "output")
  
  # Separate variables
  df_summary <- tidyr::separate(
    df_summary, output, c("summary_variable", "value"), sep = ":")
  
  # Bind summaries together
  df_summary <- rbind(df_summary, df_sd, df_variance)
  
  # Remove NAs
  df_summary <- df_summary[!is.na(df_summary$summary_variable), ]
  
  # Transform
  df_summary$value <- as.numeric(df_summary$value)
  df_summary$value <- round(df_summary$value, 3)
  
  df_summary$summary_variable <- tolower(df_summary$summary_variable)
  df_summary$summary_variable <- str_trim(df_summary$summary_variable)
  
  df_summary$summary_variable <- stringr::str_replace_all(
    df_summary$summary_variable, "\\.|'", "")
  
  df_summary$summary_variable <- stringr::str_replace_all(
    df_summary$summary_variable, " ", "_")
  
  df_summary$data_variable <- str_trim(df_summary$data_variable)
  
  # Make tidy data
  df_tidy <- tidyr::spread(df_summary, summary_variable, value)
  
  # Return
  df_tidy

}
