#' Function to format a data frame to a LaTeX table. 
#' 
#' \code{data_frame_to_latex_table} uses the \strong{xtable} package. 
#' 
#' @param df Input data frame. 
#' 
#' @param caption Text for LaTeX's table caption. 
#' 
#' @param label Label for LaTeX's table. 
#' 
#' @param caption_location Where to place the table's caption. 
#' 
#' @param digits Number of digits to print for numeric variables.  
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible character vector. 
#' 
#' @examples 
#' 
#' # Format a data frame 
#' data_frame_to_latex_table(ggplot2::mpg)
#' 
#' @export
data_frame_to_latex_table <- function(df, caption = "Example caption...", 
                                      label = "tab:", caption_location = "top",
                                      digits = NULL) {
  
  # Check input
  stopifnot(is.data.frame(df))
  
  # Use xtable here
  text <- xtable::xtable(
    df,
    caption = caption,
    label = label,
    digits = digits
  )
  
  # Final formatting
  text <- print(
    text, 
    include.rownames = FALSE, 
    caption.placement = caption_location
  )
  
  return(invisible(text))
  
}
