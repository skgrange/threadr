#' Function to export \strong{ggplot2} plot, usually as a \code{.pdf} file.
#' 
#' @param file File name to export plot to. 
#' 
#' @param plot \strong{ggplot2} object. If not used, the last plot will be 
#' retrieved. 
#' 
#' @param width Plot width in inches. 
#' 
#' @param height Plot height in inches. 
#' 
#' @param crop Should the output be cropped? \strong{systemr} needs to be 
#' installed if \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \strong{ggplot2} object. 
#' 
#' @export
export_ggplot <- function(file = NA, plot = ggplot2::last_plot(), width = 6, 
                          height = 5, crop = FALSE) {
  
  # Switch if no file name is given
  if (is.na(file[1])) file <- fs::path_expand("~/Desktop/r_plot_export.pdf")
  
  # Message if not a pdf
  if (!fs::path_ext(file) %in% c("pdf", "PDF")) {
    message("`file` is not a `.pdf` file...")
  }
  
  # Save plot
  ggplot2::ggsave(
    plot, 
    filename = file,
    width = width,
    height = height
  )
  
  # Crop
  if (crop) systemr::pdf_crop(file, file)
  
  return(invisible(plot))
  
}
