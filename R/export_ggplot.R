#' Function to export \strong{ggplot2} plot, usually as a \code{.pdf} file.
#' 
#' @param file File name to export plot to, usually as a \code{.pdf} file.
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
#' @param to_png Should the file be converted to a \code{.png} file too? 
#' 
#' @param png_resolution If \code{to_png} is \code{TRUE}, what resolution (in 
#' dpi) should be used? 
#' 
#' @param device Device to use. Use \code{cairo_pdf} (no quotes) to use the 
#' \strong{cairo} library.
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible \strong{ggplot2} object. 
#' 
#' @export
export_ggplot <- function(file = NA, plot = ggplot2::last_plot(), width = 6, 
                          height = 5, crop = FALSE, to_png = FALSE, 
                          png_resolution = 320, device = NULL, verbose = FALSE) {
  
  # Switch if no file name is given
  if (is.na(file[1])) {
    file <- fs::path_expand("~/Desktop/r_plot_export.pdf")
  } else {
    file <- fs::path_expand(file)
  }
  
  # Stop if length of file is not 1
  if (length(file) != 1L) {
    stop("`file` must have a length of 1.", call. = FALSE)
  }
  
  # Message if not a pdf
  if (!fs::path_ext(file) %in% c("pdf", "PDF")) {
    message("`file` is not a `.pdf` file...")
  }
  
  # Message
  if (verbose) message(date_message(), "Exporting `", file, "`...")
  
  # Save plot
  ggplot2::ggsave(
    plot, 
    filename = file,
    width = width,
    height = height,
    device = device
  )
  
  # Crop
  if (verbose) message(date_message(), "Cropping `", file, "`...")
  if (crop) systemr::pdf_crop(file, file)
  
  # Convert to png
  if (verbose) message(date_message(), "Converting `", file, "`...")
  if (to_png) systemr::pdf_to_png(file, resolution = png_resolution)
  
  return(invisible(file))
  
}
