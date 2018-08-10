#' Function to plot a JPG/JPEG image. 
#' 
#' @param file Image file name. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, a plot. 
#' 
#' @export
plot_image_jpg <- function(file) {
  
  image <- jpeg::readJPEG(file, native = TRUE)
  resolution <- dim(image)[1:2] 
  
  # Set plot parameters, no margins around image
  graphics::par(mar = c(0, 0, 0, 0))
  
  # Create graphics device
  graphics::plot(
    1, 1, 
    xlim = c(1, resolution[2]), 
    ylim = c(1, resolution[1]), 
    asp = 1, type = "n", xaxs = "i", yaxs= "i", xaxt = "n", yaxt = "n", 
    xlab = "", ylab = "", bty = "n"
  )
  
  # Add layer
  graphics::rasterImage(image, 0, 0, resolution[2], resolution[1])
  
  # No return
  
}
