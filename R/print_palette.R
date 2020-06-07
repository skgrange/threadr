#' Function to plot a vector of colours. 
#' 
#' @author Stuart K. Grange and amd Karthik Ram. 
#' 
#' @param x Vector of colours. 
#' 
#' @seealso \url{https://github.com/karthik/wesanderson}
#' 
#' @return NULL.
#' 
#' @export
print_palette <- function(x) {
  
  n <- length(x)
  old <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(graphics::par(old))
  
  graphics::image(
    1:n, 1, as.matrix(1:n), col = x, ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )
  
  graphics::rect(0, 0.9, n + 1, 1.1, col = grDevices::rgb(1, 1, 1, 0.8), border = NA)
  graphics::text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
  
}
