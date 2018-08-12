
#' Fills Region of a Plot
#'
#' This function fills a region of the plot.
#' @param x1 Vector of x-coordinates for the first curve.
#' @param y1 Vector of y-coordinates for the first curve.
#' @param x2 Vector of x-coordinates for the second curve.
#' @param y2 Vector of y-coordinates for the second curve.
#' @keywords plot, fill, polygon
#' @export
#' @examples
#' plot(1:10, 1:10)
#' fill(1:10, 1 + (1:10)^0.8, 1:10, sqrt(1:10), col = "gray", border = NA)

fill <- function(x1, y1, x2, y2, ...) 
{
    polygon(c(x1, rev(x2)), c(y1, rev(y2)), ...);
}
