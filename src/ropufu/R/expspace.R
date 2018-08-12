
#' Exponentially Spaced Vector
#'
#' This function creates a vector of exponentially spaced elements.
#' @param from The first element of the vector.
#' @param to The last element of the vector.
#' @param n Number of elements in the vector. Defaults to 100.
#' @keywords matlab, linspace, seq
#' @export
#' @examples
#' expspace(1, 10, 8)

expspace <- function(from, to, n = 100) 
{
    lin <- seq(from = (10 ^ from), to = (10 ^ to), length.out = n);
    return(log10(lin));
}
