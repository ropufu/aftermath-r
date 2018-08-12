
#' Linearly Spaced Vector
#'
#' This function creates a vector of linearly spaced elements.
#' @param from The first element of the vector.
#' @param to The last element of the vector.
#' @param n Number of elements in the vector. Defaults to 100.
#' @keywords matlab, linspace, seq
#' @export
#' @examples
#' linspace(1, 10, 8)

linspace <- function(from, to, n = 100) 
{
    return(seq(from = from, to = to, length.out = n));
}
