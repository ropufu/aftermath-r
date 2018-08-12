
#' Logarithmically Spaced Vector
#'
#' This function creates a vector of logarithmically spaced elements.
#' @param from The first element of the vector.
#' @param to The last element of the vector.
#' @param n Number of elements in the vector. Defaults to 100.
#' @keywords matlab, linspace, seq
#' @export
#' @examples
#' logspace(1, 10, 8)

logspace <- function(from, to, n = 100) 
{
    lin <- seq(from = log10(from), to = log10(to), length.out = n);
    return(10 ^ lin);
}
