
#' Generate Normal Random Variates
#'
#' This function generates a sequence of vector observations.
#' @param n Number of observations to generate. Defaults to 1.
#' @param size Height of each observation vector. Defaults to 1.
#' @param mean 1-by-size vector of means. Defaults to 0.
#' @param covar size-by-size covariance matrix of observations. Defaults to <id>.
#' @keywords observe
#' @export
#' @examples
#' observe.norm(100)
observe_norm <- function(n = 1, size = 1, mean = rep(0.0, times = size), covar = diag(size))
{
    a <- chol(covar);
    z <- matrix(stats::rnorm(n * size), size, n);
    m <- matrix(replicate(n, mean), size, n);
  
    x <- m + a %*% z;
    return(x);
}
