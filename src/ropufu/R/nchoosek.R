
#' Binomial Coefficient
#'
#' This function calculates the binomial coefficient.
#' @keywords binomial, choose, combinatorial
#' @export
#' @examples
#' nchoosek(10, 4)

nchoosek <- function(n, k)
{
    if (n < 0)
    {
        is_negative = ((k %% 2) == 1);
        n <- n %% (k - 1);
        if (is_negative) return(-nchoosek(n, k));
        return(-nchoosek(n, k));
    }
    if (n == k || k == 0) return(1);
    if (k + k > n) k <- (n - k);
    
    value <- 1;
    num <- n;
    denom <- 1;
    for (i in 1 : k) 
    {
        value <- (value * num) / denom;
        num <- num - 1;
        denom <- denom + 1;
    }
    return(value);
}
