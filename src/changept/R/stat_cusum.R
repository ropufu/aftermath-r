
#' CUSUM Statistic Class
#' 
#' This class describes a scalar CUSUM statistic.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format A scalar CUSUM statistic \code{\link{R6Class}} generator object
#' @keywords cusum, scalar
#' @examples
#' cs <- stat_cusum_type$new()
stat_cusum_type <- R6::R6Class("stat_cusum_type",
    public = list(
        #' stat_cusum_type Constructor
        #' 
        #' @param monte_carlo_count Number of monte-carlo statistics to run in parallel. Defaults to 1.
        #' @keywords cusum, scalar, constructor
        #' @export
        #' @examples
        #' 
        initialize = function(monte_carlo_count = 1) 
        {
            if (monte_carlo_count <= 0) stop("argument out of range");
            
            private$m_monte_carlo_count <- monte_carlo_count;
            private$m_current <- rep(0.0, times = monte_carlo_count);
        },
        
        #' Resets the value of the statistics.
        #' 
        #' @param at A \code{monte_carlo_count}-dimensional vector.
        #' 
        reset = function(at) 
        { 
            if (missing(at)) { private$m_current <- rep(0.0, times = private$m_monte_carlo_count); }
            else private$m_current[at] <- 0.0;
        },
    
        #' Observe a Scalar Log-likelihood
        #'
        #' Update the CUSUM statistic with a new instantaneous log-likelihood ratio.
        #' @param loglikelihood Instantaneous log-likelihood ratios. An \code{n}-dimensional vector, where \code{n = sum(at)}.
        #' @param at Mask indicating which monte-carlo threads to update. A \code{monte_carlo_count}-dimensional logical vector. Defaults to repeated TRUE.
        #' @keywords observe, log-likelihood, loglikelihood
        #' @export
        #' @examples
        #' 
        observe = function(loglikelihood, at = rep(TRUE, times = private$m_monte_carlo_count))
        {
            st <- private$m_current[at];
            st[st < 0.0] <- 0.0;
            st <- st + loglikelihood;
            
            # Update the statistic.
            private$m_current[at] <- st;
            
            return(self);
        },
        
        #' Current value of the statistics. A \code{monte_carlo_count}-dimensional vector. Read-only.
        #' 
        #' @param at A \code{monte_carlo_count}-dimensional vector.
        #' 
        current = function(at)
        {
            if (missing(at)) return(private$m_current); 
            return(private$m_current[at]); 
        },
        
        print = function() { return("cusum"); }
    ),
    
    active = list(
        #' Number of monte-carlo statistics to run in parallel. Read-only.
        monte_carlo_count = function() { return(private$m_monte_carlo_count); }
    ),

    private = list(
        #' Number of monte-carlo statistics to run in parallel.
        m_monte_carlo_count = 0,
        #' Current value of the statistics. A \code{monte_carlo_count}-dimensional vector.
        m_current = numeric(0)
    )
)

#' Creates an \code{\link{stat_cusum_type}} object.
#'
#' This function initializes an instance of \code{\link{stat_cusum_type}} class.
#' @param monte_carlo_count Number of monte-carlo statistics to run in parallel. Defaults to 1.
#' @keywords cusum
#' @export
#' @examples
#' cs <- stat_cusum()
stat_cusum <- function(monte_carlo_count = 1) 
{
    return(stat_cusum_type$new(monte_carlo_count));
}

