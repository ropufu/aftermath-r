
#' CUSUM Statistic Class
#' 
#' This class describes a parallel (GLR) CUSUM statistic.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format A parallel CUSUM statistic \code{\link{R6Class}} generator object
#' @keywords cusum, parallel
#' @examples
#' ch <- changept::change_norm(size = 4);
#' a1 <- c(TRUE, TRUE, FALSE, TRUE);
#' a2 <- c(FALSE, TRUE, FALSE, FALSE);
#' cs <- stat_parallel_cusum_type$new(ch, list(a1, a2));
stat_parallel_cusum_type <- R6::R6Class("stat_parallel_cusum_type",
    public = list(
        #' R6ChangeNorm Constructor
        #' 
        #' @param change Change-point descriptor object.
        #' @param possibly_affected Indicates affected subset possibilities. Either list of 1-by-\code{size} logical vectors, or an \code{n}-by-\code{size} logical matrix.
        #' @param weights Weights for the underlying statistics. A \code{count}-dimensional vector.
        #' @param monte_carlo_count Number of monte-carlo statistics to run in parallel. Defaults to 1.
        #' @keywords cusum, parallel, constructor
        #' @export
        #' @examples
        #' 
        initialize = function(change, possibly_affected, weights, monte_carlo_count = 1)
        {
            if (is.list(possibly_affected))
            {
                private$m_size  <- length(possibly_affected[[1]]); # Number of sensors. 
                private$m_count <- length(possibly_affected);      # Number of possibilities.
                # Convert the list to a matrix.
                private$m_possibly_affected <- matrix(unlist(possibly_affected), private$m_count, private$m_size, byrow = TRUE);
            }
            else if (is.matrix(possibly_affected))
            {
                d <- dim(possibly_affected);
                private$m_size  <- d[2]; # Number of sensors. 
                private$m_count <- d[1]; # Number of possibilities.
                private$m_possibly_affected <- possibly_affected; # Copy the matrix.
            }
            else stop("<possibly_affected> should be either a list or a matrix");
            
            private$m_monte_carlo_count <- monte_carlo_count;
            
            if (missing(weights))  weights <- rep(1.0, times = private$m_count);
            if (any(weights <= 0)) stop("<weights> should be positive");
            private$m_log_weights <- log(weights / sum(weights));
            
            reader <- function(i) { return(private$m_possibly_affected[i, ]); }
            ctor <- change$loglikelihood_constructor(reader, count = private$m_count);
            private$m_loglikelihood_constructor <- ctor;
            
            private$m_underlying_current <- matrix(rep(0.0, times = (private$m_count * monte_carlo_count)), private$m_count, monte_carlo_count);
            private$m_current <- rep(0.0, times = monte_carlo_count);
        },
        
        #' Resets the value of the statistics.
        #' 
        #' @param at A \code{monte_carlo_count}-dimensional vector.
        #' 
        reset = function(at) 
        {
            if (missing(at))
            {
                private$m_underlying_current <- matrix(rep(0.0, times = (private$m_count * private$m_monte_carlo_count)), private$m_count, private$m_monte_carlo_count);
                private$m_current <- rep(0.0, times = private$m_monte_carlo_count);
            }
            else
            {
                private$m_underlying_current[, at] <- 0.0;
                private$m_current[at] <- 0.0;
            }
        },
        
        #' Observe a Log-likelihood Basis
        #'
        #' Update the CUSUM statistic with a new instantaneous basis log-likelihood ratio.
        #' @param loglikelihood_basis Instantaneous basis log-likelihood ratio. A \code{size}-by-\code{n} matrix, where \code{n = sum(at)}.
        #' @param at Mask indicating which monte-carlo threads to update. A \code{monte_carlo_count}-dimensional logical vector. Defaults to repeated TRUE.
        #' @keywords observe, log-likelihood, loglikelihood
        #' @export
        #' @examples
        #' 
        observe_basis = function(loglikelihood_basis, at = rep(TRUE, times = private$m_monte_carlo_count))
        {
            if (!is.matrix(loglikelihood_basis)) dim(loglikelihood_basis) <- c(length(loglikelihood_basis), 1);
            n <- dim(loglikelihood_basis)[2];
            if (n != sum(at)) stop("Size mismatch");
            
            loglikelihood <- private$m_loglikelihood_constructor(loglikelihood_basis); # A {count}-by-{monte_carlo_count} matrix.

            st <- private$m_underlying_current[, at];
            st[st < 0.0] <- 0.0;
            st <- st + loglikelihood;
            
            # Update the underlying statistic.
            private$m_underlying_current[, at] <- st;
            # Update the global statistic.
            for (j in 1 : private$m_monte_carlo_count)
            {
                if (at[j]) private$m_current[j] <- max(private$m_underlying_current[, j] + private$m_log_weights);
            }
            
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
        #' Dimensions of the problem (number of sensors). Read-only.
        size              = function() { return(private$m_size); },
        #' Number of possibly affected subsets. Read-only.
        count             = function() { return(private$m_count); },
        #' Number of monte-carlo statistics to run in parallel. Read-only.
        monte_carlo_count = function() { return(private$m_monte_carlo_count); },
        #' Mask indicating possibly affected subsets. A \code{count}-by-\code{size} logical matrix. Read-only.
        possibly_affected = function() { return(private$m_possibly_affected); },
        #' Logarithms of proper weights for the underlying statistics. A \code{count}-dimensional vector. Read-only.
        log_weights       = function() { return(private$m_log_weights); },
        #' Log-likelihood ratio constructor. Read-only.
        loglikelihood_constructor = function() { return(private$m_loglikelihood_constructor); }
    ),

    private = list(
        #' Dimensions of the problem (number of sensors).
        m_size = 0,
        #' Number of possibly affected subsets.
        m_count = 0,
        #' Number of monte-carlo statistics to run in parallel.
        m_monte_carlo_count = 0,
        #' Mask indicating possibly affected subsets. A \code{count}-by-\code{size} logical matrix.
        m_possibly_affected = matrix(numeric(0), 0, 0),
        #' Logarithms of proper weights for the underlying statistics. A \code{count}-dimensional vector.
        m_log_weights = numeric(0),
        #' Log-likelihood ratio constructor.
        m_loglikelihood_constructor = NULL,
        #' Current values of the underlying statistics. A \code{count}-by-\code{monte_carlo_count} matrix.
        m_underlying_current = matrix(numeric(0), 0, 0),
        #' Current value of the statistics. A \code{monte_carlo_count}-dimensional vector.
        m_current = numeric(0)
    )
)

#' Creates an \code{\link{stat_parallel_cusum_type}} object.
#'
#' This function initializes an instance of \code{\link{stat_parallel_cusum_type}} class.
#' @param change Change-point descriptor object.
#' @param possibly_affected Indicates affected subset possibilities. Either list of 1-by-\code{size} logical vectors, or an \code{n}-by-\code{size} logical matrix.
#' @param weights Weights for the underlying statistics. A \code{count}-dimensional vector.
#' @param monte_carlo_count Number of monte-carlo statistics to run in parallel. Defaults to 1.
#' @keywords cusum, parallel
#' @export
#' @examples
#' ch <- changept::change_norm(size = 4);
#' a1 <- c(TRUE, TRUE, FALSE, TRUE);
#' a2 <- c(FALSE, TRUE, FALSE, FALSE);
#' cs <- stat_parallel_cusum(ch, list(a1, a2));
stat_parallel_cusum <- function(change, possibly_affected, weights, monte_carlo_count = 1) 
{
    return(stat_parallel_cusum_type$new(change, possibly_affected, weights, monte_carlo_count));
}

