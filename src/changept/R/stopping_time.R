
#' Stopping Time Class
#' 
#' This class describes a one-sided stopping time.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format A one-sided stopping time \code{\link{R6Class}} generator object
#' @keywords stopping time, one-sided
#' @examples
#' 
stopping_time_type <- R6::R6Class("stopping_time_type",
    public = list(
        #' stopping_time_type Constructor
        #' 
        #' @param statistic The underlying statistic.
        #' @param thresholds A collection of thresholds to test the statistic against.
        #' @keywords stopping time, one-sided, constructor
        #' @export
        #' @examples
        #' 
        initialize = function(statistic, thresholds) 
        {
            m <- statistic$monte_carlo_count;
            n <- length(thresholds);
            
            private$m_count_thresholds <- n;
            private$m_statistic  <- statistic;
            private$m_thresholds <- thresholds;
            
            private$m_is_running <- rep(TRUE, times = m);
            private$m_has_crossed <- matrix(rep(FALSE, times = m * n), m, n);
            private$m_run_length  <- matrix(rep(0, times = m * n), m, n);
        },
        
        #' Resets the statistic and something else.
        #' 
        reset = function()
        {
            m <- private$m_statistic$monte_carlo_count;
            n <- private$m_count_thresholds;
            
            private$m_time <- 0;
            private$m_statistic$reset();
            
            private$m_is_running <- rep(TRUE, times = m);
            private$m_has_crossed <- matrix(rep(FALSE, times = m * n), m, n);
            private$m_run_length  <- matrix(rep(0, times = m * n), m, n);
        },
        
        observe_basis = function(llr_basis, mask)
        {
            #' Don't do anything if every thread has stopped.
            if (!any(private$m_is_running)) return(self);
            
            #' \code{mask} is a \code{monte_carlo_count}-sized logical vector, with
            #' the number of ones coinciding with the width of \code{llr_basis} matrix.
            #' Example:
            #' LLR column index  1   2 3    4       5        678 9   10   
            #' mask              1---1-1----1-------1--------111-1---1---
            #' m_is_running      1---1-1------------1--------1---1-------
            #' LLR basis is a matrix with 10 columns.
            #' Out of these 10 we want to pick 6 columns: ##1, 2, 3, 5, 6, 9.
            mask <- private$m_is_running[mask];
            private$m_time <- private$m_time + 1;
            private$m_run_length[!private$m_has_crossed] <- private$m_time;
            
            n <- private$m_count_thresholds;
            statistic <- private$m_statistic;
            max_threshold <- max(private$m_thresholds);
            
            statistic$observe_basis(llr_basis[, mask], at = private$m_is_running);
            x <- statistic$current(private$m_is_running);
            
            private$m_is_running[private$m_is_running] <- (x <= max_threshold);
            for (j in 1 : n) 
            {
                has_crossed <- private$m_has_crossed[, j];
                x <- statistic$current(!has_crossed);
                private$m_has_crossed[!has_crossed, j] <- (x > private$m_thresholds[j]);
            }
            
            return(self);
        },
        
        print = function() { return("stopping time"); }
    ),
    
    active = list(
        #' Number of thresholds. Read-only.
        count_thresholds = function() { return(private$m_count_thresholds); },
        #' The underlying statistic. Read-only.
        statistic  = function()  { return(private$m_statistic); },
        #' A collection of thresholds to test the statistic against. A \code{monte_carlo_count}-dimensional logical vector. Read-only.
        thresholds = function() { return(private$m_thresholds); },
        #' Mask indicating whether a given monte-carlo thread is still running (hasn't crossed all thresholds). A \code{monte_carlo_count}-dimensional logical vector. Read-only.
        is_running = function() { return(private$m_is_running); },
        #' Counts the number of monte-carlo threads that are still running (haven't crossed all thresholds). Read-only.
        count_running = function() { return(sum(private$m_is_running)); },
        #' Run lengths for a given threshold and monte-carlo thread. A \code{monte_carlo_count}-by-\code{count} matrix. Read-only.
        run_length    = function() { return(private$m_run_length); }
    ),

    private = list(
        #' Local time.
        m_time = 0,
        #' Number of thresholds.
        m_count_thresholds = 0,
        #' The underlying statistic.
        m_statistic = NULL,
        #' A collection of thresholds to test the statistic against. A \code{count}-dimensional vector.
        m_thresholds = numeric(0),
        #' Mask indicating whether a given monte-carlo thread is still running (hasn't crossed all thresholds). A \code{monte_carlo_count}-dimensional logical vector.
        m_is_running = logical(0),
        #' Mask indicating whether a given threshold has been crossed by a given monte-carlo thread. A \code{monte_carlo_count}-by-\code{count} logical matrix.
        m_has_crossed = matrix(logical(0), 0, 0),
        #' Run lengths for a given threshold and monte-carlo thread. A \code{monte_carlo_count}-by-\code{count} matrix.
        m_run_length = matrix(numeric(0), 0, 0)
    )
)

#' Creates an \code{\link{stopping_time_type}} object.
#'
#' This function initializes an instance of \code{\link{stopping_time_type}} class.
#' @param statistic The underlying statistic.
#' @param thresholds A collection of thresholds to test the statistic against.
#' @keywords stopping time, one-sided
#' @export
#' @examples
#' 
stopping_time <- function(statistic, thresholds) 
{
    return(stopping_time_type$new(statistic, thresholds));
}

