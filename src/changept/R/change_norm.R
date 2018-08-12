
#' Change Descriptor Class
#' 
#' This class describes a possible shift in mean in a time-
#' independent sequence of multivariate normal observations.
#' The covariance matrix is not affected by the change.
#' The post-change mean in a given component only depends on
#' whether the component belongs to the affected subset, but
#' not on the subset itself.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format A normal-to-normal change in mean descriptor \code{\link{R6Class}} generator object
#' @keywords change, normal, gaussian
#' @examples
#' ch <- change_norm_type$new(mean_before = 0.0, mean_after = 1.0, covar = diag(1))
change_norm_type <- R6::R6Class("change_norm_type",
    public = list(
        #' \code{change_norm_type} Constructor
        #' 
        #' @param mean_before Pre-change means. A \code{size}-dimensional vector.
        #' @param mean_after Anticipated post-change means. A \code{size}-dimensional vector.
        #' @param covar Covariance matrix of observations. A \code{size}-by-\code{size} symmetric positive-definite matrix.
        #' @keywords change, normal, gaussian, constructor
        #' @export
        #' @examples
        #' 
        initialize = function(mean_before, mean_after, covar)
        {
            private$m_size <- length(mean_before);
            
            if (length(mean_after) != private$m_size) stop("mismatched vector sizes");
            if (all(mean_before == mean_after))       stop("there is no change to detect");
            if (!is.matrix(covar))                    stop("<covar> should be a two-dimentional matrix");
            
            d <- dim(covar);
            if (d[1] != d[2]) stop("<covar> should be a square matrix");
            if (d[1] != private$m_size) stop("mismatched covariance matrix size");
      
            private$m_mean_before <- mean_before;
            private$m_mean_after  <- mean_after;
            private$m_mean_shift  <- mean_after - mean_before;
            private$m_covar       <- covar;
      
            
            D <- diag(private$m_mean_shift);
            private$m_llr_covar <- D %*% solve(covar, D);
      
            r <- eigen(covar, symmetric = TRUE);
            private$m_x_eigenvectors   <- r$vectors;
            private$m_x_eigenvalues    <- r$values;
            private$m_x_scaling        <- (r$vectors) %*% (diag(sqrt(r$values)));
      
            r <- eigen(private$m_llr_covar, symmetric = TRUE);
            private$m_llr_eigenvectors <- r$vectors;
            private$m_llr_eigenvalues  <- r$values;
            private$m_llr_scaling      <- (r$vectors) %*% (diag(sqrt(r$values)));
      
            private$m_x_precision      <- solve(covar);
            private$m_llr_precision    <- solve(private$m_llr_covar);
        },
    
        #' Generate Post-change Normal Random Variates
        #'
        #' This function generates a (temporally independent) sequence of vector observations.
        #' @param mean_actual Actual post-change means. A \code{size}-dimensional vector.
        #' @param n Number of observations to generate. Defaults to 1.
        #' @keywords observe, normal, gaussian
        #' @export
        #' @examples
        #' 
        observe = function(mean_actual, n = 1)
        {
            z <- matrix(stats::rnorm(n * private$m_size), private$m_size, n);
            m <- matrix(replicate(n, mean_actual), private$m_size, n);
      
            x <- m + (private$m_x_scaling) %*% z;
            return(x);
        },
    
        #' Generate Pre-change Normal Random Variates
        #'
        #' This function generates a (temporally independent) sequence of pre-change vector observations.
        #' @param n Number of observations to generate. Defaults to 1.
        #' @keywords observe, pre-change, normal, gaussian
        #' @export
        #' @examples
        #' 
        observe_before = function(n = 1) { return(observe(private$m_mean_before, n)); },
    
        #' Generate Post-change Normal Random Variates
        #'
        #' This function generates a (temporally independent) sequence of post-change vector observations.
        #' @param n Number of observations to generate. Defaults to 1.
        #' @keywords observe, post-change, normal, gaussian
        #' @export
        #' @examples
        #' 
        observe_after = function(n = 1) { return(observe(private$m_mean_after, n)); },
    
        #' Generate Post-change Log-likelihood Basis Variates
        #'
        #' This function generates a (temporally independent) sequence of instantaneous log-likelihood basis ratios.
        #' @param mean_actual Actual post-change means. A \code{size}-dimensional vector.
        #' @param n Number of observations to generate. Defaults to 1.
        #' @keywords log-likelihood, loglikelihood, normal, gaussian
        #' @export
        #' @examples
        #' 
        loglikelihood_basis = function(mean_actual, n = 1)
        {
            ev <- ((private$m_x_precision) %*% (mean_actual - private$m_mean_before)) - (private$m_mean_shift * diag(private$m_x_precision)) / 2;
            ev <- private$m_mean_shift * ev;
      
            z <- matrix(stats::rnorm(n * private$m_size), private$m_size, n);
            m <- matrix(replicate(n, ev), private$m_size, n);
      
            x <- m + (private$m_llr_scaling) %*% z;
            return(x);
        },
    
        #' Reconstructor Function for Post-change Log-likelihood Variates
        #'
        #' This function constructs a 1-argument reconstructor for post-change instantaneous log-likelihood ratios from log-likelihood basis ratios.
        #' @param possibly_affected Mask indicating affected subset possibilities. Can be either:
        #'        i)   \code{size}-dimensional logical vector, a single possibility;
        #'        ii)  \code{n}-by-\code{size} logical matrix, where each row corresponds to a specific scenario;
        #'        iii) list of \code{size}-dimensional logical vectors, each representing a single possibility;
        #'        iv)  function of one argument, 1-based index, that returns the affected subset mask at the specified index.
        #' @param count Number of possibilities. Only needed if \code{possibly_affected} is a function.
        #' @keywords log-likelihood, loglikelihood, normal, gaussian
        #' @export
        #' @examples
        #' ch <- change_norm(size = 5);
        #' a1 <- c(TRUE, TRUE, FALSE, TRUE, FALSE);
        #' a2 <- c(FALSE, TRUE, TRUE, FALSE, FALSE);
        #' r <- ch$loglikelihood_constructor(list(a1, a2));
        #' basis <- ch$loglikelihood_basis(1.0, n = 10);
        #' llr <- r(basis);
        loglikelihood_constructor = function(possibly_affected, count)
        {
            if (is.list(possibly_affected))
            {
                count <- length(possibly_affected);
                reader <- function(ix) { return(possibly_affected[[ix]]); };
            }
            else if (is.vector(possibly_affected))
            {
                count <- 1;
                reader <- function(ix) { return(possibly_affected); };
            }
            else if (is.matrix(possibly_affected))
            {
                count <- dim(possibly_affected)[1];
                reader <- function(ix) { return(possibly_affected[ix, ]); };
            }
            else if (is.function(possibly_affected))
            {
                if (missing(count)) stop("<count> is required when <possibly_affected> is a function");
                reader <- possibly_affected;
            }
            else stop("<possibly_affected> not recognized");

            
            shift <- rep(0.0, times = count);
            for (i in 1 : count)
            {
                mask <- reader(i);
                
                v <- private$m_llr_covar;
                v <- sum(v[mask, mask]);
          
                w <- ((private$m_mean_shift)^2) * diag(private$m_x_precision);
                w <- sum(w[mask]);
                
                shift[i] <- (w - v) / 2;
            }
            
            #' Reconstruct Post-change Log-likelihood Variates
            #'
            #' This function reconstructs post-change instantaneous log-likelihood ratios from log-likelihood basis ratios.
            #' @param basis Instantaneous log-likelihood basis ratios. A \code{size}-by-\code{n} matrix.
            #' @keywords log-likelihood, loglikelihood, normal, gaussian
            #' @export
            #' @examples
            #' 
            ctor <- function(loglikelihood_basis) 
            {
                if (!is.matrix(loglikelihood_basis)) dim(loglikelihood_basis) <- c(length(loglikelihood_basis), 1);
                
                n <- dim(loglikelihood_basis)[2];
                value <- matrix(replicate(n, shift), count, n); # Makes sure value does not get demoted to a vector.
                
                for (i in 1 : count)
                {
                    affected_subset_mask <- reader(i);
                    
                    local_loglikelihood <- loglikelihood_basis[affected_subset_mask, ];
                    if (!is.matrix(local_loglikelihood)) dim(local_loglikelihood) <- c(length(local_loglikelihood), 1);
                    
                    value[i, ] <- value[i, ] + colSums(local_loglikelihood);
                }
                return(value); 
            };
            
            return(ctor);
        },
        
        #' Generate Post-change Log-likelihood Variates
        #'
        #' This function generates a (temporally independent) sequence of post-change instantaneous log-likelihood ratios.
        #' @param mean_actual Actual post-change means. A \code{size}-dimensional vector.
        #' @param affected_subset Mask indicating affected subset. A \code{size}-dimensional logical vector.
        #' @param n Number of log-likelihoods to generate. Defaults to 1.
        #' @keywords log-likelihood, loglikelihood, normal, gaussian
        #' @export
        #' @examples
        #' 
        bruteforce_loglikelihood = function(mean_actual, affected_subset, n = 1)
        {
            shift <- private$m_mean_shift;
            shift[!affected_subset] <- 0.0;
      
            ev <- mean_actual - private$m_mean_before - (shift / 2);
            x <- self$observe(ev, n); # K-by-n matrix of 1-by-K shifted observations.
            #warning(c("x: ", dim(x)[1], "-by-", dim(x)[2]));
      
            y <- matrix((private$m_x_precision) %*% shift, 1, private$m_size); # K-by-1 matrix.
            #warning(c("y: ", dim(y)[1], "-by-", dim(y)[2]));

            return(y %*% x);
        },
    
        #' Generate Post-change Log-likelihood Variates
        #'
        #' This function generates a (temporally independent) sequence of post-change instantaneous log-likelihood ratios.
        #' @param affected_subset Mask indicating affected subset. A \code{size}-dimensional logical vector.
        #' @param n Number of log-likelihoods to generate. Defaults to 1.
        #' @keywords log-likelihood, loglikelihood, post-change, normal, gaussian
        #' @export
        #' @examples
        #' 
        oracle_loglikelihood_after = function(affected_subset, n = 1)
        {
            v <- private$m_llr_covar;
            v <- sum(v[affected_subset, affected_subset]);
      
            return(stats::rnorm(n, mean = v / 2, sd = sqrt(v)));
        },
    
        #' Generate Pre-change Log-likelihood Variates
        #'
        #' This function generates a (temporally independent) sequence of pre-change instantaneous log-likelihood ratios.
        #' @param affected_subset Mask indicating affected subset. A \code{size}-dimensional logical vector.
        #' @param n Number of log-likelihoods to generate. Defaults to 1.
        #' @keywords log-likelihood, loglikelihood, pre-change, normal, gaussian
        #' @export
        #' @examples
        #' 
        oracle_loglikelihood_before = function(affected_subset, n = 1)
        {
            v <- private$m_llr_covar;
            v <- sum(v[affected_subset, affected_subset]);
            
            return(stats::rnorm(n, mean = -v / 2, sd = sqrt(v)));
        },
    
        print = function() { return("shift in mean normal -> normal"); }
    ),
    
    active = list(
        #' Dimensions of the problem. Read-only.
        size        = function() { return(private$m_size); },
        #' Pre-change mean. Read-only.
        mean_before = function() { return(private$m_mean_before); },
        #' Anticipated post-change mean. Read-only.
        mean_after  = function() { return(private$m_mean_after); },
        #' Anticipated shift in mean. Read-only.
        mean_shift  = function() { return(private$m_mean_shift); },
        #' Covariance matrix of observations. Read-only.
        covar       = function() { return(private$m_covar); }
    ),
  
    private = list(
        #' Dimensions of the problem.
        m_size = 0,
        #' Pre-change mean.
        m_mean_before = numeric(0),
        #' Anticipated post-change mean.
        m_mean_after = numeric(0),
        #' Anticipated shift in mean.
        m_mean_shift = numeric(0),
        #' Covariance matrix of observations.
        m_covar = matrix(numeric(0), 0, 0),
        #' Covariance matrix of the log-likelihood basis.
        m_llr_covar = matrix(numeric(0), 0, 0),
        #' Eigenvectors of the covariance matrix of observations.
        m_x_eigenvectors = matrix(numeric(0), 0, 0),
        #' Eigenvalues of the covariance matrix of observations.
        m_x_eigenvalues = numeric(0),
        #' Scaling matrix to generate observations from iid normals Z via (mean) + (sc)(Z).
        m_x_scaling = matrix(numeric(0), 0, 0),
        #' Inverse covariance matrix of observations.
        m_x_precision = matrix(numeric(0), 0, 0),
        #' Eigenvectors of the covariance matrix of the log-likelihood basis.
        m_llr_eigenvectors = matrix(numeric(0), 0, 0),
        #' Eigenvalues of the covariance matrix of the log-likelihood basis.
        m_llr_eigenvalues = numeric(0),
        #' Scaling matrix to generate the log-likelihood basis from iid normals Z via (mean) + (sc)(Z).
        m_llr_scaling = matrix(numeric(0), 0, 0),
        #' Inverse covariance matrix of the log-likelihood basis.
        m_llr_precision = numeric(0)
    )
)

#' Creates an \code{\link{change_norm_type}} object.
#'
#' This function initializes an instance of \code{\link{change_norm_type}} class.
#' @param mean_before Pre-change means. Defaults to 0.
#' @param mean_after Anticipated post-change means. Defaults to 1.
#' @param covar Covariance matrix of observations. A \code{size}-by-\code{size} symmetric positive-definite matrix.
#' @param size Dimensions of the problem.
#' @keywords change, normal, gaussian
#' @export
#' @examples
#' ch <- change_norm(mean_before = 0.0, mean_after = 1.0)
change_norm <- function(mean_before = 0.0, mean_after = 1.0, covar, size)
{
    if (missing(size))  size <- max(length(mean_before), length(mean_after));
    if (missing(covar)) covar <- diag(size);
  
    zero <- rep(0.0, times = size);
  
    mean_before <- zero + mean_before;
    mean_after  <- zero + mean_after;
  
    return(change_norm_type$new(mean_before, mean_after, covar));
}

