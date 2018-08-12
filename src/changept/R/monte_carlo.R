
#' Runs a one-sided monte-carlo simulation.
#'
#' This function does something silly...
#' @param change The change-point descriptor.
#' @param parameter_actual Actual post-change parameter.
#' @param rules Stopping times to run monte-carlo on.
#' @param monte_carlo_count Number of monte-carlo statistics to run in parallel. Defaults to 1.
#' @keywords monte-carlo, simulations
#' @export
#' @examples
#' 
mc_run_to_stop <- function(change, parameter_actual, rules, monte_carlo_count = 1) 
{
    if (!is.list(rules)) rules = list(rules);
    
    for (r in rules) r$reset();
    
    count_running <- monte_carlo_count;
    are_running <- rep(TRUE, times = monte_carlo_count);
    while (count_running > 0)
    {
        # Generate \code{count_running} copies of LLR basis.
        llr_basis <- change$loglikelihood_basis(parameter_actual, n = count_running);
        
        are_still_running <- rep(FALSE, times = monte_carlo_count);
        for (r in rules) 
        {
            if (any(r$is_running))
            {
                r$observe_basis(llr_basis, mask = are_running);
                are_still_running <- (are_still_running) | (r$is_running);
            }
        }
        count_running <- sum(are_still_running);
        are_running <- are_still_running;
    }
    
    return();
}

