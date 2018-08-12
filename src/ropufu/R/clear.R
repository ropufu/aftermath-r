
#' Clear Environment
#'
#' This function allows you to clear the current environment.
#' @keywords clear
#' @export
#' @examples
#' clear()

clear <- function() 
{
    env <- parent.frame();
    rm(pos = env, list = objects(name = env));
}
