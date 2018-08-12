
#' Build Package
#'
#' This function documents and installs the specified package.
#' @param name Name of the package to build.
#' @keywords build, document, install, reinstall
#' @export
#' @examples
#' build("ropufu")

build <- function(name) 
{
    setwd(paste("./", name, sep = ""));
    devtools::document();
    setwd("..");
    devtools::install(name);
}
