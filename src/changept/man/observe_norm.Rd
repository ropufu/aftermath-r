% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observe_norm.R
\name{observe_norm}
\alias{observe_norm}
\title{Generate Normal Random Variates}
\usage{
observe_norm(n = 1, size = 1, mean = rep(0, times = size),
  covar = diag(size))
}
\arguments{
\item{n}{Number of observations to generate. Defaults to 1.}

\item{size}{Height of each observation vector. Defaults to 1.}

\item{mean}{1-by-size vector of means. Defaults to 0.}

\item{covar}{size-by-size covariance matrix of observations. Defaults to <id>.}
}
\description{
This function generates a sequence of vector observations.
}
\examples{
observe.norm(100)
}
\keyword{observe}
