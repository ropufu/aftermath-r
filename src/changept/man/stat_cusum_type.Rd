% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_cusum.R
\docType{class}
\name{stat_cusum_type}
\alias{stat_cusum_type}
\title{CUSUM Statistic Class}
\format{A scalar CUSUM statistic \code{\link{R6Class}} generator object}
\usage{
stat_cusum_type
}
\arguments{
\item{monte_carlo_count}{Number of monte-carlo statistics to run in parallel. Defaults to 1.}

\item{at}{A \code{monte_carlo_count}-dimensional vector.

Observe a Scalar Log-likelihood

Update the CUSUM statistic with a new instantaneous log-likelihood ratio.}

\item{loglikelihood}{Instantaneous log-likelihood ratios. An \code{n}-dimensional vector, where \code{n = sum(at)}.}

\item{at}{Mask indicating which monte-carlo threads to update. A \code{monte_carlo_count}-dimensional logical vector. Defaults to repeated TRUE.}

\item{at}{A \code{monte_carlo_count}-dimensional vector.

Number of monte-carlo statistics to run in parallel. Read-only.
Number of monte-carlo statistics to run in parallel.
Current value of the statistics. A \code{monte_carlo_count}-dimensional vector.}
}
\description{
This class describes a scalar CUSUM statistic.
}
\examples{
cs <- stat_cusum_type$new()
stat_cusum_type Constructor


Resets the value of the statistics.


Current value of the statistics. A \\code{monte_carlo_count}-dimensional vector. Read-only.

}
\keyword{constructor}
\keyword{cusum,}
\keyword{log-likelihood,}
\keyword{loglikelihood}
\keyword{observe,}
\keyword{scalar}
\keyword{scalar,}
