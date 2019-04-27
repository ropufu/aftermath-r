
#' Simple Linear Regression Plot
#'
#' This function creates a simple linear regression plot with prediction bounds.
#' @param x The predictor variable.
#' @param y The response variable.
#' @keywords SAS, GLM, regression, plot
#' @export
#' @examples
#' sas.glm(sort(runif(11, min = 0, max = 10)), 0:10 + rnorm(11), fill.col = "gray", frame = "F")

sas.glm <- function(x, y, cl = 0.95, m = 100, fill.col = rgb(0,0,0,0.1), ...) 
{
  n <- length(x);
  
  a.hat <- cor(x, y) * sd(y) / sd(x); # Estimated slope.
  b.hat <- mean(y) - (a.hat) * mean(x); # Estimated intercept.
  predict.y <- function (xi) a.hat * xi + b.hat; # Prediction function.
  
  
  residual <- y - predict.y(x);
  residual.ssq <- sum(residual * residual);
  sigma.hat <- sqrt(residual.ssq / (n - 2)); # Estimator of sd(error).
  
  a.se <- sigma.hat * sqrt(1 / ((n - 1) * var(x))); # Estimator of sd(a.hat).
  b.se <- sigma.hat * sqrt((1 / n) + (mean(x) ^ 2) / ((n - 1) * var(x))); # Estimator of sd(b.hat).
  
  
  x.plot <- seq(from = min(x), to = max(x), length.out = m); # Where we evaluate predictions.
  y.plot <- predict.y(x.plot); # Predicted point values.
  predict.mean.se <- sigma.hat * sqrt((1 / n) + (x.plot - mean(x)) ^ 2 / ((n - 1) * var(x))); # Estimator of the sd of the mean predictor.
  predict.one.se <- sigma.hat * sqrt(1 + (1 / n) + (x.plot - mean(x)) ^ 2 / ((n - 1) * var(x))); # Estimator of the sd of the point predictor.
  
  
  # ~~ Create plot ~~
  plot(range(x), range(y), col = "transparent", xlab = "", ylab = "", ...);
  
  t <- stats::qt(1 - (1 - cl) / 2, n - 2);
  # ~~ Confidence intervals for mean predictor ~~
  ropufu::fill.plot(
    x.plot, y.plot + t * predict.mean.se,
    x.plot, y.plot - t * predict.mean.se,
    col = fill.col, border = NA);
  lines(x.plot, y.plot + t * predict.mean.se, lty=3, col="gray") # Confidence intervals.
  lines(x.plot, y.plot - t * predict.mean.se, lty=3, col="gray") # Confidence intervals.
  # ~~ Confidence intervals for point predictor ~~
  lines(x.plot, y.plot + t * predict.one.se, lty=2, col="gray") # Confidence intervals.
  lines(x.plot, y.plot - t * predict.one.se, lty=2, col="gray") # Confidence intervals.
  
  # ~~ Regression line ~~
  abline(b.hat, a.hat, untf = FALSE, lty = 1);
  
  # ~~ Observed values ~~
  points(x, y);
}
