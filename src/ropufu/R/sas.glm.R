
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
  x.ssq <- sum(x * x);
  y.ssq <- sum(y * y);
  
  a <- cor(x, y) * sd(y) / sd(x);
  b <- mean(y) - (a) * mean(x);
  predict.y <- function (xi) a * xi + b;
  
  
  residual <- y - predict.y(x);
  residual.ssq <- sum(residual * residual);
  err.sd <- sqrt(residual.ssq / (n - 2)); # Estimator of the sd(error).
  
  a.se <- err.sd * sqrt(1 / ((n - 1) * var(x))); # Estimator of the standard deviation of the estimator of a.
  b.se <- err.sd * sqrt((1 / n) + (mean(x) ^ 2) / ((n - 1) * var(x))); # Estimator of the standard deviation of the estimator of b.
  
  xi <- seq(from = min(x), to = max(x), length.out = m); # Where we evaluate predictions.
  predict.mean.se <- err.sd * sqrt((1 / n) + (xi - mean(x)) ^ 2 / ((n - 1) * var(x)));
  predict.one.se <- err.sd * sqrt(1 + (1 / n) + (xi - mean(x)) ^ 2 / ((n - 1) * var(x)));
  
  
  # ~~ Plot for the last simulation ~~
  plot(x, y, col = "transparent", ...);
  
  t <- qt(1 - (1 - cl) / 2, n - 2);
  # ~~ Confidence intervals for mean ~~
  lines(xi, predict.y(xi) + t * predict.mean.se, lty=3, col="gray") # Confidence intervals.
  lines(xi, predict.y(xi) - t * predict.mean.se, lty=3, col="gray") # Confidence intervals.
  polygon(c(xi, rev(xi)),
          c(predict.y(xi) + t * predict.mean.se, rev(predict.y(xi) - t * predict.mean.se)),
          col = fill.col,
          border = NA);
  # ~~ Confidence intervals for individual ~~
  lines(xi, predict.y(xi) + t * predict.one.se, lty=2, col="gray") # Confidence intervals.
  lines(xi, predict.y(xi) - t * predict.one.se, lty=2, col="gray") # Confidence intervals.
  
  # ~~ Regression line ~~
  abline(b, a, untf = FALSE, lty = 1);
  
  points(x, y);
}
