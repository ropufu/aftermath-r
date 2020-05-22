
rm(list = ls()); # Clear environment.

detail <- new.env();

detail$ci.level.from.pivot <- function(a, b, pivot.cdf, pivot.transform) {
  pivot.cutoff <- pivot.transform(c(a, b));
  
  left.tail.p <- pivot.cdf(pivot.cutoff[1]);
  right.tail.p <- 1 - pivot.cdf(pivot.cutoff[2]);
  confidence.level = 1 - left.tail.p - right.tail.p;
  
  cat("CI = (", a, ", ", b, ")", "\n", sep = "");
  cat("Pivot range = (", pivot.cutoff[1], ", ", pivot.cutoff[2], ")", "\n", sep = "");
  cat("Confidence level = ", 100 * confidence.level, "%", "\n", sep = "");
  
  return(confidence.level);
}; # detail$ci.pivot(...)

detail$ci.from.pivot <- function(left.tail.p, right.tail.p, pivot.inv.cdf, pivot.inv.transform, point.estimator) {
  confidence.level = 1 - left.tail.p - right.tail.p;
  # Pivotal quantity distribution: P(left < Q < right) = conf.level.
  left.pivot.cutoff <- pivot.inv.cdf(left.tail.p);
  right.pivot.cutoff <- pivot.inv.cdf(1 - right.tail.p);
  
  result <- pivot.inv.transform(c(left.pivot.cutoff, right.pivot.cutoff));
  interval.length <- diff(result);
  
  cat("Confidence level = ", 100 * confidence.level, "%", "\n", sep = "");
  cat("Pivot range = (", left.pivot.cutoff, ", ", right.pivot.cutoff, ")", "\n", sep = "");
  cat("CI = (", result[1], ", ", result[2], ")", "\n", sep = "");
  if (missing(point.estimator)) {
    cat("Interval length = ", interval.length, "\n", sep = "");
  } # if (...)
  else
  {
    left.margin.of.error = point.estimator - result[1];
    right.margin.of.error = result[2] - point.estimator;
    cat("Point estimator = ", point.estimator, "\n", sep = "");
    if (left.margin.of.error == right.margin.of.error) {
      cat("Margin of error = ", left.margin.of.error, "\n", sep = "");
    } # if (...)
    else {
      cat("Left margin of error = ", left.margin.of.error, "\n", sep = "");
      cat("Right margin of error = ", right.margin.of.error, "\n", sep = "");
    } # else (...)
  } # else (...)
  
  return(result);
}; # detail$ci.pivot(...)

detail$ci.level.mean <- function(a, b, sample.mean, sample.sd, n, is.student = FALSE) {
  sample.mean = (a + b) / 2;
  if (!is.student) pivot.cdf <- pnorm
  else pivot.cdf <- function(p) pt(p, df = n - 1);
  
  if (!is.student) cat("Pivotal distribution: N(0, 1)", "\n", sep = "")
  else cat("Pivotal distribution: t(", n - 1, ")", "\n", sep = "");
  
  pivot.transform <- function(j) c(
    sample.mean - j[2] * standard.error,
    sample.mean - j[1] * standard.error);
  
  result = detail$ci.from.pivot(
    left.tail.p, right.tail.p, pivot.inv.cdf, pivot.inv.transform,
    point.estimator = sample.mean);
  
  cat("Standard error = ", standard.error, "\n", sep = "");
  
  return(result);
}; # detail$ci.level.mean(...)

detail$ci.mean <- function(left.tail.p, right.tail.p, sample.mean, sample.sd, n, is.student = FALSE) {
  standard.error <- sample.sd / sqrt(n);

  if (!is.student) pivot.inv.cdf <- qnorm
  else pivot.inv.cdf <- function(p) qt(p, df = n - 1);
  
  if (!is.student) cat("Pivotal distribution: N(0, 1)", "\n", sep = "")
  else cat("Pivotal distribution: t(", n - 1, ")", "\n", sep = "");
  
  pivot.inv.transform <- function(j) c(
    sample.mean - j[2] * standard.error,
    sample.mean - j[1] * standard.error);
  
  result = detail$ci.from.pivot(
    left.tail.p, right.tail.p, pivot.inv.cdf, pivot.inv.transform,
    point.estimator = sample.mean);
  
  cat("Standard error = ", standard.error, "\n", sep = "");
  
  return(result);
}; # detail$ci.mean(...)

ci.mean.z <- function(confidence.level, sample.mean, sample.sd, n) {
  tail.p <- (1 - confidence.level) / 2;
  result = detail$ci.mean(tail.p, tail.p, sample.mean, sample.sd, n, is.student = FALSE);
  return(result);
}; # ci.mean.z(...)

ci.mean.t <- function(confidence.level, sample.mean, sample.sd, n) {
  tail.p <- (1 - confidence.level) / 2;
  result = detail$ci.mean(tail.p, tail.p, sample.mean, sample.sd, n, is.student = TRUE);
  return(result);
}; # ci.mean.t(...)

ci.proportion <- function(confidence.level, sample.successes, n) {
  sample.mean = (sample.successes) / n;
  sample.sd = sqrt(sample.mean * (1 - sample.mean));
  return(ci.mean.z(confidence.level, sample.mean, sample.sd, n));
}; # ci.proportion(...)

cat # m=^.^=m~~ 
