
## Clear Environment
ropufu::clear();

# ropufu::build("ropufu")
# ropufu::build("changept")


# ~~ Auxiliary functions ~~
last <- function(x) x[length(x)];
eps.on <- function(name)
{
  grDevices::setEPS();
  grDevices::postscript(paste("correlated_2_of_5_", name, ".eps", sep = ""),
                        paper = "special", width = 6, height = 4);
}
eps.off <- function() grDevices::dev.off();


## Setup
size <- 5;

mu    <- rep(0.0, times = size);
theta <- rep(1.0, times = size);
#theta[1] <- 2.0;

# Randomize covariance matrix.
A <- matrix(runif(size * size, min = -0.8, max = 0.8), size, size);
diag(A) <- 1.0 + runif(size, min = 0.0, max = 0.2);
#S <- diag(size);
S <- A %*% t(A);

ch <- changept::change_norm(mean_before = mu, mean_after = theta, covar = S, size = size);

# Set up possible change subsets (two in this case: P = {A1, A2}).
a1 <- a2 <- rep(FALSE, times = size);
a1[c(1, 3)]    <- TRUE; # A1 = {1, 3}.
a2[c(2, 3, 5)] <- TRUE; # A2 = {2, 3, 5}.


eta1 <- eta2 <- mu;    # Actual post-change: initialize to no-change.
eta1[a1] <- theta[a1]; # ...update A1. 
eta2[a2] <- theta[a2]; # ...update A2.

## Monte-carlo
mc_count <- 1000;

## Test Parallel CUSUM.
# Set up parallel cusum for P = {A1, A2}.
cs_parallel <- changept::stat_parallel_cusum(ch, list(a1, a2), monte_carlo_count = mc_count);
# Set up oracle cusum for A1.
cs_oracle_1 <- changept::stat_parallel_cusum(ch, list(a1), monte_carlo_count = mc_count);
cs_oracle_2 <- changept::stat_parallel_cusum(ch, list(a2), monte_carlo_count = mc_count);

# Set up one-sided stopping rules: statistic & thresholds.
stop_parallel <- changept::stopping_time(cs_parallel, seq(2.0, 7.0, by = 0.1));
stop_oracle_1 <- changept::stopping_time(cs_oracle_1, seq(2.0, 7.0, by = 0.1));
stop_oracle_2 <- changept::stopping_time(cs_oracle_2, seq(2.0, 7.0, by = 0.1));

# Simulate run lenghts to false alarm.
rules <- list(stop_parallel, stop_oracle_1, stop_oracle_2);
changept::mc_run_to_stop(ch, mu, rules, monte_carlo_count = mc_count);
arl_parallel <- colMeans(stop_parallel$run_length);
arl_oracle_1 <- colMeans(stop_oracle_1$run_length);
arl_oracle_2 <- colMeans(stop_oracle_2$run_length);

# Simulate delay to detection when A1 is affected.
rules <- list(stop_parallel, stop_oracle_1);
changept::mc_run_to_stop(ch, eta1, rules, monte_carlo_count = mc_count);
add1_parallel <- colMeans(stop_parallel$run_length);
add1_oracle   <- colMeans(stop_oracle_1$run_length);

# Simulate delay to detection when A2 is affected.
rules <- list(stop_parallel, stop_oracle_2);
changept::mc_run_to_stop(ch, eta2, rules, monte_carlo_count = mc_count);
add2_parallel <- colMeans(stop_parallel$run_length);
add2_oracle   <- colMeans(stop_oracle_2$run_length);

# Interpolate to plot differences.
arl_min = max(10^2, arl_parallel[1], arl_oracle_1[1], arl_oracle_2[1]);
arl_max = min(10^5, last(arl_parallel), last(arl_oracle_1), last(arl_oracle_2));
arl_common = seq(arl_min, arl_max, length.out = 128);

add1_parallel_common <- stats::approx(arl_parallel, add1_parallel, arl_common)$y;
add2_parallel_common <- stats::approx(arl_parallel, add2_parallel, arl_common)$y;
add1_oracle_common   <- stats::approx(arl_oracle_1, add1_oracle, arl_common)$y;
add2_oracle_common   <- stats::approx(arl_oracle_2, add2_oracle, arl_common)$y;

add_max = 1.1 * max(add1_parallel_common, add2_parallel_common);
add_min = 0.9 * min(add1_oracle_common, add2_oracle_common);

## Output
print(c(min(arl_parallel), min(arl_oracle_1), min(arl_oracle_2)));
print(c(max(arl_parallel), max(arl_oracle_1), max(arl_oracle_2)));

#eps.on("ad");
#plot(arl_common, add1_parallel_common - add1_oracle_common, type = "l", log = "x", col = "black", xlab = "ARL", ylab = "Delay to Detection");
#lines(arl_common, add2_parallel_common - add2_oracle_common, col = "red"); #ylab = expression(A[n]^2)
#eps.off();

eps.on("a1");
plot(arl_common, add1_parallel_common, type = "l", log = "x", col = "black", xlab = "ARL", ylab = "Delay to Detection", yaxs = 'i', ylim = c(add_min, add_max));
lines(arl_common, add1_oracle_common, col = "red"); #ylab = expression(A[n]^2)
eps.off();
eps.on("a2");
plot(arl_common, add2_parallel_common, type = "l", log = "x", col = "black", xlab = "ARL", ylab = "Delay to Detection", yaxs = 'i', ylim = c(add_min, add_max));
lines(arl_common, add2_oracle_common, col = "red"); #ylab = expression(A[n]^2)
eps.off();


# ~~ Here Be Dragons ~~
return()
