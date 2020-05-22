
rm(list = ls()); # Clear environment.

crude <- seq(0.0, 2.9, by = 0.1);
fine <- seq(0.00, 0.09, by = 0.01);

m <- length(crude);
n <- length(fine);

dist.table <- matrix(nrow = m, ncol = n,
  dimnames = list(
    crude, # Labels for rows.
    fine   # Labels for columns.
  ));


print(paste("Right Tail Probabilities for Standard Normal Distribution"));
cdf <- function(x) pnorm(x, mean = 0, sd = 1);

for (i in 1 : m) {
  for (j in 1 : n) {
    first.decimal <- crude[i];
    second.decimal <- fine[j];
    # Reconstruct the argument.
    x <- first.decimal + second.decimal;
    # Calculate right tail probability.
    right.tail <- 1 - cdf(x);
    dist.table[i, j] <- right.tail;
  } # for (...)
} # for (...)

print(round(dist.table, digits = 4));
