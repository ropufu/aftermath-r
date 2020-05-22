
rm(list = ls()); # Clear environment.

# Create a custom function.
sample.var <- function(x) {
  n <- length(x);

  sample.mean <- mean(x);
  distance.from.mean <- (x - sample.mean);
  squared.distance <- distance.from.mean ^ 2;
  
  sample.variance <- sum(squared.distance) / (n - 1);
  return(sample.variance);
};

# Create another custom function.
population.var <- function(x) {
  n <- length(x);

  sample.mean <- mean(x);
  distance.from.mean <- (x - sample.mean);
  squared.distance <- distance.from.mean ^ 2;
  
  population.variance <- sum(squared.distance) / (n);
  return(population.variance);
};

# Now we can use sample.var(...) and population.var(...) in the remainder of this code.

# Shorthand notation (without curly braces) can be used for simpler functions.
sample.sd <- function(x) sqrt(sample.var(x));
population.sd <- function(x) sqrt(population.var(x));

print("Testing new functions.");
x <- rnorm(10); # Generate a random vector.

print(paste("builtin var = ", var(x)));
print(paste("sample.var = ", sample.var(x)));
print(paste("population.var = ", population.var(x)));

print(paste("builtin sd = ", sd(x)));
print(paste("sample.sd = ", sample.sd(x)));
print(paste("population.sd = ", population.sd(x)));
