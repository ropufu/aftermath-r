
rm(list = ls()); # Clear environment.

m <- 10000;
n <- 50;

mu <- 1.7;
dev <- 1.1;

notvar <- function(x, mean)
{
  n <- length(x);
  r2 <- sum((x - mean)^2) / n;
  r2 # return(r2);
};

bias <- function(z, theta) { return(mean(z) - theta); };

v <- rep(0, times = m);
w <- rep(0, times = m);

for (i in 1 : m)
{
  x <- rnorm(n, mean = mu, sd = dev); # Generate n observations.
    
  v[i] <- notvar(x, mu);
  w[i] <- var(x);
}

print(bias(v, dev^2)); # Compute the average of estimates of var.
print(bias(w, dev^2)); # Compute the average of estimates of var.

print(sd(v)); # Compute the standard deviation of estimates of var.
print(sd(w)); # Compute the standard deviation of estimates of var.
