
rm(list = ls()); # Clear environment.

m <- 10000;

s <- rep(0, times = m); # Create a vector to store estimates of sd.
for (i in 1 : m)
{
  n <- 10;
  # x1 <- rnorm(n);
  # x2 <- rnorm(n, mean = 0.1);
  x3 <- rnorm(n, sd = 1.1); # Generate n observations.
  
  s[i] <- sd(x3); # Compute the estimate of sd: a specific value of sqrt(S^2).
  # print(c(mean(x1), mean(x2), mean(x3)))
  # print(c(sd(x1), sd(x2), sd(x3)))
}

# mean(s) should be close to E(sqrt(S^2)) if m is large.
print(mean(s)); # Compute the average of estimates of sd.
