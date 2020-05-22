
rm(list = ls()); # Clear environment.

# Create a normally distributed population.
population.size <- 10000;
population <- rnorm(population.size, mean = 0.123, sd = 0.456);

# Record the truth.
population.mean <- mean(population);
population.sd <- sd(population);
population.var <- (population.sd)^2;

print("Population:");
print(paste("mean = ", population.mean));
print(paste("sd = ", population.sd));
print(paste("var = ", population.var));

# Define sampling procedure.
sample.size <- 10;
count.samples <- 5000; # Number of times to repeat the experiment.
confidence.level <- 0.95;
left.tail.probability <- (1 - confidence.level) / 2;

print(paste("There are a total of", choose(population.size, sample.size), "samples possible."));
print(paste("That is slightly more than 100..00 with", floor(log10(choose(population.size, sample.size))), "zeros."));

# Create a vector to record which sample was good and which was bad.
oracle.has.captured <- rep(FALSE, times = count.samples);

for (i in 1 : count.samples) {
  x <- sample(population, size = sample.size); # Take another sample.
  
  sample.mean <- mean(x); # Compute an estimate of population mean.
  sample.sd <- sd(x);     # Compute an estimate of population sd.
  sample.var <- var(x);   # Compute an estimate of population var.

  # Critical values for symmetric confidence intervals.
  z <- -qnorm(left.tail.probability); # Should be used if the population is not normal and sample size is large.
  t <- -qt(left.tail.probability, df = sample.size - 1); # Should be used if the population is normal.
  
  lower.bound <- sample.mean - t * sample.sd / sqrt(sample.size);
  upper.bound <- sample.mean + t * sample.sd / sqrt(sample.size);
  
  # Is this a good or a bad sample?
  # ----------|---------------------x-------------------------|------------>
  #       lower.bound      population.mean                upper.bound
  is.below.upper <- (population.mean < upper.bound);
  is.above.lower <- (population.mean > lower.bound);
  # This sample is good if and only if both conditions hold.
  oracle.has.captured[i] <- is.below.upper & is.above.lower;
} # for (...)

print(paste("We have collected", count.samples, "samples."));
print("Estimated proportion of good samples:");
print(mean(oracle.has.captured)); # Compute the average of estimates of sd.
