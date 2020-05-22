
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
sample.size <- 100;

print(paste("There are a total of", choose(population.size, sample.size), "samples possible."));
print(paste("That is slightly more than 100..00 with", floor(log10(choose(population.size, sample.size))), "zeros."));

# Take one sample.
x <- sample(population, size = sample.size);
  
sample.mean <- mean(x); # Compute an estimate of population mean.
sample.sd <- sd(x);     # Compute an estimate of population sd.
sample.var <- var(x);   # Compute an estimate of population var.

print("Sample:");
print(paste("mean = ", sample.mean));
print(paste("sd = ", sample.sd));
print(paste("var = ", sample.var));

# Now, let us take more samples.
count.samples <- 10000; # Number of times to repeat the experiment.

# Reserve room for statistics: for each sample we will store its mean, sd, and var.
stat.mean <- rep(0, times = count.samples); # Create a vector to store estimates of mean.
stat.sd <- rep(0, times = count.samples);   # Create a vector to store estimates of standard deviation.
stat.var <- rep(0, times = count.samples);  # Create a vector to store estimates of variance.

for (i in 1 : count.samples) {
  x <- sample(population, size = sample.size); # Take another sample.
  
  sample.mean <- mean(x); # Compute an estimate of population mean.
  sample.sd <- sd(x);     # Compute an estimate of population sd.
  sample.var <- var(x);   # Compute an estimate of population var.
  
  # Remember what was ovserved in this particular sample.
  stat.mean[i] <- sample.mean;
  stat.sd[i] <- sample.sd;
  stat.var[i] <- sample.var;
} # for (...)

print(paste("We have collected", count.samples, "samples."));
print("Estimated expectation for the sample mean:");
print(mean(stat.mean)); # Compute the average of estimates of sd.
print(mean(stat.sd));   # Compute the average of estimates of sd.
print(mean(stat.var));  # Compute the average of estimates of sd.

# Distribution of the sample mean.
hist(stat.mean);
