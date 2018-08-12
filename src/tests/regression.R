
rm(list = ls()); # Clear environment.

a <- runif(1, min = -10, max = 10); # True value of a.
b <- runif(1, min = -10, max = 10); # True value of b.

x.mean <- runif(1, min = -10, max = 10); # True value of E(X).
x.sd   <- runif(1, min = 1, max = 100);  # True value of SD(X).
err.sd <- runif(1, min = 1, max = 100);   # True value of SD(e).

predict.x <- seq(from=x.mean - 2 * x.sd, to=x.mean + 2 * x.sd,
                 length.out=100); # Where we evaluate predictions.

m <- 10000; # This many samples to generate.
n <- 5;     # Number of observations in each sample. Keep low to make sure t is far enough from z!

repeated.a <- matrix(nrow = m);
repeated.b <- matrix(nrow = m);
repeated.residual.ssq <- matrix(nrow = m);
repeated.predict.y <- matrix(nrow = m, ncol = length(predict.x));

# Unobservable empirical distributions.
student.a <- matrix(nrow = m);
student.b <- matrix(nrow = m);

x <- rnorm(n, mean = x.mean, sd = x.sd); # We are interested in conditional observations given X.
stat.x <- x;
stat.x.ssq <- sum(stat.x * stat.x);
hat.a.sd <- err.sd * sqrt(1 / ((n - 1) * var(stat.x)));
hat.b.sd <- err.sd * sqrt((1 / n) + (mean(stat.x) ^ 2) / ((n - 1) * var(stat.x)));
cov.ab <- -mean(stat.x) * (hat.a.sd ^ 2);
residual.ssq.mean <- (err.sd ^ 2) * (n - 2);

for (j in 1 : m)
{
  err <- rnorm(n, mean = 0, sd = err.sd);
  y <- a * x + b + err;
  
  stat.y <- y;
  stat.y.ssq <- sum(stat.y * stat.y);
  
  stat.a <- cor(stat.x, stat.y) * sd(stat.y) / sd(stat.x);
  stat.b <- mean(stat.y) - (stat.a) * mean(stat.x);
  
  predict.y <- function (xi) stat.a * xi + stat.b;
  stat.residual <- stat.y - predict.y(stat.x);
  stat.residual.ssq <- sum(stat.residual * stat.residual);
  stat.err.sd <- sqrt(stat.residual.ssq / (n - 2)); # Sum of square residuals.
  
  stat.a.se <- stat.err.sd * sqrt(1 / ((n - 1) * var(stat.x)));
  stat.b.se <- stat.err.sd * sqrt((1 / n) + (mean(stat.x) ^ 2) / ((n - 1) * var(stat.x)));
  predict.mean.se <- function (xi) stat.err.sd * sqrt((1 / n) + (xi - mean(stat.x)) ^ 2 / ((n - 1) * var(stat.x)));
  predict.one.se <- function (xi) stat.err.sd * sqrt(1 + (1 / n) + (xi - mean(stat.x)) ^ 2 / ((n - 1) * var(stat.x)));
  
  # plot(stat.x, stat.residual);
  
  repeated.a[[j]] <- stat.a;
  repeated.b[[j]] <- stat.b;
  repeated.residual.ssq[[j]] <- stat.residual.ssq;
  repeated.predict.y[j, ] <- predict.y(predict.x);
  
  student.a[[j]] <- (stat.a - a) / stat.a.se;
  student.b[[j]] <- (stat.b - b) / stat.b.se;
}

# ~~ Plot for the last simulation ~~
ropufu::sas.glm(x, y, fill.col = rgb(0,0,0,0.1), frame = "F");
abline(b, a, untf = FALSE, lty = 3, col = "maroon");

# ~~ True model ~~
cat("y = (", a, ") x + (", b, ") + e", '\n');

# ~~ Point estimators ~~
cat("Rel. MC bias of mean of hat(a) = ", 100 * abs(mean(repeated.a) - a) / abs(a), "%", '\n');
cat("Rel. MC bias of mean of hat(b) = ", 100 * abs(mean(repeated.b) - b) / abs(b), "%", '\n');

cat("Rel. MC bias of SD of hat(a) = ", 100 * abs(sd(repeated.a) - hat.a.sd) / hat.a.sd, "%", '\n');
cat("Rel. MC bias of SD of hat(b) = ", 100 * abs(sd(repeated.b) - hat.b.sd) / hat.b.sd, "%", '\n');
cat("Rel. MC bias of SD of cov(hat(a), hat(b)) = ", 100 * abs(cov(repeated.a, repeated.b) - cov.ab) / abs(cov.ab), "%", '\n');

cat("Rel. MC bias of mean of residual squares(a) = ", 100 * abs(mean(repeated.residual.ssq) - residual.ssq.mean) / residual.ssq.mean, "%", '\n');


#student.r <- rt(10000, df=n - 2);
#qqplot(student.a, student.r)
#qqplot(student.b, student.r)
