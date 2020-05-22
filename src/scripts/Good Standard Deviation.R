
is.good <- function (x) { return(all(ceiling(x) == floor(x))); };
good <- FALSE;
n <- 5;
box <- c(1:10, 1:10);

goodness.factor <- 50;

while (!good) {
  x <- (n - 1) * sample(box, size = n);
  m <- mean(x);
  if (!is.good(goodness.factor * m)) next;
  s <- sd(x);
  if (!is.good(goodness.factor * s)) next;
  if (s == 0) next;
  z <- (x - m) / s;
  if (!is.good(goodness.factor * z)) next;
  
  # Ensure at least one is an "outlier".
  #if (all(abs(z) < 2)) next;
  
  good <- TRUE;
}

y <- x / (n - 1);
print(y)
print(c(mean(y), sd(y)))

z <- (y - mean(y)) / sd(y);
print(z)
print(c(mean(z), sd(z)))
