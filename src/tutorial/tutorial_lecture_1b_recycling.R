
rm(list = ls()); # Clear environment.

n <- 15;
x1 <- rep(0, times = n); # Same as rep(0, n);
x2 <- rep(c(1, 2, 3), times = 5); # Explicit recycling: replicate vector (1, 2, 3) three times.
x3 <- x2 + c(3, 2, 1); # Implicit recycling: (3, 2, 1) is replicated five times to match length(x2).
x4 <- x2 + c(3, 2, 1, 0); # Mismatched (15 is not divisible by 4) recycling issues a warning, but works too.

m <- rbind(x1, x2, x3, x4);
print(m);

# Accessing several elements at once.
ix1 <- 1 : 3;
ix2 <- c(1, 2, 5);

sub1 <- x2[ix1]; # Create a vector of (1st, 2nd, 3rd) elements of x2.
sub2 <- x2[1 + ix1]; # Create a vector of (2nd, 3rd, 4th) elements of x2.
sub3 <- m[ix1, ix2]; # Create a matrix of elements of m at: (1st, 2nd, 3rd) rows and (1st, 2nd, 5rd) columns.
sub4 <- m[ , ix2]; # Create a matrix of elements of m at: every row and (1st, 2nd, 5rd) columns.

print(sub1);
print(sub2);
print(sub3);
print(sub4);
