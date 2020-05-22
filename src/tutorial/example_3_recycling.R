
rm(list = ls()); # Clear environment.

# Explicit recycling.
x <- rep(c(5, 2, 3, 4), times = 2); # 5, 2, 3, 4, 5, 2, 3, 4: a total of 8 elements.

# Implicit recycling.
# When adding two vectors of different lengths, the shorter on is "recycled" to match the longer one.
# ========================
# 5, 2, 3, 4, 5, 2, 3, 4    <-- Long vector has 8 elements.
# 3, 2                      <-- Short vector has 2 elements.
# ========================
# becomes
# ========================
# 5, 2, 3, 4, 5, 2, 3, 4    <-- Long vector stays the same.
# 3, 2, 3, 2, 3, 2, 3, 2    <-- 4 copies of the short vector: perfect fit.
# ========================
y <- x + c(3, 2); # 8, 4, 6, 6, 8, 4, 6, 6.

# When the sizes are mismatched, a warning is issued (although recycling still works).
# ========================
# 5, 2, 3, 4, 5, 2, 3, 4    <-- Long vector has 8 elements.
# 3, 2, 0                   <-- Short vector has 3 elements.
# ========================
# becomes
# ========================
# 5, 2, 3, 4, 5, 2, 3, 4    <-- Long vector stays the same.
# 3, 2, 0, 3, 2, 0, 3, 2    <-- Almost 3 copies of the short vector: but the last 0 is left out.
# ========================
z <- x + c(3, 2, 0); # 8, 4, 3, 7, 7, 2, 6, 6.
print("The warning was generated intentionally.");

m <- rbind(x, y, z, x);
print(m);

# Accessing several elements at once.
ix1 <- 1 : 3; # 1, 2, 3: this vector will be used as the "index vector" later on.
ix2 <- c(2, 5); # 2, 5: this vector will also be used as the "index vector" later on.

sub1 <- x[ix1]; # Create a vector of (1st, 2nd, 3rd) elements of x.
sub2 <- x[1 + ix1]; # Create a vector of (2nd, 3rd, 4th) elements of x.
sub3 <- m[ix1, ix2]; # Create a matrix of elements of m at: (1st, 2nd, 3rd) rows and (2nd, 5rd) columns.
sub4 <- m[ , ix2]; # Create a matrix of elements of m at: every row and (2nd, 5rd) columns.

print("Testing accessors (bracket operator).");
print(sub1);
print(sub2);
print(sub3);
print(sub4);
