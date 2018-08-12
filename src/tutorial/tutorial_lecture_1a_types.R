
rm(list = ls()); # Clear environment.

print("Hello, world!"); # Some silly comments.
# The sequence <- is the left assigment operator.
x1 <- 17; # x1 is a numerical vector of length 1.
# The sequence -> is the right assigment operator.
18 -> x2; # x2 is a numerical vector of length 1.
c("a", "b")   -> string_vector1; # c(...) creates lists of homogeneous (same) types.
c("a", 17)    -> string_vector2; # 17 is treated as a string (since "a" cannot be recognized as a number).
c(17, "a")    -> string_vector3; # 17 is treated as a string (since "a" cannot be recognized as a number).
list("a", 17) -> l1; # list(...) allows heterogeneous types, so 17 is treated as a number, and "a" is treated as a string.

# Generating vectors.
v1 <- c(17, 16, 4, -3, 0); # Combine values into a vector.
v2 <- c(-17, 1, 4, -3, 1); # Combine values into a vector.
v3 <- rep(0, times = 12); # Same as rep(0, 12), but with named argument.
v4 <- 1 : 3; # Same as c(1, 2, 3) or as seq(1, 3).
v5 <- seq(1, 3.1, by = 0.5);        # First element is 1, last element is 3, each 0.5 from the previous. Same as c(1.0, 1.5, 2.0, 2.5, 3.0).
v6 <- seq(1, 3.1, length.out = 5); # First element is 1, last element is 3.1, and there are a total of 5 linearly spaced elements in total. Same as c(1.000, 1.525, 2.050, 2.575, 3.100).

# Generating matrices (aka arrays).
m1 <- matrix(v1, nrow = 1, ncol = length(v1)); # Creates a 1-by-length(x) matrix from x.
m2 <- rbind(v1, v2); # Arrange vectors into rows to form a matrix.
m3 <- cbind(v1, v2); # Arrange vectors into columns to form a matrix.

print(m1^2); # Operations are elementwise.
print(m2);
print(m3);

# Generating lists.
l2 <- list(v2, m2);

# Accessing elements by index.
print(l2[[1]]); # For lists use double brackets.
print(v2[3]); # For vectors use single brackets.

# Modifying elements.
print(l2[[1]]);
l2[[1]][3] <- 123; # Overwrite third element of the first entry in the list. Note: v2 is not affected; l2[[1]] is!
print(l2[[1]]);
print(v2);
