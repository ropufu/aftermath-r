
rm(list = ls()); # Clear environment.

print("Hello, world!"); # Some silly comments.

# The function c(...) creates lists of variables of the same type.
# The resulting lists are usually referred to as vectors.
# If the provided objects are of a different type, all of them will be converted to the same type.
c("a", "b")     -> string_vector1;  # "a", "b".
c("a", 17)      -> string_vector2;  # "a", "17".
c(17, "a", "b") -> string_vector3;  # "17", "a", "b".
c(-5, 1729)     -> numeric_vector1; # -5, 1729.

# The function list(...) creates lists of variables of any type without conversion.
list1 <- list("a", 17); # "a", 17.
list2 <- list(string_vector1, 3.1415, numeric_vector1); # Has three elements: vector--number--vector.

# Generating vectors.
# ==========================
# | Function | Description |
# |==========+=============|
# | c        | Combine     |
# | rep      | Replicate   |
# | seq      | Sequence    |
# ==========================
v1 <- c(1, 2, 3, 4); # 1, 2, 3, 4.
v2 <- c(10, 20, 0, 30); # 10, 20, 0, 30.
v3 <- rep(0, times = 12); # Replicate value 0 twelve times. Same as rep(0, 12), but with named argument.
v4 <- 1 : 3; # Same as c(1, 2, 3) or seq(1, 3).
v5 <- seq(1, 3.1, by = 0.5);       # Same as c(1.0, 1.5, 2.0, 2.5, 3.0).
v6 <- seq(1, 3.1, length.out = 5); # Same as c(1.000, 1.525, 2.050, 2.575, 3.100).
v7 <- c(v1, v2); # 1, 2, 3, 4, 10, 20, 0, 30.

# Generating matrices (aka arrays).
m1 <- matrix(v1, nrow = 1, ncol = length(v1)); # Creates a 1-by-length(x) matrix from x.
m2 <- rbind(v1, v2); # Arrange vectors into rows to form a matrix.
m3 <- cbind(v1, v2); # Arrange vectors into columns to form a matrix.

print("Arithmetic operations on vectors/matrices are elementwise.");
print("m1 squared:");
print(m1^2);
print("Sum of m2 and m2:");
print(m2 + m2);
print("Product of m3 and m3 (same as m3 squared):");
print(m3 * m3);

# Accessing elements by index:
# -- For lists use double brackets.
# -- For vectors use single brackets.
print("Testing accessors (bracket operator).");
print(list1[[1]]); # "a": first element of the list.
print(list2[[3]]); # (-5, 1729): third element of the list.
print(v2[3]); # 0: third element of the vector.
print(v2[4]); # 30: fourth element of the vector.

# Modifying elements.
print("This is what we used to have.");
print(v2); # 10, 20, 0, 30.
v2[3] <- 25;
print("Now that the third element was overwritten...");
print(v2); # 10, 20, 25, 30.
