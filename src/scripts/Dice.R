
rm(list = ls()); # Clear environment.

standard.die <- c(3, 1, 4, 5, 6, 2);
sicherman.small.die <- c(4, 2, 1, 2, 3, 3);
sicherman.big.die <- c(8, 4, 1, 6, 5, 3);

rindex <- function(n, length) sample.int(length, n, replace = T);

rdie <- function(n, values, sides) {
  if (missing(sides)) {
    if (missing(values)) values <- standard.die;
    sides <- length(values);
  }
  if (missing(values)) {
    values <- 1 : sides;
  }
  if (length(values) != sides) stop("number of sides incompatible with values");
  
  indices <- rindex(n, length = sides);
  return(values[indices]);
}; # rdie(...)

test.simple.hypotheses <- function(die0, die1, rolls, rule, n) {
  count.type.one <- 0;
  count.type.two <- 0;
  x0 <- rdie(rolls * n, values = die0);
  x1 <- rdie(rolls * n, values = die1);
  for (i in 1 : n) {
    from <- 1 + rolls * (i - 1);
    to <- from + rolls - 1;
    d0 <- rule(x0[from : to], rolls);
    d1 <- rule(x1[from : to], rolls);
    if (d0 == 1) count.type.one <- count.type.one + 1;
    if (d1 == 0) count.type.two <- count.type.two + 1;
  } # for (...)
  return(c(count.type.one, count.type.two) / n);
}; # test.simple.hypotheses(...)

rule.half.and.half <- function(x, rolls, threshold = 0) round(runif(1));

rule.five.six <- function(x, rolls, threshold = 0) {
  if (any(x == 5) || any(x == 6)) return(0);
  return(1);
}; # rule.five.six(...)

rule.two.three <- function(x, rolls, threshold = rolls / 2) {
  if (any(x == 5) || any(x == 6)) return(0);
  y <- sum((x == 2) | (x == 3));
  if (y > threshold) return(1);
  return(0);
}; # rule.two.three(...)

rule.neyman.pearson <- function(x, rolls, threshold = rolls) {
  if (any(x == 5) || any(x == 6)) return(0);
  y <- sum((x == 2) | (x == 3));
  t <- y;
  if (y > 0) {
    num <- rolls;
    denom <- y;
    for (i in 1 : y) {
      t <- t + log(num / denom, base = 2);
      num <- num - 1;
      denom <- denom - 1;
    } # for (...)
  } # if (...)
  if (t > threshold) return(1);
  return(0);
}; # rule.neyman.pearson(...)

reps <- 1000000;

print("Rule 5--6");
print(test.simple.hypotheses(
  standard.die,
  sicherman.small.die,
  rolls = 10, rule = rule.five.six, n = reps));

print("Rule 5--6/2--3");
print(test.simple.hypotheses(
  standard.die,
  sicherman.small.die,
  rolls = 10, rule = rule.two.three, n = reps));

print("Rule N-P");
print(test.simple.hypotheses(
  standard.die,
  sicherman.small.die,
  rolls = 10, rule = rule.neyman.pearson, n = reps));

# # ~~ Neyman-Person conditional pmf ~~
# y <- 0 : 10; t <- y + log(choose(10, y), base = 2);
# p0 <- choose(10, y) / (2^10);
# p1 <- choose(10, y) * ((4/6)^(y)) * ((2/6)^(10 - y));
# plot(t, p1, type = "h");
# 
# ix = sort(t, index.return = TRUE)$ix;
# cat("\\readlist{\\arrayY}{"); cat(y[ix], sep = ", "); cat("}\n");
# cat("\\readlist{\\arrayT}{"); cat(t[ix], sep = ", "); cat("}\n");
# cat("\\readlist{\\arrayPo}{"); cat(p0[ix], sep = ", "); cat("}\n");
# cat("\\readlist{\\arrayPa}{"); cat(p1[ix], sep = ", "); cat("}\n");
# cat("\\readlist{\\arrayFo}{"); cat(cumsum(p0[ix]), sep = ", "); cat("}\n");
# cat("\\readlist{\\arrayFa}{"); cat(cumsum(p1[ix]), sep = ", "); cat("}\n");

cat # m=^.^=m~~ 
