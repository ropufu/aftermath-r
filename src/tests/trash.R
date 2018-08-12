
## Clear Environment
ropufu::clear();

# ropufu::build("ropufu")
# ropufu::build("changept")

## Setup
size <- 5;
bound <- 3;
r <- 10;

S <- matrix(round(stats::runif(size * size, min = -r, max = r)), size, size);
S <- S + t(S);

next_mask <- function(current)
{
    pos <- 1;
    while (pos <= length(current) && current[pos])
    {
        current[pos] <- FALSE;
        pos <- pos + 1;
    }
    #' Return a vector of all FALSE's if there is no room.
    if (pos == length(current) + 1) return(current);
    #' Otherwise set the least significant FALSE to TRUE. 
    current[pos] <- TRUE;
    return(current);
}

max_sum <- -Inf;
max_mask <- rep(FALSE, times = size);
mask <- next_mask(rep(FALSE, times = size));
#for (i in 1 : ropufu::nchoosek(size, bound))
while (any(mask))
{
    if (sum(mask) == bound)
    {
        new_sum <- sum(S[mask, mask]);
        if (max_sum < new_sum) 
        {
            max_sum <- new_sum;
            max_mask <- mask;
        }
    }
    mask <- next_mask(mask);
}
cat("Max sum =", max_sum, "at", (1 : size)[max_mask], "\n")


# ~~ Here Be Dragons ~~
return()
