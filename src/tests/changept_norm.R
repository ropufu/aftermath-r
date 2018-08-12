
## Clear Environment
ropufu::clear();

# ropufu::build("ropufu")
# ropufu::build("changept")

## Setup
size <- 6;

mu    <- rep(0.0, times = size);
theta <- rep(1.0, times = size);
A <- matrix(runif(size * size, min = -0.2, max = 0.2), size, size);
diag(A) <- 1.0 + runif(size, min = 0.0, max = 0.2);
#S <- diag(size);
S <- A %*% t(A);

ch <- changept::change_norm(mean_before = mu, mean_after = theta, covar = S, size = size);

a1 <- a2 <- rep(FALSE, times = size);
a1[c(1, 3, 4)] <- TRUE;
a2[c(2, 3, 5, 6)] <- TRUE;


eta1 <- eta2 <- mu;
eta1[a1] <- theta[a1];
eta2[a2] <- theta[a2];

## Monte-carlo
mc_count <- 1000;

## Test Log-likelihood Ratios.
llr_basis_before <- ch$loglikelihood_basis(mu, n = mc_count);
llr_basis_after1 <- ch$loglikelihood_basis(eta1, n = mc_count);
llr_basis_after2 <- ch$loglikelihood_basis(eta2, n = mc_count);

ctor <- ch$loglikelihood_constructor(a1);
l1 <- ctor(llr_basis_before);
l2 <- ch$bruteforce_loglikelihood(mu, a1, n = mc_count);
l3 <- ch$oracle_loglikelihood_before(a1, n = mc_count);


## Output
print(c(mean(l1), sd(l1)));
print(c(mean(l2), sd(l2)));
print(c(mean(l3), sd(l3)));

# par(mfrow = c(3, 1));
# qqplot(l1,l2)
# qqplot(l1,l3)
# qqplot(l2,l3)

# ~~ Here Be Dragons ~~
return()
