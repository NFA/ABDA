library(here)
source(here("scripts", "slice-sampling.R"))

# Data
y <- c(0, 1, 0, 1, 0, 0, 0, 0, 0, 1)

in_range <- function(theta) { !(theta < 0.0 || theta > 1.0) }

# Beta log-prior
log_prior <- function(a, b, theta) {
  if (!in_range(theta)) return(-Inf)
  (a - 1) * log(theta) + (b - 1) * log(1 - theta)
}

# Bernoulli log-loglikelihood
log_likelihood <- function(y, theta) {
  if (!in_range(theta)) return(-Inf)
  Y <- sum(y)
  n <- length(y)
  Y * log(theta) + (n - Y) * log(1 - theta)
}

log_posterior <- function(theta) { 
  log_likelihood(y = y, theta) + log_prior(a = 1, b = 1, theta = theta)
}

posterior <- function(theta) { exp(log_posterior(theta)) }

slice_b <- function() {
  slice_sampling(posterior, initial_point = 0.5, scale_estimates = 1)
}


log_pdf <- function(x) {
  mu1 <- c(1, 2)
  mu2 <- c(-1, 2)
  C1 <- matrix(c(5, 5, 5, 10) , nrow = 2) / 10
  C2 <- matrix(c(10, -5, 5, 5), nrow = 2) / 10
  x1 <- x - mu1
  x2 <- x - mu2
  
  drop(
    log(
      0.5 * exp(-((x1 %*% solve(C1)) %*% x1)) + 
      0.5 * exp(-((x2 %*% solve(C2)) %*% x2))
  ))
}

pdf <- function(x) exp(log_pdf(x))


slice_u <- function() {
  slice_sampling(pdf, initial_point = c(1, 1), scale_estimates = c(1, 1), iter = 50000)
}