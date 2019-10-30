library(bayesboot)
library(tidyverse)
library(cowplot)
library(gridGraphics)
library(here)

library(rstan)

# Configuration options to parallelize the computation and cache the model object
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Task A
# 1) Recreate Figure 6.4 using MCMC
#
# Priors: beta(100, 100)
#         beta(18.25, 6.75)
#         beta(1, 1)
#
# Likelihood: Bernoulli(theta)

# Stan code for the prior, likelihood and posterior
# the code contain ${x} tokens that are used for string interpolation to provide
# the shape parameters for the three different priors.
prior_code <- "
parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  theta ~ beta(${a}, ${b});
}

"

likelihood_code <- "
data {
  int<lower = 0> N;
  int y[N];
}

parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  y ~ bernoulli(theta);
}

"

posterior_code <- "
data {
  int<lower = 0> N;
  int y[N];
}

parameters {
  real<lower = 0, upper = 1> theta;
}

model {
  theta ~ beta(${a}, ${b});
  y ~ bernoulli(theta);
}

"

# Specify the shape parameters for the beta priors, used for string interpolation
beta_left   <- list(a = 100,   b = 100)
beta_middle <- list(a = 18.25, b = 6.75)
beta_right  <- list(a = 1,     b = 1)

# Prepare the prior code for each prior by string interpolation
left_prior_code   <- str_interp(prior_code, beta_left)
middle_prior_code <- str_interp(prior_code, beta_middle)
right_prior_code  <- str_interp(prior_code, beta_right)

# Prepare the posterior code for each prior by string interpolation
left_posterior_code   <- str_interp(posterior_code, beta_left)
middle_posterior_code <- str_interp(posterior_code, beta_middle)
right_posterior_code  <- str_interp(posterior_code, beta_right)

# Convenience function to convert the input data to the format Stan expects
get_data <- function(ys) { list(y = ys, N = length(ys)) }

# Data for Figure 6.4. z = 17 successes out of N = 20 trials 
y <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)

# Compile the code. Sampling from the likelihood and the posterior needs the 
# data supplied as an argument
cat("Starting Stan compilation...")
tic()
left_prior      <- stan(model_code = left_prior_code)
left_likelihood <- stan(model_code = likelihood_code, data = get_data(y))
left_posterior  <- stan(model_code = left_posterior_code, data = get_data(y))

middle_prior      <- stan(model_code = middle_prior_code)
middle_likelihood <- stan(model_code = likelihood_code, data = get_data(y))
middle_posterior  <- stan(model_code = middle_posterior_code, data = get_data(y))

right_prior      <- stan(model_code = right_prior_code)
right_likelihood <- stan(model_code = likelihood_code, data = get_data(y))
right_posterior  <- stan(model_code = right_posterior_code, data = get_data(y))
toc()

# Extract samples from the chains
left_prior_samples      <- rstan::extract(left_prior)$theta
left_likelihood_samples <- rstan::extract(left_likelihood)$theta
left_posterior_samples  <- rstan::extract(left_posterior)$theta

middle_prior_samples      <- rstan::extract(middle_prior)$theta
middle_likelihood_samples <- rstan::extract(middle_likelihood)$theta
middle_posterior_samples  <- rstan::extract(middle_posterior)$theta

right_prior_samples      <- rstan::extract(right_prior)$theta
right_likelihood_samples <- rstan::extract(right_likelihood)$theta
right_posterior_samples  <- rstan::extract(right_posterior)$theta

# Construct the plots
left_prior_plot      <- ~plotPost(left_prior_samples, showMode = TRUE, cex = 1,
                                  main = "Prior beta(100, 100)", xlab = "theta")
left_likelihood_plot <- ~plotPost(left_likelihood_samples, showMode = TRUE, cex = 1,
                                  main = "Likelihood (Bernoulli)", xlab = "theta")
left_posterior_plot  <- ~plotPost(left_posterior_samples, showMode = TRUE, cex = 1,
                                  main = "Posterior (beta)", xlab = "theta")

middle_prior_plot      <- ~plotPost(middle_prior_samples, showMode = TRUE, cex = 1,
                                  main = "Prior beta(18.25, 6.75)", xlab = "theta")
middle_likelihood_plot <- ~plotPost(middle_likelihood_samples, showMode = TRUE, cex = 1,
                                  main = "Likelihood (Bernoulli)", xlab = "theta")
middle_posterior_plot  <- ~plotPost(middle_posterior_samples, showMode = TRUE, cex = 1,
                                  main = "Posterior (beta)", xlab = "theta")

right_prior_plot      <- ~plotPost(right_prior_samples, showMode = TRUE, cex = 1,
                                    main = "Prior beta(1, 1)", xlab = "theta")
right_likelihood_plot <- ~plotPost(right_likelihood_samples, showMode = TRUE, cex = 1,
                                    main = "Likelihood (Bernoulli)", xlab = "theta")
right_posterior_plot  <- ~plotPost(right_posterior_samples, showMode = TRUE, cex = 1,
                                    main = "Posterior (beta)", xlab = "theta")

# Arrange them into a grid
left   <- plot_grid(left_prior_plot, left_likelihood_plot, left_posterior_plot, ncol = 1)
middle <- plot_grid(middle_prior_plot, middle_likelihood_plot, middle_posterior_plot, ncol = 1)
right  <- plot_grid(right_prior_plot, right_likelihood_plot, right_posterior_plot, ncol = 1)

plot_grid(left, middle, right, ncol = 3)


