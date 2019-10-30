library(tictoc)
library(here)

library(rstan)
library(bayestestR)

# Configuration options to parallelize the computation and cache the model object
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#model_file <- here("models", "Assignment3TaskA.stan")
model_file <- here("models", "jointBernoulli.stan")


# Convenience function to convert the input data to the format Stan expects
get_data <- function(ys, zs) { list(y = ys, Ny = length(ys),) }

# Data for Assignment 4 Task A.2.a
# z = 11, N = 14
y <- c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1)
# z = 3, N = 10
z <- c(1, 0, 0, 0, 0, 0, 0, 1, 1, 0)

data <- list(
  y = y, Ny <- length(y),
  z = z, Nz <- length(z)
)

data <- list(y = y, N = length(y))

tic("Compiling model")
model <- stan_model(file = model_file, model_name = "Task A.2")
toc()


tic("Sampling the posterior")
# Stan's default is to run 2000 iterations, discarding the first 1000
# as warmup (ie. keeping the iterations after "burn-in")
# For this example the ESS estimated as n_eff  ~= 1500 with default 
# arguments. Increasing the iterations to 7000, provides a n_eff of
# around 12000 for both theta_y and theta_z

samples <- sampling(model, data = data, iter = 7000)
toc()

# What is the expected probability of getting a head?
# Include a 95% credible interval

# prior is beta(1,1)
# posterior update with z = 11, N = 14
# posterior is beta(1 + 11, 1 + N - z)
# posterior is beta(12, 4)
# Expected value of a beta distribution is a/(a+b) 
# E[theta] = 12/(12+4) = 12/16 = 0.75
# mode(theta) should be (a-1)(a+b-2) = 0.7857...



# Extract the parameters from the model
params <- rstan::extract(samples)

# Numeric estimaters
mean(params$theta_y)
median(params$theta_y)
map_estimate(params$theta_y)


#p 184
hdi(params$theta_y, ci = 0.95)


# What is the probability that theta > 0.5
cat("\n1) Using generated quantities, p(theta) > 0.5 =", mean(params$prob05))
cat("\n2) Using theta samples, p(theta_y) > 0.5 =", length(params$theta_y[params$theta_y > 0.5])/length(params$theta_y))
