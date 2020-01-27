library(rstan)
library(tidyverse)

library(bayestestR)
library(bayesplot)

library(here)
library(tictoc)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Reaction times
y <- scan(file = here("data", "A5_y.csv"), sep = ",", quiet = TRUE)
# ID for the individuals
ind <- scan(file = here("data", "A5_ind.csv"), sep = ",", quiet = TRUE)
# Indicator if child

child_subject <- scan(file = here("data", "A6_child_subject.csv"), sep = ",", quiet = TRUE)
# My ID is 11

attempts <- scan(file = here("data", "A7_attempts.csv"), sep = ",", quiet = TRUE)

reaction_time_data <- list(
   N_data        = length(y),
   reaction_time = y,
   N_subjects    = length(unique(ind)),
   attempts      = attempts,
   subjects      = ind,
   is_child      = child_subject,
   child_proportion = sum(child_subject)/length(unique(ind))
)

#stan_samples <- readRDS(here("data", "A7_samples.rds"))
cat("Compiling.\n")
tic("Compilation done")
reaction_time_model <- stan_model(file = here("models", "A7.stan"), 
                                     model_name = "Reaction Time A7")
toc()
tic("Sampling done")
stan_samples <- sampling(reaction_time_model, data = reaction_time_data, iter = 4000)
toc()
#
samples <- rstan::extract(stan_samples)


#expected_reaction_time <- function(ind, attempt) {
#   theta <- samples$theta[, 1, ind] + samples$theta[, 2, ind] * attempt
#   sigma <- samples$sigma
#   exp(theta + sigma^2/2)
#   
#}
#
#data <- rbind(
#   tibble(individual = 1, attempt = 1, time = expected_reaction_time(1,1)),
#   tibble(individual = 1, attempt = 5, time = expected_reaction_time(1,5)),
#   tibble(individual = 3, attempt = 1, time = expected_reaction_time(3,1)),
#   tibble(individual = 3, attempt = 5, time = expected_reaction_time(3,5)),
#   tibble(individual = 5, attempt = 1, time = expected_reaction_time(5,1)),
#   tibble(individual = 5, attempt = 5, time = expected_reaction_time(5,5))
#          ) %>% mutate(individual = factor(individual, labels = c("Oliver", "Jesper", "The Dude")), 
#                       attempt = factor(attempt))
#
#stats <- data %>% group_by(individual, attempt) %>% 
#   summarise(mean = mean(time),
#             hdi_low = hdi(time, ci = 0.95)$CI_low,
#             hdi_high = hdi(time, ci = 0.95)$CI_high)
#
#
#ggplot(data, aes(x = time, fill = attempt)) + 
#   facet_wrap(~individual, scales = "free_y") + theme_bw() + 
#   geom_density(alpha = 0.4) + 
#   geom_vline(data = stats, aes(xintercept = mean, fill = attempt)) +
#   geom_label_repel(data = stats, aes(x = mean, y = -0.0005, label = round(mean, 0))) +
#   scale_fill_brewer(palette="Dark2")
#
#
#lcs_sample <- rbind(
#   #sample_n(linear_coeffs(1), size = 300)
#   sample_n(linear_coeffs(3), size = 300)
#   #sample_n(linear_coeffs(5), size = 300)
#)
#
#lcs_stats <- rbind(
#   #linear_coeffs(1) %>% group_by(individual) %>% summarize_all(list(mean = mean))
#   linear_coeffs(3) %>% group_by(individual) %>% summarize_all(list(mean = mean))
#   #linear_coeffs(5) %>% group_by(individual) %>% summarize_all(list(mean = mean))
#)
#
#
#linear_coeffs <- function(ind) {
#   theta0 <- samples$theta[, 1, ind]
#   theta1 <- samples$theta[, 2, ind]
#   sigma <- samples$sigma
#   
#   tibble(individual = ind, theta0 = theta0, theta1 = theta1)
#}
#
#my_fun <- function(theta0, theta1, x) {
#   exp(theta0 + theta1*x)
#}
#
#jesper <- ggplot(data = tibble(x = c(0, 20)), aes(x)) + 
#   ylab("Reaction time") + xlab("Attempts") +
#   ggtitle("Reaction time for Jesper") + 
#   expand_limits(y = c(100, 800)) + theme_bw() + 
#   mapply(function(theta0, theta1) {
#      stat_function(fun = my_fun, args = list(theta0 = theta0, theta1 = theta1), 
#                    xlim = c(0, 20), alpha = 0.1)      
#   },
#   theta0 = lcs_sample$theta0,
#   theta1 = lcs_sample$theta1) +
#   mapply(function(theta0, theta1) {
#      stat_function(fun = my_fun, 
#                   args = list(theta0 = theta0, theta1 = theta1),
#                   xlim = c(0, 20), color = "steelblue", size = 1.5) 
#   },
#   theta0 = lcs_stats$theta0_mean,
#   theta1 = lcs_stats$theta1_mean)
#
#   
#
#
