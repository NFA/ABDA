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
#child_ind <- scan(file = here("data", "A6_child_ind.csv"), sep = ",", quiet = TRUE)
child_subject <- scan(file = here("data", "A6_child_subject.csv"), sep = ",", quiet = TRUE)
# My ID is 11

reaction_time_data <- list(
   N_data        = length(y),
   reaction_time = y,
   N_subjects    = length(unique(ind)),
   subjects      = ind,
   is_child = child_subject
)



cat("Compiling model. ")
tic("Model compiled")
reaction_time_model_A5 <- stan_model(file = here("models", "Assignment5_logtrans.stan"), 
                                  model_name = "Reaction Time A5")
reaction_time_model_A6 <- stan_model(file = here("models", "Assignment6_logtrans.stan"), 
                                     model_name = "Reaction Time A6")
toc()

cat("Sampling. ")
tic("Sampling done")
stan_samples_A5 <- sampling(reaction_time_model_A5, data = reaction_time_data, iter = 8000)
stan_samples_A6 <- sampling(reaction_time_model_A6, data = reaction_time_data, iter = 8000)
toc()

samples_A5 <- rstan::extract(stan_samples_A5)
samples_A6 <- rstan::extract(stan_samples_A6)

# 1. 
# Do the two groups (i.e. adults and kids) have different log(reaction times) and if so 
# what is the effect? (provide posterior point and interval estimates (mean, hdi, etc.))
mean(samples_A6$phi)
hdi(samples_A6$phi, ci = 0.95)
# 2.
# Posterior of tau is different compared to the previous case of using no indicator 
# for kids and adults (i.e. Assignment 5). 
# a) Provide plots and estimates (mean, hdi, etc.) of tau in both cases (i.e. Assignment 5 and 6)
# b) Why are they different?
# c) What does this mean in terms of shrinkage?

hdi.A5 <- hdi(samples_A5$tau, ci = 0.95)
hdi.A6 <- hdi(samples_A6$tau, ci = 0.95)

tau <- tibble(A5 = samples_A5$tau, A6 = samples_A6$tau) %>% 
   pivot_longer(cols = everything(), names_to = "model", values_to = "tau") %>% 
   mutate(model = factor(model))

tau_lbls <- c(
   "Mean A5" = paste0("Mean (A5):   ", format(mean(samples_A5$tau), digits = 3)),
   "Mean A6" = paste0("Mean (A6): ", format(median(samples_A6$tau), digits = 3)),
   "HDI A5 " = paste0("HDI:    [", 
                format(hdi.A5$CI_low, digits = 3),", ",
                format(hdi.A5$CI_high, digits = 3), "]"),
   "HDI A6 " = paste0("HDI:    [", 
                      format(hdi.A6$CI_low, digits = 3),", ",
                      format(hdi.A6$CI_high, digits = 3), "]")
)


xdelta <- 0.01
ggplot(data = tau, aes(x = tau)) + 
   geom_histogram(aes(y = ..density.., fill = model, color = model), alpha = 0.5, bins = 200) +
   geom_segment(aes(x = mean(samples_A5$tau), xend = mean(samples_A5$tau), 
                    y = -0.75, yend = -1.25, color = "A5")) + 
   annotate("text", x = mean(samples_A5$tau), y = -0.6, label = format(mean(samples_A5$tau), digits = 2)) +
   geom_segment(aes(x = mean(samples_A6$tau), xend = mean(samples_A6$tau),
                    y = -1.75, yend = -2.25, color = "A6")) +
   annotate("text", x = mean(samples_A6$tau), y = -1.6, label = format(mean(samples_A6$tau), digits = 2)) +
   geom_segment(aes(x = hdi.A5$CI_low, xend = hdi.A5$CI_high, y = -1, yend = -1, color = "A5"), size = 2) +
   annotate("text", x = hdi.A5$CI_low - xdelta,  y = -1, label = format(hdi.A5$CI_low, digits = 2)) + 
   annotate("text", x = hdi.A5$CI_high + xdelta, y = -1, label = format(hdi.A5$CI_high, digits = 2)) +
   geom_segment(aes(x = hdi.A6$CI_low, xend = hdi.A6$CI_high, y = -2, yend = -2, color = "A6"), size = 2) +
   annotate("text", x = hdi.A6$CI_low - xdelta,  y = -2, label = format(hdi.A6$CI_low, digits = 2)) + 
   annotate("text", x = hdi.A6$CI_high + xdelta, y = -2, label = format(hdi.A6$CI_high, digits = 2)) +
   theme_bw() + theme(legend.position = "none") + ylab("") + xlab(expression(tau))

# tau is the group standard deviation, in assignment A5 it had to include for the differences
# in reaction time between an adult and a child. when this difference has been decoupled, the
# standard deviation for the group reaction time shrunk as the difference is now reflected in 
# the individual thetas

# 
   
# 3.
# Plot the two prior distributions for the expected log(reaction time), one for kids and one 
# for adults (i.e. prior for theta in Gelman’s approach and prior for (theta+phi*child) in 
# Kruschke’s approach when child = 0,1). Compare against a single prior of theta in 
# Assignment 5. When plotting the prior you may plot it using the mean value of its dependent parameters.
# a) Explain what you see and can you give an explanation to why the curves look like they do?

N <- 1e4
kids <- rnorm(n = N, mean = samples_A6$mu + samples_A6$phi, sd = samples_A6$tau)
adults <- rnorm(n = N, mean = samples_A6$mu, sd = samples_A6$tau)

hdi.kids <- hdi(kids, ci = 0.95)
hdi.adult <- hdi(adult, ci = 0.95)

q3 <- tibble(kids = kids, adults = adults) %>% 
   pivot_longer(cols = everything(), names_to = "age", values_to = "reaction_time") %>% 
   mutate(age = factor(age))



xdelta <- 0.05
ggplot(data = q3, aes(x = reaction_time)) + 
   geom_histogram(aes(y = ..density.., fill = age, color = age), alpha = 0.5, bins = 200) +
   geom_segment(aes(x = mean(kids), xend = mean(kids), 
                    y = -0.2, yend = -0.3, color = "kids")) + 
   annotate("text", x = mean(kids), y = -0.15, label = format(mean(kids), digits = 2)) +
   geom_segment(aes(x = mean(adults), xend = mean(adults),
                    y = -0.45, yend = -0.55, color = "adults")) +
   annotate("text", x = mean(adults), y = -0.4, label = format(mean(adults), digits = 2)) +
   geom_segment(aes(x = hdi.kids$CI_low, xend = hdi(kids)$CI_high, y = -0.25, yend = -0.25, color = "kids"), size = 2) +
   annotate("text", x = hdi.kids$CI_low - xdelta,  y = -0.25, label = format(hdi.kids$CI_low, digits = 2)) + 
   annotate("text", x = hdi.kids$CI_high + xdelta, y = -0.25, label = format(hdi.kids$CI_high, digits = 2)) +
   geom_segment(aes(x = hdi.adults$CI_low, xend = hdi.adults$CI_high, y = -0.5, yend = -0.5, color = "adults"), size = 2) +
   annotate("text", x = hdi.adults$CI_low - xdelta,  y = -0.5, label = format(hdi.adults$CI_low, digits = 2)) + 
   annotate("text", x = hdi.adults$CI_high + xdelta, y = -0.5, label = format(hdi.adults$CI_high, digits = 2)) +
   theme_bw() + theme(legend.position = "none") + ylab("") + xlab("log(reaction_time")
   
