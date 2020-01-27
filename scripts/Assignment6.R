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
   is_child = child_subject,
   child_proportion = sum(child_subject)/length(unique(ind))
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


clear_x <- function() {
   theme(axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank())
}

kids_prior <- ggplot(data = tibble(x = c(4.5, 7)), aes(x = x)) + 
   mapply(function(mean, sd) {
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), alpha = 0.01)
   }, 
   mean = samples_A6$mu[14000:16000] + samples_A6$phi[14000:16000],
   sd = samples_A6$tau[14000:16000]) + 
   stat_function(fun = dnorm, args = list(
                       mean = mean(samples_A6$mu + samples_A6$phi),
                       sd = mean(samples_A6$tau))) +
   annotate("text", label = "Kids", x = -Inf, y = Inf, hjust = -1, vjust = 2, size = 2) +
   clear_x() +
   ggtitle("Prior distribution for expected log(reaction_time)", 
           subtitle = "Plotted with the last 2000 MCMC draws, and the chain mean")

adults_prior <- ggplot(data = tibble(x = c(4.5, 7)), aes(x = x)) + 
   mapply(function(mean, sd) {
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), alpha = 0.01)
   }, 
   mean = samples_A6$mu[14000:16000],
   sd = samples_A6$tau[14000:16000]) + 
   stat_function(fun = dnorm, args = list(
      mean = mean(samples_A6$mu),
      sd = mean(samples_A6$tau))) +
   annotate("text", label = "Adults", x = -Inf, y = Inf, hjust = -1, vjust = 2, size = 2) +
   clear_x()

A5_prior <- ggplot(data = tibble(x = c(4.5, 7)), aes(x = x)) + 
   mapply(function(mean, sd) {
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), alpha = 0.01)
   }, 
   mean = samples_A5$mu[14000:16000],
   sd = samples_A5$tau[14000:16000]) + 
   stat_function(fun = dnorm, args = list(
      mean = mean(samples_A5$mu),
      sd = mean(samples_A5$tau))) +
   annotate("text", label = "Assignment 5", x = -Inf, y = Inf, hjust = -1, vjust = 2, size = 2) +
   xlab("log(reaction_time)") 



q3 <- (kids_prior / adults_prior / A5_prior) * ylab("") * theme_bw()


ggplot(data = tibble(x = c(4.5, 7)), aes(x = x)) + 
   stat_function(fun = dnorm, args = 
                    list(mean = mean(samples_A5$mu), sd = mean(samples_A5$tau)), 
                 aes(color = "Assignment 5")) + 
   stat_function(fun = dnorm, args = 
                    list(mean = mean(samples_A6$mu), sd = mean(samples_A6$tau)), 
                 aes(color = "Adults")) + 
   stat_function(fun = dnorm, args = 
                    list(mean = mean(samples_A6$mu + samples_A6$phi), sd = mean(samples_A6$tau)), 
                 aes(color = "Kids")) +
   theme_bw() + theme(legend.position = c(0.8, 0.9), legend.title = element_blank()) + 
   ylab("") + xlab("log_reaction(time)") +
   ggtitle("Prior distributions for log(reaction_time)", 
   subtitle = "The adults, kids and from assignment 5")

# 4
#
unknown <- samples_A6$reaction_time_ppc_unknown
kid <- samples_A6$reaction_time_ppc_child
adult <- samples_A6$reaction_time_ppc_adult

hdi.unknown <- hdi(unknown, ci = 0.95)
hdi.kid <- hdi(kid, ci = 0.95)
hdi.adult <- hdi(adult, ci = 0.95)

mean.unknown <- mean(unknown)
mean.kid <- mean(kid)
mean.adult <- mean(adult)

q4 <- tibble(kid = kid, unknown = unknown, adult = adult) %>% 
   pivot_longer(cols = everything(), names_to = "age", values_to = "reaction_time") %>% 
   mutate(age = factor(age))

xdelta <- 15
hh <- ggplot(data = q4, aes(x = reaction_time)) + 
   geom_histogram(aes(y = ..density.., fill = age, color = age), alpha = 0.5, binwidth = 5) +
   theme_bw() + theme(legend.position = c(0.8, 0.85)) + ylab("") + xlab("") +
   coord_cartesian(xlim = c(90, 1000)) +
   ggtitle("Posterior predictive distributions", 
           subtitle = "Adult, kid and a proportionally mixed population")

stats <- ggplot(data = q4, aes(x = reaction_time)) + 
   geom_segment(aes(x = mean.kid, xend = mean.kid, 
                    y = 3.25, yend = 2.75, color = "kid")) + 
   annotate("text", x = mean.kid, y = 3.4, label = format(mean.kid, digits = 2)) +
   geom_segment(aes(x = mean.adult, xend = mean.adult,
                    y = 2.25, yend = 1.75, color = "adult")) +
   annotate("text", x = mean.adult, y = 2.4, label = format(mean.adult, digits = 2)) +
   geom_segment(aes(x = mean.unknown, xend = mean.unknown,
                    y = 1.25, yend = 0.75, color = "unknown")) +
   annotate("text", x = mean.unknown, y = 1.4, label = format(mean.unknown, digits = 2)) +
   
   geom_segment(aes(x = hdi.kid$CI_low, xend = hdi.kid$CI_high, y = 3, yend = 3, color = "kid"), size = 2) +
   annotate("text", x = hdi.kid$CI_low - xdelta,  y = 3, label = format(hdi.kid$CI_low, digits = 2)) + 
   annotate("text", x = hdi.kid$CI_high + xdelta, y = 3, label = format(hdi.kid$CI_high, digits = 2)) +
   geom_segment(aes(x = hdi.adult$CI_low, xend = hdi.adult$CI_high, y = 2, yend = 2, color = "adult"), size = 2) +
   annotate("text", x = hdi.adult$CI_low - xdelta,  y = 2, label = format(hdi.adult$CI_low, digits = 2)) + 
   annotate("text", x = hdi.adult$CI_high + xdelta, y = 2, label = format(hdi.adult$CI_high, digits = 2)) +
   geom_segment(aes(x = hdi.unknown$CI_low, xend = hdi.unknown$CI_high, y = 1, yend = 1, color = "unknown"), size = 2) +
   annotate("text", x = hdi.unknown$CI_low - xdelta,  y = 1, label = format(hdi.unknown$CI_low, digits = 2)) + 
   annotate("text", x = hdi.unknown$CI_high + xdelta, y = 1, label = format(hdi.unknown$CI_high, digits = 2)) +
   theme_bw() + ylab("") + coord_cartesian(xlim = c(90, 1000)) + theme(legend.position = "none") + 
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())


(hh / stats) + plot_layout(heights = c(5, 1))
