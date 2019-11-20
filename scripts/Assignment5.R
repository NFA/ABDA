library(rstan)
library(tidyverse)
library(glue)
library(bayestestR)

library(here)
library(tictoc)

# Reaction times
y <- scan(file = here("data", "A5_y.csv"), sep = ",", quiet = TRUE)
# ID for the individuals
ind <- scan(file = here("data", "A5_ind.csv"), sep = ",", quiet = TRUE)
# My ID is 11

reaction_time_data <- list(
  N_data        = length(y),
  reaction_time = y,
  N_subjects    = length(unique(ind)),
  subjects      = ind
)



cat("Compiling model. ")
tic("Model compiled")
reaction_time_model <- stan_model(file = here("models", "Assignment5_logtrans.stan"), 
                            model_name = "Reaction Time")
toc()

cat("Sampling. ")
tic("Sampling done")
stan_samples <- sampling(reaction_time_model, data = reaction_time_data, iter = 8000)
toc()

samples <- rstan::extract(stan_samples)

# Task A
# 1. What is the expected reaction time for the individual called "the dude"
# in our data set (ind = 4)
# a) Answer this by providing a histogram and appropriate summaries.
dude <- enframe(samples$theta_exp[, 4], name = NULL, value = "samples")
dude_hdi <- hdi(dude$samples, ci = 0.95)

dude_lbls <- c(
  Mean = paste0("Mean:   ", format(mean(dude$samples), digits = 3)),
  Median = paste0("Median: ", format(median(dude$samples), digits = 3)),
  MAP = paste0("MAP:    ", format(map_estimate(dude$samples), digits = 3)),
  HDI = paste0("HDI:    [", 
         format(dude_hdi$CI_low, digits = 3),", ",
         format(dude_hdi$CI_high, digits = 3), "]")
)

dude %>% ggplot(aes(x = samples)) + geom_histogram(binwidth = 5, alpha = 0.3) + 
  geom_vline(aes(xintercept = mean(dude$samples), color = "Mean")) + 
  geom_vline(aes(xintercept = map_estimate(dude$samples), color = "MAP")) + 
  geom_vline(aes(xintercept = median(dude$samples), color = "Median")) + 
  geom_segment(aes(x = dude_hdi$CI_low, xend = dude_hdi$CI_high, y = 0, yend = 0, color = "HDI"), size = 2) + 
  scale_color_manual(name = "Estimates", labels = dude_lbls, 
                     values = c(Mean = "blue", MAP = "green", Median = "red", HDI = "black")) +
  theme_bw() + theme(legend.position = c(0.9, 0.85)) + 
  labs(title = "Reaction time samples from the Dude",
       subtitle = paste(length(dude$samples), "samples."),
       x = "Reaction time (ms)", y = "Number of samples")


# b) With only one measurement, how are we able to give a 95% credible interval
# for "the dude". This is not possible using frequentist statistics


# 2. What is the group's reaction time?
mean(samples$mu_exp)

# 2.a  Given a random individual from our group (i.e. our small population), what is the:
#   i. Expected reaction time for that random individual?
#  ii. Predicted reaction time for a single measurement for that individual?

random <- enframe(samples$reaction_time_ppc, name = NULL, value = "samples")
random_hdi <- hdi(random$samples, ci = 0.95)

rnd_lbls <- c(
  Mean = paste0("Mean:   ", format(mean(random$samples), digits = 3)),
  Median = paste0("Median: ", format(median(random$samples), digits = 3)),
  MAP = paste0("MAP:    ", format(map_estimate(random$samples), digits = 3)),
  HDI = paste0("HDI:    [", 
               format(random_hdi$CI_low, digits = 3),", ",
               format(random_hdi$CI_high, digits = 3), "]")
)

random %>% ggplot(aes(x = samples)) + geom_histogram(binwidth = 5, alpha = 0.3) + 
  geom_vline(aes(xintercept = mean(random$samples), color = "Mean")) + 
  geom_vline(aes(xintercept = map_estimate(random$samples), color = "MAP")) + 
  geom_vline(aes(xintercept = median(random$samples), color = "Median")) + 
  geom_segment(aes(x = random_hdi$CI_low, xend = random_hdi$CI_high, y = 0, yend = 0, color = "HDI"), size = 2) + 
  scale_color_manual(name = "Estimates", labels = rnd_lbls, 
                     values = c(Mean = "blue", MAP = "green", Median = "red", HDI = "black")) +
  theme_bw() + theme(legend.position = c(0.9, 0.85)) + 
  labs(title = "Reaction time samples from new random individual",
       subtitle = paste(length(random$samples), "samples."),
       x = "Reaction time (ms)", y = "Number of samples")

# 3. Provide a figure that compare thetas obtained with our hierarchical 
# Bayesian model above with thetas obtained by treating the participants 
# individually and using the sample means (i.e. theta[j] is in the latter 
#case the sample mean of the j:th participants logarithmic reaction times).
#   a) Can you explain the differences?
  

group_stats <- tibble(ind = ordered(1:34, levels = 1:34), group_mean = mean(samples$mu_exp))

sample_stats <- tibble(time = y, ind = ordered(ind, levels = 1:34)) %>% 
  group_by(ind) %>% 
  summarize(mean = mean(time), sd = sd(time))

thetas <- as_tibble(samples$theta_exp) %>% 
  pivot_longer(everything(), names_to = "ind", values_to = "time", names_pattern = "V(\\d+)") %>%
  mutate(ind = ordered(ind, levels = 1:34))

thetas %>% ggplot(aes(x = time)) + geom_histogram(binwidth = 5, alpha = 0.3) + facet_wrap(~ ind) +
  geom_vline(data = sample_stats, aes(xintercept = mean)) + 
  geom_vline(data = group_stats, aes(xintercept = group_mean), color = "blue")
  theme_bw()

