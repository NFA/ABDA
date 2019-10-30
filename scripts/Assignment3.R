## Task A

# A.1 Can be found in the presentation

# A.2
probabilities <- function(prior = c(0.001, 0.999)) {
  probs <- matrix(
    nrow = 2, ncol = 2,
    dimnames = list(
      c("Positive", "Negative"), 
      c("Disease", "No Disease")
    )
  )
  hit_rate <- c(0.99, 0.05)
  probs["Positive",] <- hit_rate * prior
  probs["Negative",] <- (1 - hit_rate) * prior
  probs
}

posterior <- function(test_result = NULL, prior = c("Disease" = 0.001, "No Disease" = 0.999)) {
  if (is.null(test_result)) return(prior)
  
  p <- probabilities(prior)

  c(
    "Disease"    = p[test_result, "Disease"]    / sum(p[test_result,]),
    "No Disease" = p[test_result, "No Disease"] / sum(p[test_result,])
  )
}

disease_with_test_results <- function(results) {
  post_prob <- posterior()

  for (result in results) {
    post_prob <- posterior(result, prior = post_prob)
  }
  post_prob
}

# Task B


bernoulli_pdf <- function(y, theta) {
  theta^y * (1 - theta)^(1 - y)
}

bernoulli_likelihood <- function(y, theta) {
  prod(bernoulli_pdf(y = y, theta = theta))
}


bernoulli_probability <- function(theta) {
  c(
    "y = 0" = bernoulli_pdf(y = 0, theta = theta), 
    "y = 1" = bernoulli_pdf(y = 1, theta = theta)
  )
}

bernoulli_probability(theta = 0.4)


# B.2
barplot(bernoulli_probability(theta = 0.25), ylim = c(0, 1))
barplot(bernoulli_probability(theta = 0.50), ylim = c(0, 1))

# B.3
plot(function(x) bernoulli_pdf(y = 0, theta = x), from = 0, to = 1, 
     xlab = expression(theta), ylab = "likelihood", main = "y = 0")
plot(function(x) bernoulli_pdf(y = 1, theta = x), from = 0, to = 1, 
     xlab = expression(theta), ylab = "likelihood", main = "y = 1")

# B.4 
# eq  i is defined on lines [43, 45]
# eq ii is defined on lines [47, 49]

# Create a vector of N flips
# Evaluate the likelihood for theta = 0.5 for N = 10, 1000, 100000

# B4a
coin_toss <- function(n) {
  fair <- function(theta) { if (theta < 0.5) 1 else 0 }
  
  sapply(runif(n), fair)
}

bernoulli_likelihood(coin_toss(n = 10), theta = 0.5)
bernoulli_likelihood(coin_toss(n = 1000), theta = 0.5)
bernoulli_likelihood(coin_toss(n = 100000), theta = 0.5)

# B4b
bernoulli_logpdf <- function(y, theta) {
  y*log(theta) + (1 - theta)*log(1 - theta)  
}

bernoulli_loglikelihood <- function(y, theta) {
  Y <- sum(y)
  n <- length(y)
  Y*log(theta) + (n - Y)*log(1 - theta)
}

bernoulli_loglikelihood(coin_toss(n = 10), theta = 0.5)
bernoulli_loglikelihood(coin_toss(n = 1000), theta = 0.5)
bernoulli_loglikelihood(coin_toss(n = 100000), theta = 0.5)

#B4c
bernoulli_likelihood(coin_toss(n = 10), theta = 0.5)
exp(bernoulli_loglikelihood(coin_toss(n = 10), theta = 0.5))

#B4d
# Plot the likelihood function with respect to theta in [0, 1]
# y = [1], y = [1, 1], y = [1, 1, 0, 1]


ggplot(data = data.frame(x = 0), aes(x = x)) + theme_minimal() + 
  xlim(0, 1) + xlab("θ") + ylab("likelihood(θ)") +
  stat_function(fun = bernoulli_loglikelihood, args = list(y = c(1)), size = 1.2, aes(color = "y = [1]")) + 
  stat_function(fun = bernoulli_loglikelihood, args = list(y = c(1,1)), size = 1.2, aes(color = "y = [1, 1]")) + 
  stat_function(fun = bernoulli_loglikelihood, args = list(y = c(1,1,0,1)), size = 1.2, aes(color = "y = [1, 1, 0, 1]")) +
  scale_y_continuous(trans = "exp") + 
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank())

# B4e
# 1 trial with 1 one success puts the most likelihood on 1, while keeping 
# likelihood on other values of theta
# 2 trials, two successes, likelihood for 1 is still the same but all other values
# for theta have become less likely
# 4 trials, 3 successes
# likelihood for theta = 0 is now zero and a wide mode in the likelihood distribution
# can be seen


# B5

B <- function(a, b) {
  #integrate(f = function(theta) theta^(a - 1) * (1 - theta)^(b - 1), lower = 0, upper = 1)$value
  gamma(a) * gamma(b) / gamma(a + b)
}

posterior_beta <- function(theta, z, N) {
  a <- b <- 1
  exp(((z + a) - 1) * log(theta) + ((N - z + b) - 1) * log(1 - theta) - log(B(z + a, N - z + b)))
}


ggplot(data = data.frame(x = 0), aes(x = x)) + theme_minimal() + 
  xlim(0, 1) + xlab("theta") + ylab("p(theta|y)") +
  stat_function(fun = posterior_beta, args = list(z = 1, N = 1), size = 1.2, aes(color = "y = [1]")) + 
  stat_function(fun = posterior_beta, args = list(z = 2, N = 2), size = 1.2, aes(color = "y = [1, 1]")) + 
  stat_function(fun = posterior_beta, args = list(z = 3, N = 4), size = 1.2, aes(color = "y = [1, 1, 0, 1]")) +
  theme(legend.position = c(0.1, 0.9), legend.title = element_blank())


likelihood <- function(y, theta) exp(bernoulli_loglikelihood(y, theta))


#HDI_vec <- function(xs, prob = 0.95) {
#  HDI_range <- tibble(idx = seq_along(xs), x = xs) %>% 
#    arrange(desc(x)) %>% 
#    mutate(cumsum = cumsum(x)) %>%
#    filter(cumsum < sum(xs)*prob)
#  
#  x_min <- min(HDI_range$idx)
#  y_min <- xs[x_min]
#  x_max <- max(HDI_range$idx)
#  y_max <- xs[x_max]
#  
#  data.frame(x_min = x_min, y_min = y_min,
#             x_max = x_max, y_max = y_max)
#}

library(tidyverse)

HDI <- function(.tbl, prob = 0.95) {
  HDI_range <- .tbl %>% arrange(desc(value)) %>%
    mutate(cumsum = cumsum(value)) %>%
    filter(cumsum < sum(value)*prob)
  
  min <-  HDI_range %>%
    filter(theta == min(theta)) %>%
    select(column, distribution, theta, value)
  
  max <- HDI_range %>%
    filter(theta == max(theta)) %>%
    select(column, distribution, theta, value)
  
  inner_join(min, max, by = c("column", "distribution"), suffix = c(".min", ".max"))
}


fig6.4_data <- function(a, b, col) {
  z <- 17
  N <- 20
  tibble(theta = seq(from = 0, to = 1, by = 0.001), column = col) %>% 
    mutate(
      prior      = dbeta(theta, shape1 = a, shape2 = b),
      likelihood = dbinom(x = z, size = N, prob = theta),
      posterior  = dbeta(theta, shape1 = z + a, shape2 = N - z + b)
    ) %>%
    pivot_longer(cols = prior:posterior, names_to = "distribution")
}

dist_lvls <- c("prior", "likelihood", "posterior")

A <- fig6.4_data(a = 100, b = 100, col = "A")
B <- fig6.4_data(a = 18.25, b = 6.75, col = "B")
C <- fig6.4_data(a = 1, b = 1, col = "C")

HDI_data <- rbind(
  A %>% filter(distribution == "prior")     %>% HDI, 
  A %>% filter(distribution == "posterior") %>% HDI,
  B %>% filter(distribution == "prior")     %>% HDI, 
  B %>% filter(distribution == "posterior") %>% HDI, 
  #C %>% filter(distribution == "prior")     %>% HDI, uniform
  C %>% filter(distribution == "posterior") %>% HDI 
) %>% 
  mutate(distribution = factor(distribution, levels = dist_lvls))

fig6.4 <- rbind(A, B, C) %>% 
  mutate(distribution = factor(distribution, levels = dist_lvls))

ggplot(data = fig6.4, aes(x = theta)) + theme_light() + 
  geom_area(aes(y = value), fill = "#88b5d6") + 
  #geom_line(aes(y = value)) +
  #HDI Line
  geom_segment(data = HDI_data, aes(x = theta.min, y = value.min, xend = theta.max, yend = value.min)) +
  #HDI anchors
  geom_segment(data = HDI_data, aes(x = theta.min, y = 0, xend = theta.min, yend = value.min), linetype = "dashed") +
  geom_segment(data = HDI_data, aes(x = theta.max, y = 0, xend = theta.max, yend = value.min), linetype = "dashed") +
  geom_text(data = HDI_data, aes(x = theta.min, y = value.min, label = theta.min), nudge_y = 0.5, nudge_x = -0.1, size = 3) +
  geom_text(data = HDI_data, aes(x = theta.max, y = value.min, label = theta.max), nudge_y = 0.5, nudge_x = +0.1, size = 3) +
  facet_wrap(distribution ~ column, labeller = labeller(distribution = str_to_title), scales = "free_y") 



