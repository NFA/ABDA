
# A.1
N <- 500

coin_toss <- function(n) {
  fair <- function(theta) { if (theta < 0.5) 1 else 0 }
  
  sapply(runif(n), fair)
}


toss_data <- coin_toss(n = N)
running_proportion <- cumsum(toss_data)/(1:N)

plot(x = 1:N, y = running_proportion, type = "o", col = "steelblue", log = "x", 
     ylim = c(0.0, 1.0), 
     main = "Running Proportion of Heads",
     ylab = "Proportion Heads", xlab = "Flip Number")
abline(h = 0.5, lty = "dotted")

letter_sequence <- paste(c("T", "H")[toss_data[1:10] + 1], collapse = "")
sequence_text <- paste("Flip Sequence = ", letter_sequence, " ... ", sep = "")

text(x = 30, y = 0.9, sequence_text, adj = 0)
text(x = 30, y = 0.8, paste("End Proportion =", running_proportion[N]), adj = 0)

# Modify to a biased coin

biased_coin_toss <- function(n) {
  bias <- function(theta) { if (theta < 0.25) 1 else 0 }
  
  sapply(runif(n), bias)
}

bias_toss_data <- biased_coin_toss(n = 100)
bias_toss_data <- table(bias_toss_data)/length(bias_toss_data)

bp <- barplot(bias_toss_data, ylim = c(0, 1))
text(x = bp + 0.2, y = bias_toss_data + 0.02, labels = bias_toss_data)

pmf <- dbinom(x = 0:1, size = 1, prob = 0.25)
lines(x = bp, y = pmf, type = "h", col = "red", lwd = 2)
points(x = bp, y = pmf, col = "red", pch = 16, lwd = 2, cex = 2)
text(x = bp - 0.2, y = pmf, col = "red", labels = pmf)


# A.2
# Draw from a normal distribution with mean 3.4 and sigma squared = 3
pdf.normal <- function(x, mu = 3.4, sigma_sq = 3) {
  (1 / sqrt(2 * pi * sigma_sq)) * exp(-(x - mu)^2/(2 * sigma_sq))
}

draw <- rnorm(n = 1e4, mean = 3.4, sd = sqrt(3))

bin.breaks <- seq(min(draw) - 0.1, max(draw) + 0.1, by = 0.1)

hist(draw, breaks = bin.breaks, freq = FALSE, ylim = c(0, pdf.normal(3.4)))
curve(pdf.normal, from = min(draw), to = max(draw), n = 1e4, add = TRUE, col = "red", lwd = 2)

dx <- 0.1
xs <- seq(from = -10, to = 20, by = dx)

ex <- sum(pdf.normal(xs) * xs * dx)
vx <- sum(pdf.normal(xs) * (xs - ex)^2 * dx)

pdf.lognormal <- function(x, mu = 0, sigma_sq = 1) {
  (1 / (x * sqrt(2 * pi * sigma_sq))) * exp(-(log(x - mu))^2/(2 * sigma_sq))
}

x <- rnorm(n = 1e4)
y <- exp(x)

bin.breaks <- seq(from = min(y) - 0.1, to = max(y) + 0.1, by = 0.1)

hist(y, breaks = bin.breaks, freq = FALSE, col = "steelblue", border = "steelblue")
curve(pdf.lognormal, from = min(y), to = max(y), n = 1e4, add = TRUE, col = "red", lwd = 2)

z <- pdf.lognormal(y)

i <- which.max(z) # which(z == max(z)) 
z[i] # max(z)

zmax <- optimize(pdf.lognormal, interval = c(0, 1e4), maximum = TRUE)
zmax$objective

# Task B

library(here)
library(tidyverse)

HEC <- read_csv(here("data", "HairEyeColor.csv"))


HEC <- HEC %>% pivot_wider(names_from = Hair, values_from = Count, names_prefix = "Hair.") %>%
  column_to_rownames(var = "Eye") 

rownames(HEC) <- paste0("Eye.", rownames(HEC))

HEC <- HEC/sum(HEC)

HEC
colSums(HEC)
rowSums(HEC)
sum(HEC)

# 3.a  p(Blue Eyes, Blond Hair)
HEC["Eye.Blue", "Hair.Blond"]

# 3.b p(Brown Hair)
sum(HEC["Eye.Brown",])

# 3.c p(Hair Red | Brown Eyes)
HEC["Eye.Brown","Hair.Red"]/sum(HEC["Eye.Brown",])

# 3.d p( (Red Hair OR Blond Hair) AND (Brown Eyes OR Blue Eyes))
sum(HEC[c("Eye.Brown", "Eye.Blue"),c("Hair.Red", "Hair.Blond")])

# 3.e p( (Red Hair OR Blond Hair) OR (Brown Eyes OR Blue Eyes))
sum(HEC[c("Eye.Brown", "Eye.Blue"),]) + sum(HEC[,c("Hair.Red", "Hair.Blond")]) - sum(HEC[c("Eye.Brown", "Eye.Blue"),c("Hair.Red", "Hair.Blond")])

# 3.f 
# If the attributes are independent the following relation should hold
# p(Blue Eyes, Blond Hair) == p(Blue Eyes)p(Blond Hair)
HEC["Eye.Blue", "Hair.Blond"] == sum(HEC["Eye.Blue",]) * sum(HEC[,"Hair.Blond"])
# proof by contradiction
