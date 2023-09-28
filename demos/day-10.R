library(tidyverse)
theme_set(theme_light())

# plot likelihood
plot <- ggplot() +
  xlim(c(0,1)) +
  geom_function(fun = \(prob) dbinom(x = 7,
                                     size = 22,
                                     prob = prob),
                n = 1001)

plot

# simulation confint
sims <- tibble(successes = rbinom(1e6, size = 22, prob = 7/22),
               est_prob = successes / 22)

sims |> 
  summarize(lb = quantile(est_prob, .025),
            ub = quantile(est_prob, .975))

# analytic confidence intervals
# next member of the *binom family

## successes
qbinom(p = c(.025, .975), 
       size = 22,
       prob = 7/22)

## divide by denom for probabilities
qbinom(p = c(.025, .975), 
       size = 22,
       prob = 7/22) / 22

# this is "lumpy" because the underlying process has a denom of 22, which
# has nothing to do with the true prob. in the world! The options:
0:22 / 22

# plot
## save CI as a vector
ci <- qbinom(p = c(.025, .975), 
             size = 22,
             prob = 7/22) / 22

## add to plot above
## see how it's lopsided?
plot + geom_vline(xintercept = ci,
               linetype = "dashed")

# calculate with CLT
## try replacing the 7 with smaller or larger numbers (e.g., 1); what happens
p <- 7/22
sd <- sqrt(p * (1 - p))
se <- sd / sqrt(22)

ub <- p + 1.96*se
lb <- p - 1.96*se

ci_clt <- c(lb, ub)
ci_clt

# likelihood based grid approximation
## reminder: what is an integral? why does it matter here?
likelihoods <- tibble(
  prob = seq(from = .001,
             to =   .999,
             by =   .001)) |> 
  mutate(ll = dbinom(7, 22, prob),
         cll = cumsum(ll) / sum(ll))

# get the CI
likelihoods|> 
  filter(cll >= .025 & cll <= .975) |> 
  summarize(lb = min(prob),
            ub = max(prob)) |> 
  as.numeric() -> ll_ci

ll_ci # here it is

# add the lines to the plot
## see how it's more symmetrical?
plot + geom_vline(xintercept = ll_ci,
               linetype = "dashed")

# hypothesis testing
## null value (e.g., .5)
## alternative value (e.g., <.5)
## alpha level (e.g., .05); the complement of the CI width
## simple version: does the CI overlap a null value?

## more complex: resampling from original data
mydata <- tibble(
  conservative = c(rep(1, 7),
                   rep(0, 15))
)
