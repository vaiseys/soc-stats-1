library(tidyverse)
theme_set(theme_light())

# plot likelihood
plot <- ggplot() +
  xlim(c(0,1)) +
  geom_function(fun = \(prob) dbinom(x = 7,
                                     size = 22,
                                     prob = prob),
                n = 1000)
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

## so you can see
ggplot(sims,
       aes(x = successes)) +
  geom_bar()

## divide by denom for probabilities
qbinom(p = c(.025, .975), 
       size = 22,
       prob = 7/22) / 22

# this is "lumpy" because the underlying process has a denom of 22, which
# has nothing to do with the true prob. in the world! The options:

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
phat <- 7/22
sd <- sqrt(phat * (1 - phat))
se <- sd / sqrt(22)

ub <- phat + 1.96*se
lb <- phat - 1.96*se

ci_clt <- c(lb, ub)
ci_clt

# likelihood based grid approximation
## reminder: what is an integral? why does it matter here?
likelihoods <- tibble(
  prob = seq(from = 0,
             to =   1,
             by =    .001)) |> 
  mutate(like = dbinom(7, 22, prob),
         clike_raw = cumsum(like),
         clike_normalized = clike_raw / sum(like))

ggplot(likelihoods,
       aes(x = prob,
           y = like)) +
  geom_line()

ggplot(likelihoods,
       aes(x = prob,
           y = clike_raw)) +
  geom_line()

ggplot(likelihoods,
       aes(x = prob,
           y = clike_normalized)) +
  geom_line()

# get the CI
ll_ci <- likelihoods|> 
  filter(clike_normalized >= .025 & 
           clike_normalized <= .975) |> 
  summarize(lb = min(prob),
            ub = max(prob)) |> 
  as.numeric()

ll_ci # here it is

# add the lines to the plot
## see how it's more symmetrical?
plot + geom_vline(xintercept = ll_ci,
               linetype = "dashed")
