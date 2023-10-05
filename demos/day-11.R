### resampling, CIs, and hypothesis testing ####

library(tidyverse)
theme_set(theme_light())

# IDEA: resampling from your data is a way to simulate the sampling distribution

# pretend "original" data (n = 75)
d <- tibble(
  republican = c(rep(0,45),                       # Dem. voter (rep() means repeat though!)
                 rep(1,30))                       # Rep. voter
)

# point estimate ("maximum likelihood")
mean(d$republican)

# resampling function
resample <- function(x) sample(x,                 # sample from this input
                               length(x),         # the same size as the original
                               replace = TRUE)    # with replacement

# resamples frame
resamples <- tibble(sample = 1:10000) |>
  rowwise() |> 
  mutate(data = list(resample(d$republican)),   # list column (I will discuss)
         est_prop = mean(data)) 

# plot the CI
ggplot(resamples,
       aes(x = est_prop)) +
  geom_histogram(boundary = .5,
                 binwidth = .025,
                 color = "white") +
  scale_x_continuous(breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  geom_vline(xintercept = quantile(resamples$est_prop, c(.025, .975)),
             color = "firebrick",
             linetype = "dashed")

# hypothesis test
## null hypothesis (H0): proportion = .5
## alternative hypothesis (Ha): proportion <.5
## alpha level (e.g., .05): the complement of the CI width
## p-value: what is the probability of getting a result "at least as extreme" if the null is true?

nulls <- tibble(sample = 1:10000) |>
  rowwise() |> 
  mutate(nulldata = list(rbinom(75, 1, .5)),
         null_prop = mean(nulldata))

# visualize
ggplot(nulls,
       aes(x = null_prop)) +
  geom_histogram(boundary = .5,
                 binwidth = .025,
                 color = "white") +
  scale_x_continuous(breaks = seq(0, 1, .05),
                     limits = c(0, 1)) +
  geom_vline(xintercept = mean(d$republican),
             color = "firebrick",
             linetype = "dashed")

# how many times do we get a result as low as the real data if null is true?
pvalue <- nrow(filter(nulls, null_prop <= mean(d$republican))) /
  nrow(nulls)
pvalue

# this gives the one-sided p-value; the two-sided p-value is
pvalue*2


### EXTENDING TO TWO SAMPLES ####

## data prep
library(gssr)
gss18 <- gss_get_yr(2018) |> 
  haven::zap_labels()

d <- gss18 |> 
  mutate(female = if_else(sex == 2, 1L, 0L),
         conservative = if_else(polviews > 4, 1L, 0L)) |> 
  select(female, conservative) |> 
  drop_na()

## group proportions
d |> 
  group_by(female) |> 
  summarize(prop_cons = mean(conservative),
            samp_size = n())

## CI of the difference

### define function
resample_diff <- function() {
  # get male proportion conservative
  pm <- mean(sample(d$conservative[d$female==0],
                    size = length(d$conservative[d$female==0]),
                    replace = TRUE))
  # get female proportion conservative
  pf <- mean(sample(d$conservative[d$female==1],
                    size = length(d$conservative[d$female==1]),
                    replace = TRUE))
  # return the difference
  return(pm - pf)
}

### resample 10,000 times
resamples <- tibble(sample = 1:10000) |> 
  rowwise() |> 
  mutate(diff = resample_diff())

### plot
ggplot(resamples,
       aes(x = diff)) +
  geom_histogram(color = "white",
                 boundary = 0,
                 binwidth = .01) +
  geom_vline(xintercept = quantile(resamples$diff, c(.025, .975)),
             color = "firebrick",
             linetype = "dashed")

### the numbers
quantile(resamples$diff, c(.025, .975))

## hypothesis test
### things we need to do the null distribution
obs_prop <- mean(d$conservative)
n_males <- nrow(filter(d, female == 0))
n_females <- nrow(filter(d, female == 1))

### the null distribution (i.e., where the difference is really zero)
null_diff <- function() {
  mean(rbinom(n_males, 1, obs_prop)) -
    mean(rbinom(n_females, 1, obs_prop))
}

nulls <- tibble(sample = 1:10000) |> 
  rowwise() |> 
  mutate(diff = null_diff())

### the observed difference for reference
obs_diff <- 
  mean(d$conservative[d$female==0]) - 
  mean(d$conservative[d$female==1])

### visualize the null distribution and the observed difference
ggplot(nulls,
       aes(x = diff)) +
  geom_histogram(color = "white",
                 boundary = 0,
                 binwidth = .005) +
  geom_vline(xintercept = obs_diff,
             color = "firebrick",
             linetype = "dashed")

### p-value
pvalue <- 
  nrow(filter(nulls, diff >= obs_diff)) /
  nrow(nulls)

pvalue*2
