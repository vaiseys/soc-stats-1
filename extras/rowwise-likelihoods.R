library(tidyverse)

## binary example

data(penguins, package = "palmerpenguins")

# keep sex variable and rename
d <- 
  penguins |> 
  select(sex) |> 
  drop_na() |> 
  mutate(y = if_else(sex == "female", 1, 0))

# logistic regression model
m <- 
  glm(y ~ 1,
      data = d,
      family = binomial(link = "logit"))

# add prediction (est. of p) to the df
d <- 
  d |> 
  mutate(predprob = predict(m, 
                            type = "response"))

# add rowwise (log) likelihoods to df
d <- 
  d |> 
  mutate(l = dbinom(y, 1, predprob),  # likelihood
         ll = log(l))                 # log likelihood

sum(d$ll)       # sum of the LOG likelihoods
logLik(m)       # same from stats::logLik() for the model


## continuous example

# get outcome variable
d <- 
  penguins |> 
  select(body_mass_g) |> 
  drop_na() |> 
  rename(bodymass = body_mass_g)

# linear "regression"
m <- 
  lm(bodymass ~ 1,
     data = d)

# add prediction (mean) to df
d <- 
  d |> 
  mutate(mean = predict(m),
         sd = sd(m$residuals))

# add rowwise (log) likelihoods to df
d <- 
  d |> 
  mutate(l  = dnorm(bodymass,      # DENSITY at point in the dist.
                    mean = mean,
                    sd = sd),
         ll = dnorm(bodymass,      # more precise to do via dnorm()
                    mean = mean,
                    sd = sd,
                    log = TRUE))

sum(d$ll)       # sum of the LOG likelihoods
logLik(m)       # same from stats::logLik() for the model

