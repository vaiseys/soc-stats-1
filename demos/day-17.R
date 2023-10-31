library(tidyverse)
library(janitor)

d <- tribble(
  ~gre, ~letters, ~admitted, ~n,
  0, 0, 0, 126,
  0, 0, 1,   3,
  0, 1, 0,  89,
  0, 1, 1,  13,
  1, 0, 0,  11,
  1, 0, 1,   3,
  1, 1, 0,  16,
  1, 1, 1,  12
) |> 
  uncount(n)

mod1 <- glm(admitted ~ gre + letters,
            data = d,
            family = binomial)
broom::tidy(mod1)

mod2 <- glm(admitted ~ gre * letters,
            data = d,
            family = binomial)
broom::tidy(mod2)

LL1 <- logLik(mod1)
LL2 <- logLik(mod2)

anova(mod1,mod2)
library(performance)

compare_performance(mod1, mod2)
