library(tidyverse)
library(broom)

## first example

gss2022 <- readRDS("data/gss2022.RDS")

d <- gss2022 |> 
  select(region, degree) |> 
  drop_na() |> 
  slice_sample(n = 1000)

t <- table(d$degree,
           d$region)

dcounts <- d |> 
  summarize(count = n(), .by = c(degree, region))

m1 <- glm(count ~ factor(degree) + factor(region),
          family = poisson(),
          data = dcounts)

m2 <- glm(count ~ factor(degree) * factor(region),
          family = poisson(),
          data = dcounts)

chisq.test(t)
anova(m1, m2, test = "Rao")

## another example

d2 <- gss2022 |> 
  select(abany, sex) |> 
  drop_na() |> 
  slice_sample(n = 200)

t2 <- table(d2$abany,
            d2$sex)

dcounts2 <- d2 |> 
  summarize(count = n(), .by = c(abany, sex))

m21 <- glm(count ~ factor(abany) + factor(sex),
           family = poisson(),
           data = dcounts2)

m22 <- glm(count ~ factor(abany) * factor(sex),
           family = poisson(),
           data = dcounts2)

chisq.test(t2, correct = FALSE)
anova(m21, m22, test = "Rao")
