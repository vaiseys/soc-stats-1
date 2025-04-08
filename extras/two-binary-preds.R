library(tidyverse)
library(here)
library(broom)

gss <- readRDS(here("data", "gss2022.RDS"))

d <- gss |> 
  select(polviews,
         degree,
         attend) |>
  drop_na() |> 
  mutate(conserv = if_else(polviews > 4, 1, 0),
         weekly = if_else(attend >= 7, 1, 0),
         college = if_else(degree >= 3, 1, 0))

# table of probs
dt <- d |> 
  summarize(n = n(),
            pcons = mean(conserv),
            .by = c(weekly, college)) |> 
  arrange(weekly, college)

# identity link model (LPM)
## solve for alpha, b1, b2, b3
dt 

## confirm
lpm <- lm(conserv ~ college * weekly,
          data = d)

tidy(lpm) |> select(term, estimate)

# logit model
## add log odds
dt <- dt |> 
  mutate(logodds = log(pcons / (1 - pcons)))

## solve for alpha, b1, b2, b3
dt 

## confirm
logitm <- glm(conserv ~ college * weekly,
              data = d,
              family = binomial(link = "logit"))

tidy(logitm) |> select(term, estimate)

# identity/Normal

## get table of means
dt2 <- d |> 
  summarize(m = mean(polviews),
            .by = c(weekly, college)) |> 
  arrange(weekly, college)

## solve for a, b1, b2, b3
dt2

## confirm
linreg <- lm(polviews ~ weekly * college,
             data = d)

tidy(linreg) |> select(term, estimate)

# restricted
## model
linreg2 <- lm(polviews ~ weekly + college,
              data = d)

tidy(linreg2) |> select(term, estimate)

## check likelihoods
logLik(linreg)
logLik(linreg2)

# model selection
## likelihood ratio test
anova(linreg2, linreg, test = "Chisq")

## AIC
### for saturated model
-2 * as.numeric(logLik(linreg)) + (2 * 5)
AIC(linreg)

### for restricted
-2 * as.numeric(logLik(linreg2)) + (2 * 4)
AIC(linreg2)

# model selection for 

# randomness
## generate fake data
rdf <- tibble(
  weekly = rbinom(4000, 1, .16),             # fake weekly variable
  college = rbinom(4000, 1, .356),           # fake college variable
  pcons = .31 + (0 * weekly) + (0 * college) # no effect on prob. conserv.
) |> 
  rowwise() |> 
  mutate(conserv = rbinom(1, 1, pcons)) # random answer (31% chance!)

## estimate models
rm_sat <- glm(conserv ~ weekly * college,
              data = rdf,
              family = binomial(link = "logit"))

rm_res <- glm(conserv ~ weekly + college,
              data = rdf,
              family = binomial(link = "logit"))

rm_null <- glm(conserv ~ 1,
              data = rdf,
              family = binomial(link = "logit"))

## likelihood always gets better with more params
logLik(rm_null)
logLik(rm_res)
logLik(rm_sat)

## AIC (lower = better) and LRT (null hypothesis betas are zero)
AIC(rm_null, rm_res, rm_sat)
anova(rm_null, rm_res, rm_sat, test = "LRT")





