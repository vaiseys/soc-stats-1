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

## add predictions (didn't use but check em out)
d$yhat_sat <- predict(linreg)
d$yhat_res <- predict(linreg2)

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

# randomness

rdf <- tibble(
  y = rnorm(4000, 0, 1),
  weekly = rbinom(4000, 1, .16),
  college = rbinom(4000, 1, .356)
)

rm1 <- lm(y ~ weekly + college,
          data = rdf)

rm2 <- lm(y ~ weekly * college,
          data = rdf)

rm0 <- lm(y ~ 1,
          data = rdf)

logLik(rm0)
logLik(rm1)
logLik(rm2)

AIC(rm0, rm1, rm2)
anova(rm0, rm1, rm2, test = "Chisq")





