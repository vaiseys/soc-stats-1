library(tidyverse)
library(here)
library(broom)
library(performance)
theme_set(theme_light())

d <- haven::read_dta(here("data", "admissions.dta")) |> 
  haven::zap_labels()

glimpse(d)

m1 <- glm(admitted ~ ver100 + qua100 + ana + meanletter,
          data = d,
          family = binomial)

tidy(m1)

### GOF 1: Hosmer-Lemeshow ####

performance_hosmer(m1, n_bins = 5)

d$pred_log_odds <- predict(m1)
d$pred_prob <- predict(m1, type = "response")
d$predbin <- cut_number(d$pred_log_odds, n = 5)

d |> 
  group_by(predbin) |> 
  summarize(est_prob = mean(pred_prob),
            obs_prob = mean(admitted))


### GOF 2: linktest ####

lt0 <- glm(admitted ~ pred_log_odds,
           data = d,
           family = binomial)
tidy(lt0)

lt1 <- glm(admitted ~ pred_log_odds + I(pred_log_odds^2),
           data = d,
           family = binomial)
tidy(lt1)


### COUNT DATA ####

# [see slides]

d <- haven::read_dta(here("data", "couart4.dta"))

d <- d |> 
  mutate(hipubment = mentor > 6)

m1 <- glm(art ~ 1,
          data = d,
          family = poisson)
tidy(m1)

lambda1 <- exp(coef(m1)) # get lambda from "model"

numarts <- d |> 
  group_by(art) |> 
  summarize(observed_prop = n() / nrow(d))

numarts <- numarts |>
  rowwise() |> 
  mutate(pois_prob = dpois(art, lambda1))

# key to tidyverse/ggplot is to always make data looooong
numarts <- numarts |> 
  pivot_longer(2:3)

# compare observed and theoretical values
ggplot(numarts,
       aes(x = art,
           group = name,
           color = name,
           y = value)) +
  geom_line()

# adding predictors
m2 <- glm(art ~ hipubment,
          data = d,
          family = poisson)
tidy(m2)

m3 <- glm(art ~ mentor + female + married + phdcat,
          data = d,
          family = poisson)
tidy(m3)

m4 <- glm(art ~ mentor + female + married + factor(phdcat),
          data = d,
          family = poisson)
tidy(m4)


m5 <- glm(art ~ mentor + female * married + kid5 + kid5:female + 
            factor(phdcat),
          data = d,
          family = poisson)
tidy(m5)

m6 <- glm(art ~ mentor + female + married + kid5 + factor(phdcat),
          data = d,
          family = poisson)
tidy(m6)

BIC(m1, m2, m3, m4, m5, m6)

d$pred_log_arts <- predict(m6)
lt6 <- glm(art ~ pred_log_arts + I(pred_log_arts^2),
           data = d,
           family = poisson)
tidy(lt6)

check_model(m6)

### BONUS: Negative Binomial ####

m7 <- MASS::glm.nb(art ~ mentor + female + married + kid5 + factor(phdcat),
                   data = d)
tidy(m7)
check_model(m7)

d$pred_m7 <- predict(m7)
lt7 <- MASS::glm.nb(art ~ pred_m7 + I(pred_m7^2),
                    data = d)
tidy(lt7) # still doesn't fit well...

library(modelsummary)
msummary(list(m6,m7)) # very similar coefficient estimates

