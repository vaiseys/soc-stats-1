library(tidyverse)
library(here)
library(broom)
library(performance)
library(janitor)
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

# adding predictors (TBD in class)



