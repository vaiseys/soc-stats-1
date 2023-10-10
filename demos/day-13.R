library(tidyverse)
library(infer)
library(janitor)
theme_set(theme_light())

# 4x3 table; marginal, joint, conditional probability

## data prep
library(gssr)
gss18 <- gss_get_yr(2018) |> 
  haven::zap_labels()

d1 <- gss18 |> 
  select(degree, natenvir) |> 
  drop_na()

## marginal probabilities
tabyl(d1, degree)
tabyl(d1, natenvir)

## joint probability
tabyl(d1, degree, natenvir) |> 
  adorn_percentages(denominator = "all") |> 
  adorn_pct_formatting(digits = 1)

## rowwise conditional probabilities
tabyl(d1, degree, natenvir) |> 
  adorn_percentages() |> 
  adorn_pct_formatting(digits = 0)

## chi2
tabyl(d1, degree, natenvir) |> 
  chisq.test()

# four ways of quantifying dependence
## data prep
d2 <- gss18 |> 
  select(sex, xmarsex) |> 
  drop_na() |> 
  mutate(female = if_else(sex == 2, 1L, 0L),
         sex = if_else(female == 1, "Female", "Male"),
         xmarsex = if_else(xmarsex == 2, 1L, 0L),
         xmarsex_chr = if_else(xmarsex == 1, "Yes", "No")) |>
  relocate(sex, female, xmarsex_chr, xmarsex)

## tables
### marginals
tabyl(d2, sex)
tabyl(d2, xmarsex_chr)

### cross-tab
tabyl(d2, sex, xmarsex_chr)

### joint probability
tabyl(d2, sex, xmarsex_chr) |>
  adorn_percentages(denominator = "all") |> 
  adorn_pct_formatting(digits = 0)

### conditional probabilities
#### row
tabyl(d2, sex, xmarsex_chr) |>
  adorn_percentages(denominator = "row") |> 
  adorn_pct_formatting(digits = 0)

#### column
tabyl(d2, sex, xmarsex_chr) |>
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting(digits = 0)


## [GO TO SLIDES]
### WAY 1: difference in probabilities
### WAY 2: risk ratio
### WAY 3: odds ratio
### WAY 4: log odds ratio

## REMEMBER
### log odds are for a single probability
### log odds RATIOS are comparing two groups

ggplot() +
  xlim(c(.001,.999)) +
  geom_function(fun = \(prob) log(prob / (1-prob))) +
  labs(x = "Probability",
       y = "Log Odds")

ggplot() +
  xlim(c(-5,5)) +
  geom_function(fun = \(logodds) exp(logodds) / (1 + exp(logodds)) ) +
  labs(x = "Log Odds",
       y = "Probability")


