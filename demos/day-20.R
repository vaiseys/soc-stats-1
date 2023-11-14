library(tidyverse)
library(ggeffects)
library(performance)
library(here)
library(broom)
theme_set(theme_light())

### DATA SET UP ####

d <- haven::read_dta(here("data", "ess_bigger_imp.dta"))

ggplot(d,
       aes(x = stflife)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Life Satisfaction",
       y = "# of Respondents",
       title = "Life Satisfaction in 29 European Countries",
       subtitle = "2014 European Social Survey")


gb <- d |> filter(cntry == "GB")

ggplot(gb,
       aes(x = stflife)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Life Satisfaction",
       y = "# of Respondents",
       title = "Life Satisfaction in Great Britain",
       subtitle = "2014 European Social Survey")

gb <- gb |> 
  select(-starts_with("c_"), 
         -cntry, 
         -country,
         -gnipc1000) |> 
  haven::zap_labels()

### LINEAR REGRESSION ####

m0 <- lm(stflife ~ 1,
         data = gb)
tidy(m0)

m0 <- glm(stflife ~ 1,
          family = gaussian,
          data = gb)

tidy(m0)      # coefficients (just intercept here)
glance(m0)    # facts about the model
sigma(m0)     # estimate of residual SD

## simple LR with 2 groups (aka t-test)
my_fake_ttest <- glm(stflife ~ female,
                     family = gaussian,
                     data = gb)
tidy(my_fake_ttest)


## ANOVA
my_fake_anova <- lm(stflife ~ factor(region),
                    data = gb)
tidy(my_fake_anova)
anova(my_fake_anova)

check_model(my_fake_anova)

## simple linear regression with numeric predictor
ggplot(gb,
       aes(x = inc,
           y = stflife)) +
  geom_smooth(method = "lm") +
  geom_jitter(alpha = .1)

m1 <- glm(stflife ~ inc,
          family = gaussian,
          data = gb)
tidy(m1, conf.int = TRUE)

# rescale variables (min/max)
gb <- gb |> 
  mutate(inc01 = (inc-1)/9 )

m1r <- glm(stflife ~ inc01,
           family = gaussian,
           data = gb)
tidy(m1r)

# rescale (z-score)
mean_inc <- mean(gb$inc)
sd_inc <- sd(gb$inc)

gb <- gb |> 
  mutate(zinc = (inc - mean_inc) / sd_inc )

m1z = glm(stflife ~ zinc,
          family = gaussian,
          data = gb)

tidy(m1z)

