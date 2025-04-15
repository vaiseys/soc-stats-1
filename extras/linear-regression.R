library(tidyverse)
library(broom)
library(ggeffects)

data(bikes, package = "bayesrules")

# dist of outcome
ggplot(bikes,
       aes(x = rides)) +
  geom_histogram(binwidth = 250,
                 boundary = 0,
                 color = "white") +
  theme_light()

# dist of main predictor
ggplot(bikes,
aes(x = temp_feel)) +
  geom_histogram(binwidth = 5,
                 boundary = 0,
                 color = "white") +
  theme_light()

# scatterplot
p <- ggplot(bikes,
            aes(x = temp_feel,
                y = rides)) +
  geom_point(alpha = .4) +
  theme_light()

p

p + 
  geom_smooth(
    method = "lm",
    se = FALSE
)

p + 
  geom_smooth(
    method = "lm",
    se = TRUE
)

p + 
  geom_smooth(
    method = "lm",
    se = TRUE
) +
  xlim(0, 95)

m1 <- lm(
  rides ~ temp_feel,
  data = bikes
)

tidy(m1)

# celsius
bikes <- bikes |> 
  mutate(temp_feel_c = (temp_feel - 32) * 5/9)

m2 <- lm(
  rides ~ temp_feel_c,
  data = bikes
)

tidy(m2)

ggplot(bikes,
       aes(x = temp_feel_c,
           y = rides)) +
  geom_point(alpha = .4) +
  theme_light() +
  geom_smooth(method = "lm")

# normalization
bikes <- bikes |> 
  mutate(temp_feel_01 = (temp_feel - min(temp_feel)) /
           (max(temp_feel) - min(temp_feel)))

m3 <- lm(rides ~ temp_feel_01,
         data = bikes)

tidy(m3)

ggplot(bikes,
       aes(x = temp_feel_01,
           y = rides)) +
  geom_point(alpha = .4) +
  theme_light() +
  geom_smooth(method = "lm")

# standardization
bikes <- bikes |> 
  mutate(temp_feel_z = (temp_feel - mean(temp_feel)) /
           sd(temp_feel))

m4 <- lm(rides ~ temp_feel_z,
         data = bikes)
tidy(m4)

# standardizing y and x
bikes <- bikes |> 
  mutate(rides_z = (rides - mean(rides)) /
           sd(rides))

m5 <- lm(rides_z ~ temp_feel_z,
         data = bikes)
tidy(m5)

ggplot(bikes,
       aes(x = temp_feel_z,
           y = rides_z)) +
  geom_point(alpha = .4) +
  theme_light() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

# reverse the order
m6 <- lm(temp_feel_z ~ rides_z,
         data = bikes)
tidy(m6)

# "regression"
ggplot(bikes,
       aes(x = temp_feel_z,
           y = rides_z)) +
  geom_point(alpha = .4) +
  theme_light() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = 2)

# logit
bikes <- bikes |> 
  mutate(hi_ride = if_else(rides > 5000, 1, 0))

binmod1 <- glm(hi_ride ~ temp_feel_z,
               data = bikes,
               family = binomial())
tidy(binmod1)
