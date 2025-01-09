library(tidyverse)
library(broom)
data(bikes, package = "bayesrules")

m0 <- lm(rides ~ 1,
         data = bikes)
tidy(m0)

m1 <- lm(rides ~ temp_feel,
         data = bikes)
tidy(m1, conf.int = TRUE)

ggplot(bikes,
       aes(x = temp_feel,
           y = rides)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = .2)


m2 <- lm(rides ~ temp_feel + I(temp_feel^2),
         data = bikes)
tidy(m2)

library(ggeffects)
ggpredict(m2, terms = "temp_feel") |> plot()

bikes$weekend <- as.factor(bikes$weekend)

m3 <- lm(rides ~ (temp_feel + I(temp_feel^2)) * weekend,
         data = bikes)
tidy(m3)
ggpredict(m3, terms = c("temp_feel", "weekend")) |> plot()

BIC(m0,m1,m2,m3)

