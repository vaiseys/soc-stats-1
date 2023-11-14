library(tidyverse)
library(ggeffects)
library(performance)
library(here)
theme_set(theme_light())

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