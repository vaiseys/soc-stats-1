library(tidyverse)
library(here)
library(broom)

d <- haven::read_dta(here("data", "admissions.dta")) |> 
  haven::zap_labels()

glimpse(d)

m1 <- glm(admitted ~ ver100 + qua100 + ana + meanletter,
          data = d,
          family = binomial)

tidy(m1)