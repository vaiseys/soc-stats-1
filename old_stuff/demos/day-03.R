library(tidyverse)

data(mtcars)

mean(mtcars$mpg)

mtcars$mpg |> mean()
mtcars |> filter(am == 1) |> pull(mpg) |> mean()

mtcars |> pull(hp)

library(palmerpenguins)
data(penguins)

library(openintro)
data(bac)
data("biontech_adolescents")

table(biontech_adolescents$outcome)

table(biontech_adolescents$group, 
      biontech_adolescents$outcome)

library(haven)
gss2022 <- read_dta("data/GSS2022.dta") |> 
  zap_labels()

glimpse(gss2022)



  