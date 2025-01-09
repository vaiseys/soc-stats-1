### slides: polytomous-and-multiple-predictors.pptx

library(tidyverse)
library(janitor)

mytable <- tribble(
  ~verbal, ~admitted, ~n,
  "<600",      0,    159,  
  "<600",      1,      3,
  "600-690",   0,     99,
  "600-690",   1,     14,
  "700+",      0,     36,
  "700+",      1,     20
)

d <- uncount(mytable, n)

d |> 
  tabyl(verbal, admitted) |> 
  adorn_percentages() |>
  adorn_pct_formatting()
  