library(tidyverse)

d <- gss2022 |> 
  select(tvhours, age) |> 
  drop_na() |> 
  mutate(age3 = case_when(
    age < 40 ~ "young",
    age >= 40 & age < 60 ~ "middle",
    age >= 60 ~ "older"),
    age3_num = case_when(
      age < 40 ~ 0,
      age >= 40 & age < 60 ~ 1,
      age >= 60 ~ 2))

d |> 
  group_by(age3) |> 
  summarize(mean_y = mean(tvhours))
# character version

m_chr <- lm(tvhours ~ age3,
        data = d)

summary(m_chr)

# numeric version
m_num <- lm(tvhours ~ factor(age3_num),
            data = d)

summary(m_num)



