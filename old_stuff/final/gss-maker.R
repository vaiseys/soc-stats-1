library(tidyverse)
library(gssr)

gss2022 <- gss_get_yr(2022) |> 
  haven::zap_labels() |> 
  select(id, educ, age, region, degree, polviews, partyid, sex, attend, mode) |> 
  drop_na() |> 
  mutate(college = if_else(degree >= 3, "college", "no college"),
         weekly = if_else(attend >= 7, "weekly", "not weekly"),
         sex = if_else(sex == 2, "female", "male"),
         mode = if_else(mode == 4, "web", "not web"))

gss2022$college <- relevel(as.factor(gss2022$college), ref = "no college")

save(gss2022, file = "gss2022.Rdata")
