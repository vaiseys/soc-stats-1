library(tidyverse)
library(infer)
library(janitor)
theme_set(theme_light())

## data prep
library(gssr)
gss18 <- gss_get_yr(2018) |> 
  haven::zap_labels()

d <- gss18 |> 
  mutate(female = if_else(sex == 2, 1L, 0L),
         weekly = if_else(attend >= 7, 1L, 0L)) |> 
  select(female, weekly) |> 
  drop_na() |> 
  mutate(sex = if_else(female == 1, "Female", "Male"))

# cross-tab
d |> 
  tabyl(female, weekly) |> 
  adorn_percentages("row") |>
  adorn_pct_formatting(digits = 2) |> 
  adorn_ns()

# picture
d |> 
  group_by(sex) |> 
  summarize(percent = mean(weekly)*100) |> 
  ggplot(aes(x = sex,
             y = percent,
             fill = sex)) +
  geom_col() +
  coord_flip()

# diff in props
## confint
set.seed(12345)
boot_dist <- d |> 
  specify(weekly ~ sex) |> 
  generate(reps = 1000,
           type = "bootstrap") |> 
  calculate(stat = "diff in means",
            order = c("Female", "Male"))

ci <- boot_dist |> 
  get_confidence_interval(level = .95)
ci

boot_dist |> 
  visualize() +
  shade_ci(ci)


## h test
## new concept: permutation
obs_diff = mean(d$weekly[d$sex=="Female"]) - mean(d$weekly[d$sex=="Male"])

null_dist <- d |> 
  specify(weekly ~ sex) |> 
  hypothesise(null = "independence") |> 
  generate(reps = 1000,
           type = "permute") |> 
  calculate(stat = "diff in means",
            order = c("Female", "Male")) 

null_dist |> 
  get_p_value(obs_diff, 
              direction = "both")

null_dist |> 
  visualize() +
  shade_p_value(obs_diff,
                direction = "both")


### CHI-SQUARE ####
d <- d |> 
  mutate(church = if_else(weekly == 1, "Weekly", "Less Often"))

d |> 
  tabyl(sex)

d |> 
  tabyl(church)

## expected values under independence
0.5510292*0.7770154 * nrow(d)
0.4489708*0.7770154 * nrow(d)
0.5510292*0.2229846 * nrow(d)
0.4489708*0.2229846 * nrow(d)

# how it works
## notice how the marginal counts (row and column totals) don't change
d |> 
  specify(church ~ sex,
          success = "Weekly") |>
  hypothesise(null = "independence") |> 
  generate(reps = 1,
           type = "permute") |> 
  tabyl(sex, church) |> 
  adorn_totals(c("row", "col"))

## do it
obs_chi_square <- d |> 
  specify(church ~ sex,
          success = "Weekly") |>
  hypothesise(null = "independence") |> 
  calculate(stat = "Chisq")
obs_chi_square

## h-test
null_dist <- d |> 
  specify(church ~ sex,
          success = "Weekly") |>
  hypothesise(null = "independence") |> 
  generate(reps = 1000,
           type = "permute") |> 
  calculate(stat = "Chisq") 

null_dist |> 
  get_p_value(obs_chi_square,
              direction = "greater")

null_dist |> 
  visualize() +
  shade_p_value(obs_chi_square,
                direction = "greater")


### extending beyond 2x2 ####






