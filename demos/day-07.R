# for this you will need Kieran's gssr package
# you will need the remotes package (install.packages("remotes"))
# then type: remotes::install_github("kjhealy/gssr")

library(tidyverse)
library(gssr)  
theme_set(theme_minimal())

# What proportion of American adults identifies as conservative?
gss18 <- gss_get_yr(2018)

ggplot(gss18,
       aes(x = polviews)) +
  geom_bar()

prop_polviews <- gss18 |>
  select(polviews) |> 
  drop_na() |> 
  group_by(polviews) |> 
  summarize(count = n()) |> 
  mutate(prop = count/sum(count))

prop_polviews

ggplot(prop_polviews,
       aes(x = polviews,
           y = prop)) +
  geom_col()

gss_polviews <- gss18 |> 
  select(polviews) |> 
  drop_na() |> 
  mutate(conservative = if_else(polviews > 4, 1L, 0L))

est_prop <- mean(gss_polviews$conservative)
est_prop

# how confident should we be in this estimate?
# do a simulation

# set up simulation "dresser"
num_sims <- 10000                 # how many do I want to do?
svy_size <- nrow(gss_polviews)    # number of people in each "poll"

sims <- tibble(sim_num = 1:num_sims) |> 
  uncount(svy_size)

# do the sims
sims <- sims |> 
  mutate(conservative = rbinom(num_sims*svy_size, 1, est_prop)) |> 
  group_by(sim_num) |> 
  summarize(prop = mean(conservative))

# visualize the results (probability given p)
ggplot(sims,
       aes(x = prop)) +
  geom_histogram(boundary = round(est_prop, 2), # round to nearest pct. point
                 binwidth = .005,
                 color = "white") +
  scale_x_continuous(breaks = seq(.3, .4, .01)) # don't worry too much

# TAKE HOME POINT: even if we KNOW exactly what the population parameter is
# we will still experience variability in the realized samples

# let's characterize the simulation results
mean(sims$prop)
sd(sims$prop)



