library(tidyverse)
theme_set(theme_light())

# Law of Large Numbers
lln_sim <- tibble(
  n = 2^(3:14),
  est_prop = rbinom(12, n, .5) / n
)

ggplot(lln_sim,
       aes(x = n,
           y = est_prop)) +
  geom_line(alpha = .8) +
  geom_point(alpha = .2) +
  geom_hline(yintercept = .5,
             linetype = "dashed",
             alpha = .2) +
  ylim(.2, .6)


# last time from GSS we got ~.33 estimate of the proportion conservative
# how confident should we be in this estimate?

# set up simulation "dresser"
set.seed(123)
est_prop <- .33
num_sims <- 10000 # how many do I want to do?
svy_size <- 2247  # number of people in each "poll"

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

# a different visualization
ggplot(sims,
       aes(x = prop)) +
  geom_density(color = NA,
               fill = "blue",
               alpha = .5) +
  scale_x_continuous(breaks = seq(.3, .4, .01)) # don't worry too much

# let's characterize the simulation results
mean(sims$prop)
sd(sims$prop)

# confidence interval
# boundaries of a integral that bounds (usually) a % of the estimates

# 95%
lower_bound95 <- quantile(sims$prop, .025)
upper_bound95 <- quantile(sims$prop, .975)
ci95 <- c(lower_bound95, upper_bound95)
ci95

# 99%
lower_bound99 <- quantile(sims$prop, .005)
upper_bound99 <- quantile(sims$prop, .995)
ci99 <- c(lower_bound99, upper_bound99)
ci99

# we don't really know if .33 is the "right" place to start
# but the *spread* of that sampling distribution is based on the sample size
# the SD of the sampling dist. is called the "standard error"

# for analytical (i.e., not simulation) results, we need normal distribution
# write a "football field" simulation to see where it comes from!

# <YOUR SIMULATION HERE>

# way 1
locs <- tibble(
  pid = 1:100) |> 
  rowwise() |> 
  mutate(loc = 50 + sum(sample(c(-1,1), 50, replace = TRUE)))

# way 2
foot_sims <- tibble(
  pid = 1:100
) |> 
  uncount(50) |> 
  mutate(flip = sample(c(-1,1),
                       replace = TRUE,
                       size = 5000, 
                       prob = c(.5, .5))) |> 
  group_by(pid) |> 
  summarize(location = sum(flip) + 50)

# way 3
foot_sims <- 
  expand_grid(person_id = 1:1000,
              flip_id = 1:50) |> 
  mutate(flip = sample(c(-1,1),
                       replace = TRUE,
                       size = 50000, 
                       prob = c(.5, .5))) |>
  group_by(person_id) |> 
  summarize(location = sum(flip) + 50)

# plot
ggplot(foot_sims,
       aes(x = location)) +
  geom_bar()

# central limit theorem: things get normal
# "analytic" CI for proportion
  




