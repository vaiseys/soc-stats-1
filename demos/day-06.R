library(tidyverse)
theme_set(theme_minimal())

# set up simulation
num_sims <- 500      # how many do I want to do?
poll_size <- 900   # number of people in each "poll"

sims <- tibble(sim_num = 1:num_sims) |> 
  uncount(poll_size)

# inspect simulation "skeleton"
sims

# draw the samples
sims <- sims |> 
  mutate(vote = sample(c("Dem", "Rep"),
                       size = num_sims*poll_size,
                       prob = c(.52, .48),
                       replace = TRUE)
  )

# summarize the results
results <- sims |> 
  mutate(dem_vote = if_else(vote == "Dem", 1L, 0L )) |> 
  group_by(sim_num) |> 
  summarize(dem_prop = mean(dem_vote))

# plot the results
ggplot(results,
       aes(x = dem_prop)) +
  geom_histogram(color = "white",
                 boundary = .5,
                 binwidth = .01)

# how often does the poll predict the winner?
results <- results |> 
  mutate(dem_win = if_else(dem_prop > .5, 1L, 0L))

results |> 
  summarise(prop_correct = mean(dem_win))

# shade the same plot
results <- results |> 
  mutate(winner = if_else(dem_win == 1, "Dem", "Rep"))

ggplot(results,
       aes(x = dem_prop,
           fill = winner)) +
         geom_histogram(color = "white",
                        boundary = .5,
                        binwidth = .01) 
