library(tidyverse)
theme_set(theme_light())

# let's recap

# football field simulation people
# what happens as I change the number of people?

npeople <- 1000
nflips <- 50

foot_sims <- 
  expand_grid(person_id = 1:npeople,
              flip_id = 1:nflips) |> 
  mutate(flip = sample(c(-1,1),
                       replace = TRUE,
                       size = npeople*nflips, 
                       prob = c(.5, .5))) |>
  group_by(person_id) |> 
  summarize(location = sum(flip) + 50)

# plot
ggplot(foot_sims,
       aes(x = location)) +
  geom_bar()

# going to infinity using dbinom

# what we expect to happen
bin_probs <- tibble(
  heads = 0:50,
  probability = dbinom(heads, 50, .5) # we will say more soon
)
  
ggplot(bin_probs,
       aes(x = heads,
           y = probability)) +
  geom_line()

# convert probabilities into numbers of people
# convert heads into locations
bin_probs <- bin_probs |> 
  mutate(people = probability*npeople,
         location = (heads-(.5*nflips))*2 + 50 )

# collapsing down the simulated data
# "how many people at each location on the field?"
sim_loc_counts <- foot_sims |> 
  group_by(location) |> 
  summarize(people = n())

# overlay the two
ggplot(bin_probs,
       aes(x = location,
           y = people)) +
  geom_line(color = "blue",
            linewidth = 1) +
  geom_col(data = sim_loc_counts,
           mapping = aes(x = location,
                         y = people),
           alpha = .8)

### Likelihood ####
# probability: what will happen given that the world is LIKE THIS
# likelihood: what is likely true about the world given these data?

# back to sampling conservatives
# we got 737 conservatives out of 2247 respondents
# what does that mean about the "true" proportion?
# last time we got a confidence interval
# how does that relate to likelihood?

hyp_33 <- dbinom(737, 2247, .33)
hyp_32 <- dbinom(737, 2247, .32)

hyp_33 # prob of getting 737 cons from 2247 r's with p = .33
hyp_32 # prob of getting 737 cons from 2247 r's with p = .32

hyp_33 / hyp_32 # factor MORE LIKELY to be the correct value

hyp_328 <- dbinom(737, 2247, .328)
hyp_327 <- dbinom(737, 2247, .327)

hyp_328
hyp_327

hyp_328 / hyp_327






