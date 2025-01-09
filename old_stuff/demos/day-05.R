library(tidyverse)

coins <- tibble(
  coin1 = c(1, 0, 1, 0),
  coin2 = c(1, 0, 0, 1)
)

coins <- coins |> 
  mutate(num_heads = coin1 + coin2)

durham <- tibble(
  female = rbinom(285000, 1, .52)
)

# small sample
durham_sample <- durham |> 
  slice_sample(n = 1000,
               replace = FALSE)

durham_sample |> 
  group_by(female) |> 
  summarize(count = n())

# full census
durham_census <- durham |> 
  slice_sample(n = 285000,
               replace = FALSE)

durham_census |> 
  group_by(female) |> 
  summarize(count = n())


# simulations
one_durham_sample <- tibble(
  female = rbinom(100, 1, .52)
)

set.seed(1234)
sims <- tibble(
  sim_num = 1:3000,
  females = rbinom(3000, 10000, .52)
)

ggplot(data = sims,
       mapping = aes(x = females)) +
  geom_bar()

ggplot(data = sims,
       mapping = aes(x = females)) +
  geom_histogram(color = "white",
                 boundary = 5200,
                 binwidth = 20)




