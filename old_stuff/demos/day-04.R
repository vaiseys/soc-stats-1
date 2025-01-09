library(tidyverse)
library(nycflights13)

data(flights)

table(flights$dest)
table(flights$origin)
table(flights$dest, 
      flights$origin)

flights |> count(origin, dest)

flights |> 
  group_by(origin) |> 
  summarize(count = n())

flights |> 
  summarize(count = n())

flights |>
  group_by(dest) |> 
  summarize(count = n())

d_numflights <- flights |> 
  group_by(origin) |> 
  summarize(nflights = n())

data(mtcars)
mtcars_tbl <- as_tibble(mtcars, 
                        rownames = "car")

flights <- flights |>
  mutate(late = if_else(dep_delay > 0, 1L, 0L))

flights |>
  drop_na() |> 
  group_by(carrier) |> 
  summarize(prop_late = mean(late)) |> 
  arrange(desc(prop_late))


flights_jfk <- flights |> 
  filter(origin == "JFK")

flight_selected <- flights |> 
  select(origin, dest, carrier, late)

flights_s2 <- flights |> 
  select(-year)

flights_s3 <- flights |> 
  select(dep_time:carrier, origin, dest)
